#!/usr/bin/env python
"""Convert IPython notebooks to other formats, such as ReST, and HTML.

Example:
  ./nbconvert.py --format rst file.ipynb

Produces 'file.rst', along with auto-generated figure files
called nb_figure_NN.png.
"""
#-----------------------------------------------------------------------------
# Imports
#-----------------------------------------------------------------------------
from __future__ import print_function, absolute_import
# Stdlib imports
import subprocess
import copy
import json
import re
import os
import sys
import codecs
import io
import logging
import pprint
from types import FunctionType
from cStringIO import StringIO

# Third-party imports
from pygments.lexers import PythonLexer, BashLexer
from pygments.lexer import bygroups, using
from pygments.token import Keyword, Operator, Text
from markdown import markdown

# IPython imports
from IPython.utils.text import indent
from IPython.utils import py3compat
from IPython.nbformat.v3.nbjson import BytesEncoder
from IPython.external import argparse
from IPython.nbformat import current as nbformat
from IPython.utils.traitlets import List, Unicode, Type, Bool, Dict, CaselessStrEnum
from IPython.utils import path
from IPython.frontend.html.notebook import notebookapp

# All the stuff needed for the configurable things
from IPython.config.application import Application, catch_config_error
from IPython.config.configurable import Configurable, SingletonConfigurable
from IPython.config.loader import Config, ConfigFileNotFound


class IPythonLexer(PythonLexer):
    name = 'IPython'
    aliases = ['ip', 'ipython']
    filenames = ['*.ipy']
    tokens = PythonLexer.tokens.copy()
    tokens['root'] = [
        (r'(\%+)(\w+)\s+(\.*)(\n)', bygroups(Operator, Keyword,
                                             using(BashLexer), Text)),
        (r'(\%+)(\w+)\b', bygroups(Operator, Keyword)),
        (r'^(!)(.+)(\n)', bygroups(Operator, using(BashLexer), Text)),
    ] + tokens['root']

#-----------------------------------------------------------------------------
# Globals and constants
#-----------------------------------------------------------------------------
_multiline_outputs = ['text', 'html', 'svg', 'latex', 'javascript', 'json']


#-----------------------------------------------------------------------------
# Utility functions
#-----------------------------------------------------------------------------
def highlight(src, lang='ipython'):
    """
    Return a syntax-highlighted version of the input source as html output.
    """
    from pygments.formatters import HtmlFormatter
    return pygment_highlight(src, HtmlFormatter(), lang)

def highlight2latex(src, lang='ipython'):
    """
    Return a syntax-highlighted version of the input source as latex output.
    """
    from pygments.formatters import LatexFormatter
    return pygment_highlight(src, LatexFormatter(), lang)

def pygment_highlight(src, output_formatter, lang='ipython'):
    """
    Return a syntax-highlighted version of the input source
    """
    from pygments import highlight
    from pygments.lexers import get_lexer_by_name

    if lang == 'ipython':
        lexer = IPythonLexer()
    else:
        lexer = get_lexer_by_name(lang, stripall=True)

    return highlight(src, lexer, output_formatter) 

def get_lines(src, start=None,end=None):
    """
    Split the input text into separate lines and then return the 
    lines that the caller is interested in.
    """
    
    # Split the input into lines.
    lines = src.split("\n")
    
    # Return the right lines.
    return "\n".join(lines[start:end]) #re-join

def output_container(f):
    """add a prompt-area next to an output"""
    def wrapped(self, output):
        rendered = f(self, output)
        if not rendered:
            # empty output
            return []
        lines = []
        lines.append('<div class="hbox output_area">')
        lines.extend(self._out_prompt(output))
        classes = "output_subarea output_%s" % output.output_type
        if 'html' in output.keys():
            classes += ' output_html rendered_html'
        if output.output_type == 'stream':
            classes += " output_%s" % output.stream
        lines.append('<div class="%s">' % classes)
        lines.extend(rendered)
        lines.append('</div>')  # subarea
        lines.append('</div>')  # output_area

        return lines

    return wrapped


def text_cell(f):
    """wrap text cells in appropriate divs"""
    def wrapped(self, cell):
        rendered = f(self, cell)
        classes = "text_cell_render border-box-sizing rendered_html"
        lines = ['<div class="%s">' % classes] + rendered + ['</div>']
        return lines
    return wrapped


def remove_fake_files_url(cell):
    """Remove from the cell source the /files/ pseudo-path we use.
    """
    src = cell.source
    cell.source = re.sub(r"""([\(/"'])files/""", r'\1',src)


# ANSI color functions:

def remove_ansi(src):
    """Strip all ANSI color escape sequences from input string.

    Parameters
    ----------
    src : string

    Returns
    -------
    string
    """
    return re.sub(r'\033\[(0|\d;\d\d)m', '', src)


def ansi2html(txt):
    """Render ANSI colors as HTML colors

    This is equivalent to util.fixConsole in utils.js

    Parameters
    ----------
    txt : string

    Returns
    -------
    string
    """

    ansi_colormap = {
        '30': 'ansiblack',
        '31': 'ansired',
        '32': 'ansigreen',
        '33': 'ansiyellow',
        '34': 'ansiblue',
        '35': 'ansipurple',
        '36': 'ansicyan',
        '37': 'ansigrey',
        '01': 'ansibold',
    }

    # do ampersand first
    txt = txt.replace('&', '&amp;')
    html_escapes = {
        '<': '&lt;',
        '>': '&gt;',
        "'": '&apos;',
        '"': '&quot;',
        '`': '&#96;',
    }
    for c, escape in html_escapes.iteritems():
        txt = txt.replace(c, escape)

    ansi_re = re.compile('\x1b' + r'\[([\dA-Fa-f;]*?)m')
    m = ansi_re.search(txt)
    opened = False
    cmds = []
    opener = ''
    closer = ''
    while m:
        cmds = m.groups()[0].split(';')
        closer = '</span>' if opened else ''
        # True if there is there more than one element in cmds, *or*
        # if there is only one but it is not equal to a string of zeroes.
        opened = len(cmds) > 1 or cmds[0] != '0' * len(cmds[0])
        classes = []
        for cmd in cmds:
            if cmd in ansi_colormap:
                classes.append(ansi_colormap.get(cmd))

        if classes:
            opener = '<span class="%s">' % (' '.join(classes))
        else:
            opener = ''
        txt = re.sub(ansi_re, closer + opener, txt, 1)

        m = ansi_re.search(txt)

    if opened:
        txt += '</span>'
    return txt


# Pandoc-dependent code

def markdown2latex(src):
    """Convert a markdown string to LaTeX via pandoc.

    This function will raise an error if pandoc is not installed.

    Any error messages generated by pandoc are printed to stderr.

    Parameters
    ----------
    src : string
      Input string, assumed to be valid markdown.

    Returns
    -------
    out : string
      Output as returned by pandoc.
    """
    p = subprocess.Popen('pandoc -f markdown -t latex'.split(),
                         stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    out, err = p.communicate(src.encode('utf-8'))
    if err:
        print(err, file=sys.stderr)
    #print('*'*20+'\n', out, '\n'+'*'*20)  # dbg
    return unicode(out, 'utf-8')


def markdown2rst(src):
    """Convert a markdown string to LaTeX via pandoc.

    This function will raise an error if pandoc is not installed.

    Any error messages generated by pandoc are printed to stderr.

    Parameters
    ----------
    src : string
      Input string, assumed to be valid markdown.

    Returns
    -------
    out : string
      Output as returned by pandoc.
    """
    p = subprocess.Popen('pandoc -f markdown -t rst'.split(),
                         stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    out, err = p.communicate(src.encode('utf-8'))
    if err:
        print(err, file=sys.stderr)
    #print('*'*20+'\n', out, '\n'+'*'*20)  # dbg
    return unicode(out, 'utf-8')


def rst_directive(directive, text=''):
    """
    Makes ReST directive block and indents any text passed to it.
    """
    out = [directive, '']
    if text:
        out.extend([indent(text), ''])
    return out


def coalesce_streams(outputs):
    """merge consecutive sequences of stream output into single stream

    to prevent extra newlines inserted at flush calls

    TODO: handle \r deletion
    """
    new_outputs = []
    last = outputs[0]
    new_outputs = [last]
    for output in outputs[1:]:
        if (output.output_type == 'stream' and
            last.output_type == 'stream' and
            last.stream == output.stream
        ):
            last.text += output.text
        else:
            new_outputs.append(output)

    return new_outputs


def rst2simplehtml(infile):
    """Convert a rst file to simplified html suitable for blogger.

    This just runs rst2html with certain parameters to produce really simple
    html and strips the document header, so the resulting file can be easily
    pasted into a blogger edit window.
    """

    # This is the template for the rst2html call that produces the cleanest,
    # simplest html I could find.  This should help in making it easier to
    # paste into the blogspot html window, though I'm still having problems
    # with linebreaks there...
    cmd_template = ("rst2html --link-stylesheet --no-xml-declaration "
                    "--no-generator --no-datestamp --no-source-link "
                    "--no-toc-backlinks --no-section-numbering "
                    "--strip-comments ")

    cmd = "%s %s" % (cmd_template, infile)
    proc = subprocess.Popen(cmd,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE,
                            shell=True)
    html, stderr = proc.communicate()
    if stderr:
        raise IOError(stderr)

    # Make an iterator so breaking out holds state.  Our implementation of
    # searching for the html body below is basically a trivial little state
    # machine, so we need this.
    walker = iter(html.splitlines())

    # Find start of main text, break out to then print until we find end /div.
    # This may only work if there's a real title defined so we get a 'div
    # class' tag, I haven't really tried.
    for line in walker:
        if line.startswith('<body>'):
            break

    newfname = os.path.splitext(infile)[0] + '.html'
    with open(newfname, 'w') as f:
        for line in walker:
            if line.startswith('</body>'):
                break
            f.write(line)
            f.write('\n')

    return newfname


#-----------------------------------------------------------------------------
# Cell-level functions -- similar to IPython.nbformat.v3.rwbase functions
# but at cell level instead of whole notebook level
#-----------------------------------------------------------------------------

def writes_cell(cell, **kwargs):
    kwargs['cls'] = BytesEncoder
    kwargs['indent'] = 3
    kwargs['sort_keys'] = True
    kwargs['separators'] = (',', ': ')
    if kwargs.pop('split_lines', True):
        cell = split_lines_cell(copy.deepcopy(cell))
    return py3compat.str_to_unicode(json.dumps(cell, **kwargs), 'utf-8')


def split_lines_cell(cell):
    """
    Split lines within a cell as in
    IPython.nbformat.v3.rwbase.split_lines

    """
    if cell.cell_type == 'code':
        if 'input' in cell and isinstance(cell.input, basestring):
            cell.input = (cell.input + '\n').splitlines()
        for output in cell.outputs:
            for key in _multiline_outputs:
                item = output.get(key, None)
                if isinstance(item, basestring):
                    output[key] = (item + '\n').splitlines()
    else:  # text, heading cell
        for key in ['source', 'rendered']:
            item = cell.get(key, None)
            if isinstance(item, basestring):
                cell[key] = (item + '\n').splitlines()
    return cell


def cell_to_lines(cell):
    '''
    Write a cell to json, returning the split lines.
    '''
    split_lines_cell(cell)
    s = writes_cell(cell).strip()
    return s.split('\n')

def clean_filename(filename):
    """
    Remove non-alphanumeric characters from filenames.

    Parameters
    ----------
    filename : str
        The filename to be sanitized.

    Returns
    -------
    clean : str
        A sanitized filename that contains only alphanumeric
        characters and underscores.
    """
    filename = re.sub(r'[^a-zA-Z0-9_]', '_', filename)
    return filename

#-----------------------------------------------------------------------------
# nbconvert.converters.config
#-----------------------------------------------------------------------------

class GlobalConfigurable(Configurable):
    """Global configurable class for shared config

    Usefull for display data priority that might be use by many trasnformers
    """

    display_data_priority = List(['html', 'pdf', 'svg', 'latex', 'png', 'jpg', 'jpeg' , 'text'],
            config=True,
              help= """
                    An ordered list of prefered output type, the first
                    encounterd will usually be used when converting discarding
                    the others.
                    """
            )

    def __init__(self, config=None, **kw):
        super(GlobalConfigurable, self).__init__( config=config, **kw)


#-----------------------------------------------------------------------------
# nbconvert.converters.transformers
#-----------------------------------------------------------------------------
class ConfigurableTransformers(GlobalConfigurable):
    """ A configurable transformer

    Inherit from this class if you wish to have configurability for your
    transformer.

    Any configurable traitlets this class exposed will be configurable in profiles
    using c.SubClassName.atribute=value

    you can overwrite cell_transform to apply a transformation independently on each cell
    or __call__ if you prefer your own logic. See orresponding docstring for informations.


    """

    def __init__(self, config=None, **kw):
        super(ConfigurableTransformers, self).__init__(config=config, **kw)

    def __call__(self, nb, other):
        """transformation to apply on each notebook.

        received a handle to the current notebook as well as a dict of resources
        which structure depends on the transformer.

        You should return modified nb, other.

        If you wish to apply on each cell, you might want to overwrite cell_transform method.
        """
        try :
            for worksheet in nb.worksheets :
                for index, cell in enumerate(worksheet.cells):
                    worksheet.cells[index], other = self.cell_transform(cell, other, 100*index)
            return nb, other
        except NotImplementedError:
            raise NotImplementedError('should be implemented by subclass')

    def cell_transform(self, cell, other, index):
        """
        Overwrite if you want to apply a transformation on each cell,

        receive the current cell, the resource dict and the index of current cell as parameter.

        You should return modified cell and resource dict.
        """
        raise NotImplementedError('should be implemented by subclass')
        return cell, other


class ActivatableTransformer(ConfigurableTransformers):
    """A simple ConfigurableTransformers that have an enabled flag

    Inherit from that if you just want to have a transformer which is
    no-op by default but can be activated in profiles with

    c.YourTransformerName.enabled = True
    """

    enabled = Bool(False, config=True)

    def __call__(self, nb, other):
        if not self.enabled :
            return nb, other
        else :
            return super(ActivatableTransformer, self).__call__(nb, other)


def cell_preprocessor(function):
    """ wrap a function to be executed on all cells of a notebook

    wrapped function  parameters :
        cell  : the cell
        other : external resources
        index : index of the cell
    """
    def wrappedfunc(nb, other):
        for worksheet in nb.worksheets :
            for index, cell in enumerate(worksheet.cells):
                worksheet.cells[index], other = function(cell, other, index)
        return nb, other
    return wrappedfunc


@cell_preprocessor
def haspyout_transformer(cell, other, count):
    """
    Add a haspyout flag to cell that have it

    Easier for templating, where you can't know in advance
    wether to write the out prompt

    """
    cell.type = cell.cell_type
    cell.haspyout = False
    for out in cell.get('outputs', []):
        if out.output_type == 'pyout':
            cell.haspyout = True
            break
    return cell, other

@cell_preprocessor
def coalesce_cells(cell, other, count):
    """merge consecutive sequences of stream output into single stream

    to prevent extra newlines inserted at flush calls

    TODO: handle \r deletion
    """
    outputs = cell.get('outputs', [])
    if not outputs:
        return cell, other
    new_outputs = []
    last = outputs[0]
    new_outputs = [last]
    for output in outputs[1:]:
        if (output.output_type == 'stream' and
            last.output_type == 'stream' and
            last.stream == output.stream
        ):
            last.text += output.text
        else:
            new_outputs.append(output)

    cell.outputs = new_outputs
    return cell, other

class ExtractFigureTransformer(ActivatableTransformer):


    extra_ext_map =  Dict({},
            config=True,
            help="""extra map to override extension based on type.
            Usefull for latex where svg will be converted to pdf before inclusion
            """
            )

    key_format_map =  Dict({},
            config=True,
            )

    figname_format_map =  Dict({},
            config=True,
            )


    #to do change this to .format {} syntax
    default_key_tpl = Unicode('_fig_{count:02d}.{ext}', config=True)

    def _get_ext(self, ext):
        if ext in self.extra_ext_map :
            return self.extra_ext_map[ext]
        return ext

    def _new_figure(self, data, fmt, count):
        """Create a new figure file in the given format.

        """
        tplf = self.figname_format_map.get(fmt, self.default_key_tpl)
        tplk = self.key_format_map.get(fmt, self.default_key_tpl)

        # option to pass the hash as data ?
        figname = tplf.format(count=count, ext=self._get_ext(fmt))
        key     = tplk.format(count=count, ext=self._get_ext(fmt))

        # Binary files are base64-encoded, SVG is already XML
        binary = False
        if fmt in ('png', 'jpg', 'pdf'):
            data = data.decode('base64')
            binary = True

        return figname, key, data, binary


    def cell_transform(self, cell, other, count):
        if other.get('figures', None) is None :
            other['figures'] = {'text':{},'binary':{}}
        for out in cell.get('outputs', []):
            for out_type in self.display_data_priority:
                if out.hasattr(out_type):
                    figname, key, data, binary = self._new_figure(out[out_type], out_type, count)
                    out['key_'+out_type] = figname
                    if binary :
                        other['figures']['binary'][key] = data
                    else :
                        other['figures']['text'][key] = data
                    count = count+1
        return cell, other


class RevealHelpTransformer(ConfigurableTransformers):

    def __call__(self, nb, other):
        for worksheet in nb.worksheets :
            for i, cell in enumerate(worksheet.cells):
                if not cell.get('metadata', None):
                    break
                cell.metadata.slide_type = cell.metadata.get('slideshow', {}).get('slide_type', None)
                if cell.metadata.slide_type is None:
                    cell.metadata.slide_type = '-'
                if cell.metadata.slide_type in ['slide']:
                    worksheet.cells[i - 1].metadata.slide_helper = 'slide_end'
                if cell.metadata.slide_type in ['subslide']:
                    worksheet.cells[i - 1].metadata.slide_helper = 'subslide_end'
        return nb, other


class CSSHtmlHeaderTransformer(ActivatableTransformer):

    def __call__(self, nb, resources):
        """Fetch and add css to the resource dict

        Fetch css from IPython adn Pygment to add at the beginning
        of the html files.

        Add this css in resources in the "inlining.css" key
        """
        resources['inlining'] = {}
        resources['inlining']['css'] = self.header
        return nb, resources

    header = []

    def __init__(self, config=None, **kw):
        super(CSSHtmlHeaderTransformer, self).__init__(config=config, **kw)
        if self.enabled :
            self.regen_header()

    def regen_header(self):
        ## lazy load asa this might not be use in many transformers
        import os
        from IPython.utils import path
        import io
        from pygments.formatters import HtmlFormatter
        header = []
        static = os.path.join(path.get_ipython_package_dir(),
            'frontend', 'html', 'notebook', 'static',
        )
        here = os.path.split(os.path.realpath(__file__))[0]
        css = os.path.join(static, 'css')
        for sheet in [
            # do we need jquery and prettify?
            # os.path.join(static, 'jquery', 'css', 'themes', 'base',
            # 'jquery-ui.min.css'),
            # os.path.join(static, 'prettify', 'prettify.css'),
            os.path.join(css, 'boilerplate.css'),
            os.path.join(css, 'fbm.css'),
            os.path.join(css, 'notebook.css'),
            os.path.join(css, 'renderedhtml.css'),
            os.path.join(css, 'style.min.css'),
        ]:
            try:
                with io.open(sheet, encoding='utf-8') as f:
                    s = f.read()
                    header.append(s)
            except IOError:
                # new version of ipython with style.min.css, pass
                pass

        pygments_css = HtmlFormatter().get_style_defs('.highlight')
        header.append(pygments_css)
        self.header = header

#-----------------------------------------------------------------------------
# nbconvert.converters.base
#-----------------------------------------------------------------------------

class ConversionException(Exception):
    pass


class DocStringInheritor(type):
    """
    This metaclass will walk the list of bases until the desired
    superclass method is found AND if that method has a docstring and only
    THEN does it attach the superdocstring to the derived class method.

    Please use carefully, I just did the metaclass thing by following
    Michael Foord's Metaclass tutorial
    (http://www.voidspace.org.uk/python/articles/metaclasses.shtml), I may
    have missed a step or two.

    source:
    http://groups.google.com/group/comp.lang.python/msg/26f7b4fcb4d66c95
    by Paul McGuire
    """
    def __new__(meta, classname, bases, classDict):
        newClassDict = {}
        for attributeName, attribute in classDict.items():
            if type(attribute) == FunctionType:
                # look through bases for matching function by name
                for baseclass in bases:
                    if hasattr(baseclass, attributeName):
                        basefn = getattr(baseclass, attributeName)
                        if basefn.__doc__:
                            attribute.__doc__ = basefn.__doc__
                            break
            newClassDict[attributeName] = attribute
        return type.__new__(meta, classname, bases, newClassDict)



class Converter(Configurable):
    #__metaclass__ = DocStringInheritor
    #-------------------------------------------------------------------------
    # Class-level attributes determining the behaviour of the class but
    # probably not varying from instance to instance.
    #-------------------------------------------------------------------------
    default_encoding = 'utf-8'
    extension = str()
    blank_symbol = " "
    # Which display data format is best? Subclasses can override if
    # they have specific requirements.
    display_data_priority = ['pdf', 'svg', 'png', 'jpg', 'text']
    #-------------------------------------------------------------------------
    # Instance-level attributes that are set in the constructor for this
    # class.
    #-------------------------------------------------------------------------
    infile = Unicode()

    highlight_source = Bool(True,
                     config=True,
                     help="Enable syntax highlighting for code blocks.")

    preamble = Unicode("" ,
                        config=True,
                        help="Path to a user-specified preamble file")
    fragment = Bool(
        True,
        config=True,
        help="Produce a text fragment rather than a standalone page",
    )

    infile_dir = Unicode()
    infile_root = Unicode()
    clean_name = Unicode()
    files_dir = Unicode()
    outbase = Unicode()
    #-------------------------------------------------------------------------
    # Instance-level attributes that are set by other methods in the base
    # class.
    #-------------------------------------------------------------------------
    figures_counter = 0
    output = Unicode()
    #-------------------------------------------------------------------------
    # Instance-level attributes that are not actually mentioned further
    # in this class. TODO: Could they be usefully moved to a subclass?
    #-------------------------------------------------------------------------
    with_preamble = Bool(True,config=True)
    user_preamble = None
    raw_as_verbatim = False


    def __init__(self, infile='', config=None, exclude=[], **kw):
        super(Converter,self).__init__(config=config)

        #DocStringInheritor.__init__(self=config)
        # N.B. Initialized in the same order as defined above. Please try to
        # keep in this way for readability's sake.
        self.exclude_cells = exclude
        self.infile = infile
        self.infile_dir, infile_root = os.path.split(infile)
        self.infile_root = os.path.splitext(infile_root)[0]
        self.clean_name = clean_filename(self.infile_root)
        # Handle the creation of a directory for ancillary files, for
        # formats that need one.
        files_dir = os.path.join(self.infile_dir, self.clean_name + '_files')
        if not os.path.isdir(files_dir):
            os.mkdir(files_dir)
        self.files_dir = files_dir
        self.outbase = os.path.join(self.infile_dir, self.infile_root)

    def __del__(self):
        if os.path.isdir(self.files_dir) and not os.listdir(self.files_dir):
            os.rmdir(self.files_dir)

    def _get_prompt_number(self, cell):
        return cell.prompt_number if hasattr(cell, 'prompt_number') \
            else self.blank_symbol

    def dispatch(self, cell_type):
        """return cell_type dependent render method,  for example render_code
        """
        return getattr(self, 'render_' + cell_type, self.render_unknown)

    def dispatch_display_format(self, format):
        """
        return output_type dependent render method,  for example
        render_output_text
        """
        return getattr(self, 'render_display_format_' + format,
                       self.render_unknown_display)

    def convert(self, cell_separator='\n'):
        """
        Generic method to converts notebook to a string representation.

        This is accomplished by dispatching on the cell_type, so subclasses of
        Convereter class do not need to re-implement this method, but just
        need implementation for the methods that will be dispatched.

        Parameters
        ----------
        cell_separator : string
          Character or string to join cells with. Default is "\n"

        Returns
        -------
        out : string
        """
        lines = []
        lines.extend(self.optional_header())
        lines.extend(self.main_body(cell_separator))
        lines.extend(self.optional_footer())
        return u'\n'.join(lines)

    def main_body(self, cell_separator='\n'):
        converted_cells = []
        for worksheet in self.nb.worksheets:
            for cell in worksheet.cells:
                #print(cell.cell_type)  # dbg
                conv_fn = self.dispatch(cell.cell_type)
                if cell.cell_type in ('markdown', 'raw'):
                    remove_fake_files_url(cell)
                converted_cells.append('\n'.join(conv_fn(cell)))
        cell_lines = cell_separator.join(converted_cells).split('\n')
        return cell_lines

    def render(self, buffer=None, encoding='utf8'):
        "read, convert, and save self.infile"
        if not hasattr(self, 'nb'):
            self.read()
        if buffer is not None:
            buffer.write(self.convert().encode(encoding))
            return
        self.output = self.convert()
        assert(type(self.output) == unicode)
        return self.save()

    def read(self):
        "read and parse notebook into NotebookNode called self.nb"
        with open(self.infile) as f:
            self.nb = nbformat.read(f, 'json')

    def save(self, outfile=None, encoding=None):
        "read and parse notebook into self.nb"
        if outfile is None:
            outfile = self.outbase + '.' + self.extension
        if encoding is None:
            encoding = self.default_encoding
        with io.open(outfile, 'w', encoding=encoding) as f:
            f.write(self.output)
        return os.path.abspath(outfile)

    def optional_header(self):
        """
        Optional header to insert at the top of the converted notebook

        Returns a list
        """
        return []

    def optional_footer(self):
        """
        Optional footer to insert at the end of the converted notebook

        Returns a list
        """
        return []

    def _new_figure(self, data, fmt):
        """Create a new figure file in the given format.

        Returns a path relative to the input file.
        """
        figname = '%s_fig_%02i.%s' % (self.clean_name,
                                      self.figures_counter, fmt)
        self.figures_counter += 1
        fullname = os.path.join(self.files_dir, figname)

        # Binary files are base64-encoded, SVG is already XML
        if fmt in ('png', 'jpg', 'pdf'):
            data = data.decode('base64')
            fopen = lambda fname: open(fname, 'wb')
        else:
            fopen = lambda fname: codecs.open(fname, 'wb',
                                              self.default_encoding)

        with fopen(fullname) as f:
            f.write(data)

        return fullname

    def render_heading(self, cell):
        """convert a heading cell

        Returns list."""
        raise NotImplementedError

    def render_code(self, cell):
        """Convert a code cell

        Returns list."""
        raise NotImplementedError

    def render_markdown(self, cell):
        """convert a markdown cell

        Returns list."""
        raise NotImplementedError

    def _img_lines(self, img_file):
        """Return list of lines to include an image file."""
        # Note: subclasses may choose to implement format-specific _FMT_lines
        # methods if they so choose (FMT in {png, svg, jpg, pdf}).
        raise NotImplementedError

    def render_display_data(self, output):
        """convert display data from the output of a code cell

        Returns list.
        """
        for fmt in self.display_data_priority:
            if fmt in output:
                break
        else:
            for fmt in output:
                if fmt != 'output_type':
                    break
            else:
                raise RuntimeError('no display data')

        # Is it an image?
        if fmt in ['png', 'svg', 'jpg', 'pdf']:
            img_file = self._new_figure(output[fmt], fmt)
            # Subclasses can have format-specific render functions (e.g.,
            # latex has to auto-convert all SVG to PDF first).
            lines_fun = getattr(self, '_%s_lines' % fmt, None)
            if not lines_fun:
                lines_fun = self._img_lines
            lines = lines_fun(img_file)
        else:
            lines_fun = self.dispatch_display_format(fmt)
            lines = lines_fun(output)

        return lines

    def render_raw(self, cell):
        """convert a cell with raw text

        Returns list."""
        raise NotImplementedError

    def render_unknown(self, cell):
        """Render cells of unkown type

        Returns list."""
        data = pprint.pformat(cell)
        logging.warning('Unknown cell: %s' % cell.cell_type)
        return self._unknown_lines(data)

    def render_unknown_display(self, output, type):
        """Render cells of unkown type

        Returns list."""
        data = pprint.pformat(output)
        logging.warning('Unknown output: %s' % output.output_type)
        return self._unknown_lines(data)

    def render_stream(self, output):
        """render the stream part of an output

        Returns list.

        Identical to render_display_format_text
        """
        return self.render_display_format_text(output)

    def render_pyout(self, output):
        """convert pyout part of a code cell

        Returns list."""
        raise NotImplementedError

    def render_pyerr(self, output):
        """convert pyerr part of a code cell

        Returns list."""
        raise NotImplementedError

    def _unknown_lines(self, data):
        """Return list of lines for an unknown cell.

        Parameters
        ----------
        data : str
          The content of the unknown data as a single string.
        """
        raise NotImplementedError

    # These are the possible format types in an output node

    def render_display_format_text(self, output):
        """render the text part of an output

        Returns list.
        """
        raise NotImplementedError

    def render_display_format_html(self, output):
        """render the html part of an output

        Returns list.
        """
        raise NotImplementedError

    def render_display_format_latex(self, output):
        """render the latex part of an output

        Returns list.
        """
        raise NotImplementedError

    def render_display_format_json(self, output):
        """render the json part of an output

        Returns list.
        """
        raise NotImplementedError

    def render_display_format_javascript(self, output):
        """render the javascript part of an output

        Returns list.
        """
        raise NotImplementedError

#-----------------------------------------------------------------------------
# nbconvert.converters.rst
#-----------------------------------------------------------------------------
class ConverterRST(Converter):
    #-------------------------------------------------------------------------
    # Class-level attributes determining the behaviour of the class but
    # probably not varying from instance to instance.
    #-------------------------------------------------------------------------
    extension = 'rst'
    heading_level = {1: '=', 2: '-', 3: '`', 4: '\'', 5: '.', 6: '~'}

    def render_heading(self, cell):
        marker = self.heading_level[cell.level]
        return ['{0}\n{1}\n'.format(cell.source, marker * len(cell.source))]

    def render_code(self, cell):
        # Note: cell has type 'IPython.nbformat.v3.nbbase.NotebookNode'
        if not cell.input:
            return []

        lines = ['In[%s]:' % self._get_prompt_number(cell), '']
        lines.extend(rst_directive('.. code:: python', cell.input))

        for output in cell.outputs:
            conv_fn = self.dispatch(output.output_type)
            lines.extend(conv_fn(output))

        return lines

    def render_markdown(self, cell):
        #return [cell.source]
        return [markdown2rst(cell.source)]

    def render_raw(self, cell):
        if self.raw_as_verbatim:
            return ['::', '', indent(cell.source), '']
        else:
            return [cell.source]

    def render_pyout(self, output):
        lines = ['Out[%s]:' % self._get_prompt_number(output), '']

        # output is a dictionary like object with type as a key
        if 'latex' in output:
            lines.extend(rst_directive('.. math::', output.latex))

        if 'text' in output:
            lines.extend(rst_directive('.. parsed-literal::', output.text))

        return lines

    def render_pyerr(self, output):
        # Note: a traceback is a *list* of frames.
        return ['::', '', indent(remove_ansi('\n'.join(output.traceback))), '']

    def _img_lines(self, img_file):
        return ['.. image:: %s' % img_file, '']

    def render_display_format_text(self, output):
        return rst_directive('.. parsed-literal::', output.text)

    def _unknown_lines(self, data):
        return rst_directive('.. warning:: Unknown cell') + [data]

    def render_display_format_html(self, output):
        return rst_directive('.. raw:: html', output.html)

    def render_display_format_latex(self, output):
        return rst_directive('.. math::', output.latex)

    def render_display_format_json(self, output):
        return rst_directive('.. raw:: json', output.json)

    def render_display_format_javascript(self, output):
        return rst_directive('.. raw:: javascript', output.javascript)

#-----------------------------------------------------------------------------
# nbconvert.converters.html
#-----------------------------------------------------------------------------
class ConverterHTML(Converter):
    #-------------------------------------------------------------------------
    # Class-level attributes determining the behaviour of the class but
    # probably not varying from instance to instance.
    #-------------------------------------------------------------------------
    extension = 'html'
    blank_symbol = '&nbsp;'

    #def __init__(self, *args, **kw):
    #    self.fragment = kw.pop('fragment', True)
    #    super(ConverterHTML, self).__init__(*args, **kw)

    def in_tag(self, tag, src, attrs=None):
        """Return a list of elements bracketed by the given tag"""
        attr_s = '' if attrs is None else \
                 ' '.join("%s=%s" % (attr, value)
                          for attr, value in attrs.iteritems())
        return ['<%s %s>' % (tag, attr_s), src, '</%s>' % tag]

    def _ansi_colored(self, text):
        return ['<pre>%s</pre>' % ansi2html(text)]

    def _stylesheet(self, fname):
        with io.open(fname, encoding='utf-8') as f:
            s = f.read()
        return self.in_tag('style', s, dict(type='"text/css"'))

    def _out_prompt(self, output):
        if output.output_type == 'pyout':
            content = 'Out[%s]:' % self._get_prompt_number(output)
        else:
            content = ''
        return ['<div class="prompt output_prompt">%s</div>' % content]

    def header_body(self):
        """Return the body of the header as a list of strings."""

        from pygments.formatters import HtmlFormatter

        header = []
        static = getattr(notebookapp, 'DEFAULT_STATIC_FILES_PATH', None)
        # ipython < 1.0
        if static is None:
            static = os.path.join(path.get_ipython_package_dir(),
            'frontend', 'html', 'notebook', 'static',
            )
        here = os.path.split(os.path.realpath(__file__))[0]
        css = os.path.join(static, 'css')
        for sheet in [
            # do we need jquery and prettify?
            # os.path.join(static, 'jquery', 'css', 'themes', 'base',
            # 'jquery-ui.min.css'),
            # os.path.join(static, 'prettify', 'prettify.css'),
            os.path.join(css, 'boilerplate.css'),
            #os.path.join(css, 'style.min.css'),
            # our overrides:
            os.path.join(here, 'css', 'static_html.css'),
        ]:
            header.extend(self._stylesheet(sheet))

        # pygments css
        pygments_css = HtmlFormatter().get_style_defs('.highlight')
        header.extend(['<meta charset="UTF-8">'])
        header.extend(self.in_tag('style', pygments_css,
                                  dict(type='"text/css"')))

        # TODO: this should be allowed to use local mathjax:
        header.extend(self.in_tag('script', '', {'type': '"text/javascript"',
            'src': '"https://c328740.ssl.cf1.rackcdn.com/mathjax/'
                   'latest/MathJax.js?config=TeX-AMS_HTML"',
        }))
        with io.open(os.path.join(here, 'js', 'initmathjax.js'),
                     encoding='utf-8') as f:
            header.extend(self.in_tag('script', f.read(),
                                      {'type': '"text/javascript"'}))
        return header

    def optional_header(self):
        if self.fragment:
            return []
        return ['<html>', '<head>'] + self.header_body() + ['</head>', '<body>']

    def optional_footer(self):
        if self.fragment:
            return []
        return ['</body>', '</html>']

    @text_cell
    def render_heading(self, cell):
        marker = cell.level
        return [u'<h{1}>\n  {0}\n</h{1}>'.format(cell.source, marker)]

    def render_code(self, cell):
        if not cell.input:
            return []

        lines = ['<div class="cell border-box-sizing code_cell vbox">']

        lines.append('<div class="input hbox">')
        n = self._get_prompt_number(cell)
        lines.append(
            '<div class="prompt input_prompt">In&nbsp;[%s]:</div>' % n
        )
        lines.append('<div class="input_area box-flex1">')
        lines.append(highlight(cell.input) if self.highlight_source
                     else cell.input)
        lines.append('</div>')  # input_area
        lines.append('</div>')  # input

        if cell.outputs:
            lines.append('<div class="vbox output_wrapper">')
            lines.append('<div class="output vbox">')

            #print(cell.outputs)
            for output in coalesce_streams(cell.outputs):
                conv_fn = self.dispatch(output.output_type)
                lines.extend(conv_fn(output))

            lines.append('</div>')  # output
            lines.append('</div>')  # output_wrapper

        lines.append('</div>')  # cell

        return lines

    @text_cell
    def render_markdown(self, cell):
        return [markdown(cell.source)]

    def render_raw(self, cell):
        if self.raw_as_verbatim:
            return self.in_tag('pre', cell.source)
        else:
            return [cell.source]

    @output_container
    def render_pyout(self, output):
        for fmt in ['html', 'latex', 'png', 'jpeg', 'svg', 'text']:
            if fmt in output:
                conv_fn = self.dispatch_display_format(fmt)
                return conv_fn(output)
        return []

    render_display_data = render_pyout

    @output_container
    def render_stream(self, output):
        return self._ansi_colored(output.text)

    @output_container
    def render_pyerr(self, output):
        # Note: a traceback is a *list* of frames.
        # lines = []

        # stb =
        return self._ansi_colored('\n'.join(output.traceback))

    def _img_lines(self, img_file):
        return ['<img src="%s">' % img_file, '</img>']

    def _unknown_lines(self, data):
        return ['<h2>Warning:: Unknown cell</h2>'] + self.in_tag('pre', data)

    def render_display_format_png(self, output):
        return ['<img src="data:image/png;base64,%s"></img>' % output.png]

    def render_display_format_svg(self, output):
        return [output.svg]

    def render_display_format_jpeg(self, output):
        return ['<img src="data:image/jpeg;base64,%s"></img>' % output.jpeg]

    def render_display_format_text(self, output):
        return self._ansi_colored(output.text)

    def render_display_format_html(self, output):
        return [output.html]

    def render_display_format_latex(self, output):
        return [output.latex]

    def render_display_format_json(self, output):
        # html ignores json
        return []

    def render_display_format_javascript(self, output):
        return [output.javascript]
#-----------------------------------------------------------------------------
# nbconvert.py
#-----------------------------------------------------------------------------

# When adding a new format, make sure to add it to the `converters`
# dictionary below. This is used to create the list of known formats,
# which gets printed in case an unknown format is encounteres, as well
# as in the help

converters = {
    'rst': ConverterRST,
    #'markdown': ConverterMarkdown,
    'html': ConverterHTML,
    #'blogger-html': ConverterBloggerHTML,
    #'latex': ConverterLaTeX,
    #'pdf' : ConverterLaTeXToPDF,
    #'py': ConverterPy,
    #'reveal': ConverterReveal,
    }

default_format = 'rst'

# Extract the list of known formats and mark the first format as the default.
known_formats = ', '.join([key + " (default)" if key == default_format else key
                           for key in converters])

class NbconvertApp(Application):


    fmt = CaselessStrEnum(converters.keys(),
                          default_value='rst',
                          config=True,
                          help="Supported conversion format")

    exclude = List( [],
                    config=True,
                    help = 'list of cells to exclude while converting')

    aliases = {
            'format':'NbconvertApp.fmt',
            'exclude':'NbconvertApp.exclude',
            'highlight':'Converter.highlight_source',
            'preamble':'Converter.preamble',
            'fragment':'Converter.fragment',
            }

    def __init__(self, **kwargs):
        super(NbconvertApp, self).__init__(**kwargs)
        # ensure those are registerd
        self.classes.insert(0,Converter)
        self.classes.insert(0,ConverterRST)
        self.classes.insert(0,ConverterHTML)
        #self.classes.insert(0,ConverterMarkdown)
        #self.classes.insert(0,ConverterBloggerHTML)
        #self.classes.insert(0,ConverterLaTeX)
        #self.classes.insert(0,ConverterPy)

    def initialize(self, argv=None):
        self.parse_command_line(argv)
        cl_config = self.config
        self.update_config(cl_config)

    def run(self, out=None, encoding='utf8'):
        """Convert a notebook in one step"""
        ConverterClass = converters[self.fmt]
        infile = (self.extra_args or [None])[0]
        converter = ConverterClass(infile=infile,  config=self.config)
        converter.render(buffer=out, encoding=encoding)

def ipynb2html(infile, encoding='utf8'):
    """Convert an IPython Notebook to HTML"""
    buffer = StringIO()
    app = NbconvertApp.instance()
    app.description = __doc__
    app.initialize(argv=['--format', 'html', '--fragment', 'True', infile])
    app.start()
    app.run(out=buffer, encoding=encoding)
    # return unicode
    return buffer.getvalue().decode(encoding)

def main():
    """Convert a notebook to html in one step"""
    app = NbconvertApp.instance()
    app.description = __doc__
    print("""
======================================================
Warning, we are deprecating this version of nbconvert,
please consider using the new version.
======================================================
    """)
    app.initialize()
    app.start()
    app.run()
#-----------------------------------------------------------------------------
# Script main
#-----------------------------------------------------------------------------

if __name__ == '__main__':
    # TODO: consider passing file like object around, rather than filenames
    # would allow us to process stdin, or even http streams
    #parser.add_argument('infile', nargs='?', type=argparse.FileType('r'),
    #                    default=sys.stdin)

    #parser.add_argument('-e', '--exclude', default='',
    #                    help='Comma-separated list of cells to exclude')
    #exclude_cells = [s.strip() for s in args.exclude.split(',')]

   main()
