#!/usr/bin/env python

import os
from distutils.core import setup


__version__ = '0.2.3'
here = os.path.dirname(os.path.abspath(__file__))

setup(
    name="ipynb2html",
    version=__version__,
    author="cyclebelfast",
    author_email="cyclebelfast@gmail.com",
    description="Convert iPython Notebook to HTML",
    py_modules=['ipynb2html'],
)
