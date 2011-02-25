#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Examle of egg package.
# Copyright (c) 2011, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# # Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# # Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# # Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
#    its contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
import os
import sys
import locale
from distutils.core import setup
from setuptools import find_packages

locale.setlocale(locale.LC_ALL, "")
try:
    sys.setappdefaultencoding("utf-8")
except AttributeError:
    try:
        reload(sys)
        sys.setdefaultencoding("utf-8")
    except LookupError:
        pass

if sys.version_info[0] == 3:
    extra_args = dict(use_2to3=True)
else:
    extra_args = dict()

long_description = """\
Examle of egg package.
"""

classifiers = [
    "Development Status :: 3 - Alpha",
    "Intended Audience :: Healthcare Industry",
    "License :: OSI Approved :: New BSD License",
    "Natural Language :: English",
    "Operating System :: MacOS :: MacOS X",
    "Operating System :: Microsoft :: Windows",
    "Operating System :: POSIX",
    "Operating System :: Unix",
    "Programming Language :: Python :: 2.7",
]

def run():
    setup(name="my_egg",
          version="0.1",
          url="http://code.google.com/p/nycholas/",
          download_url="http://code.google.com/p/nycholas/",
          license="New BSD License",
          description="""Example of egg package.""",
          long_description=long_description,
          classifiers=classifiers,
          platforms=["Many"],
          packages=['my_package'],
          scripts=['my_program.py'],
          zipfile=None,
          data_files=['README'],
          setup_requires=[],
          **extra_args
    )


# Commands:
# ./setup.py clean -a
# ./setup.py build
# ./setup.py py2exe
# ./setup.py install -c -O2
# ./setup.py sdist --formats=bztar, gztar, tar, zip, ztar
# ./setup.py bdist --formats=rpm, gztar, bztar, ztar, tar, wininst, zip
if __name__ == "__main__":
    run()
