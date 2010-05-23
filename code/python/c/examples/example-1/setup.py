#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# A simple calculator that works with whole numbers written in C/Python.
# Copyright (C) 2009 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
from distutils.core import setup, Extension

def run():
    setup(name='pycalc',
          version='0.1',
          author='Nycholas de Oliveira e Oliveira',
          author_email='nycholas@gmail.com',
          license='GNU Lesser General Public License (LGPL)',
          description="""A simple calculator that works with """
                      """whole numbers written in C/Python.""",
          platforms=['Many'],
          ext_modules=[Extension('pycalc', sources = ['pycalc.c'])]
    )

# Commands:
#
# ++ clean up temporary files from 'build' command
# ./setup.py clean -a
#
# ++ build everything needed to install
# ./setup.py build
#
# ++ install everything from build directory
# ./setup.py install -c -O2
#

if __name__ == "__main__":
    run()