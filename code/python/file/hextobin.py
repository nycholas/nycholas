#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Convert file hex in bin.
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
import sys

try:
  src, dst = sys.argv[1:3]
except ValueError, e:
  print >> sys.stderr, '''Usage: \
python hextobin.py FILE_SOURCE FILE_DESTINATION'''
  sys.exit(1)
fsrc = open(src, 'rb')
tsrc = fsrc.read().decode('hex')
fsrc.close()
fdst = open(dst, 'wb')
tdst = fdst.write(tsrc)
fdst.close()
