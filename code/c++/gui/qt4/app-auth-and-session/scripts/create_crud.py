#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Simple example Qt - Mainwindow with authentication and session.
# Copyright (c) 2010, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
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
import re
import sys

BASE_DIR = os.path.join(os.path.abspath(os.path.dirname(__file__)), os.pardir)
TEMPLATE_DIR = os.path.join(BASE_DIR, 'src', 'core')

TEMPLATES = (
    os.path.join(TEMPLATE_DIR, 'models', 'xxxmodel.cpp'),
    os.path.join(TEMPLATE_DIR, 'models', 'xxxmodel.h'),
    os.path.join(TEMPLATE_DIR, 'xxx.cpp'),
    os.path.join(TEMPLATE_DIR, 'xxx.h'),
    os.path.join(TEMPLATE_DIR, 'widgets', 'xxx.ui'),
    os.path.join(TEMPLATE_DIR, 'xxxform.cpp'),
    os.path.join(TEMPLATE_DIR, 'xxxform.h'),
    os.path.join(TEMPLATE_DIR, 'widgets', 'xxxform.ui'),
    os.path.join(TEMPLATE_DIR, 'xxxsearch.cpp'),
    os.path.join(TEMPLATE_DIR, 'xxxsearch.h'),
    os.path.join(TEMPLATE_DIR, 'widgets', 'xxxsearch.ui'),
)

NAME_CRUD = raw_input('Name CRUD: ').strip()

def file_name(filepath, filename):
    fl = os.path.split(filepath)[-1]
    filename = ''.join(filename.replace('-', '').lower().split())
    return re.sub('xxx', filename, fl)

def defined_name(st):
    return ''.join(st.replace('-', '').upper().split())

def import_name(st):
    return ''.join(st.replace('-', '').lower().split())

def class_name(st):
    return ''.join(st.replace('-', ' ').capitalize().split())

def table_name(st):
    return ''.join(st.replace('-', '_').lower().split())

def var_name(st):
    s = st.replace('-', ' ').capitalize().split()
    return ''.join([s[0].lower()] + s[1:])

for template in TEMPLATES:
    print '++ open %s...' % os.path.split(template)[-1]
    fl = open(template, 'r')
    ct = fl.read()

    print ' :: reading and process...'
    ct = re.sub('XXX', defined_name(NAME_CRUD), ct)
    ct = re.sub('xxx', import_name(NAME_CRUD), ct)
    ct = re.sub('Xxx', class_name(NAME_CRUD), ct)
    ct = re.sub('xXx', var_name(NAME_CRUD), ct)
    filename = file_name(template, NAME_CRUD)

    print ' :: create new crud %s...' % filename
    new_fl = open(filename, 'w')
    new_fl.write(ct)
    new_fl.close()
    fl.close()