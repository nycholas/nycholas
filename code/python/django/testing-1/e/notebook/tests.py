# -*- coding: utf-8 -*-
#
# Django example 'e' project.
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
import datetime

from django.test import TestCase
from django.test.client import Client
from django.core.urlresolvers import reverse

from .models import Notebook


class SimpleTest(TestCase):
    def setUp(self):
        self.client = Client()
        self.client_csrf = Client(enforce_csrf_checks=True)

    def test_notebook_list(self):
        response = self.client.get(reverse('notebook.views.notebook_list'))
        self.failUnlessEqual(response.status_code, 200)

    def test_notebook_add(self):
        response = self.client_csrf.get(reverse('notebook.views.notebook_add'))
        self.failUnlessEqual(response.status_code, 200)

    def test_notebook_insert(self):
        data = {
            'name': u'Django',
            'description': 'The Web framework for perfectionists with deadlines.',
            'date_joined': datetime.datetime.now(),
            'status': True,
        }
        response = self.client_csrf.get(reverse('notebook.views.notebook_add'), data)
        self.failUnlessEqual(response.status_code, 200)

    def test_notebook_edit(self):
        response = self.client_csrf.get(reverse('notebook.views.notebook_edit',
                                        kwargs={'notebook_id': 1}))
        self.failUnlessEqual(response.status_code, 200)

    def test_notebook_change(self):
        data = {
            'name': u'Django',
            'description': 'Django is a high-level Python Web framework that ' \
                'encourages rapid development and clean, pragmatic design.',
            'date_joined': datetime.datetime.now(),
            'status': True,
        }
        response = self.client_csrf.get(reverse('notebook.views.notebook_edit',
                                        kwargs={'notebook_id': 1}), data)
        self.failUnlessEqual(response.status_code, 200)

    def test_notebook_status(self):
        response = self.client.get(reverse('notebook.views.notebook_status',
                                   kwargs={'notebook_id': 1}))
        self.failUnlessEqual(response.status_code, 302)

    def test_notebook_delete(self):
        response = self.client.get(reverse('notebook.views.notebook_delete',
                                   kwargs={'notebook_id': 1}))
        self.failUnlessEqual(response.status_code, 302)

    def test_basic_addition(self):
        """
        Tests that 1 + 1 always equals 2.
        """
        self.failUnlessEqual(1 + 1, 2)

__test__ = {"doctest": """
Simple test django.

>>> import datetime
>>>
>>> from django.test import TestCase
>>> from django.test.client import Client
>>> from django.core.urlresolvers import reverse
>>>
>>> from .models import Notebook
>>>
>>> client = Client()
>>> client_csrf = Client(enforce_csrf_checks=True)
>>>
>>> notebook = Notebook(name='Django', description='Master of Insanity',
...                     date_joined=datetime.datetime.now(), status=True)
>>> notebook.save()
>>>
>>> response = client.get(reverse('notebook.views.notebook_list'))
>>> response.status_code
200
>>> response = client_csrf.get(reverse('notebook.views.notebook_add'))
>>> response.status_code
200
>>> response = client_csrf.get(reverse('notebook.views.notebook_edit',
...                                    kwargs={'notebook_id': notebook.id}))
>>> response.status_code
200
>>> response = client.get(reverse('notebook.views.notebook_status',
...                               kwargs={'notebook_id': notebook.id}))
>>> response.status_code
302
>>> response = client.get(reverse('notebook.views.notebook_delete',
...                               kwargs={'notebook_id': notebook.id}))
>>> response.status_code
302
"""}

