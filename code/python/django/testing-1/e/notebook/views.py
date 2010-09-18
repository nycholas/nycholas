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
import logging
import datetime

from django.http import HttpResponse, HttpResponseRedirect
from django.views.decorators.csrf import csrf_protect
from django.utils.translation import ugettext as _
from django.shortcuts import render_to_response
from django.template import RequestContext
from django.utils import simplejson
from django.core import serializers

from .models import Notebook
from .forms import NotebookForm

def notebook_list(request):
    logging.debug('In notebook.views::notebook_list()')
    notebooks = Notebook.objects.all()
    return render_to_response('notebook_list.html', {
        'notebooks': notebooks,
    })

@csrf_protect
def notebook_add(request):
    logging.debug('In notebook.views::notebook_add()')
    if request.method == 'POST':
        notebook_form = NotebookForm(request.POST)
        if notebook_form.is_valid():
            new_notebook = notebook_form.save(commit=False)
            new_notebook.date_joined = datetime.datetime.now()
            new_notebook.save()
            if request.POST.get('save_and_add_another'):
                return HttpResponseRedirect('/notebook/add/')
            elif request.POST.get('save_and_continue_editing'):
                return HttpResponseRedirect(new_notebook.get_absolute_url())
            else:
                return HttpResponseRedirect('/')
    else:
        notebook_form = NotebookForm()
    return render_to_response('notebook_add.html', {
        'notebook_form': notebook_form,
    }, context_instance=RequestContext(request))

@csrf_protect
def notebook_edit(request, notebook_id):
    logging.debug('In notebook.views::notebook_edit()')
    notebook =  Notebook.objects.get(pk=notebook_id)
    if request.method == 'POST':
        print request.POST
        notebook_form = NotebookForm(request.POST, instance=notebook)
        if notebook_form.is_valid():
            new_notebook = notebook_form.save()
            if request.POST.get('save_and_add_another'):
                return HttpResponseRedirect('/notebook/add/')
            elif request.POST.get('save_and_continue_editing'):
                return HttpResponseRedirect(new_notebook.get_absolute_url())
            else:
                return HttpResponseRedirect('/')
    else:
        notebook_form = NotebookForm(instance=notebook)
    return render_to_response('notebook_add.html', {
        'notebook_form': notebook_form,
        'notebook': notebook,
    }, context_instance=RequestContext(request))

def notebook_status(request, notebook_id):
    logging.debug('In notebook.views::notebook_status()')
    notebook = Notebook.objects.get(pk=notebook_id)
    notebook.status = not notebook.status
    notebook.save()
    return HttpResponseRedirect('/')

def notebook_delete(request, notebook_id):
    logging.debug('In notebook.views::notebook_delete()')
    notebook = Notebook.objects.get(pk=notebook_id)
    notebook.delete()
    return HttpResponseRedirect('/')
