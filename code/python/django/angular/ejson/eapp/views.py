# -*- coding: utf-8 -*-
# Copyright (c) 2012, Cenobit Technologies, Inc. http://cenobit.es/
# All rights reserved.
from django.core import serializers
from django.utils import simplejson
from django.http import HttpResponse
from django.shortcuts import render_to_response
from django.template.context import RequestContext
from django.views.decorators.csrf import csrf_protect
from django.utils.translation import ugettext as _

from eapp.models import Notebook

def index(request):
    return render_to_response('index.html', {
    }, context_instance=RequestContext(request))

def find(request):
    notebooks = Notebook.objects.all()
    response = serializers.serialize('json', notebooks)
    return HttpResponse(response, mimetype='application/json')

def find_by_id(request, notebook_id):
    notebook = Notebook.objects.get(pk=notebook_id)
    response = serializers.serialize('json', [notebook])
    return HttpResponse(response, mimetype='application/json')

def update(request, notebook_id):
    HttpResponse('TODO')

def delete(request, notebook_id):
    HttpResponse('TODO')
