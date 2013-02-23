# -*- coding: utf-8 -*-
# Copyright (c) 2012, Cenobit Technologies, Inc. http://cenobit.es/
# All rights reserved.
from django.conf.urls.defaults import patterns, include, url

urlpatterns = patterns('eapp.views',
    url(r'^$', 'index', name='eapp.notebook.index'),
    url(r'^notebook/$', 'find', name='eapp.notebook.find'),
    url(r'^notebook/(?P<notebook_id>\d+)/$', 'find_by_id', name='eapp.notebook.find_by_id'),
    url(r'^notebook/save/$', 'save', name='eapp.notebook.save'),
    url(r'^notebook/(?P<notebook_id>\d+)/udpate/$', 'update', name='eapp.notebook.udpate'),
    url(r'^notebook/(?P<notebook_id>\d+)/delete/$', 'notebook_delete', name='eapp.notebook.delete'),
)
