# -*- coding: utf-8 -*-
from django.conf.urls.defaults import *

urlpatterns = patterns('ask_undrgz.question.views',
    (r'^$', 'index'),
)
