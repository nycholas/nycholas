# -*- coding: utf-8 -*-
from django.conf.urls.defaults import *

# Uncomment the next two lines to enable the admin:
# from django.contrib import admin
# admin.autodiscover()

urlpatterns = patterns('',
    # Example:
    # (r'^ask_undrgz/', include('ask_undrgz.foo.urls')),
    (r'^$', 'ask_undrgz.question.views.index'),
    (r'^(?P<question_key>\w+)/answer/$', 'ask_undrgz.question.views.answer'),
    (r'^question_top10/$', 'ask_undrgz.question.views.question_top10'),
    (r'^is_online/$', 'ask_undrgz.question.views.is_online'),
    (r'^send_message/$', 'ask_undrgz.question.views.send_message'),
    (r'^_ah/xmpp/message/chat/', 'ask_undrgz.question.views.incoming_chat'),

    # Uncomment the admin/doc line below and add 'django.contrib.admindocs' 
    # to INSTALLED_APPS to enable admin documentation:
    # (r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    # (r'^admin/', include(admin.site.urls)),
)
