# -*- coding: utf-8 -*-
import logging, os, sys

# Google App Engine imports.
from google.appengine.ext.webapp import util
from google.appengine.dist import use_library

#use_library('django', '1.2')

# Remove the standard version of Django.
for k in [k for k in sys.modules if k.startswith('django')]:
    del sys.modules[k]

# Force sys.path to have our own directory first, in case we want to import
# from it.
sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)))

# Must set this env var *before* importing any part of Django
os.environ['DJANGO_SETTINGS_MODULE'] = 'ask_undrgz.settings'

import django.core.handlers.wsgi
import django.core.signals
import django.db
import django.dispatch
import django.dispatch.dispatcher

def log_exception(*args, **kwds):
    logging.exception('Exception in request:')
   
# Migrating django.dispatch.dispatcher from Django 0.96 to 1.0.2
# Log errors.
django.dispatch.Signal.connect(
    django.core.signals.got_request_exception, log_exception)

# Unregister the rollback event handler.
django.dispatch.Signal.disconnect(
    django.core.signals.got_request_exception,
    django.db._rollback_on_exception)

def main():
    # Create a Django application for WSGI.
    application = django.core.handlers.wsgi.WSGIHandler()

    # Run the WSGI CGI handler with that application.
    util.run_wsgi_app(application)

if __name__ == '__main__':
    main()