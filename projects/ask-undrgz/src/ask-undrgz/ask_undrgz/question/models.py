# -*- coding: utf-8 -*-
#
# ask-undrgz system of questions uses data from underguiz.
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

from google.appengine.ext import db

from django.core.urlresolvers import reverse
from django.utils.translation import ugettext_lazy

from ask_undrgz.utils.models import SlugProperty


class Question(db.Model):
    ask = db.StringProperty(required=True)
    ask_slug = SlugProperty(ask)
    asked = db.DateTimeProperty(required=True, auto_now_add=True)
    answer = db.TextProperty()
    answered = db.DateTimeProperty()

    def __str__(self):
        logging.debug('In Question::__str__()')
        return '%s' % self.ask

    def __unicode__(self):
        logging.debug('In Question::__unicode__()')
        return u'%s' % self.ask
    
    def get_absolute_url(self):
        logging.debug('In Question::get_absolute_url()')
        return reverse('ask_undrgz.question.views.answer', 
                       kwargs={'ask_slug': self.slugify()})
        
    def slugify(self):
        logging.debug('In Question::slugify()')
        slug = SlugProperty.slugify(self.ask) or str(self.key())
        count = Question.all().filter('ask_slug = ', slug).count()
        self.ask_slug = str(self.key()) if count > 1 else slug
        return self.ask_slug
    
    def is_exists(self):
        logging.debug('In Question::is_exists()')
        q = Question.all().filter('ask = ', self.ask).get()
        return (q != None)
    
    def to_dict(self):
        logging.debug('In Question::to_dict()')
        d = dict([(p, unicode(getattr(self, p))) for p in self.properties()])
        d['get_absolute_url'] = self.get_absolute_url()
        return d
