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
import datetime

from google.appengine.api import xmpp

from django.http import HttpResponse, HttpResponseRedirect
from django.utils.translation import ugettext as _
from django.shortcuts import render_to_response
from django.utils import simplejson
from django.core import serializers

from ask_undrgz.question.forms import QuestionForm
from ask_undrgz.question.models import Question

def _get_question_top10():
    return Question.all().order('-asked').fetch(10)

def _send_message(user_address, message):
    chat_message_sent = False
    xmpp.send_invite(user_address)
    if xmpp.get_presence(user_address):
        status_code = xmpp.send_message(user_address, message)
        chat_message_sent = (status_code != xmpp.NO_ERROR)
    return chat_message_sent

def index(request):
    logging.debug('In question.views::index()')
    if request.method == 'POST':
        question_form = QuestionForm(request.POST)
        if question_form.is_valid():
            new_question = question_form.save(commit=False)
            if not new_question.is_exists():
                new_question.save()
                _send_message('underguiz@ilostmyself.org', 
                              '%s: %s' % (new_question.key().id(), 
                                          new_question.ask))
            else:
                q = Question.all().filter('ask = ', new_question.ask)
                new_question = q.get()
                new_question.asked = datetime.datetime.now()
                new_question.save()
            return HttpResponseRedirect(new_question.get_absolute_url())
    else:
        question_form = QuestionForm()
    return render_to_response('index.html', {
        'question_form': question_form,
        'top10': _get_question_top10(),
    })
        
def answer(request, ask):
    logging.debug('In question.views::answer()')
    question = Question.all().filter('ask_slug = ', ask).get()
    if not question:
        #im_from = db.IM('xmpp', 'nycholas@gmail.com')
        new_question = Question(ask=ask) #Question(ask=ask, asker=im_from)
        new_question.save()
        _send_message('underguiz@ilostmyself.org', 
                      '%s: %s' % (new_question.key().id(), new_question.ask))
        return HttpResponseRedirect(new_question.get_absolute_url())
    if not question.answer:
        _send_message('underguiz@ilostmyself.org', 
                      '%s: %s' % (question.key().id(), question.ask))
    if request.is_ajax():
        return HttpResponse(simplejson.dumps(question.to_dict()), 
                            mimetype='application/json')
    initial = {}
    initial['ask'] = question.ask
    initial['ask_slug'] = question.ask_slug
    question_form = QuestionForm(initial=initial)
    return render_to_response('index.html', {
        'question_form': question_form,
        'top10': _get_question_top10(),
        'ask_slug': question.ask_slug,
        'answer': question.answer,
    })
    
def top10(request):
    logging.debug('In question.views::top10()')
    question_top10 = _get_question_top10()
    if request.is_ajax():
        return HttpResponse(simplejson.dumps([q.to_dict() for q in question_top10]), 
                            mimetype='application/json')
    return HttpResponse('')

def is_online(request):
    logging.debug('In question.views::is_online()')
    user_address = request.REQUEST.get('from')
    chat_message_sent = False
    if not user_address:
        if request.is_ajax():
            return HttpResponse(simplejson.dumps(chat_message_sent), 
                                mimetype='application/json')
        return HttpResponse('from is required', status=405)
    chat_message_sent = xmpp.get_presence(user_address)
    if request.is_ajax():
        return HttpResponse(simplejson.dumps(chat_message_sent), 
                            mimetype='application/json')
    return HttpResponse(chat_message_sent)
    
def send_message(request):
    logging.debug('In question.views::send_message()')
    user_address = request.REQUEST.get('from')
    message = request.REQUEST.get('body')
    chat_message_sent = False
    if not user_address or not message:
        if request.is_ajax():
            return HttpResponse(simplejson.dumps(chat_message_sent), 
                                mimetype='application/json')
        return HttpResponse('from and message is required', status=405)
    chat_message_sent = _send_message(user_address, message)
    if request.is_ajax():
        return HttpResponse(simplejson.dumps(chat_message_sent), 
                            mimetype='application/json')
    return HttpResponse(chat_message_sent)
    
def incoming_chat(request):
    '''/_ah/xmpp/message/chat/
    
    This handles incoming XMPP (chat) messages.
    
    Just reply saying we ignored the chat.
    '''
    logging.debug('In question.views::incoming_chat()')
    if request.method != 'POST':
        return HttpResponse('XMPP requires POST', status=405)
    st = False
    sender = request.POST.get('from')
    toaddr = request.POST.get('to')
    message = request.POST.get('body')
    if not sender:
        logging.warn('Incoming chat without \'from\' key ignored')
    else:
        body = message.split(':')
        id_question = int(body[0])
        answer = ''.join(body[1:])
        question = Question.get_by_id(id_question)
        question.answer = answer
        question.answered = datetime.datetime.now()
        question.save()
        sts = xmpp.send_message([toaddr], answer)
        logging.debug('XMPP status %r', sts)
    return HttpResponse(st)

