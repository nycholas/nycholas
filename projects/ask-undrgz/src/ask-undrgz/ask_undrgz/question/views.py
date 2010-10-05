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
import math
import urllib
import base64
import logging
import datetime

from google.appengine.api import xmpp
from google.appengine.api import urlfetch

from django.http import HttpResponse, HttpResponseRedirect
from django.utils.translation import ugettext as _
from django.shortcuts import render_to_response
from django.utils import simplejson
from django.core import serializers

import tweepy

from ask_undrgz import settings as ask_undrgz_settings
from ask_undrgz.question.forms import QuestionForm
from ask_undrgz.question.models import Question, OAuthToken

def _recent_stupid_questions():
    return Question.all().filter('answered != ', None) \
        .order('-answered').fetch(30)
        
def _send_invite_xmpp(user_address):
    xmpp.send_invite(user_address)

def _send_message_xmpp(user_address, message):
    chat_message_sent = False
    #self._send_invite_xmpp(user_address)
    if xmpp.get_presence(user_address):
        status_code = xmpp.send_message([user_address], message)
        chat_message_sent = (status_code != xmpp.NO_ERROR)
    return chat_message_sent

def index(request):
    logging.debug('In question.views::index()')
    if request.method == 'POST':
        question_form = QuestionForm(request.POST)
        if question_form.is_valid():
            new_question = question_form.save(commit=False)
            if new_question.is_exists():
                new_question = Question.all() \
                    .filter('ask =', new_question.ask).get()
            new_question.asked = datetime.datetime.now()    
            new_question.save()
            _send_message_xmpp('underguiz@ilostmyself.org', 
                               '%s: %s' % (new_question.key().id(), 
                                      new_question.ask))
            return HttpResponseRedirect(new_question.get_absolute_url())
    else:
        question_form = QuestionForm()
    return render_to_response('index.html', {
        'question_form': question_form,
        'recent_stupid_questions': _recent_stupid_questions(),
    })
    
def new_ask(request, ask):
    logging.debug('In question.views::new_answer()')
    new_question = Question.all().filter('ask = ', ask).get()
    if not new_question.is_exists():
        new_question = Question(ask=ask)
    new_question = datetime.datetime.now()
    new_question.save()
    _send_message_xmpp('underguiz@ilostmyself.org', 
                       '%s: %s' % (new_question.key().id(), 
                                   new_question.ask))
    return HttpResponseRedirect(new_question.get_absolute_url())
        
def answer(request, ask_slug):
    logging.debug('In question.views::answer()')
    question = Question.all().filter('ask_slug = ', ask_slug).get()
    if question is None:
        question = Question.get(ask_slug)
    if not question.answer:
        d1 = datetime.datetime.now()
        d2 = question.asked
        if (abs(d1.minute-d2.minute) % 5) == 0 and d1.second == 0:
            question.asked = d1
            question.save()
            _send_message_xmpp('underguiz@ilostmyself.org', 
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
        'recent_stupid_questions': _recent_stupid_questions(),
        'ask_slug': question.slugify(),
        'answer': question.answer,
    })
    
def recent_stupid_questions(request):
    logging.debug('In question.views::recent_stupid_questions()')
    question_top10 = _recent_stupid_questions()
    if request.is_ajax():
        return HttpResponse(
                    simplejson.dumps([q.to_dict() for q in question_top10]), 
                    mimetype='application/json')
    return HttpResponse('')

def is_online(request):
    logging.debug('In question.views::is_online()')
    user_address = request.REQUEST.get('from')
    # Brazil - America/Sao_Paulo, ;-)
    dt = datetime.datetime.utcnow()
    dt_hour = dt.hour - 3 if dt.hour - 3 > 0 else (dt.hour - 3) + 24
    easter_egg = False
    if dt_hour == 6 and dt.minute == 6 and dt.second >= 6:
        easter_egg = True
    chat_message_sent = False
    if not user_address:
        if request.is_ajax():
            return HttpResponse(simplejson.dumps({
                'is_online': chat_message_sent,
                'easter_egg': easter_egg
            }), mimetype='application/json')
        return HttpResponse('from is required', status=405)
    chat_message_sent = xmpp.get_presence(user_address)
    if request.is_ajax():
        return HttpResponse(simplejson.dumps({
                'is_online': chat_message_sent,
                'easter_egg': easter_egg
            }), mimetype='application/json')
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
    chat_message_sent = _send_message_xmpp(user_address, message)
    if request.is_ajax():
        return HttpResponse(simplejson.dumps(chat_message_sent), 
                            mimetype='application/json')
    return HttpResponse(chat_message_sent)
    
def incoming_chat(request):
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
        try:
            body = message.split(':')
            if len(body) <= 1 and not body[0].isdigit():
                logging.warn('Message not format ID:MESSAGE: %s' % body)
                return HttpResponse(st)
            id_question = int(body[0]) if body[0].isdigit() else 0
            answer = ''.join(body[1:]).strip()
            question = Question.get_by_id(id_question)
            if question.answer:
                space = '<br />' + '&nbsp;'*16
                space = ''
                question.answer = '%s; %s%s' % (question.answer, space, answer)
            else:
                question.answer = answer 
            question.answered = datetime.datetime.now()
            question.save()
            sts = xmpp.send_message([toaddr], answer)
            logging.debug('XMPP status %s', str(sts))
            if question.answer:
                username = ask_undrgz_settings.TWITTER_USERNAME
                token_key = ask_undrgz_settings.TWITTER_CONSUMER_KEY
                token_secret = ask_undrgz_settings.TWITTER_CONSUMER_SECRET
                token_callback = ask_undrgz_settings.TWITTER_CALLBACK
                auth = tweepy.OAuthHandler(token_key, token_secret, 
                                           token_callback)
                try:
                    logging.info('Build a new oauth handler and display ' \
                                 'authorization url to user')
                    auth_url = auth.get_authorization_url()
                    logging.debug('auth_url: %s' % str(auth_url))
                except tweepy.TweepError, e:
                    logging.error('Failed to get a request token: %s' % str(e))
                    return HttpResponse(
                        'Failed to get a request token: %s' % str(e))
                logging.info('We must store the request token for later use ' \
                             'in the callback page')
                request_token = OAuthToken(
                        token_key = auth.request_token.key,
                        token_secret = auth.request_token.secret
                )
                request_token.put()
                
                oauth_token = auth.request_token.key
                oauth_verifier = auth.request_token.secret
                logging.info('Lookup the request token')
                request_token = OAuthToken.gql('WHERE token_key=:key', 
                                               key=oauth_token).get()
                if request_token is None:
                    logging.warning('We do not seem to have this ' \
                                    'request token, show an error')
                    return HttpResponse('Invalid token!')
                logging.info('Rebuild the auth handler')
                token_key = ask_undrgz_settings.TWITTER_CONSUMER_KEY
                token_secret = ask_undrgz_settings.TWITTER_CONSUMER_SECRET
                auth = tweepy.OAuthHandler(token_key, token_secret)
                auth.set_request_token(request_token.token_key, 
                                       request_token.token_secret)
                logging.info('Fetch the access token')
                try:
                    auth.get_access_token(oauth_verifier)
                except tweepy.TweepError, e:
                    logging.error('Failed to get access token: %s', str(e))
                    return HttpResponse(
                        'Failed to get access token: %s', str(e))
                
                oauth_token = auth.access_token.key
                oauth_token_secret = auth.access_token.secret
                auth = tweepy.OAuthHandler(token_key, token_secret)
                auth.set_access_token(oauth_token_secret, oauth_token)
                api = tweepy.API(auth)
                try:
                    answer = question.answer
                    s = '@%s %s: %s' % (username, question.ask, question.answer)
                    logging.debug('Send twitter: %s' % s)
                    if int(math.ceil(len(s)/140.0)) > 1:
                        s1 = '@%s %s: ' % (username, question.ask)
                        if int(math.ceil(len(s1)/140.0)) > 1:
                            api.update_status(
                                '@%s +1 stupid question!' % username)
                        else:
                            for i in range(int(math.ceil(len(s1)/140.0))):
                                api.update_status(
                                    s1 + question.answer[140*i:(140*i)+140])
                    else:
                        api.update_status(s)
                except Exception, e:
                    logging.error('Error in send for twitter: %s' % str(e))
        except Exception, e:
            logging.error('Error in send for xmpp: %s' % str(e))
    return HttpResponse(st)

def oauth_twitter(request):
    logging.debug('In question.views::oauth_twitter()')
    token_key = ask_undrgz_settings.TWITTER_CONSUMER_KEY
    token_secret = ask_undrgz_settings.TWITTER_CONSUMER_SECRET
    token_callback = ask_undrgz_settings.TWITTER_CALLBACK
    auth = tweepy.OAuthHandler(token_key, token_secret, token_callback)
    try:
        logging.info('Build a new oauth handler and display ' \
                     'authorization url to user')
        auth_url = auth.get_authorization_url()
        logging.debug('auth_url: %s' % str(auth_url))
    except tweepy.TweepError, e:
        logging.error('Failed to get a request token: %s' % str(e))
        return HttpResponse('Failed to get a request token: %s' % str(e))
    logging.info('We must store the request token for later use ' \
                 'in the callback page')
    request_token = OAuthToken(
            token_key = auth.request_token.key,
            token_secret = auth.request_token.secret
    )
    request_token.put()
    return HttpResponseRedirect(auth_url)

def oauth_twitter_callback(request):
    logging.debug('In question.views::oauth_twitter_callback()')
    oauth_token = request.GET.get('oauth_token', None)
    oauth_verifier = request.GET.get('oauth_verifier', None)
    if oauth_token is None:
        logging.warning('Ivalid request!')
        return HttpResponse('Missing required parameters!')
    logging.info('Lookup the request token')
    request_token = OAuthToken.gql('WHERE token_key=:key', 
                                   key=oauth_token).get()
    if request_token is None:
        logging.warning('We do not seem to have this request token, ' \
                        'show an error')
        return HttpResponse('Invalid token!')
    logging.info('Rebuild the auth handler')
    token_key = ask_undrgz_settings.TWITTER_CONSUMER_KEY
    token_secret = ask_undrgz_settings.TWITTER_CONSUMER_SECRET
    auth = tweepy.OAuthHandler(token_key, token_secret)
    auth.set_request_token(request_token.token_key, request_token.token_secret)
    logging.info('Fetch the access token')
    try:
        auth.get_access_token(oauth_verifier)
    except tweepy.TweepError, e:
        logging.error('Failed to get access token: %s', str(e))
        return HttpResponse('Failed to get access token: %s', str(e))
    return HttpResponse(auth.access_token)

def show_me_underguiz(request):
    logging.debug('In question.views::show_me_underguiz()')
    question_form = QuestionForm()
    return render_to_response('easter_egg.html', {
        'question_form': question_form,
        'recent_stupid_questions': _recent_stupid_questions(),
    })