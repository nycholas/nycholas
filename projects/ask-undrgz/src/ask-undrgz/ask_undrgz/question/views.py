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

import tweepy

from google.appengine.api import xmpp
from google.appengine.api import urlfetch

from django.conf import settings
from django.http import HttpResponse
from django.http import HttpResponseRedirect
from django.core import serializers
from django.utils import simplejson
from django.shortcuts import render_to_response
from django.utils.translation import ugettext as _

from ask_undrgz.utils import oauth
from ask_undrgz.question.models import Question
from ask_undrgz.question.forms import QuestionForm


def _recent_stupid_questions():
    '''The latest stupid questions.
    '''
    logging.debug('In question.views::_recent_stupid_questions()')
    questions = Question.all()
    if not settings.DEBUG:
        logging.debug('Debug mode active')
        questions.filter('answered != ', None)
    questions = questions.order('-answered').fetch(30)
    return questions
        
def _send_invite_xmpp(user_address):
    '''Sends an invitation to address gtalk.
    '''
    logging.debug('In question.views::_send_invite_xmpp()')
    xmpp.send_invite(user_address)

def _send_message_xmpp(user_address, message):
    '''Sends a message to the address gtalk.
    '''
    logging.debug('In question.views::_send_message_xmpp()')
    chat_message_sent = False
    
    # Sends an invitation to address gtalk
    #_send_invite_xmpp(user_address)
    
    # Checks whether the user is online and talk on the list of appengine
    if xmpp.get_presence(user_address):
        logging.info('Presence OK: %s' % (user_address,))
        users_address = [user_address,] #'nycholas@gmail.com']
        status_code = xmpp.send_message(users_address, message)
        chat_message_sent = (status_code != xmpp.NO_ERROR)
    return chat_message_sent

def index(request):
    '''Main page of the application.
    '''
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
    '''Writes a new question and redirects to the response page.
    '''
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
    '''Response page.
    '''
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
    '''Latest stupid questions.
    '''
    logging.debug('In question.views::recent_stupid_questions()')
    question_top10 = _recent_stupid_questions()
    if request.is_ajax():
        return HttpResponse(
                    simplejson.dumps([q.to_dict() for q in question_top10]), 
                    mimetype='application/json')
    return HttpResponse('')

def is_online(request):
    '''Checks if the underguiz (clients) is online.
    '''
    logging.debug('In question.views::is_online()')
    user_address = request.REQUEST.get('from')
    
    # Brazil - America/Sao_Paulo, ;-)
    dt = datetime.datetime.utcnow()
    dt_hour = dt.hour - 3 if dt.hour - 3 > 0 else (dt.hour - 3) + 24
    
    # Easter Egg! HA! 
    easter_egg = False
    if dt_hour == 6 and dt.minute == 6 and dt.second >= 6:
        easter_egg = True
        
    # Is online?
    chat_message_sent = False
    
    # If the parameter 'from' (e-mail) GET is empty, then returns user offline
    if not user_address:
        if request.is_ajax():
            return HttpResponse(simplejson.dumps({
                'is_online': chat_message_sent,
                'easter_egg': easter_egg,
            }), mimetype='application/json')
        return HttpResponse('from is required', status=405)
    chat_message_sent = xmpp.get_presence(user_address)
    
    # If debug mode, then always online
    if settings.DEBUG:
        logging.debug('Debug mode active')
        chat_message_sent = True
        
    if request.is_ajax():
        return HttpResponse(simplejson.dumps({
                'is_online': chat_message_sent,
                'easter_egg': easter_egg,
            }), mimetype='application/json')
    return HttpResponse(chat_message_sent)
    
def send_message(request):
    '''Sends message to underguiz (clients).
    '''
    logging.debug('In question.views::send_message()')
    user_address = request.REQUEST.get('from')
    message = request.REQUEST.get('body')
    chat_message_sent = False
    if not user_address or not message:
        if request.is_ajax():
            return HttpResponse(simplejson.dumps(chat_message_sent), 
                                mimetype='application/json')
        return HttpResponse(_('From and message is required'), status=405)
    chat_message_sent = _send_message_xmpp(user_address, message)
    if request.is_ajax():
        return HttpResponse(simplejson.dumps(chat_message_sent), 
                            mimetype='application/json')
    return HttpResponse(chat_message_sent)
    
def incoming_chat(request):
    '''Mounts a chat with the underguiz (clients).
    '''
    logging.debug('In question.views::incoming_chat()')
    if request.method != 'POST':
        return HttpResponse(_('XMPP requires POST'), status=405)
    
    st = False
    sender = request.POST.get('from')
    toaddr = request.POST.get('to')
    message = request.POST.get('body')
    
    if not sender:
        logging.warn('Incoming chat without \'from\' key ignored')
        return HttpResponse(st)
    elif not message:
        logging.warning('Incoming chat without \'body\' key ignored')
        return HttpResponse(st)
    
    try:
        body = message.split(':')
        if len(body) <= 1 and not body[0].isdigit():
            logging.warn('Message not format ID:MESSAGE: %s' % (body,))
            return HttpResponse(st)
        
        id_question = int(body[0]) if body[0].isdigit() else 0
        answer = ''.join(body[1:]).strip()
        
        question = Question.get_by_id(id_question)
        
        # If the answer already exists, then concatenates the responses 
        # with HTML formatting
        if question.answer:
            space = '<br />' + '&nbsp;'*16
            space = ''
            question.answer = '%s; %s%s' % (question.answer, space, answer)
        else:
            question.answer = answer 
        question.answered = datetime.datetime.now()
        question.save()
        
        # Send XMPP message
        toaddrs = [toaddr,] #'nycholas@gmail.com']
        sts = xmpp.send_message(toaddrs, answer)
        logging.debug('XMPP status %s', (str(sts),))
    except Exception, e:
        logging.error('Error in send for xmpp: %s' % (str(e),))
        return HttpResponse(st)
        
    # Send twitter message
    if answer:
        username = settings.TWITTER_USERNAME
        token_key = settings.TWITTER_CONSUMER_KEY
        token_secret = settings.TWITTER_CONSUMER_SECRET
        oauth_token = settings.TWITTER_OAUTH_TOKEN
        oauth_token_secret = settings.TWITTER_OAUTH_TOKEN_SECRET
        token_callback = settings.TWITTER_CALLBACK
        
        try:
            logging.info('Creating an OAuthHandler instance')
            auth = tweepy.OAuthHandler(token_key, token_secret, 
                                       token_callback)
        except Exception, e:
            logging.error('Error: %s' % (str(e),))
            return HttpResponse(st)
            
        try:
            auth.set_access_token(oauth_token, oauth_token_secret)
            api = tweepy.API(auth)
        except Exception, e:
            logging.error('Error: %s' % (str(e),))
            return HttpResponse(st)
            
        try:
            s = '%s %s' % (question.ask, answer)
            logging.debug('Send twitter: %s' % (s,))
            if int(math.ceil(len(s)/140.0)) > 1:
                s1 = '%s ' % (question.ask,)
                if int(math.ceil(len(s1)/140.0)) > 1:
                    api.update_status(_('+1 stupid question!'))
                else:
                    for i in range(int(math.ceil(len(s1)/140.0))):
                        api.update_status(
                            s1 + answer[140*i:(140*i)+140])
            else:
                api.update_status(s)
        except Exception, e:
            logging.error('Error in send for twitter: %s' % (str(e),))
            return HttpResponse(st)
    return HttpResponse(st)

def oauth_twitter(request):
    logging.debug('In question.views::oauth_twitter()')
    token_key = settings.TWITTER_CONSUMER_KEY
    token_secret = settings.TWITTER_CONSUMER_SECRET
    token_callback = settings.TWITTER_CALLBACK
    auth = tweepy.OAuthHandler(token_key, token_secret, token_callback)
    #auth = oauth.TwitterClient(token_key, token_secret, token_callback)
    try:
        logging.info(
            'Build a new oauth handler and display authorization url to user')
        auth_url = auth.get_authorization_url()
        logging.debug('auth_url: %s' % (str(auth_url),))
    except Exception, e:
        logging.error('Failed to get a request token: %s' % (str(e),))
        return HttpResponse(_('Failed to get a request token: %(error)s') % \
                              {'error': str(e)})
    logging.info(
        'We must store the request token for later use in the callback page')
    return HttpResponseRedirect(auth_url)

def oauth_twitter_callback(request):
    logging.debug('In question.views::oauth_twitter_callback()')
    oauth_token = request.GET.get('oauth_token', None)
    oauth_verifier = request.GET.get('oauth_verifier', None)
    if not oauth_token or not oauth_verifier:
        logging.warning('Invalid request!')
        return HttpResponse(_('Missing required parameters!'))
    
    logging.info('Lookup the request token')
    token_key = settings.TWITTER_CONSUMER_KEY
    token_secret = settings.TWITTER_CONSUMER_SECRET
    token_callback = settings.TWITTER_CALLBACK
    
    logging.info('Rebuild the auth handler')
    auth = tweepy.OAuthHandler(token_key, token_secret, token_callback)
    #auth = oauth.TwitterClient(token_key, token_secret, token_callback)
    
    logging.info('Fetch the access token')
    try:
        auth.get_access_token(oauth_verifier)
        #auth.set_access_token(auth.access_token.key, auth.access_token.secret)
    except Exception, e:
        logging.error('Failed to get access token: %s', (str(e),))
        return HttpResponse(_('Failed to get access token: %(error)s') % \
                              {'error': str(e)})
    
    #api = tweepy.API(auth)
    #api.update_status('test from hell!')
    
    return HttpResponse(True)

def show_me_underguiz(request):
    '''Shows a underguiz (Easter Egg).
    '''
    logging.debug('In question.views::show_me_underguiz()')
    question_form = QuestionForm()
    return render_to_response('easter_egg.html', {
        'question_form': question_form,
        'recent_stupid_questions': _recent_stupid_questions(),
    })