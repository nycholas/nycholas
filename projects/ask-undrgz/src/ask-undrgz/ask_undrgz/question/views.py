# -*- coding: utf-8 -*-
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

def index(request):
    logging.debug("In question.views::index()")
    question_curr_key = request.GET.get('key')
    if request.method == 'POST':
        question_form = QuestionForm(request.POST)
        if question_form.is_valid():
            new_question = question_form.save()
            return HttpResponseRedirect(new_question.get_absolute_url())
    else:
        initial = {}
        if question_curr_key: 
            question = Question.get(question_curr_key)
            initial['ask'] = question.ask
        question_form = QuestionForm(initial=initial)
    question_top10 = Question.all().order('-created').fetch(10)
    return render_to_response('index.html', {
        'question_curr_key': question_curr_key,
        'question_top10': question_top10,
        'question_form': question_form,
    })
        
def answer(request, question_key):
    logging.debug("In question.views::answer()")
    question = Question.get(question_key)
    if request.is_ajax():
        return HttpResponse(simplejson.dumps(question.to_dict()), 
                            mimetype='application/json')
    initial = {}
    initial['ask'] = question.ask
    question_form = QuestionForm(initial=initial)
    question_top10 = Question.all().order('-created').fetch(10)
    return render_to_response('answer.html', {
        'question_curr_key': question_key,
        'question_top10': question_top10,
        'question_form': question_form,
        'answer_by_question': question.answer,
    })
    
def question_top10(request):
    logging.debug("In question.views::question_top10()")
    question_top10 = Question.all().order('-created').fetch(10)
    if request.is_ajax():
        return HttpResponse(simplejson.dumps([q.to_dict() for q in question_top10]), 
                            mimetype='application/json')
    return HttpResponse('')

def is_online(request):
    logging.debug("In question.views::incoming_chat()")
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
    logging.debug("In question.views::send_message()")
    user_address = request.REQUEST.get('from')
    message = request.REQUEST.get('body')
    chat_message_sent = False
    if not user_address or not message:
        if request.is_ajax():
            return HttpResponse(simplejson.dumps(chat_message_sent), 
                                mimetype='application/json')
        return HttpResponse('from and message is required', status=405)
    if xmpp.get_presence(user_address):
        status_code = xmpp.send_message(user_address, message)
        chat_message_sent = (status_code != xmpp.NO_ERROR)
    if request.is_ajax():
        return HttpResponse(simplejson.dumps(chat_message_sent), 
                            mimetype='application/json')
    return HttpResponse(chat_message_sent)
    
def incoming_chat(request):
    """/_ah/xmpp/message/chat/
    
    This handles incoming XMPP (chat) messages.
    
    Just reply saying we ignored the chat.
    """
    logging.debug("In question.views::incoming_chat()")
    if request.method != 'POST':
        return HttpResponse('XMPP requires POST', status=405)
    st = False
    sender = request.POST.get('from')
    message = request.POST.get('body')
    if not sender:
        logging.warn('Incoming chat without "from" key ignored')
    else:
        sts = xmpp.send_message([sender], message)
        logging.debug('XMPP status %r', sts)
    return HttpResponse(st)

