# -*- coding: utf-8 -*-
import datetime
from django.http import HttpResponse, HttpResponseRedirect
from django.utils.translation import ugettext as _
from django.shortcuts import render_to_response
from django.utils import simplejson
from django.core import serializers

from ask_undrgz.middleman.orchestra import Orchestra
from ask_undrgz.question.forms import QuestionForm
from ask_undrgz.question.models import Question

def index(request):
    question_curr_key = request.GET.get('key')
    if request.method == 'POST':
        question_form = QuestionForm(request.POST)
        if question_form.is_valid():
            new_question = question_form.save()
            #orchestra = Orchestra(new_question.ask)
            #orchestra.start()
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
    })