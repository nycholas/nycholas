# -*- coding: utf-8 -*-
from google.appengine.ext.db import djangoforms

from ask_undrgz.question.models import Question 


class QuestionForm(djangoforms.ModelForm):
    class Meta:
        model = Question
        exclude = ['answer', 'created']
