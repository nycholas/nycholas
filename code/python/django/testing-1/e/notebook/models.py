# -*- coding: utf-8 -*-
#
# Django example 'e' project.
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
from django.db import models
from django.core.urlresolvers import reverse


class Notebook(models.Model):
    """Notebook tests

    >>> import datetime
    >>>
    >>> notebook = Notebook(name='Django', description='Master of Insanity',
    ...                     date_joined=datetime.datetime.now(), status=True)
    >>> notebook.save()
    >>>
    >>> notebook_id = notebook.id
    >>>
    >>> notebook.name
    'Django'
    >>>
    >>> notebook.name = 'Python'
    >>> notebook.save()
    >>> notebook.name
    'Python'
    >>> notebook.status
    True
    >>>
    >>> notebook = Notebook.objects.get(pk=notebook_id)
    >>> notebook.status = not notebook.status
    >>> notebook.save()
    >>> notebook.status
    False
    >>>
    >>> notebook = Notebook.objects.get(pk=notebook_id)
    >>> notebook.delete()
    >>>
    >>> notebook = Notebook.objects.get(pk=notebook_id)
    Traceback (most recent call last):
        ...
    DoesNotExist: Notebook matching query does not exist.
    """
    name = models.CharField(max_length=60)
    description = models.TextField()
    date_joined = models.DateTimeField()
    status = models.BooleanField()

    def __str__(self):
        return '%s' % self.name

    def __unicode__(self):
        return u'%s' % self.name

    def get_absolute_url(self):
        return reverse('notebook.views.notebook_edit',
                       kwargs={'notebook_id': self.id})