# -*- coding: utf-8 -*-
#
# Simple example by comet, orbited and stomp.
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
import json
import datetime

from django.shortcuts import render_to_response
from django.http import HttpResponse

import stomp

from models import Clock

conn = stomp.Connection()
conn.start()
conn.connect()
conn.subscribe(destination='/clock', ack='auto')

def index(request):
    clock = Clock.objects.order_by('-time')
    clock = clock[0] if len(clock) > 0 else None
    return render_to_response('index.html', {
        'clock': clock,
    })

def clock_list(request):
    clocks = Clock.objects.all()
    return render_to_response('clock_list.html', {
        'clocks': clocks,
    })
    
def clock_add(request):
    clock = Clock(time=datetime.datetime.today())
    clock.save()
    message = json.dumps({
        'time': clock.time.strftime('%d/%m/%Y %H:%M:%S')
    })
    conn.send(message, destination='/clock')
    return HttpResponse('ok')
    