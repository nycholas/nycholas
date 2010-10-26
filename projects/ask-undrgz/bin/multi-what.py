#!/usr/bin/env python
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
import sys
import socket
import string
import threading

NICK="undrz-answers"
IDENT="undrz-answers"
REALNAME="undrz-answers"
readbuffer=""
CHANNELINIT="#moonstonemedical"


class What(threading.Thread):
    def __init__(self, id, username):
        super(What, self).__init__()
        self.id = id
        self.username = "%s-%d" % (username, id)

    def run(self):
        server = socket.socket()
        server.connect(("irc.freenode.net", 6667))
        server.send("NICK %s\r\n" % self.username)
        server.send("USER %s %s :%s\r\n" % (self.username, self.username, self.username))
        server.send('JOIN %s\r\n' % CHANNELINIT)
        server.send('NOTICE underguiz WTF?\r\n')
        while True:
            data = server.recv(1024)
            if data.find('PING') != -1:
                server.send("PONG %s\r\n" % data.split()[1])
            print data



number_threads = 1
threads = [What(i, NICK) for i in range(number_threads)]
for i, thread in enumerate(threads):
    thread.start()
for i, thread in enumerate(threads):
    thread.join()
