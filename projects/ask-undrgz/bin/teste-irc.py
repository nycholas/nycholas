#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# ask-undrgz system of questions uses data from underguiz.
# Copyright (c) 2011, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS'
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
import random

NICK='ask-undrgz'
IDENT='ask-undrgz'
REALNAME='ask-undrgz'
CHANNEL='#moonstonemedical'

server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server.connect(('irc.freenode.net', 6667))
server.send('NICK %s\r\n' % (NICK,))
server.send('USER %s %s ask-undrgz.appspot.com %s\r\n' % (NICK, IDENT, REALNAME))
server.send('JOIN %s\r\n' % (CHANNEL,))

while True:
    data = server.recv(1024)
    data_split = data.split(':')

    # Ping message
    # data: PING :jordan.freenode.net
    if data.find('PING') != -1:
        server.send('PONG %s\r\n' % data.split()[1])
    # Private message
    # <o_lalertom> ask-undrgz, hai!
    # data: :o_lalertom!~Nycholas@666-666-666-666.cenobites.org PRIVMSG #moonstonemedical :ask-undrgz, hai!
    # <o_lalertom> ha! ask-undrgz ha!
    # data: :o_lalertom!~Nycholas@666-666-666-666.cenobites.org PRIVMSG #moonstonemedical :ha! ask-undrgz ha!
    elif data_split[-1].find(NICK) != -1:
        server.send('PRIVMSG %s :%s\r\n' % (CHANNEL, 'hai!'))
    # Public message
    # <o_lalertom> hai!
    # data: :o_lalertom!~Nycholas@666-666-666-666.cenobites.org PRIVMSG #moonstonemedical :hai!
    sys.stdout.write(data)
