#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# ask-undrgz system of questions uses data from underguiz.
# Copyright (C) 2010 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import sys
import socket
import string

NICK="undrz-answers"
IDENT="undrz-answers"
REALNAME="undrz-answers"
readbuffer=""
CHANNELINIT="#ask-undrz"

server = socket.socket()
server.connect(("irc.freenode.net", 6667))
server.send("NICK undrz-answers\r\n")
server.send("USER undrz-answers undrz-answers :undrz-answers\r\n")
server.send('JOIN #ask-undrz\r\n')
server.send('NOTICE underguiz WTF?\r\n')

while True:
    data = server.recv(1024)
    if data.find('PING') != -1:
        server.send("PONG %s\r\n" % data.split()[1])
    print data
