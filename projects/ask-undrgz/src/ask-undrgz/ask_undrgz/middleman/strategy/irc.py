# -*- coding: utf-8 -*-
import socket
import threading

TARGET = 'underguiz'
USERNAME = 'ask-undrz'
CHANNELINIT = '#moonstonemedical'


class IRC(threading.Thread):
    
    def __init__(self, id, server, question):
        self.id = id
        self.server = server
        self.question = question
        self.target = TARGET
        self.username = '%s-%d' % (USERNAME, id)
        self.channel = CHANNELINIT
    
    def run(self):
        self.server.send('NICK %s\r\n' % self.username)
        self.server.send('USER %s %s %s %s\r\n' % (self.username,) * 4)
        self.server.send('JOIN %s\r\n' % self.channel)
        self.server.send('NOTICE %s %s\r\n' % (self.target, self.question))