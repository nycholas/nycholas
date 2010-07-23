# -*- coding: utf-8 -*-
import socket

from strategy.irc import IRC

HOST = 'irc.freenode.net'
PORT = 6667


class Orchestra:
    
    def __init__(self, question):
        question = '%s?' % question if question.find('?') != -1 else question
        self.questions = [question]
        self.server = socket.socket()
        self.server.connect((HOST, PORT))
        
    def add(self, question):
        self.questions.append(question)
        
    def start(self, number_threads=5):
        if number_threads > len(self.questions):
            number_threads = len(self.questions)
        slices = [self.questions[i::number_threads] for i in range(number_threads)]
        threads = [IRC(i, self.server, slices[i]) for i in range(number_threads)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
            
    def loop(self):
        pass
