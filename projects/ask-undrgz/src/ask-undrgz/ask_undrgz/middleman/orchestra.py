# -*- coding: utf-8 -*-
from strategy.xmpp import XMPP

class Orchestra:
    
    def __init__(self, question):
        question = '%s?' % question if question.find('?') != -1 else question
        self.questions = [question]
        
    def start(self, number_threads=5):
        if number_threads > len(self.questions):
            number_threads = len(self.questions)
        slices = [self.questions[i::number_threads] for i in range(number_threads)]
        threads = [XMPP(i, slices[i]) for i in range(number_threads)]
        for thread in threads:
            thread.run()
        return
    
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
            
    def loop(self):
        pass
