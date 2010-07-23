# -*- coding: utf-8 -*-
import logging
from google.appengine.api import xmpp


class XMPP:
    
    def __init__(self, id, message):
        logging.debug("In XMPP::__init__()")
        self.id = id
        self.message = message
            
    def run(self):
        logging.debug("In XMPP::run()")
        user_address = 'underguiz@ilostmyself.org'
        chat_message_sent = False
        if xmpp.get_presence(user_address):
            status_code = xmpp.send_message(user_address, self.message)
            chat_message_sent = (status_code != xmpp.NO_ERROR)
        if not chat_message_sent:
            # Send an email message instead...
            pass

        user_address = 'nycholas@gmail.com'
        chat_message_sent = False
        if xmpp.get_presence(user_address):
            status_code = xmpp.send_message(user_address, self.message)
            chat_message_sent = (status_code != xmpp.NO_ERROR)
        if not chat_message_sent:
            # Send an email message instead...
            pass
