# -*- coding: utf-8 -*-
from google.appengine.api import xmpp


class XMPP:
    def __init__(self, id, message):
        self.id = id
        self.message = message
            
    def run(self):
        user_address = 'underguiz@ilostmyself.org'
        chat_message_sent = False
        if xmpp.get_presence(user_address):
            status_code = xmpp.send_message(user_address, self.message)
            chat_message_sent = (status_code != xmpp.NO_ERROR)
        if not chat_message_sent:
            # Send an email message instead...
            pass
