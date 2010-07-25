# -*- coding: utf-8 -*-
import logging

from google.appengine.api import xmpp


class XMPP:
    def __init__(self, id, user_address):
        logging.debug("In XMPP::__init__()")
        self.id = id
        self.user_address = user_address
        
    def is_online(self):
        logging.debug("In XMPP::is_online()")
        return xmpp.get_presence(self.user_address)
            
    def send_message(self, message):
        logging.debug("In XMPP::send_message()")
        chat_message_sent = False
        if xmpp.get_presence(self.user_address):
            status_code = xmpp.send_message(self.user_address, self.message)
            chat_message_sent = (status_code != xmpp.NO_ERROR)
        return chat_message_sent
