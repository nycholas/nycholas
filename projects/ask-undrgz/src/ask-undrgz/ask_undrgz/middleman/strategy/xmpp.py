# -*- coding: utf-8 -*-
import logging

from google.appengine.api import xmpp
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app


class XMPPHandler(webapp.RequestHandler):
    def post(self):
        message = xmpp.Message(self.request.POST)
        if message.body[0:5].lower() == 'hello':
            message.reply("Greetings!")

application = webapp.WSGIApplication([('/_ah/xmpp/message/chat/', XMPPHandler)],
                                     debug=True)

run_wsgi_app(application)

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
