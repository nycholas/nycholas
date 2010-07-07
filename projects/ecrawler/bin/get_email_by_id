#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# eCrawler - E-mail Crawler.
# Copyright (C) 2010 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
#
# This file is part of eCrawler.
#
# eCrawler is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import sys
if sys.hexversion < 0x02040000:
    print "This script requires Python 2.4 or later."
    print "Currently run with version: %s" % sys.version
    print "Please install it. The source for Python can be found at: " \
          "http://www.python.org/."
    sys.exit(-1)
import os
import email
import imaplib
import optparse

__version__ = 0.1

def get_email_by_id(profile):
    hostname = profile.get("hostname") 
    port = profile.get("port")
    username = profile.get("username")
    password = profile.get("password")
    email_id = str(profile.get("email_id"))
    
    print "Connecting in %s:%d..." % (hostname, port)
    m = imaplib.IMAP4_SSL(hostname, port)

    print "Authenticating with %s..." % (username,)
    m.login(username, password)

    print "Select a mailbox..."
    m.select()

    print ":: Get email by email_id: %s" % (email_id,)
    resp, data = m.fetch(email_id, "(RFC822)")
    email_body = data[0][1]
    msg = email.message_from_string(email_body)
    print "++ email_header: %s" % msg.as_string()
        
    print "Close connection..."
    m.close()

    print "Logout with %s..." % (username,)
    m.logout()

def main(args):
    parser = optparse.OptionParser(
        usage="Usage: %prog [options]",
        version="%prog " + str(__version__))
    parser.add_option("-o", "--hostname",
                      action="store", dest="hostname",
                      help="Hostname of the server e-mail (IMAP)")
    parser.add_option("-p", "--port", type="int",
                      action="store", dest="port",
                      help="Port of the server e-mail (IMAP)")
    parser.add_option("-u", "--username",
                      action="store", dest="username",
                      help="Username of the server e-mail (IMAP)")
    parser.add_option("-s", "--password",
                      action="store", dest="password",
                      help="Password of the server e-mail (IMAP), " \
                           "using a plaintext password")
    parser.add_option("-i", "--id", type="int",
                      action="store", dest="email_id",
                      help="Mail id of the server e-mail (IMAP)")
    (options, args) = parser.parse_args()
    opts = options.__dict__
    if len([i for i in opts.values() if not i is None]) != len(opts.items()):
        parser.error("All options are requires.\n"
                     "Try '-h' or '--help' option for help message.")
    get_email_by_id(opts)


if __name__ == '__main__':
    main(sys.argv)
