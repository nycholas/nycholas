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
import logging
import optparse

# Include base path in sytem path for Python old.
syspath = os.path.abspath(os.path.dirname(__file__))
if not syspath in sys.path:
    sys.path.append(syspath)

from ecrawler.orchestra_collector import Orchestra

__version__ = 0.1

def main(args):
    logging.debug("In ecrawler.main()")
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
    parser.add_option("-m", "--mailbox",
                      action="store", dest="mailbox",
                      help="Mailbox of the server e-mail (IMAP) " \
                           "[default: UNSEEN]")
    parser.add_option("-d", "--directory",
                      action="store", dest="directory",
                      help="Directory swap file-sharing [default: /tmp]")
    parser.add_option("-f", "--forwards",
                      action="store", dest="forwards",
                      help="Forwards objects.")
    parser.add_option("-j", "--jobs", type="int",
                      action="store", dest="jobs", default=5,
                      help="Specifies  the  number of jobs (commands) to run " \
                           "simultaneously.")
    parser.add_option("-r", "--remove",
                      action="store_true", dest="is_remove", default=False,
                      help="Remove files of directory swap")
    parser.add_option("-t", "--test",
                      action="store_true", dest="is_test", default=False,
                      help="Test application with forward")
    parser.add_option("-l", "--logging",
                      action="store_true", dest="is_logging", default=False,
                      help="Logging console mode")
    parser.add_option("-v", "--verbose",
                      action="store_true", dest="is_debug", default=False,
                      help="Verbose (debug) mode")
    (options, args) = parser.parse_args()
    logging.debug(":: options: %s" % options)
    logging.debug(":: args: %s" % args)

    # Convert string in dictionary!
    opts = eval(str(options))

    orchestra = Orchestra(opts=opts, args=args)
    orchestra.start(int(options.jobs))


if __name__ == '__main__':
    main(sys.argv)
