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

# Include base path in sytem path
sys.path.append(os.path.abspath(os.path.dirname(__file__)))

from ecrawler.utils import constant as constant
from ecrawler.utils import debuger as debuger
from ecrawler.utils import logger as logger
from ecrawler.utils import configure as configure
from ecrawler.utils import i18n as i18n
from ecrawler.orchestra_collector import Orchestra

__version__ = 0.1

def main(args):
    logging.debug("In ecrawler.main()")
    parser = optparse.OptionParser(
        usage="Usage: %prog [options]",
        version="%prog " + str(__version__))
    parser.add_option("-o", "--hostname",
                      action="store", dest="hostname",
                      help="Hostname")
    parser.add_option("-p", "--port",
                      action="store", dest="port",
                      help="Port")
    parser.add_option("-u", "--username",
                      action="store", dest="username",
                      help="Username")
    parser.add_option("-s", "--password",
                      action="store", dest="password",
                      help="Password")
    parser.add_option("-m", "--mailbox",
                      action="store", dest="mailbox",
                      help="Mailbox")
    parser.add_option("-d", "--directory",
                      action="store", dest="directory",
                      help="Directory")
    parser.add_option("-f", "--forwards",
                      action="store", dest="forwards",
                      help="Forwards")
    parser.add_option("-t", "--test",
                      action="store", dest="is_test",
                      help="Test application with forward")
    (options, args) = parser.parse_args()
    logging.debug(":: options: %s" % options)
    logging.debug(":: args: %s" % args)

    opts = eval(str(options))
    if args:
        args = dict([i.split(":") for i in args])
    else:
        args = {}
    orchestra = Orchestra(opts=opts, args=args)
    orchestra.start()


if __name__ == '__main__':
    main(sys.argv)
