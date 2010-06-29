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

# Include base path in sytem path for Python old.
syspath = os.path.abspath(os.path.dirname(__file__))
if not syspath in sys.path:
    sys.path.append(syspath)

from utils import constant as constant
from utils import debuger as debuger
from utils import logger as logger
from utils import configure as configure
from utils import i18n as i18n

configure.sys_coding()
debuger.sys_debugging()
logger.sys_logging()
i18n.gettext_locale()
