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
import os
import sys
import glob
import locale
import logging
import logging.handlers
import logging.config

import constant as constant

def sys_coding():
    logging.debug("In sys_coding()")
    locale.setlocale(locale.LC_ALL, '')
    try:
        sys.setappdefaultencoding(constant.CODING)
    except AttributeError:
        try:
            reload(sys)
            sys.setdefaultencoding(constant.CODING)
        except LookupError:
            pass
