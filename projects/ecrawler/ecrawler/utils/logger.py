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
import logging
import logging.handlers
import logging.config

import constant as constant

def sys_logging():
    logging.debug("In sys_logging()")
    if constant.LOGGING:
        fileconf = constant.LOGGING_FILE_CONF
        if os.path.exists(fileconf):
            try:
                logging.config.fileConfig(fileconf)
            except IOError, e:
                print "!! Logger not initialize: %s" % fileconf
                print "!! Logger error: %s" % e
        else:
            filelog = os.path.join(constant.BASE_DIR, "share", "log"
                                   "%s.log" % constant.NAME_UNIX)
            level = constant.LOGGING_LEVEL
            logging.basicConfig(
                filename=filelog,
                filemode="w+",
                format="%(asctime)s %(levelname)-8s %(message)s",
                datefmt="%Y-%m-%d %H:%M:%S",
                level=level)
        logging.debug("** Initialize logging **")