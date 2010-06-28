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
import logging

DEBUG = False
LOGGING = True

NAME_PROGRAM = "eCrawler"
SENTENCE_PROGRAM = "E-mail Crawler"
TITLE_PROGRAM = "%s :: %s" % (NAME_PROGRAM, SENTENCE_PROGRAM)
VERSION_PROGRAM = "0.1"
NAME_BATTLE = "Night Crawler"
NAME_UNIX = "ecrawler"

BASE_DIR = os.path.join(os.path.abspath(os.path.dirname(__file__)),
                        os.pardir, os.pardir)
SHARE_DIR = os.path.join(BASE_DIR, "share")
RESOURCES_DIR = os.path.join(BASE_DIR, NAME_UNIX, "resources")

LOGGING_LEVELS = {
    "critical": logging.CRITICAL,
    "error": logging.ERROR,
    "warning": logging.WARNING,
    "info": logging.INFO,
    "debug": logging.DEBUG
}
LOGGING_LEVEL = LOGGING_LEVELS.get("debug", logging.NOTSET)
LOGGING_FILE_CONF = os.path.join(RESOURCES_DIR, "logging.conf")

PROFILE_FILE_CONF = os.path.join(RESOURCES_DIR, "profile.conf")

FORWARD_FILE_CONF = os.path.join(RESOURCES_DIR, "forward.conf")
FORWARD_DIR = os.path.join(BASE_DIR, NAME_UNIX, "forwards")

HOME_DIR = os.path.abspath(os.path.expanduser("~"))
PACKAGE_DIR = ".%s" % HOME_DIR
USER_CONFIG_DIR = os.path.join(HOME_DIR, PACKAGE_DIR)

USE_I18N = True
CODING = "utf-8"
QT_CODING = "utf8"
LOCALE_DOMAIN = NAME_UNIX
LOCALE_DIR = os.path.abspath(os.path.join(SHARE_DIR, "locale"))
