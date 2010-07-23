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

HOME_DIR = os.path.abspath(os.path.expanduser("~"))
PACKAGE_DIR = ".%s" % NAME_UNIX
USER_CONFIG_DIR = os.path.join(HOME_DIR, PACKAGE_DIR)

BASE_DIR = os.path.join(os.path.abspath(os.path.dirname(__file__)),
                        os.pardir, os.pardir)
SHARE_DIR = os.path.join("usr", NAME_UNIX, "share")
if not os.path.exists(SHARE_DIR):
    SHARE_DIR = os.path.join(BASE_DIR, "share")
RESOURCES_DIR = os.path.join(USER_CONFIG_DIR, "resources")
if not os.path.exists(RESOURCES_DIR):
    RESOURCES_DIR = os.path.join(BASE_DIR, NAME_UNIX, "resources")

LOGGING_LEVELS = {
    "critical": logging.CRITICAL,
    "error": logging.ERROR,
    "warning": logging.WARNING,
    "info": logging.INFO,
    "debug": logging.DEBUG
}
LOGGING_DIR = os.path.join(RESOURCES_DIR, "log")
if not os.path.exists(LOGGING_DIR):
    LOGGING_DIR = os.path.join(BASE_DIR, "share", "log")
LOGGING_LEVEL = LOGGING_LEVELS.get("info", logging.NOTSET)
LOGGING_FILE_CONF = os.path.join(RESOURCES_DIR, "logging.conf")

PROFILE_FILE_CONF = os.path.join(RESOURCES_DIR, "profile.conf")

FORWARD_DIR = os.path.join(BASE_DIR, NAME_UNIX, "forwards")
FORWARD_FILE_CONF = os.path.join(RESOURCES_DIR, "forward.conf")

USE_I18N = True
CODING = "utf-8"
LOCALE_DOMAIN = NAME_UNIX
LOCALE_DIR = os.path.abspath(os.path.join(SHARE_DIR, "locale"))
