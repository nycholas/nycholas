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


class Error(Exception):
    """Base class for exceptions in this module."""
    pass

        
class ParseError(Error):
    pass


class LoggingParseError(ParseError):
    pass


class ProfileParseError(ParseError):
    pass


class ForwardParseError(ParseError):
    pass


class ConnectionError(Error):
    pass


class StructureError(Error):
    pass


class EmailError(Error):
    
    def __init__(self, email_id, msg):
        self.email_id = email_id
        self.msg = msg


class ModelError(EmailError):
    pass


class ForwardError(EmailError):
    pass


class RollbackError(Error):
    pass

