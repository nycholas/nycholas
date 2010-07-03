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
import logging

from ecrawler.middleman import ForwardBase


class PostgreSQLForward(ForwardBase):
    def __init__(self):
        logging.debug("In PostgreSQLForward::__init__()")

    def execute(self, items):
        logging.debug("In PostgreSQLForward::execute()")
        logging.debug("++ items: %s" % str(items))
        
        list_table = items.get("tables")
        logging.info(":: Number of tables: %d" % len(list_table))
        if not list_table:
            return
