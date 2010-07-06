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
import re
import copy
import logging

from ecrawler.utils.commons import *


class EmailModel(object):
    EMAIL_ID = "__ECRAWLER_EMAIL_ID__"
    EMAIL_TO = "__ECRAWLER_EMAIL_TO__"
    EMAIL_FROM = "__ECRAWLER_EMAIL_FROM__"
    EMAIL_SUBJECT = "__ECRAWLER_EMAIL_SUBJECT__"
    EMAIL_DATE = "__ECRAWLER_EMAIL_DATE__"
    EMAIL_CONTENT = "__ECRAWLER_EMAIL_CONTENT__"
    EMAIL_ANNEX = "__ECRAWLER_EMAIL_ANNEX__"
    
    def __init__(self, opts):
        logging.debug("In Email::__init__()")
        self.__id = opts.get("id", 0)
        self.__email_id = opts.get("email_id")
        self.__to = opts.get("to")
        self.__from = opts.get("from")
        self.__subject = opts.get("subject")
        self.__date = opts.get("date")
        self.__content_dir = opts.get("content_dir")
        self.__content_files = search_files(opts.get("content_dir"))
        self.__content = files_to_str(opts.get("content_dir"))
        self.__annex_dir = opts.get("annex_dir")
        self.__annex_zipfile = search_zipfile(opts.get("annex_dir"))
        self.__annex = search_zipfile(opts.get("annex_dir"))
        self.tables = {}
        
    def __str__(self):
        return str(self.tables)
        
    def __populate(self, column):
        d = {
            EmailModel.EMAIL_ID: self.__email_id, 
            EmailModel.EMAIL_TO: self.__to, 
            EmailModel.EMAIL_FROM: self.__from,
            EmailModel.EMAIL_SUBJECT: self.__subject, 
            EmailModel.EMAIL_DATE: self.__date, 
            EmailModel.EMAIL_CONTENT: self.__content, 
            EmailModel.EMAIL_ANNEX: self.__annex
        }
        for k, v in d.iteritems():
            column = re.sub(k, v, column)
        return column
    
    def get(self, key=None):
        if key is None:
            return self.tables
        return self.tables.get(key)
    
    def email_id(self):
        return self.__email_id
        
    def populate(self, forwards):
        logging.debug("In Email::populate()")
        logging.debug("++ forwards: %s" % str(forwards))       
        self.tables = copy.deepcopy(forwards) # ;-), Yeah! Deepcopy!
        for forward, confs in self.tables.iteritems():
            logging.debug(":: forward: %s" % str(forward))
            for table in confs.get("tables"):
                logging.debug(":: table: %s" % str(table))
                for table_name, columns in table.iteritems():
                    for column in columns:
                        for k, v in column.iteritems():
                            logging.debug(":: key: %s, value: %s" % (k, v))
                            column[k] = self.__populate(v)
                logging.debug(":: Populating table: %s" % (str(table),))
        logging.debug(":: Table populate: %s" % (str(self.tables),))
        

if __name__ == "__main__":
    pass