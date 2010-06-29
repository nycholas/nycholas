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
        #self.__annex = file_to_bin(search_zipfile(opts.get("annex_dir")))
        self.__annex = search_zipfile(opts.get("annex_dir"))
        self.tables = {}
        
    def get(self, key):
        return self.tables.get(key)
        
    def populate(self, forwards):
        logging.debug("In Email::populate()")
        logging.debug("++ forwards: %s" % str(forwards))       
        self.tables = copy.deepcopy(forwards) # ;-), Yeah! Deepcopy!
        for forward, confs in self.tables.items():
            logging.debug(":: forward: %s" % str(forward))
            for table in confs.get("tables"):
                logging.debug(":: table: %s" % str(table))
                for t, columns in table.items():
                    for k, v in columns.items():
                        logging.debug(":: key: %s, value: %s" % (k, v))
                        columns[k] = re.sub(EmailModel.EMAIL_ID, 
                                            self.__email_id, columns[k])
                        columns[k] = re.sub(EmailModel.EMAIL_TO, 
                                            self.__to, columns[k])
                        columns[k] = re.sub(EmailModel.EMAIL_FROM, 
                                            self.__from, columns[k])
                        columns[k] = re.sub(EmailModel.EMAIL_SUBJECT, 
                                            self.__subject, columns[k])
                        columns[k] = re.sub(EmailModel.EMAIL_DATE, 
                                            self.__date, columns[k])
                        columns[k] = re.sub(EmailModel.EMAIL_CONTENT, 
                                            self.__content, columns[k])
                        columns[k] = re.sub(EmailModel.EMAIL_ANNEX, 
                                            self.__annex, columns[k])
                logging.debug(":: Populating table: %s" % (str(table),))
        logging.debug(":: Table populate: %s" % (str(self.tables),))
        

if __name__ == "__main__":
    pass