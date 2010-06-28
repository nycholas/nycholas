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
import logging


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
        self.__content = opts.get("content")
        self.__annex = opts.get("annex")
        self.tables = {}
        
    def get(self, key):
        return self.tables.get(key)
        
    def populate(self, forwards):
        logging.debug("In Email::populate()")
        logging.debug("++ forwards: %s" % str(forwards))
        for forward, confs in forwards.iteritems():
            logging.debug(":: forward: %s" % str(forward))
            for table in confs.get("tables"):
                logging.debug(":: table: %s" % str(table))
                for t, columns in table.copy().iteritems():
                    for k, v in columns.iteritems():
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
        logging.debug(":: Table: %s" % (str(forwards),))
        self.tables = forwards
        
    def to_dict(self, keyname):
        logging.debug("In Email::to_dict()")
        #return self.table.get(keyname)
        
    def to_tuple(self, keyname):
        logging.debug("In Email::to_tuple()")
        #table = self.table.get(keyname)
        #return tuple(table.values())
        

if __name__ == "__main__":
    pass