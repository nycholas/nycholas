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
import cx_Oracle

from ecrawler.middleman import ForwardBase


class OracleForward(ForwardBase):
    def __init__(self):
        logging.debug("In OracleForward::__init__()")
        
    def _zipfile_to_bin(self, zipfile):
        logging.debug("In OracleForward::_zipfile_to_bin()")
        return open(zipfile, "rb").read()

    def _search_zipfile(self, items):
        logging.debug("In OracleForward::_search_zipfile()")
        logging.debug(":: items: %s" % str(items))
        for k, v in items.iteritems():
            if v.find(".zip") != -1:
                if os.path.exists(v) and os.path.isfile(v):
                    self.cur.setinputsizes(eval("%s=cx_Oracle.BLOB" % k))
                    items[k] = self._zipfile_to_bin(v)
                    
    def _search_column(self, items, column, func):
        logging.debug("In OracleForward::_search_column()")
        logging.debug(":: column: %s" % str(column))
        items.setdefault(column, func(items.get(column)))

    def execute(self, items):
        logging.debug("In OracleForward::execute()")
        logging.debug("++ items: %s" % str(items))

        tables = items.get("tables")
        logging.debug(":: Number of items: %s" % len(tables))
        if not tables:
            return
        
        hostname = items.get("hostname") or "localhost"
        try:
            port = int(items.get("port"))
        except ValueError: port = 1521
        username = items.get("username") or "oracle"
        hostdb = "%s:%d/%s" % (hostname, port, items.get("name"))

        logging.info("Connecting in database: %s..." % hostdb)
        self.conn = cx_Oracle.connect(items.get("username"), 
                                      items.get("password"), hostdb)

        logging.info("Genareting query...")
        query = self.generate_query(tables[-1])
        logging.debug(":: query: %s" % query)
        
        logging.info("Running and inserting items in database...")
        self.cur = self.conn.cursor()

        t = []
        for column in tables:
            t.extend(self.execute_many(column.values()))

        self.cur.prepare(query)
        self.cur.executemany(None, t)
        logging.info("Insert a row of data")

        logging.info("Save (commit) the changes...")
        self.conn.commit()

        logging.info("Close connection database...")
        self.conn.close()

    def execute_many(self, items):
        logging.debug("In OracleForward::execute_many()")
        logging.debug(":: items: %s" % str(items))
        [self._search_zipfile(i) for i in items]
        many = [tuple(i.values()) for i in items]
        logging.debug(":: Items for database: %s" % str(many))
        return many

    def generate_query(self, items):
        logging.debug("In OracleForward::generate_query()")
        table = items.keys()[0]
        columns = items.values()[0]
        query = "insert into " + table + " "
        keys = "(" + "".join([i+"," for i in columns.keys()])[:-1] + ")"
        values = "("
        for i, value in enumerate(columns.values()):
            values += ":"+str(i+1)+"," 
        values = values[:-1] + ")"
        query += keys + " values " + values
        return query
