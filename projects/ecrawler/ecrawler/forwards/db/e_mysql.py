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
import MySQLdb

from ecrawler.middleman import ForwardBase


class MySQLForward(ForwardBase):
    def __init__(self):
        logging.debug("In MySQLForward::__init__()")

    def _zipfile_to_bin(self, zipfile):
        logging.debug("In MySQLForward::_zipfile_to_bin()")
        return zipfile

    def _search_zipfile(self, items):
        logging.debug("In MySQLForward::_search_zipfile()")
        logging.debug(":: items: %s" % str(items))
        for k, v in items.iteritems():
            if v.find(".zip") != -1:
                if os.path.exists(v) and os.path.isfile(v):
                    items[k] = self._zipfile_to_bin(v)

    def execute(self, items):
        logging.debug("In MySQLForward::execute()")
        logging.debug("++ items: %s" % str(items))

        tables = items.get("tables")
        logging.debug(":: Number of items: %s" % len(tables))
        if not tables:
            return

        hostname = items.get("hostname") or "localhost"
        try:
            port = int(items.get("port"))
        except ValueError: port = 3306
        username = items.get("username") or "root"

        logging.info("Connecting in database: %s:%s@%s:%s..." % \
                     (username, items.get("name"), hostname, port))
        self.conn = MySQLdb.connect(host=hostname, port=port,
                                    user=items.get("username"),
                                    passwd=items.get("password"),
                                    db=items.get("name"))

        logging.info("Genareting query...")
        query = self.generate_query(tables[-1])
        logging.debug(":: query: %s" % query)

        t = []
        for column in tables:
            t.extend(self.execute_many(column.values()))

        print t

        logging.info("Running and inserting items in database...")
        cur = self.conn.cursor()
        cur.executemany(query, t)
        logging.info("Insert a row of data")

        logging.info("Save (commit) the changes...")
        self.conn.commit()

        logging.info("Close connection database...")
        self.conn.close()

    def execute_many(self, items):
        logging.debug("In MySQLForward::execute_many()")
        logging.debug(":: items: %s" % str(items))
        [self._search_zipfile(i) for i in items]
        many = [tuple(i.values()) for i in items]
        logging.debug(":: Items for database: %s" % str(many))
        return many

    def generate_query(self, items):
        logging.debug("In MySQLForward::generate_query()")
        table = items.keys()[0]
        columns = items.values()[0]
        query = "INSERT INTO " + table + " "
        keys = "(" + "".join([i+"," for i in columns.keys()])[:-1] + ")"
        #values = "(" + "".join([i+"," for i in columns.values()])[:-1] + ")"
        values = "(" + "".join("%s," for i in columns.values())[:-1] + ")"
        query += keys + " VALUES " + values
        return query

    def clean(self, table):
        logging.debug("In MySQLForward::clean()")

        logging.info("Running and delete items in database...")
        cur = self.conn.cursor()
        cur.execute("delete from %s" % table)
        logging.info("Delete a row of data")

        logging.info("Save (commit) the changes...")
        self.conn.commit()

