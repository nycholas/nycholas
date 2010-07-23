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
try:
    import sqlite3
except ImportError, e:
    logging.warning("!! Module sqlite3 not found")
    import sqlite as sqlite3

from ecrawler.middleman import ForwardBase
from ecrawler.utils.commons import *


class SQLite3Forward(ForwardBase):
    SQLITE3_INTEGER = "INTEGER"
    SQLITE3_TEXT = "TEXT"
    SQLITE3_BLOB = "BLOB"
    SQLITE3_TYPES = (SQLITE3_INTEGER, SQLITE3_TEXT, SQLITE3_BLOB)

    def __init__(self):
        logging.debug("In SQLite3Forward::__init__()")

    def execute(self, items):
        logging.debug("In SQLite3Forward::execute()")
        logging.debug("++ items: %s" % str(items))

        list_table = items.get("tables")
        logging.info(":: Number of tables: %d" % len(list_table))
        if not list_table:
            return

        logging.info("Connecting in database: %s..." % items.get("name"))
        self.conn = sqlite3.connect(items.get("name"))

        logging.info("Create cursor...")
        cur = self.conn.cursor()

        for tables in list_table:
            for table, rows in tables.iteritems():
                logging.info(":: Number of rows: %d" % len(rows))

                logging.info("Genareting query: %s..." % table)
                query = self.generate_query(table, rows[-1])
                logging.debug(":: query: %s" % query)

                logging.info("Running and inserting items in database...")
                cur.executemany(query, self.execute_many(rows))
                logging.info("Insert a row of data")
        logging.info("Save (commit) the changes...")
        self.conn.commit()

        logging.info("Close connection database...")
        self.conn.close()

    def execute_many(self, rows):
        logging.debug("In SQLite3Forward::execute_many()")
        logging.debug(":: rows: %s" % str(rows))
        for row in rows[:]:
            for column, value in row.copy().iteritems():
                if column.find(":") != -1:
                    (name_column, type_column) = column.split(":")
                    if type_column == SQLite3Forward.SQLITE3_INTEGER:
                        pass
                    elif type_column == SQLite3Forward.SQLITE3_TEXT:
                        pass
                    elif type_column == SQLite3Forward.SQLITE3_BLOB:
                        if os.path.exists(value) and os.path.isfile(value):
                            row[column] = sqlite3.Binary(value)
        many = [tuple(i.values()) for i in rows]
        logging.debug(":: Items for database: %s" % str(many))
        return many

    def generate_query(self, table, items):
        logging.debug("In SQLite3Forward::generate_query()")
        keys = ""
        values = ""
        for column, value in items.iteritems():
            if column.find(":") != -1:
                (column, type_column) = column.split(":")
            keys += "%s," % column
            values += "?," #values += "%s," % value
        keys = "(%s)" % keys[:-1]
        values = "(%s)" % values[:-1]
        query = "insert into %s %s values %s" % (table, keys, values)
        return query

    def clean(self, table):
        logging.debug("In SQLite3Forward::clean()")

        logging.info("Running and delete items in database...")
        cur = self.conn.cursor()
        cur.execute("delete from %s" % table)
        logging.info("Delete a row of data")

        logging.info("Save (commit) the changes...")
        self.conn.commit()


if __name__ == "__main__":
    pass