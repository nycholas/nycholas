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
    ORACLE_VARCHAR = "VARCHAR"
    ORACLE_NVARCHAR2 = "NVARCHAR2"
    ORACLE_LONG = "LONG"
    ORACLE_CHAR = "CHAR"
    ORACLE_NUMBER = "NUMBER"
    ORACLE_FLOAT = "FLOAT"
    ORACLE_DATE = "DATE"
    ORACLE_TIMESTAMP = "TIMESTAMP"
    ORACLE_CLOB = "CLOB"
    ORACLE_BLOB = "BLOB"
    ORACLE_BUILTIN = "BUILTIN"

    def __init__(self):
        logging.debug("In OracleForward::__init__()")

    def _search_column(self, items, column, func):
        logging.debug("In OracleForward::_search_column()")
        logging.debug(":: column: %s" % str(column))
        items.setdefault(column, func(items.get(column)))

    def execute(self, items):
        logging.debug("In OracleForward::execute()")
        logging.debug("++ items: %s" % str(items))

        list_table = items.get("tables")
        logging.info(":: Number of tables: %d" % len(list_table))
        if not list_table:
            return

        hostname = items.get("hostname") or "localhost"
        try:
            port = int(items.get("port"))
        except ValueError: 
            port = 1521
        username = items.get("username") or "oracle"
        hostdb = "%s:%d/%s" % (hostname, port, items.get("name"))

        logging.info("Connecting in database: %s..." % hostdb)
        self.conn = cx_Oracle.connect(items.get("username"),
                                      items.get("password"), hostdb)

        logging.info("Create cursor...")
        self.cur = self.conn.cursor()

        for tables in list_table:
            for table, rows in tables.iteritems():
                logging.info(":: Number of rows: %d" % len(rows))

                logging.info("Genareting query: %s..." % table)
                query = self.generate_query(table, rows[-1])
                logging.debug(":: query: %s" % query)

                self.execute_many(rows)

                logging.info("Running and inserting items in database...")
                self.cur.executemany(query, self.execute_many(rows))
                logging.info("Insert a row of data")
        logging.info("Save (commit) the changes...")
        self.conn.commit()

        logging.info("Close connection database...")
        self.conn.close()

    def execute_many(self, rows):
        logging.debug("In OracleForward::execute_many()")
        logging.debug(":: rows: %s" % str(rows))
        for row in rows[:]:
            for column, value in row.copy().iteritems():
                logging.debug(":: column: %s, value: %s" % (str(column),
                                                            str(value)))
                if column.find(":") != -1:
                    slicers = column.split(":")
                    (name_column, type_column) = slicers[0:2]
                    if type_column == OracleForward.ORACLE_VARCHAR:
                        pass
                    elif type_column == OracleForward.ORACLE_BUILTIN:
                        del row[column]
                    elif type_column == OracleForward.ORACLE_BLOB:
                        if os.path.exists(value) and os.path.isfile(value):
                            self.cur.setinputsizes(column=cx_Oracle.BLOB)
                            row[column] = open(value, "rb").read()
        many = [tuple(i.values()) for i in rows]
        logging.debug(":: Items for database: %s" % str(many))
        return many

    def generate_query(self, table, items):
        logging.debug("In OracleForward::generate_query()")
        keys = ""
        values = ""
        for column, value in items.iteritems():
            logging.debug(":: column: %s, value: %s" % (str(column), str(value)))
            if column.find(":") != -1:
                slicers = column.split(":")
                (column, type_column) = slicers[0:2]
            if type_column == OracleForward.ORACLE_BUILTIN:
                value = value
            elif type_column == OracleForward.ORACLE_BLOB:
                value = ":%s" % column
            else:
                value = ":%s" % column
            keys += "%s," % column
            values += "%s," % value
        keys = "(%s)" % keys[:-1]
        values = "(%s)" % values[:-1]
        query = "insert into %s %s values %s" % (table, keys, values)
        return query
