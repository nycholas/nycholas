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
    MYSQL_INT = "INT"
    MYSQL_VARCHAR = "VARCHAR"
    MYSQL_DATETIME = "DATETIME"
    MYSQL_LONGTEXT = "LONGTEXT"
    MYSQL_LONGBLOB = "LONGBLOB"
    
    def __init__(self):
        logging.debug("In MySQLForward::__init__()")

    def execute(self, items):
        logging.debug("In MySQLForward::execute()")
        logging.debug("++ items: %s" % str(items))

        list_table = items.get("tables")
        logging.info(":: Number of tables: %d" % len(list_table))
        if not list_table:
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

        for tables in list_table:
            for table, rows in tables.iteritems():
                logging.info(":: Number of rows: %d" % len(rows))
                
                logging.info("Genareting query: %s..." % table)
                query = self.generate_query(table, rows[-1])
                logging.debug(":: query: %s" % query)
        
                logging.info("Running and inserting items in database...")
                cur = self.conn.cursor()
                cur.executemany(query, self.execute_many(rows))
                logging.info("Insert a row of data")
        logging.info("Save (commit) the changes...")
        self.conn.commit()

        logging.info("Close connection database...")
        self.conn.close()
        
    def execute_many(self, rows):
        logging.debug("In MySQLForward::execute_many()")
        logging.debug(":: rows: %s" % str(rows))
        for row in rows[:]:
            for column, value in row.copy().iteritems():
                if column.find(":") != -1:
                    slicers = column.split(":")
                    (name_column, type_column) = slicers[0:2]
                    if type_column == MySQLForward.MYSQL_INT:
                        pass
                    elif type_column == MySQLForward.MYSQL_INT:
                        pass
                    elif type_column == MySQLForward.MYSQL_LONGBLOB:
                        if os.path.exists(value) and os.path.isfile(value):
                            row[column] = open(value, "rb").read()
        many = [tuple(i.values()) for i in rows]
        logging.debug(":: Items for database: %s" % str(many))
        return many

    def generate_query(self, table, items):
        logging.debug("In MySQLForward::generate_query()")
        keys = ""
        values = ""
        for column, value in items.iteritems():
            if column.find(":") != -1:
                slicers = column.split(":")
                (column, type_column) = slicers[0:2]
            keys += "%s," % column
            values += "%s," #values += "%s," % value
        keys = "(%s)" % keys[:-1]
        values = "(%s)" % values[:-1]
        query = "insert into %s %s values %s" % (table, keys, values)
        return query

