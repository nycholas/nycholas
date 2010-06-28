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

from ecrawler.utils.constant import SHARE_DIR
from ecrawler.middleman import ForwardBase


class SQLite3Forward(ForwardBase):
    def __init__(self, database=None):
        logging.debug("In SQLite3Forward::__init__()")
        self.database = database or os.path.join(SHARE_DIR, "test",
                                                 "ecrawlerdb_test.db")
        logging.info("Connecting in database: %s..." % self.database)
        self.conn = sqlite3.connect(self.database)

    def execute(self, items):
        logging.debug("In SQLite3Forward::execute()")
        logging.debug(":: Number of items: %s" % len(items))
        if not items:
            return
        self.clean() # For test!

        for table in items.get("tables"):
            logging.info("Genareting query...")
            query = self.generate_query(table)
            logging.debug(":: query: %s" % query)

            logging.info("Running and inserting items in database...")
            cur = self.conn.cursor()
            cur.executemany(query, self.execute_many(table.values()))
            logging.info("Insert a row of data")
        logging.info("Save (commit) the changes...")
        self.conn.commit()

        #logging.info("Close connection database...")
        #self.conn.close()

    def execute_many(self, items):
        logging.debug("In SQLite3Forward::execute_many()")
        logging.debug(":: items: %s" % str(items))
        many = [tuple(i.values()) for i in items]
        logging.debug(":: Items for database: %s" % str(many))
        return many

    def generate_query(self, items):
        logging.debug("In SQLite3Forward::generate_query()")
        table = items.keys()[0]
        columns = items.values()[0]
        query = "insert into " + table + " "
        keys = "(" + "".join([i+"," for i in columns.keys()])[:-1] + ")"
        #values = "(" + "".join([i+"," for i in columns.values()])[:-1] + ")"
        values = "(" + "".join("?," for i in columns.values())[:-1] + ")"
        query += keys + " values " + values
        return query

    def clean(self):
        logging.debug("In SQLite3Forward::clean()")

        logging.info("Running and delete items in database...")
        cur = self.conn.cursor()
        cur.execute("delete from emails")
        logging.info("Delete a row of data")

        logging.info("Save (commit) the changes...")
        self.conn.commit()


def test():
    import os
    from ecrawler.utils.constant import SHARE_DIR
    from ecrawler.models import EmailModel

    models = []
    for i in range(10):
        model = EmailModel()
        model.id = i
        model.email_id = i
        model.from_ = "email_from-%d" % i
        model.subject = "email_subject-%d" % i
        model.to = "email_to-%d" % i
        model.date = "email_date-%d" % i
        model.content = "email_content-%d" % i
        model.annex = None
        models.append(model)

    pathdb = os.path.join(SHARE_DIR, "test", "ecrawlerdb_test.db")
    sql = SQLite3Forward(pathdb)
    sql.clean()
    sql.execute(models)


if __name__ == "__main__":
    pass
