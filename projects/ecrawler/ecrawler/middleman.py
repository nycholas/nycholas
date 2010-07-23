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
import ConfigParser

from error import *
import utils.constant as constant

try:
    import abc

    class ForwardBase(object):
        __metaclass__ = abc.ABCMeta

        def __init__(self):
            logging.debug("In ForwardBase::__init__()")
            logging.warning("ForwardBase::__init__() not implemented")

        @abc.abstractmethod
        def execute(self, items):
            logging.debug("In ForwardBase::execute()")
            logging.warning("ForwardBase::execute() not implemented")
except ImportError, e:
    logging.warning("!! Not module abc: %s" % e)

    class ForwardBase(object):

        def __init__(self):
            logging.debug("In ForwardBase::__init__()")
            logging.warning("ForwardBase::__init__() not implemented")

        def execute(self, items):
            logging.debug("In ForwardBase::execute()")
            logging.warning("ForwardBase::execute() not implemented")


class Destiny(object):

    def __init__(self, forwards, file_config=None):
        logging.debug("In Destiny::__init__()")
        file_config = file_config or constant.FORWARD_FILE_CONF
        self._plugins = {}
        self._email_id_errors = []

        logging.info("Loading config parser (%s)..." % file_config)
        config = ConfigParser.ConfigParser()
        try:
            config.read(file_config)
        except (ConfigParser.MissingSectionHeaderError,
                ConfigParser.ParsingError), e:
            logging.error("!! Error: %s" % str(e))
            raise ForwardParseError, e
        if not config.has_section("forwards"):
            logging.warning("Section forwards not found")
            return
        if not config.has_option("forwards", "keys"):
            logging.warning("Options forwards::keys not found")
            return
        keys = config.get("forwards", "keys")
        destinations = [i.strip() for i in keys.split(",")]
        if forwards:
            destinations = [i for i in forwards if i in destinations]
        logging.debug(":: Destinations: %s" % destinations)
        for destiny in destinations:
            logging.debug(":: Destiny: %s" % destiny)
            section = "forward_%s" % destiny
            if not config.has_section(section):
                logging.warning("Section %s not found" % section)
                continue
            file_module = config.get(section, "path")
            clazz = config.get(section, "class")
            try:
                import runpy

                if not __package__ is None:
                    file_module = "%s.%s" % (__package__, file_module)
                logging.info("Loading module: %s..." % file_module)
                run_module = runpy.run_module(file_module)
                logging.info("Instantiating class: %s..." % clazz)
                clazzobj = run_module.get(clazz)
                if issubclass(clazzobj, ForwardBase):
                    self.add(destiny, clazzobj())
            except (NameError, ImportError), e:
                logging.warning("!! Not module runpy: %s" % e)
                file_module = "ecrawler.%s" % file_module
                logging.info("Loading module: %s..." % file_module)
                run_module = __import__(file_module, locals(), globals(), -1)
                logging.info("Instantiating class: %s..." % clazz)
                clazzobj = type(clazz, (eval('run_module.%s' % clazz),), {})
                if issubclass(clazzobj, ForwardBase):
                    self.add(destiny, clazzobj())

    def add_email_error(self, email_id):
        logging.debug("In Destiny::add_email_error()")
        logging.debug("++ email_id: %s" % (email_id,))
        if not email_id in self._email_id_errors:
            self._email_id_errors.append(email_id)

    def email_errors(self):
        logging.debug("In Destiny::email_error()")
        return self._email_id_errors

    def update(self, status):
        logging.debug("In Destiny::update()")
        self._status = status

    def add(self, destiny, plugin):
        logging.debug("In Destiny::add()")
        logging.debug("++ destiny: %s" % str(destiny))
        logging.debug("++ plugin: %s" % str(plugin))
        self._plugins.setdefault(destiny, []).append(plugin)

    def remove(self, plugin):
        logging.debug("In Destiny::remove()")
        logging.debug("++ plugin: %s" % str(plugin))
        if self._plugins.has_key(plugin):
            del self._plugins[plugin]

    def execute(self, email_models):
        logging.debug("In Destiny::execute()")
        if not email_models:
            return
        emails_id = []
        destinations = {}
        for destiny in self._plugins.keys():
            destinations.setdefault(destiny, email_models[-1].get(destiny))
            emails_id.append(email_models[-1].email_id())
        for email_model in email_models[:-1]:
            for destiny in self._plugins.keys():
                d_tables = destinations.get(destiny).get("tables")
                for d_table in d_tables:
                    for k, v in d_table.iteritems():
                        m_tables = email_model.get(destiny).get("tables")
                        for m_table in m_tables:
                            if k in m_table:
                                d_table.setdefault(k, []).extend(m_table[k])
            emails_id.append(email_model.email_id())
        for destiny, models in destinations.iteritems():
            for forward in self._plugins.get(destiny):
                try:
                    forward.execute(models)
                except Exception, e:
                    logging.error("!! Error-execute: %s" % (str(e),))
                    logging.info("Add emails in queure error: %s" % str(emails_id))
                    for email_id in emails_id:
                        self.add_email_error(email_id)
                    continue

if __name__ == "__main__":
    pass