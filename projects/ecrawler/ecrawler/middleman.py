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
        self.file_config = file_config or constant.FORWARD_FILE_CONF
        self._plugins = []

        logging.info("Loading config parser (%s)..." % self.file_config)
        config = ConfigParser.ConfigParser()
        config.read(self.file_config)
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
                    self.add((destiny, clazzobj()))
            except (NameError, ImportError), e:
                logging.warning("!! Not module runpy: %s" % e)
                file_module = "ecrawler.%s" % file_module
                logging.info("Loading module: %s..." % file_module)
                run_module = __import__(file_module, locals(), globals(), -1)
                clazzobj = type(clazz, (eval('run_module.%s' % clazz),), {})
                self.add((destiny, clazzobj()))

    def update(self, status):
        logging.debug("In Destiny::update()")
        self._status = status

    def add(self, plugin):
        logging.debug("In Destiny::add()")
        logging.debug(":: plugin: %s" % str(plugin))
        if not plugin in self._plugins:
            self._plugins.append(plugin)
        else:
            if plugin is None:
                self._plugins.append(None)

    def remove(self, plugin):
        logging.debug("In Destiny::remove()")
        try:
            self._plugins.remove(plugin)
        except ValueError, e:
            logging.error(e)

    def execute(self, items):
        logging.debug("In Destiny::execute()")
        for item in items:
            for key, plugin in self._plugins:
                plugin.execute(item.get(key))

    def childs(self, index=None):
        logging.debug("In Destiny::childs()")
        if index is None:
            return set(self._plugins)
        return set(self._plugins[index])

    def lenght(self, index=None):
        logging.debug("In Destiny::lenght()")
        if index is None:
            return len(self.childs())
        return len(self.childs(index))


if __name__ == "__main__":
    pass