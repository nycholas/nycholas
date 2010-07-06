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
import socket
import logging
import imaplib
import ConfigParser

from error import *
import utils.constant as constant
from crawl_mail import Crawler


class Orchestra(object):
    
    def __init__(self, opts={}, args={}, file_config=None):
        hostname = opts.get("hostname")
        port = opts.get("port")
        username = opts.get("username")
        password = opts.get("password")
        mailbox = opts.get("mailbox")
        smtp_hostname = opts.get("smtp_hostname")
        smtp_port = opts.get("smtp_port")
        smtp_username = opts.get("smtp_username")
        smtp_password = opts.get("smtp_password")
        smtp_from_addr = opts.get("smtp_from_addr")
        smtp_to_addrs = opts.get("smtp_to_addrs")
        smtp_prefix = opts.get("smtp_prefix")
        directory = opts.get("directory")
        forwards = opts.get("forwards")
        is_remove = opts.get("is_remove")
        is_test = opts.get("is_test")
        file_config = file_config or constant.PROFILE_FILE_CONF
        self.args = args
        self.profiles = []

        logging.info("Loading config parser (%s)..." % file_config)
        config = ConfigParser.ConfigParser()
        try:
            config.read(file_config)
        except (ConfigParser.MissingSectionHeaderError,
                ConfigParser.ParsingError), e:
            logging.error("!! Error: %s" % str(e))
            raise ProfileParseError, e
        for section in config.sections():
            logging.info("Loading profile %s..." % section)
            if config.has_option(section, "hostname"):
                logging.debug("Options %s::hostname found" % section)
                if not hostname:
                    hostname = config.get(section, "hostname")
            if config.has_option(section, "port"):
                logging.debug("Options %s::port found" % section)
                if not port:
                    try:
                        port = int(config.get(section, "port"))
                    except TypeError, e:
                        logging.error("!! Error: %s" % str(e))
                        raise ProfileParseError, e
            if config.has_option(section, "username"):
                logging.debug("Options %s::username found" % section)
                if not username:
                    username = config.get(section, "username")
            if config.has_option(section, "password"):
                logging.debug("Options %s::password found" % section)
                if not password:
                    password = config.get(section, "password")
            if config.has_option(section, "mailbox"):
                logging.debug("Options %s::mailbox not found" % section)
                if not mailbox:
                    mailbox = config.get(section, "mailbox")
            if config.has_option(section, "smtp_hostname"):
                logging.debug("Options %s::smtp_hostname found" % section)
                if not smtp_hostname:
                    smtp_hostname = config.get(section, "smtp_hostname")
            if config.has_option(section, "smtp_port"):
                logging.debug("Options %s::smtp_port found" % section)
                if not smtp_port:
                    try:
                        smtp_port = int(config.get(section, "smtp_port"))
                    except TypeError, e:
                        logging.error("!! Error: %s" % str(e))
                        raise ProfileParseError, e
            if config.has_option(section, "smtp_username"):
                logging.debug("Options %s::smtp_username found" % section)
                if not smtp_username:
                    smtp_username = config.get(section, "smtp_username")
            if config.has_option(section, "smtp_password"):
                logging.debug("Options %s::smtp_password found" % section)
                if not smtp_password:
                    smtp_password = config.get(section, "smtp_password")
            if config.has_option(section, "smtp_from_addr"):
                logging.debug("Options %s::smtp_from_addr not found" % section)
                if not smtp_from_addr:
                    smtp_from_addr = config.get(section, "smtp_from_addr")
            if config.has_option(section, "smtp_to_addrs"):
                logging.debug("Options %s::smtp_to_addrs not found" % section)
                if not smtp_to_addrs:
                    smtp_to_addrs = config.get(section, "smtp_to_addrs")
            if config.has_option(section, "smtp_prefix"):
                logging.debug("Options %s::smtp_prefix not found" % section)
                if not smtp_prefix:
                    smtp_prefix = config.get(section, "smtp_prefix")
            if config.has_option(section, "directory"):
                logging.debug("Options %s::directory found" % section)
                if not directory:
                    directory = config.get(section, "directory")
            if config.has_option(section, "forwards"):
                logging.debug("Options %s::forwards found" % section)
                forwards_opt = config.get(section, "forwards")
                logging.debug("forwards: %s" % forwards_opt)
                if not forwards and forwards_opt:
                    forwards = forwards_opt
            forwards = self.__parser_forwards(forwards)
            profile = {
                "profile": section,
                "hostname": hostname, "port": port,
                "username": username, "password": password,
                "mailbox": mailbox, "directory": directory,
                "smtp_hostname": smtp_hostname, "smtp_port": smtp_port,
                "smtp_username": smtp_username, "smtp_password": smtp_password,
                "smtp_from_addr": smtp_from_addr, "smtp_to_addrs": smtp_to_addrs, 
                "smtp_prefix": smtp_prefix, "forwards": forwards,
                "is_remove": is_remove, "is_test": is_test,
            }
            self.profiles.append(profile)
        logging.debug(":: profiles: %s" % self.profiles)

    def __parser_forwards(self, str_forwards):
        logging.debug("In Orchestra::parser_forwards()")
        str_forwards = str(str_forwards).strip()
        forwards = {}
        try:
            list_forwards = re.findall(r"(\<.*?\>)", str_forwards, re.DOTALL)
            for forward in list_forwards:
                db1 = re.findall(r"(\[.*?\])", forward, re.DOTALL)
                db2 = [re.findall(r"(\(.*?\))", i, re.DOTALL) for i in db1]
                db3 = [re.sub("\(|\)", "", i, re.DOTALL).split(",") for i in db2[0]]
                database_dict = dict([[j.strip() for j in i] for i in db3])
                tables = re.findall(r"([a-zA-Z0-9_]\w+.?{.*?})", forward, re.DOTALL)
                for t in tables:
                    table = re.findall(r"([a-zA-Z0-9_]\w+).?{", t, re.DOTALL)[0]
                    c1 = re.findall(r"(\{.*?\})", t, re.DOTALL)
                    c2 = [re.findall(r"(\(.*?\))", i, re.DOTALL) for i in c1]
                    c3 = [re.sub("\(|\)", "", i, re.DOTALL).split(",") for i in c2[0]]
                    columns = dict([[j.strip() for j in i] for i in c3])
                    table_dict = {}
                    table_dict.setdefault(table, [columns])
                    forwards.setdefault(database_dict["forward"], database_dict)
                    forwards.get(database_dict["forward"]) \
                        .setdefault("tables", []).append(table_dict)
        except (ValueError, IndexError), e:
            logging.error("!! Error: %s" % str(e))
            raise ProfileParseError, e
        logging.debug(":: forwards: %s" % str(forwards))
        if not forwards:
            logging.warning("!! No forward set")
        return forwards

    def start(self, number_threads=5):
        logging.debug("In Orchestra::start()")
        logging.debug("++ number_threads: %s" % number_threads)
        logging.debug("++ number_profiles: %s" % len(self.profiles))

        if number_threads < 1:
            logging.warning("!! The number of jobs must be greater than zero")
            return

        profiles = []
        for p in self.profiles:
            profile = p.get("profile")
            hostname = p.get("hostname")
            port = p.get("port")
            username = p.get("username")
            password = p.get("password")
            mailbox = p.get("mailbox")

            logging.info("Running profile: %s" % profile)
            logging.info("Connecting in %s:%d..." % (hostname, port))
            try:
                m = imaplib.IMAP4_SSL(hostname, port)
            except (socket.gaierror, socket.error), e:
                logging.error("!! Error: %s [%s:%d]" % (str(e), hostname, port))
                raise ConnectionError, e

            logging.info("Authenticating with %s..." % username)
            try:
                m.login(username, password)
            except imaplib.IMAP4_SSL.error, e:
                logging.error("!! Error: %s [%s@%s:%d]" % (str(e),
                              username, hostname, port))
                raise ConnectionError, e

            logging.info("Select a mailbox...")
            m.select()

            logging.info("Search mailbox (%s)..." % mailbox)
            try:
                resp, items = m.search(None, mailbox)
                items = items[0].split()
                #items = ["36364", "36448", "36676", "36677"] # :]-, Emails test!
                items.reverse()
            except imaplib.IMAP4_SSL.error, e:
                logging.error("!! Error: %s [mailbox: %s]" % (str(e), mailbox))
                try:
                    raise ConnectionError, e
                finally:
                    logging.info("Close connection...")
                    m.close()

                    logging.info("Logout with %s..." % username)
                    m.logout()

            logging.info("Number of unread emails: %d" % len(items))

            logging.info("Close connection...")
            m.close()

            logging.info("Logout with %s..." % username)
            m.logout()

            logging.info("Scaling threadings...")
            if number_threads > len(items):
                number_threads = len(items)
            slices = [items[i::number_threads] for i in range(number_threads)]
            threads = [Crawler(i, slices[i], p) \
                       for i in range(number_threads)]
            profiles.append(threads)

        for i, threads in enumerate(profiles):
            logging.info("[%d] - Running email crawlers..." % i)
            for thread in threads:
                logging.debug("[%d] - Running thread-%s..." % (i, thread))
                thread.start()
        for i, threads in enumerate(profiles):
            logging.info("[%d] - Joining email crawlers..." % i)
            for thread in threads:
                logging.debug("[%d] - Joining thread-%s..." % (i, thread))
                thread.join()

        logging.info(":D, So long and good luck!")
        logging.debug("Done, HA!")


if __name__ == "__main__":
    orchestra = Orchestra()
    orchestra.start()