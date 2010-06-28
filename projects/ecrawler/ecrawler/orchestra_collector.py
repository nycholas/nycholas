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
import imaplib
import ConfigParser

import utils.constant as constant
from crawl_mail import Crawler


class Orchestra(object):
    def __init__(self, opts={}, args={}, file_config=None):
        hostname = opts.get("hostname") 
        port = opts.get("port")
        username = opts.get("username")
        password = opts.get("password")
        mailbox = opts.get("mailbox")
        directory = opts.get("directory")
        forwards = opts.get("forwards")
        is_test = opts.get("is_test")
        file_config = file_config or constant.PROFILE_FILE_CONF
        self.args = args
        self.profiles = []
        
        logging.info("Loading config parser (%s)..." % file_config)
        config = ConfigParser.ConfigParser()
        config.read(file_config)
        for section in config.sections():
            logging.info("Loading profile %s..." % section)
            if config.has_option(section, "hostname"):
                logging.info("Options %s::hostname found" % section)
                if not hostname:
                    hostname = config.get(section, "hostname")
            if config.has_option(section, "port"):
                logging.info("Options %s::port found" % section)
                if not port:
                    port = int(config.get(section, "port"))
            if config.has_option(section, "username"):
                logging.info("Options %s::username found" % section)
                if not username:
                    username = config.get(section, "username")
            if config.has_option(section, "password"):
                logging.info("Options %s::password found" % section)
                if not password:
                    password = config.get(section, "password")
            if config.has_option(section, "mailbox"):
                logging.info("Options %s::mailbox not found" % section)
                if not mailbox:
                    mailbox = config.get(section, "mailbox")
            if config.has_option(section, "directory"):
                logging.info("Options %s::directory found" % section)
                if not directory:
                    directory = config.get(section, "directory")
            if config.has_option(section, "forwards"):
                logging.info("Options %s::forwards found" % section)
                forwards_opt = config.get(section, "forwards")
                logging.debug("forwards: %s" % forwards_opt)
                if not forwards and forwards_opt:
                    forwards = forwards_opt
                forwards = self.parser_forwards(forwards)
            profile = {
                "profile": section,
                "hostname": hostname, "port": port,
                "username": username, "password": password,
                "mailbox": mailbox, "directory": directory,
                "forwards": forwards, "is_test": is_test,
            }
            self.profiles.append(profile)
        logging.debug(":: profiles: %s" % self.profiles)
        
    def parser_forwards(self, str_forwards):
        logging.debug("In Orchestra::parser_forwards()")
        str_forwards = str(str_forwards).strip()
        forwards = {}
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
                table_dict.setdefault(table, columns)
                forwards.setdefault(database_dict["forward"], database_dict)
                forwards.get(database_dict["forward"]) \
                    .setdefault("tables", []).append(table_dict)
        logging.debug(":: forwards: %s" % str(forwards))
        return forwards
        
    def start(self, number_threads=5):
        logging.debug("In Orchestra::start()")
        logging.debug("++ number_threads: %s" % number_threads)
        logging.debug("++ number_profiles: %s" % len(self.profiles))

        profiles = []
        for profile in self.profiles:
            logging.info("Running profile: %s" % profile.get("profile"))
            logging.info("Connecting in %s:%d..." % (profile.get("hostname"), 
                                                     profile.get("port")))
            m = imaplib.IMAP4_SSL(profile.get("hostname"), profile.get("port"))
    
            logging.info("Authenticating with %s..." % profile.get("username"))
            m.login(profile.get("username"), profile.get("password"))
    
            logging.info("Select a mailbox...")
            m.select()
    
            logging.info("Search mailbox (%s)..." % profile.get("mailbox"))
            resp, items = m.search(None, profile.get("mailbox"))
            items = items[0].split()
            items.reverse()
            #items.append("36364") # Email test!
    
            logging.info("Number of unread emails: %d" % len(items))
    
            logging.info("Close connection...")
            m.close()
    
            logging.info("Logout with %s..." % profile.get("username"))
            m.logout()
    
            logging.info("Scaling threadings...")
            if number_threads > len(items):
                number_threads = len(items)
            slices = [items[i::number_threads] for i in range(number_threads)]
            threads = [Crawler(i, slices[i], profile) \
                       for i in range(number_threads)]
            profiles.append(threads)
            
        for i, threads in enumerate(profiles):
            logging.info("[%d] - Running email crawlers..." % i)
            for thread in threads:
                logging.debug("[%d] - Running thread-%s..." % (i, thread))
                thread.start()
        for threads in profiles:
            logging.info("[%d] - Joining email crawlers..." % i)
            for thread in threads:
                logging.debug("[%d] - Joining thread-%s..." % (i, thread))
                thread.join()

        logging.debug("Done, HA!")


if __name__ == "__main__":
    orchestra = Orchestra()
    orchestra.start()