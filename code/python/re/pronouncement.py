#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Search for a/or several words on the web and touches your pronunciation.
# Copyright (C) 2009 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
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
import sys
import urllib
import pygame

pygame.init()

url_search = 'http://www.merriam-webster.com/dictionary/%s'
url_audio = 'http://www.merriam-webster.com/cgi-bin/audio.pl?%s=%s'

search = raw_input('Enter a word or phrase: ')

lst_word = []
for word in [x.strip().lower() for x in search.split(' ')]:
    if len(word) > 0 and word.isalpha():
        print ' + connecting in %s%s...' % (url_search, word)
        content = urllib.urlopen(url_search % word).read()
        reg = re.compile(r'au\(\'([a-zA-Z0-9].*)\', \'%s\'\);' % word)
        find_word = re.findall(reg, content)
        if len(find_word) > 0:
            print '  :: word found: %s' % find_word[0]
            lst_word.append(find_word[0])
        else:
            print '  :: the word, %s, was not found!' % word

lst_audio = []
for word in lst_word:
    content = urllib.urlopen(url_audio % (word, search)).read()
    reg = re.compile(r'(http://.*/%s)' % word)
    audio = re.findall(reg, content)
    if len(audio) > 0:
        print ' + audio file found: %s' % audio[0]
        lst_audio.append(audio[0])

lst_sound = []
for audio in lst_audio:
    print ' + download %s...' % audio
    sound_file = urllib.urlretrieve(audio)
    if len(sound_file) > 0:
        lst_sound.append(sound_file[0])

for audio in lst_sound:
    sound = pygame.mixer.Sound(audio)
    print ' + play file: %s, time: %s' % (audio, sound.get_length())
    sound.play()
    pygame.time.delay(int(sound.get_length() * 1000) + 500)