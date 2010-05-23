#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Simple example PyQt4.
# Copyright (C) 2009 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import sys
from PyQt4 import QtCore, QtGui


class Example(QtGui.QApplication):
    def __init__(self, args):
        QtGui.QApplication.__init__(self, args)
        self.addWidgets()
        self._connectSlots()
        self.exec_()

    def addWidgets(self):
        self.button = QtGui.QPushButton("All Gone To Hell!", None)
        self.button.show()

    def _connectSlots(self):
        self.connect(self, QtCore.SIGNAL("lastWindowClosed()"), self,
                QtCore.SLOT("quit()"))
        self.connect(self.button, QtCore.SIGNAL("clicked()"), self._slotButton)

    def _slotButton(self):
        print "All Gone To Hell!"


if __name__ == "__main__":
    app = Example(sys.argv)

