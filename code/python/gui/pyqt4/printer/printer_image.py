#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Printer image.
# Copyright (C) 2010 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
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

IMAGE = './image.svg' # Any image!

def main(args):
    app = QtGui.QApplication(sys.argv)

    imageLabel = QtGui.QLabel()
    imageLabel.setPixmap(QtGui.QPixmap.fromImage(QtGui.QImage(IMAGE)))
    imageLabel.adjustSize()
    imageLabel.show()

    printer = QtGui.QPrinter(QtGui.QPrinter.HighResolution)
    dlg = QtGui.QPrintDialog(printer)
    dlg.setWindowTitle("Print Image")
    if dlg.exec_() != QtGui.QDialog.Accepted:
        return

    painter = QtGui.QPainter(printer)
    rect = painter.viewport()
    size = imageLabel.pixmap().size()
    size.scale(rect.size(), QtCore.Qt.KeepAspectRatio)
    painter.setViewport(rect.x(), rect.y(), size.width(), size.height())
    painter.setWindow(imageLabel.pixmap().rect())
    painter.drawPixmap(0, 0, imageLabel.pixmap())
    painter.end()

    app.exec_()


if __name__ == "__main__":
    main(sys.argv)
