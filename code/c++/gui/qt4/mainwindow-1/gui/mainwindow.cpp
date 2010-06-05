/**
* Simple example Qt - MainWindow with QWidget.
* Copyright (C) 2010 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
**/
#include "mainwindow.h"

MainWindow::MainWindow(QMainWindow *parent)
    : QMainWindow(parent)
{
    setupUi(this);
    createActions();
}

MainWindow::~MainWindow()
{
}

void MainWindow::createActions(void)
{
    connect(actionNew, SIGNAL(triggered()), this, SLOT(newWindow()));
    connect(actionOpen, SIGNAL(triggered()), this, SLOT(openWindow()));
    connect(actionClose, SIGNAL(triggered()), this, SLOT(closeWindow()));
    connect(actionQuit, SIGNAL(triggered()), this, SLOT(quitMainWindow()));
}

void MainWindow::newWindow(void) {
    QIcon icon;
    icon.addFile(QString::fromUtf8(
                 ":/default/static/default/icons/22x22/face-devilish.png"),
                 QSize(), QIcon::Normal, QIcon::Off);

    window = new Window(this);
    if (window->objectName().isEmpty())
        window->setObjectName(QString::fromUtf8("Window"));
    window->setWindowIcon(icon);
    window->resize(100, 100);
    setCentralWidget(window);
    window->show();
}

void MainWindow::openWindow(void) {}

void MainWindow::closeWindow(void) {
    if (window != NULL)
        window->close();
}

void MainWindow::quitMainWindow(void) {
    close();
}
