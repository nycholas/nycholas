/**
 * Simple example Qt - MainWindow with QWidget.
 * Copyright (c) 2010, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *  * Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
 *    its contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
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
