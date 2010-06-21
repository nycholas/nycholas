/**
 * Simple example Qt - Mainwindow with authentication and session.
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
#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "widgets/ui_mainwindow.h"
#include "login.h"
#include "changepassword.h"
#include "contenttypes.h"
#include "permission.h"
#include "group.h"
#include "user.h"

#include <QtCore/QSettings>
#include <QtGui/QCloseEvent>
#include <QtGui/QMdiSubWindow>

class MainWindow: public QMainWindow, private Ui::MainWindow {
Q_OBJECT

public:
	MainWindow(QMainWindow *parent = 0);
	~MainWindow();

private slots:
	void closeWindowAction(void);
	void quitMainWindowAction(void);

	void changePassword(void);
	void logout(void);

	void contentTypesAction(void);
	void permissionsAction(void);
	void groupsAction(void);
	void usersAction(void);

private:
	void isAuthentic(void);
	void readSettings(void);
	void writeSettings(void);
	void createActions(void);
	void updateWidgets(void);

protected:
	virtual void closeEvent(QCloseEvent * event);
};

#endif /* MAINWINDOW_H */
