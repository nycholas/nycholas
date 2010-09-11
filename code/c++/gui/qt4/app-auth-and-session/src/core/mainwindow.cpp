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
#include "mainwindow.h"

MainWindow::MainWindow(QMainWindow *parent) :
	QMainWindow(parent) {
	qDebug() << "In MainWindow::MainWindow()";
	setupUi(this);
	isAuthentic();
	readSettings();
	createActions();
	updateWidgets();
}

MainWindow::~MainWindow() {
	qDebug() << "In MainWindow::~MainWindow()";
	writeSettings();
}

void MainWindow::closeEvent(QCloseEvent *event) {
	qDebug() << "In MainWindow::closeEvent()";
	/*if (userReallyWantsToQuit()) {
	 writeSettings();
	 event->accept();
	 } else {
	 event->ignore();
	 }*/
	event->accept();
}

void MainWindow::isAuthentic(void) {
	qDebug() << "In MainWindow::isAuthentic()";
	QSettings settings("Cenobites Technology", "A3S");
	bool isAuthentic = settings.value("session/isAuthentic", false).toBool();
	if (!isAuthentic) {
		if (LoginObj().exec() == QDialog::Accepted) {
			show();
			return;
		}
	}
	close();
}

void MainWindow::changePassword(void) {
	qDebug() << "In MainWindow::changePassword()";
	ChangePassword *dialog = new ChangePassword();
	dialog->exec();
}

void MainWindow::logout(void) {
	qDebug() << "In MainWindow::logout()";
	QSettings settings("Cenobites Technology", "A3S");
	settings.setValue("session/isAuthentic", false);
	if (LoginObj().exec() == QDialog::Rejected) {
		close();
		return;
	}
}

void MainWindow::readSettings(void) {
	qDebug() << "In MainWindow::readSettings()";
	QSettings settings("Cenobites Technology", "A3S");
	settings.beginGroup("mainwindow");
	resize(settings.value("size", QSize(800, 600)).toSize());
	move(settings.value("pos", QPoint(200, 200)).toPoint());
	settings.endGroup();
}

void MainWindow::writeSettings(void) {
	qDebug() << "In MainWindow::writeSettings()";
	QSettings settings("Cenobites Technology", "A3S");
	settings.beginGroup("mainwindow");
	settings.setValue("size", size());
	settings.setValue("pos", pos());
	settings.endGroup();
}

void MainWindow::createActions(void) {
	qDebug() << "In MainWindow::createActions()";
	connect(actionClose, SIGNAL(triggered()), this, SLOT(closeWindowAction()));
	connect(actionQuit, SIGNAL(triggered()), this, SLOT(quitMainWindowAction()));

	connect(changePasswordPushButton, SIGNAL(released()), this, SLOT(
			changePassword()));
	connect(logoutPushButton, SIGNAL(released()), this, SLOT(logout()));

	connect(actionContentTypes, SIGNAL(triggered()), this, SLOT(
			contentTypesAction()));
	connect(actionPermissions, SIGNAL(triggered()), this, SLOT(
			permissionsAction()));
	connect(actionGroups, SIGNAL(triggered()), this, SLOT(groupsAction()));
	connect(actionUsers, SIGNAL(triggered()), this, SLOT(usersAction()));
}

void MainWindow::updateWidgets(void) {
	qDebug() << "In MainWindow::updateWidgets()";
	QSettings settings("Cenobites Technology", "A3S");
	QString username = settings.value("session/username").toString();
	welcomeLabel->setText(
			QString(qApp->tr("Welcome, <b>%1</b>.")).arg(username));
}

void MainWindow::closeWindowAction(void) {
	qDebug() << "In MainWindow::closeWindowAction()";
	QMdiSubWindow *mdi = mdiArea->activeSubWindow();
	if (mdi)
		mdi->close();
}

void MainWindow::quitMainWindowAction(void) {
	qDebug() << "In MainWindow::quitMainWindowAction()";
	writeSettings();
	close();
}

void MainWindow::contentTypesAction(void) {
	qDebug() << "In MainWindow::contentTypesAction()";
	QIcon icon;
	icon.addFile(QString::fromUtf8(
			":/default/static/default/icons/22x22/face-devilish.png"), QSize(),
			QIcon::Normal, QIcon::Off);

	ContentTypes *window = new ContentTypes(this);
	if (window->objectName().isEmpty())
		window->setObjectName(QString::fromUtf8("ContenTypes"));
	window->setWindowIcon(icon);

	QMdiSubWindow *mdi = new QMdiSubWindow();
	mdi->setWidget(window);
	mdi->setAttribute(Qt::WA_DeleteOnClose);
	mdi->setWindowIcon(icon);

	mdiArea->addSubWindow(mdi);

	mdi->showMaximized();
}

void MainWindow::permissionsAction(void) {
	qDebug() << "In MainWindow::permissionsAction()";
	QIcon icon;
	icon.addFile(QString::fromUtf8(
			":/default/static/default/icons/22x22/face-devilish.png"), QSize(),
			QIcon::Normal, QIcon::Off);

	Permission *window = new Permission(this);
	if (window->objectName().isEmpty())
		window->setObjectName(QString::fromUtf8("Permission"));
	window->setWindowIcon(icon);

	QMdiSubWindow *mdi = new QMdiSubWindow();
	mdi->setWidget(window);
	mdi->setAttribute(Qt::WA_DeleteOnClose);
	mdi->setWindowIcon(icon);

	mdiArea->addSubWindow(mdi);

	mdi->showMaximized();
}

void MainWindow::groupsAction(void) {
	qDebug() << "In MainWindow::groupsAction()";
	QIcon icon;
	icon.addFile(QString::fromUtf8(
			":/default/static/default/icons/22x22/face-devilish.png"), QSize(),
			QIcon::Normal, QIcon::Off);

	Group *window = new Group(this);
	if (window->objectName().isEmpty())
		window->setObjectName(QString::fromUtf8("Group"));
	window->setWindowIcon(icon);

	QMdiSubWindow *mdi = new QMdiSubWindow();
	mdi->setWidget(window);
	mdi->setAttribute(Qt::WA_DeleteOnClose);
	mdi->setWindowIcon(icon);

	mdiArea->addSubWindow(mdi);

	mdi->showMaximized();
}

void MainWindow::usersAction(void) {
	qDebug() << "In MainWindow::userAction()";
	QIcon icon;
	icon.addFile(QString::fromUtf8(
			":/default/static/default/icons/22x22/face-devilish.png"), QSize(),
			QIcon::Normal, QIcon::Off);

	User *window = new User(this);
	if (window->objectName().isEmpty())
		window->setObjectName(QString::fromUtf8("User"));
	window->setWindowIcon(icon);

	QMdiSubWindow *mdi = new QMdiSubWindow();
	mdi->setWidget(window);
	mdi->setAttribute(Qt::WA_DeleteOnClose);
	mdi->setWindowIcon(icon);

	mdiArea->addSubWindow(mdi);

	mdi->showMaximized();
}
