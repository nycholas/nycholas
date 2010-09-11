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
#include "login.h"

// Singleton
Login &LoginObj(void) {
	static Login login;
	return login;
}

Login::Login(QDialog *parent) :
	QDialog(parent) {
	qDebug() << "In Login::Login()";
	setupUi(this);
	readSettings();
	createActions();
	updateWidgets();
}

Login::~Login(void) {
	qDebug() << "In Login::~Login()";
	writeSettings();
}

void Login::accept(void) {
	qDebug() << "In Login::loginAction()";
	QString username = usernameLineEdit->text();
	QString password = passwordLineEdit->text();
	if (username == "admin" && password == "admin") {
		qDebug() << " :: Loging: OK";
		QSettings settings("Cenobites Technology", "A3S");
		settings.beginGroup("session");
		settings.setValue("isAuthentic", true);
		settings.setValue("username", username);
		settings.endGroup();
		settings.sync();
		updateForms();
		QDialog::accept();
	} else {
		qDebug() << " :: Loging: FALSE";
		QSettings settings("Cenobites Technology", "A3S");
		settings.beginGroup("session");
		settings.setValue("isAuthentic", false);
		settings.setValue("usename", qApp->tr("Annonymous"));
		settings.endGroup();
		settings.sync();
		/*
		 * Please enter a correct username and password.
		 * Note that both fields are case-sensitive.
		 */
	}
}

void Login::readSettings(void) {
	qDebug() << "In Login::readSettings()";
	QSettings settings("Cenobites Technology", "A3S");
	settings.beginGroup("login");
	resize(settings.value("size", QSize(387, 250)).toSize());
	move(settings.value("pos", QPoint(200, 200)).toPoint());
	settings.endGroup();
}

void Login::writeSettings(void) {
	qDebug() << "In Login::writeSettings()";
	QSettings settings("Cenobites Technology", "A3S");
	settings.beginGroup("login");
	settings.setValue("size", size());
	settings.setValue("pos", pos());
	settings.endGroup();
}

void Login::cancelAction(void) {
	qDebug() << "In Login::cancelAction()";
	reject();
}

void Login::createActions(void) {
	qDebug() << "In Login::createActions()";
	connect(loginPushButton, SIGNAL(released()), this, SLOT(accept()));
	connect(cancelPushButton, SIGNAL(released()), this, SLOT(cancelAction()));
}

void Login::updateWidgets(void) {
	qDebug() << "In Login::updateWidgets()";
	updateForms();
}

void Login::updateForms(void) {
	qDebug() << "In Login::updateForms()";
	clear();
	focusDefault();
	loginPushButton->setDefault(true);
}

void Login::clear(void) {
	qDebug() << "In Login()";
	usernameLineEdit->clear();
	passwordLineEdit->clear();
}

void Login::focusDefault(void) {
	qDebug() << "In Login::focusDefault()";
	usernameLineEdit->setFocus();
}
