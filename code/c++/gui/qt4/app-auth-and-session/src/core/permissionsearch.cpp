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
#include "permission.h"

PermissionSearch::PermissionSearch(PermissionModel *model, QDialog *parent) :
	QDialog(parent) {
	setupUi(this);
	permissionModel = model;
	createActions();
	updateWidgets();
}

PermissionSearch::~PermissionSearch(void) {
}

void PermissionSearch::closeEvent(QCloseEvent *event) {
	emit formSearchClose();
	event->accept();
}

void PermissionSearch::searchAction(void) {
	QString name = nameLineEdit->text();
	QString codename = codenameLineEdit->text();

	QString query = " 1=1 ";
	if (!name.isEmpty())
		query.append(QString(" AND name LIKE '%1\%'").arg(name));
	if (!codename.isEmpty())
		query.append(QString(" AND codename LIKE '\%%1\%'").arg(codename));
	permissionModel->setF(query);
	permissionModel->setBegin(0);
	emit formSearched();
}

void PermissionSearch::cancelAction(void) {
	permissionModel->setF("");
	permissionModel->setBegin(0);
	emit
	formSearchClose();
	hide();
}

void PermissionSearch::closeAction(void) {
	emit formSearchClose();
	hide();
}

void PermissionSearch::createActions(void) {
	connect(searchPushButton, SIGNAL(released()), this, SLOT(searchAction()));
	connect(cancelPushButton, SIGNAL(released()), this, SLOT(cancelAction()));
	connect(closePushButton, SIGNAL(released()), this, SLOT(closeAction()));
}

void PermissionSearch::updateWidgets(void) {
	updateForms();
}

void PermissionSearch::updateForms(void) {
	clear();
	focusDefault();
	searchPushButton->setDefault(true);
}

void PermissionSearch::clear(void) {
	nameLineEdit->clear();
	codenameLineEdit->clear();
}

void PermissionSearch::focusDefault(void) {
	nameLineEdit->setFocus();
}
