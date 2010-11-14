/**
 * Simple example Qt - CRUD.
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
 *    this software without specific prior written user.
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
#include "user.h"

UserSearch::UserSearch(QDialog *parent) :
	QDialog(parent) {
	setupUi(this);
	createActions();
	updateWidgets();
}

UserSearch::UserSearch(QSqlRelationalTableModel *model,
		QDialog *parent) :
	QDialog(parent) {
	setupUi(this);
	userModel = model;
	createActions();
	updateWidgets();
}

UserSearch::~UserSearch(void) {
}

void UserSearch::searchAction(void) {
	QString name = nameLineEdit->text();

	QString query = " 1=1 ";
	if (!name.isEmpty())
		query.append(QString(" AND name LIKE '%1\%'").arg(name));
	userModel->setFilter(query);
	emit formSearched();
}

void UserSearch::cancelAction(void) {
	userModel->setFilter("");
	emit
	formSearchClose();
	close();
}

void UserSearch::closeAction(void) {
	emit formSearchClose();
	close();
}

void UserSearch::createActions(void) {
	connect(searchPushButton, SIGNAL(released()), this, SLOT(searchAction()));
	connect(cancelPushButton, SIGNAL(released()), this, SLOT(cancelAction()));
	connect(closePushButton, SIGNAL(released()), this, SLOT(closeAction()));
}

void UserSearch::updateWidgets(void) {
	setWindowFlags(Qt::Dialog);
	updateForms();
}

void UserSearch::updateForms(void) {
	clear();
	focusDefault();
	searchPushButton->setDefault(true);
}

void UserSearch::clear(void) {
	nameLineEdit->clear();
}

void UserSearch::focusDefault(void) {
	nameLineEdit->setFocus();
}
