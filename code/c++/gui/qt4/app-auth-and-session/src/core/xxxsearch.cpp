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
#include "xxx.h"

XxxSearch::XxxSearch(XxxModel *model, QDialog *parent) :
	QDialog(parent) {
	setupUi(this);
	xXxModel = model;
	createActions();
	updateWidgets();
}

XxxSearch::~XxxSearch(void) {
}

void XxxSearch::closeEvent(QCloseEvent *event) {
	emit formSearchClose();
	event->accept();
}

void XxxSearch::searchAction(void) {
	QString name = nameLineEdit->text();
	QString description = descriptionLineEdit->text();
	int isActive = isActivedCheckBox->isChecked() ? 1 : 0;
	QString dateJoinedInit = dateJoinedInitDateEdit->date().toString(
			"yyyy-MM-dd");
	QString dateJoinedEnd =
			dateJoinedEndDateEdit->date().toString("yyyy-MM-dd");

	QString query = " 1=1 ";
	if (!name.isEmpty())
		query.append(QString(" AND name LIKE '%1\%'").arg(name));
	if (!description.isEmpty())
		query.append(QString(" AND description LIKE '\%%1\%'").arg(description));
	query.append(QString(" AND is_active='%1'").arg(isActive));
	query.append(QString(" AND (date_joined >= '%1 00:00:00' AND "
		"date_joined <= '%2 23:59:59')").arg(dateJoinedInit, dateJoinedEnd));
	xXxModel->setF(query);
	xXxModel->setBegin(0);
	emit formSearched();
}

void XxxSearch::cancelAction(void) {
	xXxModel->setF("");
	xXxModel->setBegin(0);
	emit
	formSearchClose();
	hide();
}

void XxxSearch::closeAction(void) {
	emit formSearchClose();
	hide();
}

void XxxSearch::createActions(void) {
	connect(searchPushButton, SIGNAL(released()), this, SLOT(searchAction()));
	connect(cancelPushButton, SIGNAL(released()), this, SLOT(cancelAction()));
	connect(closePushButton, SIGNAL(released()), this, SLOT(closeAction()));
}

void XxxSearch::updateWidgets(void) {
	updateForms();
}

void XxxSearch::updateForms(void) {
	clear();
	focusDefault();
	searchPushButton->setDefault(true);
}

void XxxSearch::clear(void) {
	nameLineEdit->clear();
	descriptionLineEdit->clear();
	isActivedCheckBox->setChecked(true);
	dateJoinedInitDateEdit->setDate(QDate::currentDate());
	dateJoinedEndDateEdit->setDate(QDate::currentDate());
}

void XxxSearch::focusDefault(void) {
	nameLineEdit->setFocus();
}
