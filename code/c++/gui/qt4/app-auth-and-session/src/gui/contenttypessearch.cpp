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
#include "contenttypes.h"

ContentTypesSearch::ContentTypesSearch(QDialog *parent) :
	QDialog(parent) {
	setupUi(this);
	createActions();
	updateWidgets();
}

ContentTypesSearch::ContentTypesSearch(QSqlRelationalTableModel *model,
		QDialog *parent) :
	QDialog(parent) {
	setupUi(this);
	contentTypesModel = model;
	createActions();
	updateWidgets();
}

ContentTypesSearch::~ContentTypesSearch(void) {
}

void ContentTypesSearch::searchAction(void) {
	QString name = nameLineEdit->text();
	QString appLabel = appLabelLineEdit->text();
	QString model = modelLineEdit->text();

	QString query = " 1=1 ";
	if (!name.isEmpty())
		query.append(QString(" AND name LIKE '%1\%'").arg(name));
	if (!appLabel.isEmpty())
		query.append(QString(" AND app_label LIKE '\%%1\%'").arg(appLabel));
	if (!model.isEmpty())
		query.append(QString(" AND model LIKE '\%%1\%'").arg(model));
	contentTypesModel->setFilter(query);
	emit formSearched();
}

void ContentTypesSearch::cancelAction(void) {
	contentTypesModel->setFilter("");
	emit
	formSearchClose();
	close();
}

void ContentTypesSearch::closeAction(void) {
	emit formSearchClose();
	close();
}

void ContentTypesSearch::createActions(void) {
	connect(searchPushButton, SIGNAL(released()), this, SLOT(searchAction()));
	connect(cancelPushButton, SIGNAL(released()), this, SLOT(cancelAction()));
	connect(closePushButton, SIGNAL(released()), this, SLOT(closeAction()));
}

void ContentTypesSearch::updateWidgets(void) {
	setWindowFlags(Qt::Dialog);
	updateForms();
}

void ContentTypesSearch::updateForms(void) {
	clear();
	focusDefault();
	searchPushButton->setDefault(true);
}

void ContentTypesSearch::clear(void) {
	nameLineEdit->clear();
	appLabelLineEdit->clear();
	modelLineEdit->clear();
}

void ContentTypesSearch::focusDefault(void) {
	nameLineEdit->setFocus();
}
