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
#include "notebook.h"

NotebookItem::NotebookItem(QDialog *parent) :
	QDialog(parent) {
	setupUi(this);

	_id = 0;
	_timerStatus = new QTimer(this);

	createActions();
	updateWidgets();
}

NotebookItem::NotebookItem(int id, QDialog *parent) :
	QDialog(parent) {
	setupUi(this);

	_id = id;
	_timerStatus = new QTimer(this);

	createActions();
	updateWidgets();
}

NotebookItem::~NotebookItem(void) {
}

void NotebookItem::okStatus(const QString &msg) {
	labelStatus->setText(msg);
	labelStatus->setStyleSheet("color: green; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid green;");
	labelStatus->show();
	timerStatus();
}

void NotebookItem::infoStatus(const QString &msg) {
	labelStatus->setText(msg);
	labelStatus->setStyleSheet("color: blue; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid blue;");
	labelStatus->show();
	timerStatus();
}

void NotebookItem::alertStatus(const QString &msg) {
	labelStatus->setText(msg);
	labelStatus->setStyleSheet("color: yellow; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid yellow;");
	labelStatus->show();
	timerStatus();
}

void NotebookItem::errorStatus(const QString &msg) {
	labelStatus->setText(msg);
	labelStatus->setStyleSheet("color: red; background-color: white;"
		"margin: 2px; padding: 3px; border: 1px solid red;");
	labelStatus->show();
	timerStatus();
}

void NotebookItem::timerStatusAction(void) {
	labelStatus->hide();
	labelStatus->setText("");
}

void NotebookItem::saveAction(void) {
	bool exec = false;
	if (_id > 0) {
		exec = updateForm(_id);
	} else {
		exec = insertForm();
	}
	if (!exec) {
		errorStatus(qApp->tr("Failure trying to register the record."));
	} else {
		close();
		emit tableListChanged();
		QMessageBox::information(0, qApp->tr("Registration successful."),
				qApp->tr("Registration successful."), QMessageBox::Ok);
	}
}

void NotebookItem::saveAndContinueSavingAction(void) {
	bool exec = false;
	if (_id > 0) {
		exec = updateForm(_id);
		_id = 0;
	} else {
		exec = insertForm();
	}
	if (!exec) {
		errorStatus(qApp->tr("Failure trying to register the record."));
	} else {
		clearForm();
		updateWidgets();
		lineEditName->setFocus();
		okStatus(qApp->tr("Registration successful."));
		emit tableListChanged();
	}
}

void NotebookItem::removeAction(void) {
	bool exec = removeForm(_id);
	if (!exec) {
		errorStatus(qApp->tr("Fails to remove the record."));
	} else {
		close();
		emit tableListChanged();
		QMessageBox::information(0, qApp->tr("Record deleted successfully."),
				qApp->tr("Record deleted successfully."), QMessageBox::Ok);
	}
}

void NotebookItem::cancelAction(void) {
	close();
}

void NotebookItem::createActions(void) {
	connect(_timerStatus, SIGNAL(timeout()), this, SLOT(timerStatusAction()));

	connect(pushButtonSave, SIGNAL(released()), this, SLOT(saveAction()));
	connect(pushButtonSaveAndContinueSaving, SIGNAL(released()), this,
			SLOT(saveAndContinueSavingAction()));
	connect(pushButtonRemove, SIGNAL(released()), this, SLOT(removeAction()));
	connect(pushButtonCancel, SIGNAL(released()), this, SLOT(cancelAction()));
}

void NotebookItem::updateWidgets(void) {
	clearForm();
	if (_id > 0) {
		selectForId(_id);
		groupBox->setTitle(qApp->tr("&Change NotebookItem"));
		pushButtonSave->setText(qApp->tr("&Update"));
		pushButtonSaveAndContinueSaving->setText(qApp->tr(
				"Update and &continue saving"));
		pushButtonRemove->show();
	} else {
		groupBox->setTitle(qApp->tr("&Add NotebookItem"));
		pushButtonSave->setText(qApp->tr("&Save"));
		pushButtonSaveAndContinueSaving->setText(qApp->tr(
				"Save and &continue saving"));
		pushButtonRemove->hide();
	}
	pushButtonSave->setDefault(true);
	labelStatus->hide();
}

void NotebookItem::timerStatus(void) {
	_timerStatus->start(3000);
}

void NotebookItem::selectForId(int id) {
	QSqlQuery query;
	query.prepare("SELECT name, description, is_active  "
		"FROM notebook "
		"WHERE id=:id");
	query.bindValue(":id", id);
	query.exec();
	int indexName = query.record().indexOf("name");
	int indexDescription = query.record().indexOf("description");
	int indexIsActive = query.record().indexOf("is_active");
	if (query.next()) {
		lineEditName->setText(query.value(indexName).toString());
		textEditDescription->setText(query.value(indexDescription).toString());
		checkBoxIsActived->setChecked(
				(query.value(indexIsActive).toBool() ? true : false));
	}
}

bool NotebookItem::insertForm(void) {
	QString name = lineEditName->text();
	QString description = textEditDescription->toPlainText();
	QString dateTimeCurr = QDateTime::currentDateTime().toString(
			"yyyy-MM-dd hh:mm:ss");
	int isActive = checkBoxIsActived->isChecked() ? 1 : 0;

	QSqlQuery query;
	query.prepare(
			"INSERT INTO notebook (name, description, date_joined, is_active) "
				"VALUES (:name, :description, :date_joined, :is_active)");
	query.bindValue(":name", name);
	query.bindValue(":description", description);
	query.bindValue(":date_joined", dateTimeCurr);
	query.bindValue(":is_active", isActive);
	return query.exec();
}

bool NotebookItem::updateForm(int id) {
	QString name = lineEditName->text();
	QString description = textEditDescription->toPlainText();
	QString dateTimeCurr = QDateTime::currentDateTime().toString(
			"yyyy-MM-dd hh:mm:ss");
	int isActive = checkBoxIsActived->isChecked() ? 1 : 0;

	QSqlQuery query;
	query.prepare("UPDATE notebook "
		"SET name=:name, description=:description, "
		"date_changed=:date_changed, is_active=:is_active "
		"WHERE id=:id");
	query.bindValue(":id", id);
	query.bindValue(":name", name);
	query.bindValue(":description", description);
	query.bindValue(":date_changed", dateTimeCurr);
	query.bindValue(":is_active", isActive);
	return query.exec();
}

bool NotebookItem::removeForm(int id) {
	QSqlQuery query;
	query.prepare("DELETE FROM notebook "
		"WHERE id=:id");
	query.bindValue(":id", id);
	return query.exec();
}

void NotebookItem::clearForm(void) {
	lineEditName->clear();
	textEditDescription->clear();
	checkBoxIsActived->setChecked(true);
}

int NotebookItem::getId(void) {
	return _id;
}

void NotebookItem::setId(int id) {
	_id = id;
}
