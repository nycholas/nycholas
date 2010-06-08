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
#include "notebookmodel.h"

NotebookModel::NotebookModel(QObject *parent) :
	QSqlQueryModel(parent) {
	id = 0;
}

/*QVariant NotebookModel::data(const QModelIndex &index, int role) const {
 QVariant value = QSqlQueryModel::data(index, role);
 if (value.isValid() && role == Qt::DisplayRole) {
 if (index.column() == 0)
 return value.toString().prepend("#");
 else if (index.column() == 2)
 return value.toString().toUpper();
 }
 if (role == Qt::TextColorRole && index.column() == 1)
 return qVariantFromValue(QColor(Qt::blue));
 return value;
 }*/

void NotebookModel::selectById(int id, NotebookModel *n) {
	QSqlQuery query;
	query.prepare("SELECT *  "
		"FROM notebook "
		"WHERE id=:id");
	query.bindValue(":id", id);
	if (!query.exec())
		return;
	int idCol = query.record().indexOf("id");
	int nameCol = query.record().indexOf("name");
	int descriptionCol = query.record().indexOf("description");
	int dateJoinedCol = query.record().indexOf("date_joined");
	int dateChangedCol = query.record().indexOf("date_changed");
	int isActiveCol = query.record().indexOf("is_active");
	if (query.next()) {
		n->setId(query.value(idCol).toInt());
		n->setName(query.value(nameCol).toString());
		n->setDescription(query.value(descriptionCol).toString());
		n->setDateJoined(QDateTime::fromString(
				query.value(dateJoinedCol).toString(), "yyyy-MM-dd hh:mm:ss"));
		n->setDateChanged(QDateTime::fromString(
				query.value(dateChangedCol).toString(), "yyyy-MM-dd hh:mm:ss"));
		n->setIsActive((query.value(isActiveCol).toBool() ? true : false));
	}
}

bool NotebookModel::save(void) {
	if (getId() > 0) {
		return update();
	}
	return insert();
}

bool NotebookModel::insert(void) {
	QSqlQuery query;
	query.prepare(
			"INSERT INTO notebook (name, description, date_joined, is_active) "
				"VALUES (:name, :description, :date_joined, :is_active)");
	query.bindValue(":name", name);
	query.bindValue(":description", description);
	query.bindValue(":date_joined", dateJoined.toString("yyyy-MM-dd hh:mm:ss"));
	query.bindValue(":is_active", isActive);
	return query.exec();
}

bool NotebookModel::update(void) {
	QSqlQuery query;
	query.prepare("UPDATE notebook "
		"SET name=:name, description=:description, "
		"date_changed=:date_changed, is_active=:is_active "
		"WHERE id=:id");
	query.bindValue(":id", id);
	query.bindValue(":name", name);
	query.bindValue(":description", description);
	query.bindValue(":date_changed", dateChanged);
	query.bindValue(":is_active", isActive);
	return query.exec();
}

bool NotebookModel::status(void) {
	QSqlQuery query;
	query.prepare("UPDATE notebook "
		"SET date_changed=:date_changed, is_active=:is_active "
		"WHERE id=:id");
	query.bindValue(":id", id);
	query.bindValue(":date_changed", dateChanged);
	query.bindValue(":is_active", isActive);
	return query.exec();
}

bool NotebookModel::remove(void) {
	QSqlQuery query;
	query.prepare("DELETE FROM notebook "
		"WHERE id=:id");
	query.bindValue(":id", id);
	return query.exec();
}

int NotebookModel::getId(void) {
	return id;
}

void NotebookModel::setId(int _id) {
	id = _id;
}

QString NotebookModel::getName(void) {
	return name;
}

void NotebookModel::setName(QString _name) {
	name = _name;
}

QString NotebookModel::getDescription(void) {
	return description;
}

void NotebookModel::setDescription(QString _description) {
	description = _description;
}

QDateTime NotebookModel::getDateJoined(void) {
	return dateJoined;
}

void NotebookModel::setDateJoined(QDateTime _dateJoined) {
	dateJoined = _dateJoined;
}

QDateTime NotebookModel::getDateChanged(void) {
	return dateChanged;
}

void NotebookModel::setDateChanged(QDateTime _dateChanged) {
	dateChanged = _dateChanged;
}

int NotebookModel::getIsActive(void) {
	return isActive;
}

void NotebookModel::setIsActive(int _isActive) {
	isActive = _isActive;
}
