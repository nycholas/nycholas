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
#include "xxxmodel.h"

XxxModel::XxxModel(int id, QObject *parent, QSqlDatabase db) :
	QSqlRelationalTableModel(parent, db) {
	_id = id;
	_f = "";
	_begin = 0;
	_limit = 25;
}

void XxxModel::paginator(void) {
	QString f = _f.isEmpty() ? "1=1" : _f;
	setFilter(QString("%1 %2 LIMIT %3, %4 --").arg(f).arg(orderByClause()).arg(
			_begin).arg(_limit));
	select();
}

int XxxModel::count(void) {
	QSqlQuery query;
	QString f = _f.isEmpty() ? "1=1" : _f;
	query.prepare(QString("SELECT count(id) FROM x_x_x WHERE %1").arg(f));
	if (!query.exec())
		return 0;
	int count = 0;
	if (query.next())
		count = query.value(0).toInt();
	return count;
}

QString XxxModel::f(void) {
	return _f;
}

void XxxModel::setF(const QString &f) {
	_f = f;
}

int XxxModel::begin(void) {
	return _begin;
}

void XxxModel::setBegin(int begin) {
	_begin = begin;
}

int XxxModel::limit(void) {
	return _limit;
}

void XxxModel::setLimit(int limit) {
	_limit = limit;
}

/*void XxxModel::selectAll(QList<XxxModel> *list) {
 QSqlQuery query;
 query.prepare("SELECT * FROM x_x_x");
 if (!query.exec())
 return;
 int idCol = query.record().indexOf("id");
 int nameCol = query.record().indexOf("name");
 int descriptionCol = query.record().indexOf("description");
 int dateJoinedCol = query.record().indexOf("date_joined");
 int dateChangedCol = query.record().indexOf("date_changed");
 int isActiveCol = query.record().indexOf("is_active");
 while (query.next()) {
 XxxModel m;
 m.setId(query.value(idCol).toInt());
 m.setName(query.value(nameCol).toString());
 m.setDescription(query.value(descriptionCol).toString());
 m.setDateJoined(QDateTime::fromString(
 query.value(dateJoinedCol).toString(), "yyyy-MM-dd hh:mm:ss"));
 m.setDateChanged(QDateTime::fromString(
 query.value(dateChangedCol).toString(), "yyyy-MM-dd hh:mm:ss"));
 m.setIsActive((query.value(isActiveCol).toBool() ? true : false));
 list->append(m);
 }
 }*/

void XxxModel::selectById(int id, XxxModel *n) {
	QSqlQuery query;
	query.prepare("SELECT *  "
		"FROM x_x_x "
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

bool XxxModel::save(void) {
	if (_id > 0) {
		return update();
	}
	return insert();
}

bool XxxModel::insert(void) {
	QSqlQuery query;
	query.prepare(
			"INSERT INTO x_x_x (name, description, date_joined, is_active) "
				"VALUES (:name, :description, :date_joined, :is_active)");
	query.bindValue(":name", _name);
	query.bindValue(":description", _description);
	query.bindValue(":date_joined", _dateJoined.toString("yyyy-MM-dd hh:mm:ss"));
	query.bindValue(":is_active", _isActive);
	return query.exec();
}

bool XxxModel::update(void) {
	QSqlQuery query;
	query.prepare("UPDATE x_x_x "
		"SET name=:name, description=:description, "
		"date_changed=:date_changed, is_active=:is_active "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	query.bindValue(":name", _name);
	query.bindValue(":description", _description);
	query.bindValue(":date_changed", _dateChanged);
	query.bindValue(":is_active", _isActive);
	return query.exec();
}

bool XxxModel::status(void) {
	QSqlQuery query;
	query.prepare("UPDATE x_x_x "
		"SET date_changed=:date_changed, is_active=:is_active "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	query.bindValue(":date_changed", _dateChanged);
	query.bindValue(":is_active", _isActive);
	return query.exec();
}

bool XxxModel::remove(void) {
	QSqlQuery query;
	query.prepare("DELETE FROM x_x_x "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	return query.exec();
}

int XxxModel::getId(void) {
	return _id;
}

void XxxModel::setId(int id) {
	_id = id;
}

QString XxxModel::getName(void) {
	return _name;
}

void XxxModel::setName(QString name) {
	_name = name;
}

QString XxxModel::getDescription(void) {
	return _description;
}

void XxxModel::setDescription(QString description) {
	_description = description;
}

QDateTime XxxModel::getDateJoined(void) {
	return _dateJoined;
}

void XxxModel::setDateJoined(QDateTime dateJoined) {
	_dateJoined = dateJoined;
}

QDateTime XxxModel::getDateChanged(void) {
	return _dateChanged;
}

void XxxModel::setDateChanged(QDateTime dateChanged) {
	_dateChanged = dateChanged;
}

int XxxModel::getIsActive(void) {
	return _isActive;
}

void XxxModel::setIsActive(int isActive) {
	_isActive = isActive;
}
