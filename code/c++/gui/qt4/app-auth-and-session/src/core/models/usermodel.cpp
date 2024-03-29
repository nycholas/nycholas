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
#include "usermodel.h"

UserModel::UserModel(int id, QObject *parent, QSqlDatabase db) :
	QSqlRelationalTableModel(parent, db) {
	_id = id;
	_f = "";
	_begin = 0;
	_limit = 25;
}

void UserModel::paginator(void) {
	QString f = _f.isEmpty() ? "1=1" : _f;
	setFilter(QString("%1 %2 LIMIT %3, %4 --").arg(f).arg(orderByClause()).arg(
			_begin).arg(_limit));
	select();
}

int UserModel::count(void) {
	QSqlQuery query;
	QString f = _f.isEmpty() ? "1=1" : _f;
	query.prepare(QString("SELECT count(id) FROM auth_user WHERE %1").arg(f));
	if (!query.exec())
		return 0;
	int count = 0;
	if (query.next())
		count = query.value(0).toInt();
	return count;
}

QString UserModel::f(void) {
	return _f;
}

void UserModel::setF(const QString &f) {
	_f = f;
}

int UserModel::begin(void) {
	return _begin;
}

void UserModel::setBegin(int begin) {
	_begin = begin;
}

int UserModel::limit(void) {
	return _limit;
}

void UserModel::setLimit(int limit) {
	_limit = limit;
}

/*void UserModel::selectAll(QList<UserModel> *list) {
 QSqlQuery query;
 query.prepare("SELECT * FROM auth_user");
 if (!query.exec())
 return;
 int idCol = query.record().indexOf("id");
 int nameCol = query.record().indexOf("name");
 while (query.next()) {
 UserModel m;
 m.setId(query.value(idCol).toInt());
 m.setName(query.value(nameCol).toString());
 list->append(m);
 }
 }*/

void UserModel::selectById(int id, UserModel *m) {
	QSqlQuery query;
	query.prepare("SELECT *  "
		"FROM auth_user "
		"WHERE id=:id");
	query.bindValue(":id", id);
	if (!query.exec())
		return;
	int idCol = query.record().indexOf("id");
	int nameCol = query.record().indexOf("name");
	if (query.next()) {
		m->setId(query.value(idCol).toInt());
		m->setName(query.value(nameCol).toString());
	}
}

bool UserModel::save(void) {
	if (_id > 0) {
		return update();
	}
	return insert();
}

bool UserModel::insert(void) {
	QSqlQuery query;
	query.prepare("INSERT INTO auth_user "
		"(name) "
		"VALUES (:name)");
	query.bindValue(":name", _name);
	return query.exec();
}

bool UserModel::update(void) {
	QSqlQuery query;
	query.prepare("UPDATE auth_user "
		"SET name=:name "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	query.bindValue(":name", _name);
	return query.exec();
}

bool UserModel::status(void) {
	/*QSqlQuery query;
	 query.prepare("UPDATE auth_user "
	 "SET date_changed=:date_changed, is_active=:is_active "
	 "WHERE id=:id");
	 query.bindValue(":id", _id);
	 query.bindValue(":date_changed", _dateChanged);
	 query.bindValue(":is_active", _isActive);
	 return query.exec();*/
	return false;
}

bool UserModel::remove(void) {
	QSqlQuery query;
	query.prepare("DELETE FROM auth_user "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	return query.exec();
}

int UserModel::getId(void) {
	return _id;
}

void UserModel::setId(int id) {
	_id = id;
}

QString UserModel::getName(void) {
	return _name;
}

void UserModel::setName(QString name) {
	_name = name;
}
