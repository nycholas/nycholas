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
#include "usermodel.h"

UserModel::UserModel() {
	id = 0;
}

void UserModel::selectAll(QList<UserModel> *list) {
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
}

void UserModel::selectById(int _id, UserModel *m) {
	QSqlQuery query;
	query.prepare("SELECT *  "
		"FROM auth_user "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	if (!query.exec())
		return;
	int idCol = query.record().indexOf("id");
	int nameCol = query.record().indexOf("name");
	if (query.next()) {
		m->id = query.value(idCol).toInt();
		m->name = query.value(nameCol).toString();
	}
}

bool UserModel::save(void) {
	if (id > 0) {
		return update();
	}
	return insert();
}

bool UserModel::insert(void) {
	QSqlQuery query;
	query.prepare("INSERT INTO auth_user "
		"(name) "
		"VALUES (:name)");
	query.bindValue(":name", name);
	return query.exec();
}

bool UserModel::update(void) {
	QSqlQuery query;
	query.prepare("UPDATE auth_user "
		"SET name=:name "
		"WHERE id=:id");
	query.bindValue(":id", id);
	query.bindValue(":name", name);
	return query.exec();
}

bool UserModel::status(void) {
	qDebug() << "In UserModel::status()";
	qDebug() << "Not implemented";
	return false;
}

bool UserModel::remove(void) {
	QSqlQuery query;
	query.prepare("DELETE FROM auth_user "
		"WHERE id=:id");
	query.bindValue(":id", id);
	return query.exec();
}

int UserModel::getId(void) {
	return id;
}

void UserModel::setId(int _id) {
	id = _id;
}

QString UserModel::getName(void) {
	return name;
}

void UserModel::setName(QString _name) {
	name = _name;
}
