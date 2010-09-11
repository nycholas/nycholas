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
#include "permissionmodel.h"

PermissionModel::PermissionModel(int id, QObject *parent, QSqlDatabase db) :
	QSqlRelationalTableModel(parent, db) {
	_id = id;
	_f = "";
	_begin = 0;
	_limit = 25;
}

void PermissionModel::paginator(void) {
	QString f = _f.isEmpty() ? "1=1" : _f;
	setFilter(QString("%1 %2 LIMIT %3, %4 --").arg(f).arg(orderByClause()).arg(
			_begin).arg(_limit));
	select();
}

int PermissionModel::count(void) {
	QSqlQuery query;
	QString f = _f.isEmpty() ? "1=1" : _f;
	query.prepare(
			QString("SELECT count(id) FROM auth_permission WHERE %1").arg(f));
	if (!query.exec())
		return 0;
	int count = 0;
	if (query.next())
		count = query.value(0).toInt();
	return count;
}

QString PermissionModel::f(void) {
	return _f;
}

void PermissionModel::setF(const QString &f) {
	_f = f;
}

int PermissionModel::begin(void) {
	return _begin;
}

void PermissionModel::setBegin(int begin) {
	_begin = begin;
}

int PermissionModel::limit(void) {
	return _limit;
}

void PermissionModel::setLimit(int limit) {
	_limit = limit;
}

/*void PermissionModel::selectAll(QList<PermissionModel> *list) {
 QSqlQuery query;
 query.prepare("SELECT * FROM auth_permission");
 if (!query.exec())
 return;
 int idCol = query.record().indexOf("id");
 int contentTypesCol = query.record().indexOf("app_content_types_id");
 int nameCol = query.record().indexOf("name");
 int codenameCol = query.record().indexOf("codename");
 while (query.next()) {
 ContentTypesModel *ct;
 ct->setId(query.value(contentTypesCol).toInt());
 ContentTypesModel::selectById(ct->getId(), ct);
 PermissionModel m;
 m.setId(query.value(idCol).toInt());
 m.setContentTypes(ct);
 m.setName(query.value(nameCol).toString());
 m.setCodename(query.value(codenameCol).toString());
 list->append(m);
 }
 }*/

void PermissionModel::selectById(int id, PermissionModel *m) {
	QSqlQuery query;
	query.prepare("SELECT *  "
		"FROM auth_permission "
		"WHERE id=:id");
	query.bindValue(":id", id);
	if (!query.exec())
		return;
	int idCol = query.record().indexOf("id");
	int contentTypesCol = query.record().indexOf("app_content_types_id");
	int nameCol = query.record().indexOf("name");
	int codenameCol = query.record().indexOf("codename");
	ContentTypesModel *ct;
	if (query.next()) {
		ct = new ContentTypesModel();
		ct->setId(query.value(contentTypesCol).toInt());
		ContentTypesModel::selectById(ct->getId(), ct);
		m->setId(query.value(idCol).toInt());
		m->setContentTypes(ct);
		m->setName(query.value(nameCol).toString());
		m->setCodename(query.value(codenameCol).toString());
	}
}

bool PermissionModel::save(void) {
	if (_id > 0) {
		return update();
	}
	return insert();
}

bool PermissionModel::insert(void) {
	QSqlQuery query;
	query.prepare("INSERT INTO auth_permission "
		"(app_content_types_id, name, codename) "
		"VALUES (:app_content_types_id, :name, :codename)");
	query.bindValue(":app_content_types_id", _contentTypes->getId());
	query.bindValue(":name", _name);
	query.bindValue(":codename", _codename);
	return query.exec();
}

bool PermissionModel::update(void) {
	QSqlQuery query;
	query.prepare("UPDATE auth_permission "
		"SET app_content_types_id=:app_content_types_id, "
		"name=:name, codename=:codename"
		"WHERE id=:id");
	query.bindValue(":id", _id);
	query.bindValue(":app_content_types_id", _contentTypes->getId());
	query.bindValue(":name", _name);
	query.bindValue(":codename", _codename);
	return query.exec();
}

bool PermissionModel::status(void) {
	qDebug() << "In PermissionModel::status()";
	qDebug() << "Not implemented";
	return false;
}

bool PermissionModel::remove(void) {
	QSqlQuery query;
	query.prepare("DELETE FROM auth_permission "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	return query.exec();
}

int PermissionModel::getId(void) {
	return _id;
}

void PermissionModel::setId(int id) {
	_id = id;
}

ContentTypesModel *PermissionModel::getContentTypes(void) {
	return _contentTypes;
}

void PermissionModel::setContentTypes(ContentTypesModel *contentTypes) {
	_contentTypes = contentTypes;
}

QString PermissionModel::getName(void) {
	return _name;
}

void PermissionModel::setName(QString name) {
	_name = name;
}

QString PermissionModel::getCodename(void) {
	return _codename;
}

void PermissionModel::setCodename(QString codename) {
	_codename = codename;
}
