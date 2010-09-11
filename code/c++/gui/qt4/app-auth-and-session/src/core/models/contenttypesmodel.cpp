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
#include "contenttypesmodel.h"

ContentTypesModel::ContentTypesModel(int id, QObject *parent, QSqlDatabase db) :
	QSqlRelationalTableModel(parent, db) {
	_id = id;
	_f = "";
	_begin = 0;
	_limit = 25;
}

void ContentTypesModel::paginator(void) {
	QString f = _f.isEmpty() ? "1=1" : _f;
	setFilter(QString("%1 %2 LIMIT %3, %4 --").arg(f).arg(orderByClause()).arg(
			_begin).arg(_limit));
	select();
}

int ContentTypesModel::count(void) {
	QSqlQuery query;
	QString f = _f.isEmpty() ? "1=1" : _f;
	query.prepare(
			QString("SELECT count(id) FROM app_content_type WHERE %1").arg(f));
	if (!query.exec())
		return 0;
	int count = 0;
	if (query.next())
		count = query.value(0).toInt();
	return count;
}

QString ContentTypesModel::f(void) {
	return _f;
}

void ContentTypesModel::setF(const QString &f) {
	_f = f;
}

int ContentTypesModel::begin(void) {
	return _begin;
}

void ContentTypesModel::setBegin(int begin) {
	_begin = begin;
}

int ContentTypesModel::limit(void) {
	return _limit;
}

void ContentTypesModel::setLimit(int limit) {
	_limit = limit;
}

/*void ContentTypesModel::selectAll(QList<ContentTypesModel> *list) {
 QSqlQuery query;
 query.prepare("SELECT * FROM app_content_type");
 if (!query.exec())
 return;
 int idCol = query.record().indexOf("id");
 int nameCol = query.record().indexOf("name");
 int descriptionCol = query.record().indexOf("description");
 int dateJoinedCol = query.record().indexOf("date_joined");
 int dateChangedCol = query.record().indexOf("date_changed");
 int isActiveCol = query.record().indexOf("is_active");
 while (query.next()) {
 ContentTypesModel m;
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

void ContentTypesModel::selectById(int id, ContentTypesModel *m) {
	QSqlQuery query;
	query.prepare("SELECT *  "
		"FROM app_content_type "
		"WHERE id=:id");
	query.bindValue(":id", id);
	if (!query.exec())
		return;
	int idCol = query.record().indexOf("id");
	int nameCol = query.record().indexOf("name");
	int appLabelCol = query.record().indexOf("app_label");
	int modelCol = query.record().indexOf("model");
	if (query.next()) {
		m->setId(query.value(idCol).toInt());
		m->setName(query.value(nameCol).toString());
		m->setAppLabel(query.value(appLabelCol).toString());
		m->setModel(query.value(modelCol).toString());
	}
}

bool ContentTypesModel::save(void) {
	if (_id > 0) {
		return update();
	}
	return insert();
}

bool ContentTypesModel::insert(void) {
	QSqlQuery query;
	query.prepare("INSERT INTO app_content_type (name, app_label, model) "
		"VALUES (:name, :app_label, :model)");
	query.bindValue(":name", _name);
	query.bindValue(":app_label", _appLabel);
	query.bindValue(":model", _model);
	return query.exec();
}

bool ContentTypesModel::update(void) {
	QSqlQuery query;
	query.prepare("UPDATE app_content_type "
		"SET name=:name, app_label=:app_label, "
		"model=:model "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	query.bindValue(":name", _name);
	query.bindValue(":app_label", _appLabel);
	query.bindValue(":model", _model);
	return query.exec();
}

bool ContentTypesModel::status(void) {
	qDebug() << "In GroupModel::status()";
	qDebug() << "Not implemented";
	return false;
}

bool ContentTypesModel::remove(void) {
	QSqlQuery query;
	query.prepare("DELETE FROM app_content_type "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	return query.exec();
}

int ContentTypesModel::getId(void) {
	return _id;
}

void ContentTypesModel::setId(int id) {
	_id = id;
}

QString ContentTypesModel::getName(void) {
	return _name;
}

void ContentTypesModel::setName(QString name) {
	_name = name;
}

QString ContentTypesModel::getAppLabel(void) {
	return _appLabel;
}

void ContentTypesModel::setAppLabel(QString appLabel) {
	_appLabel = appLabel;
}

QString ContentTypesModel::getModel(void) {
	return _model;
}

void ContentTypesModel::setModel(QString model) {
	_model = model;
}
