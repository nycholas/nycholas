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
#include "contenttypesmodel.h"

ContentTypesModel::ContentTypesModel() {
	id = 0;
}

void ContentTypesModel::selectAll(QList<ContentTypesModel> *list) {
	QSqlQuery query;
	query.prepare("SELECT * FROM app_content_type");
	if (!query.exec())
		return;
	int idCol = query.record().indexOf("id");
	int nameCol = query.record().indexOf("name");
	int appLabelCol = query.record().indexOf("app_label");
	int modelCol = query.record().indexOf("model");
	while (query.next()) {
		ContentTypesModel m;
		m.setId(query.value(idCol).toInt());
		m.setName(query.value(nameCol).toString());
		m.setAppLabel(query.value(appLabelCol).toString());
		m.setModel(query.value(modelCol).toString());
		list->append(m);
	}
}

void ContentTypesModel::selectById(int _id, ContentTypesModel *m) {
	QSqlQuery query;
	query.prepare("SELECT *  "
		"FROM app_content_type "
		"WHERE id=:id");
	query.bindValue(":id", _id);
	if (!query.exec())
		return;
	int idCol = query.record().indexOf("id");
	int nameCol = query.record().indexOf("name");
	int appLabelCol = query.record().indexOf("app_label");
	int modelCol = query.record().indexOf("model");
	if (query.next()) {
		m->id = query.value(idCol).toInt();
		m->name = query.value(nameCol).toString();
		m->appLabel = query.value(appLabelCol).toString();
		m->model = query.value(modelCol).toString();
	}
}

bool ContentTypesModel::save(void) {
	if (id > 0) {
		return update();
	}
	return insert();
}

bool ContentTypesModel::insert(void) {
	QSqlQuery query;
	query.prepare("INSERT INTO app_content_type (name, app_label, model) "
		"VALUES (:name, :app_label, :model)");
	query.bindValue(":name", name);
	query.bindValue(":app_label", appLabel);
	query.bindValue(":model", model);
	return query.exec();
}

bool ContentTypesModel::update(void) {
	QSqlQuery query;
	query.prepare("UPDATE app_content_type "
		"SET name=:name, app_label=:app_label, "
		"model=:model "
		"WHERE id=:id");
	query.bindValue(":id", id);
	query.bindValue(":name", name);
	query.bindValue(":app_label", appLabel);
	query.bindValue(":model", model);
	return query.exec();
}

bool ContentTypesModel::status(void) {
	qDebug() << "In ContentTypesModel::status()";
	qDebug() << "Not implemented";
	return false;
}

bool ContentTypesModel::remove(void) {
	QSqlQuery query;
	query.prepare("DELETE FROM app_content_type "
		"WHERE id=:id");
	query.bindValue(":id", id);
	return query.exec();
}

int ContentTypesModel::getId(void) {
	return id;
}

void ContentTypesModel::setId(int _id) {
	id = _id;
}

QString ContentTypesModel::getName(void) {
	return name;
}

void ContentTypesModel::setName(QString _name) {
	name = _name;
}

QString ContentTypesModel::getAppLabel(void) {
	return appLabel;
}

void ContentTypesModel::setAppLabel(QString _appLabel) {
	appLabel = _appLabel;
}

QString ContentTypesModel::getModel(void) {
	return model;
}

void ContentTypesModel::setModel(QString _model) {
	model = _model;
}
