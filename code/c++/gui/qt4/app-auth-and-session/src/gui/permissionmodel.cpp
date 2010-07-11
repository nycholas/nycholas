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
#include "permissionmodel.h"

PermissionModel::PermissionModel() {
	id = 0;
}

void PermissionModel::selectAll(QList<PermissionModel> *list) {
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
}

void PermissionModel::selectById(int _id, PermissionModel *m) {
	QSqlQuery query;
	query.prepare("SELECT *  "
		"FROM auth_permission "
		"WHERE id=:id");
	query.bindValue(":id", _id);
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
		m->id = query.value(idCol).toInt();
		m->contentTypes = ct;
		m->name = query.value(nameCol).toString();
		m->codename = query.value(codenameCol).toString();
	}
}

bool PermissionModel::save(void) {
	if (id > 0) {
		return update();
	}
	return insert();
}

bool PermissionModel::insert(void) {
	QSqlQuery query;
	query.prepare("INSERT INTO auth_permission "
		"(app_content_types_id, name, codename) "
		"VALUES (:app_content_types_id, :name, :codename)");
	query.bindValue(":app_content_types_id", contentTypes->getId());
	query.bindValue(":name", name);
	query.bindValue(":codename", codename);
	return query.exec();
}

bool PermissionModel::update(void) {
	QSqlQuery query;
	query.prepare("UPDATE auth_permission "
		"SET app_content_types_id=:app_content_types_id, "
		"name=:name, codename=:codename"
		"WHERE id=:id");
	query.bindValue(":id", id);
	query.bindValue(":app_content_types_id", contentTypes->getId());
	query.bindValue(":name", name);
	query.bindValue(":codename", codename);
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
	query.bindValue(":id", id);
	return query.exec();
}

int PermissionModel::getId(void) {
	return id;
}

void PermissionModel::setId(int _id) {
	id = _id;
}

ContentTypesModel *PermissionModel::getContentTypes(void) {
	return contentTypes;
}

void PermissionModel::setContentTypes(ContentTypesModel *_contentTypes) {
	contentTypes = _contentTypes;
}

QString PermissionModel::getName(void) {
	return name;
}

void PermissionModel::setName(QString _name) {
	name = _name;
}

QString PermissionModel::getCodename(void) {
	return codename;
}

void PermissionModel::setCodename(QString _codename) {
	codename = _codename;
}
