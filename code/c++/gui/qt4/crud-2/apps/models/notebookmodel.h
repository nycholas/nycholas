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
#ifndef NOTEBOOKMODEL_H
#define NOTEBOOKMODEL_H

#include <QDebug>
#include <QSqlQueryModel>
#include <QSqlRecord>
#include <QSqlQuery>
#include <QDateTime>
#include <QVariant>
#include <QColor>

class NotebookModel: public QSqlQueryModel {
Q_OBJECT

private:
	int id;
	QString name;
	QString description;
	QDateTime dateJoined;
	QDateTime dateChanged;
	int isActive;

public:
	NotebookModel(QObject *parent = 0);
	//QVariant data(const QModelIndex &item, int role) const;
	static void selectById(int id, NotebookModel *notebookModel);
	bool save(void);
	bool insert(void);
	bool update(void);
	bool status(void);
	bool remove(void);

	int getId(void);
	void setId(int id);
	QString getName(void);
	void setName(QString name);
	QString getDescription(void);
	void setDescription(QString description);
	QDateTime getDateJoined(void);
	void setDateJoined(QDateTime dateJoined);
	QDateTime getDateChanged(void);
	void setDateChanged(QDateTime dateChanged);
	int getIsActive(void);
	void setIsActive(int isActive);
};

#endif /* NOTEBOOKMODEL_H_ */
