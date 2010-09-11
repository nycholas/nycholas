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
#ifndef USERMODEL_H
#define USERMODEL_H

#include <QDebug>
#include <QSqlRelationalTableModel>
#include <QSqlQueryModel>
#include <QSqlDatabase>
#include <QSqlRecord>
#include <QSqlQuery>
#include <QDateTime>
#include <QVariant>
#include <QColor>
#include <QList>

class UserModel: public QSqlRelationalTableModel {
Q_OBJECT

private:
	int _id;
	QString _name;

	QString _f;
	int _begin;
	int _limit;

public:
			UserModel(int id = 0, QObject *parent = 0, QSqlDatabase db =
					QSqlDatabase());
	void paginator(void);
	int count(void);
	QString f(void);
	void setF(const QString &f = "");
	int begin(void);
	void setBegin(int begin);
	int limit(void);
	void setLimit(int limit);

	//static void selectAll(QList<UserModel> *list);
	static void selectById(int id, UserModel *userModel);
	bool save(void);
	bool insert(void);
	bool update(void);
	bool status(void);
	bool remove(void);

	int getId(void);
	void setId(int id);
	QString getName(void);
	void setName(QString name);
};

#endif /* USERMODEL_H */
