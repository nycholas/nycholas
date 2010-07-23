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
 *    this software without specific prior written group.
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
#ifndef CONTENTTYPES_H
#define CONTENTTYPES_H

#include <QDebug>
#include <QSqlQuery>
#include <QSqlRecord>
#include <QSqlRelationalTableModel>
#include <QSqlRelationalDelegate>
#include <QSqlError>
#include <QModelIndex>
#include <QDateTime>
#include <QTimer>
#include <QMessageBox>

#include "widgets/ui_group.h"
#include "groupmodel.h"
#include "groupform.h"
#include "groupsearch.h"

class Group: public QWidget, private Ui::Group {
	Q_OBJECT

private slots:
	void timerStatusAction(void);
	void newAction(void);
	void activateAction(void);
	void desactivateAction(void);
	void removeAction(void);
	void searchAdvancedAction(void);
	void searchTextChangedAction(void);
	void closeAction(void);
	void selectedItemViewAction(const QModelIndex &index);
	void lastestAction(void);
	void nextAction(void);
	void previousAction(void);
	void oldestAction(void);

private:
	void createModels(void);
	void createViews(void);
	void createActions(void);
	void createWidgets(void);
	void updateWidgets(void);

	void timerStatus(void);
	void okStatus(const QString &msg);
	void infoStatus(const QString &msg);
	void alertStatus(const QString &msg);
	void errorStatus(const QString &msg);

	QTimer *statusTimer;
	GroupModel *groupModel;
	QSqlRelationalDelegate *groupDelegate;
	GroupSearch *groupSearch;

public slots:
	void updateModels(void);
	void updateSearchForm(void);
	void updateSearchFormClose(void);
	void updateStatus(const QString &msg, int code);

public:
	Group(QWidget *parent = 0);
	~Group();

	enum {
		group_id = 0, group_name = 1
	};
};

#endif /* CONTENTTYPES_H */
