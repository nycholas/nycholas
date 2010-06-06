/**
 * Simple example Qt - CRUD.
 * Copyright (C) 2010 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 **/
#ifndef NOTEBOOKITEM_H
#define NOTEBOOKITEM_H

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

#include "widgets/ui_notebookitem.h"

class NotebookItem: public QDialog, private Ui::NotebookItem {
Q_OBJECT

protected:
	void okStatus(const QString &msg);
	void infoStatus(const QString &msg);
	void alertStatus(const QString &msg);
	void errorStatus(const QString &msg);

private slots:
	void timerStatusAction(void);
	void saveAction(void);
	void saveAndContinueSavingAction(void);
	void removeAction(void);
	void cancelAction(void);

private:
	void createActions(void);
	void updateWidgets(void);
	void timerStatus(void);
	void selectForId(int id);
	bool insertForm(void);
	bool updateForm(int id);
	bool removeForm(int id);
	void clearForm(void);

	int _id;
	QTimer *_timerStatus;

signals:
	void tableListChanged(void);

public:
	NotebookItem(QDialog *parent = 0);
	NotebookItem(int id, QDialog *parent = 0);
	~NotebookItem();

	int getId(void);
	void setId(int id);
};

#endif /* NOTEBOOKITEM_H_ */
