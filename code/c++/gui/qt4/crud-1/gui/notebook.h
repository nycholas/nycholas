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
#ifndef NOTEBOOK_H
#define NOTEBOOK_H

#include <QSqlTableModel>

#include "widgets/ui_notebook.h"
#include "notebookitem.h"

class Notebook: public QWidget, private Ui::Notebook {
Q_OBJECT

private slots:
	void newAction(void);
	void closeAction(void);
	void doubleClickedItemViewAction(const QModelIndex &);

private:
	void createModels(void);
	void createDelegates(void);
	void createViews(void);
	void createActions(void);
	void updateWidgets(void);

	QSqlRelationalTableModel *model;
	QSqlRelationalDelegate *delegate;
	enum {
		notebook_id = 0,
		notebook_name = 1,
		notebook_description = 2,
		notebook_dateJoined = 3,
		notebook_dateChanged = 4,
		notebook_isActive = 5
	};

public slots:
	void updateModels(void);

public:
	Notebook(QWidget *parent = 0);
	~Notebook();
};

#endif /* NOTEBOOK_H_ */
