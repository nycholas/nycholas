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
#include "notebook.h"

Notebook::Notebook(QWidget *parent) :
	QWidget(parent) {
	setupUi(this);
	createModels();
	createDelegates();
	createViews();
	createActions();
	updateWidgets();
}

Notebook::~Notebook(void) {
}

void Notebook::newAction(void) {
	NotebookItem *notebookItem = new NotebookItem();
	connect(notebookItem, SIGNAL(tableListChanged()), this,
			SLOT(updateModels()));
	notebookItem->show();
}

void Notebook::closeAction(void) {
	close();
}

void Notebook::doubleClickedItemViewAction(const QModelIndex &index) {
	if (!index.isValid()) {
		QMessageBox::warning(0, qApp->tr("No items."), qApp->tr(
				"Please select an item to edit."), QMessageBox::Ok);
		return;
	}

	QSqlRecord record = model->record(index.row());
	int id = record.value(notebook_id).toInt();

	NotebookItem *notebookItem = new NotebookItem(id);
	connect(notebookItem, SIGNAL(tableListChanged()), this,
			SLOT(updateModels()));
	notebookItem->show();
}

void Notebook::createModels(void) {
	model = new QSqlRelationalTableModel(this);
	model->setTable("notebook");
	model->setHeaderData(notebook_id, Qt::Horizontal, qApp->tr("Id"));
	model->setHeaderData(notebook_name, Qt::Horizontal, qApp->tr("Name"));
	model->setHeaderData(notebook_description, Qt::Horizontal, qApp->tr(
			"Description"));
	model->setSort(notebook_id, Qt::DescendingOrder);
	model->select();
}

void Notebook::createDelegates(void) {
	delegate = new QSqlRelationalDelegate(tableViewList);
}

void Notebook::createViews(void) {
	tableViewList->setModel(model);
	tableViewList->setItemDelegate(delegate);
	tableViewList->setSelectionMode(QAbstractItemView::ExtendedSelection);
	tableViewList->setSelectionBehavior(QAbstractItemView::SelectRows);
	tableViewList->setColumnHidden(notebook_dateJoined, true);
	tableViewList->setColumnHidden(notebook_dateChanged, true);
	tableViewList->setColumnHidden(notebook_isActive, true);
	tableViewList->resizeColumnsToContents();
	tableViewList->setEditTriggers(QAbstractItemView::NoEditTriggers);

	QHeaderView *header = tableViewList->horizontalHeader();
	header->setStretchLastSection(true);
}

void Notebook::createActions(void) {
	connect(model, SIGNAL(beforeInsert(QSqlRecord &)), this,
			SLOT(beforeInsertNotebookItemAction()));
	connect(model, SIGNAL(primeInsert(int, QSqlRecord &)), this,
			SLOT(primeInsertNotebookItemAction(int, QSqlRecord &)));

	connect(pushButtonNew, SIGNAL(released()), this, SLOT(newAction()));
	//connect(pushButtonCancel, SIGNAL(released()), this, SLOT(closeAction()));

	connect(tableViewList, SIGNAL(doubleClicked(const QModelIndex &)), this,
			SLOT(doubleClickedItemViewAction(const QModelIndex &)));
}

void Notebook::updateWidgets(void) {
}

void Notebook::updateModels(void) {
	model->setTable("notebook");
	model->setHeaderData(notebook_id, Qt::Horizontal, qApp->tr("Id"));
	model->setHeaderData(notebook_name, Qt::Horizontal, qApp->tr("Name"));
	model->setHeaderData(notebook_description, Qt::Horizontal, qApp->tr(
			"Description"));
	model->setSort(notebook_id, Qt::DescendingOrder);
	model->select();
}
