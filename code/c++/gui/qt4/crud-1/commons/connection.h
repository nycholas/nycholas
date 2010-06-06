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
#ifndef CONNECTION_H
#define CONNECTION_H

#include <QMessageBox>
#include <QSqlDatabase>
#include <QSqlError>
#include <QSqlQuery>

/**
 * Creates a QSqlDatabase  connection that uses the driver referred to
 * by type. If the type  is not recognized, the database connection
 * will have no functionality.
 * The currently available driver types are:
 *
 * QDB2       IBM DB2
 * QIBASE     Borland InterBase Driver
 * QMYSQL     MySQL Driver
 * QOCI       Oracle Call Interface Driver
 * QODBC      ODBC Driver (includes Microsoft SQL Server)
 * QPSQL      PostgreSQL Driver
 * QSQLITE    SQLite version 3 or above
 * QSQLITE2   SQLite version 2
 * QTDS       Sybase Adaptive Server
 */
static bool createConnection(void) {
	QSqlDatabase db = QSqlDatabase::addDatabase("QMYSQL");
	db.setHostName("localhost");
	db.setDatabaseName("cruddb");
	db.setUserName("root");
	db.setPassword("1q2w3e");
	if (!db.open()) {
		QMessageBox::critical(0, qApp->tr("Cannot open database"), qApp->tr(
				"Unable to establish a database connection.\n"
					"This example needs SQLite support. Please read "
					"the Qt SQL driver documentation for information how "
					"to build it.\n\n"
					"Click Cancel to exit."), QMessageBox::Cancel);
		return false;
	}
	return true;
}

static void closeConnection(void) {
	QSqlDatabase db = QSqlDatabase::database();
	if (db.open()) {
		db.removeDatabase("cruddb");
		db.close();
	}
}

#endif /* CONNECTION_H */
