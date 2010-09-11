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
#ifndef CONNECTION_H
#define CONNECTION_H

#include <QtGui/QMessageBox>
#include <QtSql/QSqlDatabase>
#include <QtSql/QSqlError>
#include <QtSql/QSqlQuery>

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
	db.setDatabaseName("appdb");
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
		db.removeDatabase("appdb");
		db.close();
	}
}

#endif
