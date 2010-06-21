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
static bool createConnection()
{
    QSqlDatabase db = QSqlDatabase::addDatabase("QMYSQL");
    db.setHostName("localhost");
    db.setDatabaseName("appdb");
    db.setUserName("root");
    db.setPassword("1q2w3e");
    if (!db.open()) {
        QMessageBox::critical(0, qApp->tr("Cannot open database"),
            qApp->tr("Unable to establish a database connection.\n"
                     "This example needs SQLite support. Please read "
                     "the Qt SQL driver documentation for information how "
                     "to build it.\n\n"
                     "Click Cancel to exit."), QMessageBox::Cancel);
        return false;
    }
    return true;
}

#endif
