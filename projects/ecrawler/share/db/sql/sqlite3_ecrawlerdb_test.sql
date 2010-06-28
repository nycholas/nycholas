CREATE TABLE "main"."emails" (
    "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "email_id" INTEGER NOT NULL,
    "email_from" TEXT NOT NULL,
    "email_to" TEXT NOT NULL,
    "email_subject" TEXT NOT NULL,
    "email_date" TEXT NOT NULL,
    "email_content" TEXT,
    "email_annex" BLOB
);
