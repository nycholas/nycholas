package org.helllabs.android.examples;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;

public class NotebookDatabaseAdapter {

	private static final String TAG = "NotebookDatabaseAdapter";

	private static final int DATABASE_VERSION = 2;
	private static final String DATABASE_NAME = "e.db";
	private static final String NOTEBOOK_TABLE_NAME = "notebook";

	private DatabaseHelper mDbHelper;
	private SQLiteDatabase mDb;

	private final Context mCtx;
	
	public static final String KEY_ID = "_id";
	public static final String KEY_TITLE = "title";
	public static final String KEY_DESCRIPTION = "description";
	public static final String KEY_CREATED = "created";
	public static final String KEY_MODIFIED = "modified";
	public static final String KEY_STATUS = "status";

	private static class DatabaseHelper extends SQLiteOpenHelper {

		private static final String TAG = "DatabaseHelper";

		public DatabaseHelper(Context context) {
			super(context, DATABASE_NAME, null, DATABASE_VERSION);
		}

		@Override
		public void onCreate(SQLiteDatabase db) {
			Log.d(TAG, "In onCreate()");
			db.execSQL("CREATE TABLE " + NOTEBOOK_TABLE_NAME + " (" + KEY_ID
					+ " INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, "
					+ KEY_TITLE + " TEXT NOT NULL, " + KEY_DESCRIPTION
					+ " TEXT, " + KEY_CREATED + " INTEGER NOT NULL, "
					+ KEY_MODIFIED + " INTEGER, " + KEY_STATUS
					+ " INTEGER NOT NULL DEFAULT (1)" + ");");
		}

		@Override
		public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
			Log.d(TAG, "In onUpgrade()");
			Log.w(TAG, "Upgrading database from version " + oldVersion + " to "
					+ newVersion + ", which will destroy all old data");
			db.execSQL("DROP TABLE IF EXISTS " + NOTEBOOK_TABLE_NAME);
			onCreate(db);
		}
	}

	public NotebookDatabaseAdapter(Context ctx) {
		this.mCtx = ctx;
	}

	public NotebookDatabaseAdapter open() throws SQLException {
		mDbHelper = new DatabaseHelper(mCtx);
		mDb = mDbHelper.getWritableDatabase();
		return this;
	}

	public void close() {
		mDbHelper.close();
	}

	public Cursor allNotebook() {
		return mDb.query(NOTEBOOK_TABLE_NAME, new String[] { KEY_ID, KEY_TITLE,
				KEY_DESCRIPTION, KEY_CREATED, KEY_MODIFIED, KEY_STATUS }, null,
				null, null, null, null);
	}

	public Cursor getByIdNotebook(long id) {
		Cursor mCursor = mDb.query(true, NOTEBOOK_TABLE_NAME, new String[] {
				KEY_ID, KEY_TITLE, KEY_DESCRIPTION, KEY_CREATED, KEY_MODIFIED,
				KEY_STATUS }, KEY_ID + "=" + id, null, null, null, null, null);
		if (mCursor != null) {
			mCursor.moveToFirst();
		}
		return mCursor;
	}

	public long createNotebook(String title, String description, boolean status) {
		ContentValues values = new ContentValues();
		values.put(KEY_TITLE, title);
		values.put(KEY_DESCRIPTION, description);
		values.put(KEY_CREATED, System.currentTimeMillis());
		values.put(KEY_STATUS, (status ? 1 : 0));

		return mDb.insert(NOTEBOOK_TABLE_NAME, null, values);
	}

	public boolean updateNotebook(long id, String title, String description,
			long created, boolean status) {
		ContentValues values = new ContentValues();
		values.put(KEY_TITLE, title);
		values.put(KEY_DESCRIPTION, description);
		values.put(KEY_CREATED, created);
		values.put(KEY_MODIFIED, System.currentTimeMillis());
		values.put(KEY_STATUS, (status ? 1 : 0));

		return mDb.update(NOTEBOOK_TABLE_NAME, values, KEY_ID + "=" + id, null) > 0;
	}

	public boolean deleteNotebook(long id) {
		return mDb.delete(NOTEBOOK_TABLE_NAME, KEY_ID + "=" + id, null) > 0;
	}
}