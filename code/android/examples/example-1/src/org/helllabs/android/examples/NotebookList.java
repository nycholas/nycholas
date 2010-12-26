package org.helllabs.android.examples;

import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.ListView;
import android.widget.SimpleCursorAdapter;

public class NotebookList extends ListActivity {

	private static final int ACTIVITY_CREATE = 0;
	private static final int ACTIVITY_EDIT = 1;

	private static final int INSERT_ID = Menu.FIRST;
	private static final int DELETE_ID = Menu.FIRST + 1;

	private NotebookDatabaseAdapter mDbHelper;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mDbHelper = new NotebookDatabaseAdapter(this);
		mDbHelper.open();
		setContentView(R.layout.notebook_list);
		
		updateNotebookList();
		
		registerForContextMenu(getListView());
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);
		menu.add(0, INSERT_ID, 0, R.string.new_notebook);
		return true;
	}

	@Override
	public boolean onMenuItemSelected(int featureId, MenuItem item) {
		switch (item.getItemId()) {
		case INSERT_ID:
			createNotebook();
			return true;
		}

		return super.onMenuItemSelected(featureId, item);
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v,
			ContextMenuInfo menuInfo) {
		super.onCreateContextMenu(menu, v, menuInfo);
		menu.add(0, DELETE_ID, 0, R.string.delete_notebook);
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case DELETE_ID:
			AdapterContextMenuInfo info = (AdapterContextMenuInfo) item
					.getMenuInfo();
			mDbHelper.deleteNotebook(info.id);
			updateNotebookList();
			return true;
		}
		return super.onContextItemSelected(item);
	}

	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		super.onListItemClick(l, v, position, id);
		Intent i = new Intent(this, NotebookEdit.class);
		i.putExtra(NotebookDatabaseAdapter.KEY_ID, id);
		startActivityForResult(i, ACTIVITY_EDIT);
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode,
			Intent intent) {
		super.onActivityResult(requestCode, resultCode, intent);
		updateNotebookList();
	}

	private void updateNotebookList() {
		Cursor notebooksCursor = mDbHelper.allNotebook();
		startManagingCursor(notebooksCursor);

		String[] from = new String[] { NotebookDatabaseAdapter.KEY_TITLE,
				NotebookDatabaseAdapter.KEY_DESCRIPTION };

		int[] to = new int[] { R.id.textViewTitle, R.id.textViewDescription };

		// Now create a simple cursor adapter and set it to display
		SimpleCursorAdapter notebooks = new SimpleCursorAdapter(this,
				R.layout.notebook_list_row, notebooksCursor, from, to);
		setListAdapter(notebooks);
	}

	private void createNotebook() {
		Intent i = new Intent(this, NotebookEdit.class);
		startActivityForResult(i, ACTIVITY_CREATE);
	}
}
