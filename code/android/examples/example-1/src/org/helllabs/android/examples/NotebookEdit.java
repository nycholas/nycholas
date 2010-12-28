package org.helllabs.android.examples;

import android.app.Activity;
import android.database.Cursor;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;

public class NotebookEdit extends Activity {

	private Long id;
	private EditText editTextTitle;
	private EditText editTextDescription;
	private CheckBox checkBoxStatus;
	private NotebookDatabaseAdapter mDbHelper;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mDbHelper = new NotebookDatabaseAdapter(this);
		mDbHelper.open();
		setContentView(R.layout.notebook_edit);

		editTextTitle = (EditText) findViewById(R.id.editTextTitle);
		editTextDescription = (EditText) findViewById(R.id.editTextDescription);
		// checkBoxStatus = (CheckBox) findViewById(R.id.checkBoxStatus);

		Button buttonSave = (Button) findViewById(R.id.buttonSave);
		Button buttonCancel = (Button) findViewById(R.id.buttonCancel);

		id = savedInstanceState != null ? savedInstanceState
				.getLong(NotebookDatabaseAdapter.KEY_ID) : null;
		if (id == null) {
			Bundle extras = getIntent().getExtras();
			id = extras != null ? extras
					.getLong(NotebookDatabaseAdapter.KEY_ID) : null;
		}

		updateFields();

		buttonSave.setOnClickListener(new View.OnClickListener() {
			public void onClick(View view) {
				setResult(RESULT_OK);
				finish();
			}
		});
		buttonCancel.setOnClickListener(new View.OnClickListener() {
			public void onClick(View view) {
				setResult(RESULT_CANCELED);
				finish();
			}
		});
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putLong(NotebookDatabaseAdapter.KEY_ID, id);
	}

	@Override
	protected void onPause() {
		super.onPause();
		saveState();
	}

	@Override
	protected void onResume() {
		super.onResume();
		updateFields();
	}

	@Override
	protected void onStop() {
		super.onStop();
	}

	private void updateFields() {
		if (id != null) {
			Cursor notebookCursor = mDbHelper.getByIdNotebook(id);
			startManagingCursor(notebookCursor);
			editTextTitle.setText(notebookCursor.getString(notebookCursor
					.getColumnIndexOrThrow(NotebookDatabaseAdapter.KEY_TITLE)));
			editTextDescription
					.setText(notebookCursor.getString(notebookCursor
							.getColumnIndexOrThrow(NotebookDatabaseAdapter.KEY_DESCRIPTION)));
			// checkBoxStatus
			// .setText(notebookCursor.getString(notebookCursor
			// .getColumnIndexOrThrow(NotebookDatabaseAdapter.KEY_STATUS)));
		}
	}

	private void saveState() {
		String title = editTextTitle.getText().toString();
		String description = editTextDescription.getText().toString();
		Long created = System.currentTimeMillis();
		boolean status = true; // String status =
								// checkBoxStatus.getText().toString();

		if (id == null) {
			long new_id = mDbHelper.createNotebook(title, description, status);
			if (new_id > 0) {
				id = new_id;
			}
		} else {
			mDbHelper.updateNotebook(id, title, description, created, status);
		}
	}
}
