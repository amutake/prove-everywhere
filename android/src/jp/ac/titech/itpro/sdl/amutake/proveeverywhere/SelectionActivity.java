package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import java.util.ArrayList;

import android.app.Activity;
import android.app.ActionBar;
import android.app.AlertDialog;
import android.app.Fragment;
import android.app.AlertDialog.Builder;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.os.Build;
import android.preference.PreferenceFragment;

public class SelectionActivity extends Activity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		Log.d("SelectionActivity", "onCreate");
		setContentView(R.layout.activity_selection);
	}

	@Override
	public void onResume() {
		super.onResume();
		Log.d("SelectionActivity", "onResume");
		CoqCodeDbOpenHelper helper = new CoqCodeDbOpenHelper(this);
		final SQLiteDatabase db = helper.getWritableDatabase();
		Cursor cursor = db.query(CoqCodeColumns.TBNAME, null, null, null, null, null, CoqCodeColumns.LAST_MODIFIED_AT + " DESC");
		ArrayList<CoqCode> codeList = new ArrayList<CoqCode>();
		if (cursor != null) {
			while (cursor.moveToNext()) {
				long id = cursor.getLong(cursor.getColumnIndex(CoqCodeColumns._ID));
				String name = cursor.getString(cursor.getColumnIndex(CoqCodeColumns.NAME));
				String code = cursor.getString(cursor.getColumnIndex(CoqCodeColumns.CODE));
				codeList.add(new CoqCode(id, name, code));
			}
		}

		ListView codeListView = (ListView) findViewById(R.id.code_list);
		final ArrayAdapter<CoqCode> adapter = new ArrayAdapter<CoqCode>(this, android.R.layout.simple_list_item_1, codeList);
		codeListView.setAdapter(adapter);
		codeListView.setOnItemClickListener(new OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				CoqCode selected = adapter.getItem(position);
				Intent intent = new Intent(getApplicationContext(), EditerActivity.class);
				intent.putExtra(Strings.codeId, selected.getId());
				intent.putExtra(Strings.codeName, selected.getName());
				intent.putExtra(Strings.codeContent, selected.getCode());
				startActivity(intent);
			}
		});
		codeListView.setOnItemLongClickListener(new OnItemLongClickListener() {
			@Override
			public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
				final CoqCode selected = adapter.getItem(position);
				AlertDialog.Builder adb = new AlertDialog.Builder(SelectionActivity.this);
				adb.setTitle("Delete?");
				adb.setMessage("Are you sure you want to delete '" + selected.getName() + "'?");
				adb.setNegativeButton("Cancel", new AlertDialog.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
					}
				});
				adb.setPositiveButton("OK", new AlertDialog.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						String whereClause = CoqCodeColumns._ID + " = ?";
						String whereArgs[] = new String[1];
						whereArgs[0] = Long.toString(selected.getId());
						db.delete(CoqCodeColumns.TBNAME, whereClause, whereArgs);
						adapter.remove(selected);
						adapter.notifyDataSetChanged();
					}
				});
				adb.create().show();
				return true;
			}
		});
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {

		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.selection, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		// Handle action bar item clicks here. The action bar will
		// automatically handle clicks on the Home/Up button, so long
		// as you specify a parent activity in AndroidManifest.xml.
		int id = item.getItemId();
		switch (id) {
		case R.id.action_settings:
			startActivity(new Intent(this, CoqPreferenceActivity.class));
			return true;
		case R.id.new_code:
			startActivity(new Intent(this, EditerActivity.class));
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}
}
