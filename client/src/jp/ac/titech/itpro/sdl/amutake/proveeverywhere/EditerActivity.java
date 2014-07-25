package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import java.sql.Timestamp;

import org.json.JSONException;
import org.json.JSONObject;

import com.android.volley.Response.Listener;

import android.app.ActionBar;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.ContentValues;
import android.content.Intent;
import android.content.SharedPreferences;
import android.database.sqlite.SQLiteDatabase;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

public class EditerActivity extends Activity {

	private long codeId; // if this is -1, it means new code.
	private String codeName;
	private String codeContent;

	private int coqtopId;
	private CoqtopState coqtopState;
	private CoqtopClient client;

	private EditText nameArea;
	private EditCoqCode codeArea;
	private TextView proofStateArea;
	private TextView infoArea;
	private Button nextButton;
	private Button backButton;
	private Button gotoButton;
	private Button restartButton;

	private SQLiteDatabase db;

	private final static String CODE_KEY = "CODE_KEY";
	private final static String PROOF_STATE_KEY = "PROOF_STATE_KEY";
	private final static String INFO_KEY = "INFO_KEY";
	private final static String COQTOP_ID_KEY = "COQTOP_ID_KEY";

	// debug
	private final static String TAG = "editer activity";

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_editer);

		Intent i = getIntent();
		codeId = i.getLongExtra(Strings.codeId, -1);
		codeName = i.getStringExtra(Strings.codeName);
		codeContent = i.getStringExtra(Strings.codeContent);

		CoqCodeDbOpenHelper helper = new CoqCodeDbOpenHelper(this);
		db = helper.getWritableDatabase();

		View codeAreaWrapper = findViewById(R.id.code_area_wrapper);
		View infoAreaWrapper = findViewById(R.id.info_area_wrapper);
		nameArea = (EditText) codeAreaWrapper.findViewById(R.id.name_area);
		codeArea = (EditCoqCode) codeAreaWrapper.findViewById(R.id.code_area);
		proofStateArea = (TextView) infoAreaWrapper.findViewById(R.id.proof_state_area);
		infoArea = (TextView) infoAreaWrapper.findViewById(R.id.info_area);

		nextButton = (Button) findViewById(R.id.next_button);
		backButton = (Button) findViewById(R.id.back_button);
		gotoButton = (Button) findViewById(R.id.goto_button);
		restartButton = (Button) findViewById(R.id.restart_button);

		SharedPreferences pref = PreferenceManager.getDefaultSharedPreferences(this);
		client = new CoqtopClient(
				pref.getString(Strings.hostnameKey, ""),
				Integer.parseInt(pref.getString(Strings.portKey, "0")),
				getApplicationContext());

		setListeners();

		// coqtop start
		client.startCoqtop(new Listener<InitialInfo>() {
			@Override
			public void onResponse(InitialInfo info) {
				coqtopId = info.getId();
				coqtopState = info.getState();
				proofStateArea.setText("");
				infoArea.setText(info.getOutput());
			}
		});

		if (savedInstanceState == null) {
			nameArea.setText(codeName);
			codeArea.setText(codeContent);
		} else {
			codeArea.setText(savedInstanceState.getString(CODE_KEY));
			proofStateArea.setText(savedInstanceState.getString(PROOF_STATE_KEY));
			infoArea.setText(savedInstanceState.getString(INFO_KEY));
		}

		ActionBar actionBar = getActionBar();
		actionBar.setHomeButtonEnabled(true);
		actionBar.setDisplayHomeAsUpEnabled(true);
	}

	@Override
	public void onDestroy() {
		super.onDestroy();
		client.terminateCoqtop(coqtopId, new Listener<JSONObject>() {
			@Override
			public void onResponse(JSONObject json) {
				codeArea.reset();
				Log.d(TAG, "terminate coqtop");
			}
		});
	}

	@Override
	public boolean onMenuItemSelected(int featureId, MenuItem item) {
		int id = item.getItemId();
		switch (id) {
		case android.R.id.home:
			updateCoqCode();
			finish();
			return true;
		default:
			return super.onMenuItemSelected(featureId, item);
		}
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
	}

	private void updateCoqCode() {
		ContentValues values = new ContentValues();
		values.put(CoqCodeColumns.CODE, codeArea.getText().toString());
		values.put(CoqCodeColumns.NAME, nameArea.getText().toString());
		Long ts = System.currentTimeMillis();
		if (codeId == -1) {
			values.put(CoqCodeColumns.CREATED_AT, new Timestamp(ts).toString());
			values.put(CoqCodeColumns.LAST_MODIFIED_AT, new Timestamp(ts).toString());
			db.insert(CoqCodeColumns.TBNAME, "", values);
		} else {
			values.put(CoqCodeColumns.LAST_MODIFIED_AT, new Timestamp(ts).toString());
			String whereClause = CoqCodeColumns._ID + " = ?";
			String whereArgs[] = new String[1];
			whereArgs[0] = Long.toString(codeId);
			db.update(CoqCodeColumns.TBNAME, values, whereClause, whereArgs);
		}
	}

	// Refactor this
	private void setListeners() {
		nextButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				String command = codeArea.getNextCommand();
				codeArea.evaluating(command.length());

				Log.d("command", command);

				client.commandCoqtop(coqtopId, command, new Listener<OutputDetail>() {
					@Override
					public void onResponse(OutputDetail output) {
						coqtopState = output.getState();
						Output lastOutput = output.getLastOutput();
						if (lastOutput != null) {
							switch (lastOutput.getType()) {
							case PROOF:
								proofStateArea.setText(lastOutput.getOutput());
								infoArea.setText("");
								break;
							case INFO:
								infoArea.setText(lastOutput.getOutput());
								break;
							default:
								Log.d("CommandButton", "unexpected response (lastOutput.getType() == error)");
								break;
							}
						}
						Output errorOutput = output.getErrorOutput();
						if (errorOutput != null) {
							infoArea.setText(errorOutput.getOutput());
							codeArea.rollback();
						} else {
							codeArea.commit();
						}
					}
				}, new Listener<JSONException>() {
					@Override
					public void onResponse(JSONException e) {
						AlertDialog.Builder alertDialog = new AlertDialog.Builder(getApplicationContext());
						alertDialog.setTitle("unexpected error");
						alertDialog.setMessage(e.toString());
						alertDialog.create().show();
						codeArea.rollback();
					}
				});
			}
		});



		backButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				int offset = codeArea.getBackOffset();
				codeArea.evaluating(-offset);
				client.commandCoqtop(coqtopId, "Back 1.", new Listener<OutputDetail>() {
					@Override
					public void onResponse(OutputDetail output) {
						CoqtopState oldState = coqtopState;
						coqtopState = output.getState();
						Output lastOutput = output.getLastOutput();
						if (lastOutput != null) {
							switch (lastOutput.getType()) {
							case PROOF:
								proofStateArea.setText(lastOutput.getOutput());
								infoArea.setText("");
								break;
							case INFO:
								infoArea.setText(lastOutput.getOutput());
								break;
							default:
								Log.d("BackButton", "unexpected response");
								break;
							}
						}
						Output errorOutput = output.getErrorOutput();
						if (errorOutput != null) {
							infoArea.setText(errorOutput.getOutput());
							codeArea.rollback();
						} else {
							int d = oldState.getWholeStateNumber() - coqtopState.getWholeStateNumber();
							if (d == 1) {
								codeArea.commit();
							} else {
								int offset = codeArea.getMultiBackOffset(d);
								codeArea.evaluating(-offset);
								codeArea.commit();
							}
						}
					}
				}, new Listener<JSONException>() {
					@Override
					public void onResponse(JSONException e) {
						AlertDialog.Builder alertDialog = new AlertDialog.Builder(getApplicationContext());
						alertDialog.setTitle("unexpected error");
						alertDialog.setMessage(e.toString());
						alertDialog.create().show();
						codeArea.rollback();
					}
				});
			}
		});

		gotoButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				String command = codeArea.gotoWithEvaluating();

				Log.d("command", command);

				client.commandCoqtop(coqtopId, command, new Listener<OutputDetail>() {
					@Override
					public void onResponse(OutputDetail output) {
						coqtopState = output.getState();
						Output lastOutput = output.getLastOutput();
						if (lastOutput != null) {
							switch (lastOutput.getType()) {
							case PROOF:
								proofStateArea.setText(lastOutput.getOutput());
								infoArea.setText("");
								break;
							case INFO:
								infoArea.setText(lastOutput.getOutput());
								break;
							default:
								Log.d("CommandButton", "unexpected response (lastOutput.getType() == error)");
								break;
							}
						}
						Output errorOutput = output.getErrorOutput();
						if (errorOutput != null) {
							infoArea.setText(errorOutput.getOutput());
							codeArea.rollback();
						} else {
							codeArea.commit();
						}
					}
				}, new Listener<JSONException>() {
					@Override
					public void onResponse(JSONException e) {
						AlertDialog.Builder alertDialog = new AlertDialog.Builder(getApplicationContext());
						alertDialog.setTitle("unexpected error");
						alertDialog.setMessage(e.toString());
						alertDialog.create().show();
						codeArea.rollback();
					}
				});
			}
		});


		restartButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				codeArea.evaluating(-codeArea.getEvaluatedOffset());

				client.terminateCoqtop(coqtopId, new Listener<JSONObject>() {
					@Override
					public void onResponse(JSONObject json) {
						Log.i(TAG, "terminate coqtop");
					}
				});

				client.startCoqtop(new Listener<InitialInfo>() {
					@Override
					public void onResponse(InitialInfo info) {
						coqtopId = info.getId();
						coqtopState = info.getState();
						proofStateArea.setText("");
						infoArea.setText(info.getOutput());
						codeArea.commit();
					}
				});

			}
		});
	}
}