package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

public class EditerActivity extends Activity {

	private int codeId; // if this is -1, it means new code.
	private String codeName;
	private String codeContent;

	private int coqtopId;
	private CoqtopState coqtopState;
	private CoqtopClient client;
	private EditCoqCode codeArea;
	private TextView proofStateArea;
	private TextView infoArea;
	private Button nextButton;
	private Button backButton;
	private Button gotoButton;
	private Button restartButton;

	private final static String CODE_KEY = "CODE_KEY";
	private final static String PROOF_STATE_KEY = "PROOF_STATE_KEY";
	private final static String INFO_KEY = "INFO_KEY";
	private final static String COQTOP_ID_KEY = "COQTOP_ID_KEY";

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_editer);

		Intent i = getIntent();
		codeId = i.getIntExtra(Strings.codeId, -1);
		codeName = i.getStringExtra(Strings.codeName);
		codeContent = i.getStringExtra(Strings.codeContent);

		View codeAreaWrapper = findViewById(R.id.code_area_wrapper);
		View infoAreaWrapper = findViewById(R.id.info_area_wrapper);
		codeArea = (EditCoqCode) codeAreaWrapper.findViewById(R.id.code_area);
		proofStateArea = (TextView) infoAreaWrapper.findViewById(R.id.proof_state_area);
		infoArea = (TextView) infoAreaWrapper.findViewById(R.id.info_area);

		nextButton = (Button) findViewById(R.id.next_button);
		backButton = (Button) findViewById(R.id.back_button);
		gotoButton = (Button) findViewById(R.id.goto_button);
		restartButton = (Button) findViewById(R.id.restart_button);

		SharedPreferences pref = getSharedPreferences("pref", Context.MODE_PRIVATE);
		client = new CoqtopClient(pref.getString(Strings.hostnameKey, ""), pref.getInt(Strings.portKey, 0), getApplicationContext());


	}
}
