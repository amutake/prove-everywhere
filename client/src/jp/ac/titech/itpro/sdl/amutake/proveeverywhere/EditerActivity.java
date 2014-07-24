package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;

public class EditerActivity extends Activity {

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		Intent i = getIntent();
		String code = i.getStringExtra(Strings.codeContent);

	}
}
