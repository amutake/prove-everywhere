package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

public class Output {
	private OutputType type;
	private String output;

	public Output(JSONObject json) {
		try {
			type = OutputType.getValue(json.getString("type"));
			output = json.getString("output");
		} catch(JSONException e) {
			Log.d("Output", e.toString());
		}
	}

	public OutputType getType() {
		return type;
	}

	public String getOutput() {
		return output;
	}
}
