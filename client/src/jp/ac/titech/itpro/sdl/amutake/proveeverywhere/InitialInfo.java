package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

public class InitialInfo {
	private int id;
	private String output;
	private CoqtopState state;

	public InitialInfo(JSONObject json) {
		try {
			id = json.getInt("id");
			output = json.getString("output");
			state = new CoqtopState(json.getJSONObject("state"));
		} catch(JSONException e) {
			Log.d("InitialInfo", e.toString());
		}
	}

	public int getId() {
		return id;
	}

	public String getOutput() {
		return output;
	}

	public CoqtopState getState() {
		return state;
	}
}
