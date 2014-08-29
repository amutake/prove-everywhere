package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

public class CoqtopError {
	private int id;
	private ErrorType type;
	private String message;

	public CoqtopError(JSONObject json) {
		try {
			JSONObject error = json.getJSONObject("error");
			id = error.getInt("id");
			type = ErrorType.valueOf(error.getString("type"));
			message = error.getString("message");
		} catch(JSONException e) {
			Log.d("CoqtopError", e.toString());
		}
	}

	public int getId() {
		return id;
	}

	public ErrorType getType() {
		return type;
	}

	public String getMessage() {
		return message;
	}
}
