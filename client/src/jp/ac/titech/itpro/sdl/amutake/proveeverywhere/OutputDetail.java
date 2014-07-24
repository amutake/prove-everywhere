package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

public class OutputDetail {
	private final int id;
	private final int succeeded;
	private final int remaining;
	private final Output lastOutput;
	private final Output errorOutput;
	private final CoqtopState state;

	public OutputDetail(JSONObject json) throws JSONException {
		id = json.getInt("id");
		succeeded = json.getInt("succeeded");
		remaining = json.getInt("remaining");
		if (json.isNull("last_output")) {
			lastOutput = null;
		} else {
			lastOutput = new Output(json.getJSONObject("last_output"));
		}
		if (json.isNull("error_output")) {
			errorOutput = null;
		} else {
			errorOutput = new Output(json.getJSONObject("error_output"));
		}
		state = new CoqtopState(json.getJSONObject("state"));
	}

	public int getId() {
		return id;
	}

	public int getSucceeded() {
		return succeeded;
	}

	public int getRemaining() {
		return remaining;
	}

	public Output getLastOutput() {
		return lastOutput;
	}

	public Output getErrorOutput() {
		return errorOutput;
	}

	public CoqtopState getState() {
		return state;
	}
}
