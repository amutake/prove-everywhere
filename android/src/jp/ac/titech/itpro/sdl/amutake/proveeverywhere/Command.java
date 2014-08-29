package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import org.json.JSONException;
import org.json.JSONObject;

public class Command {
	private final String command;
	public Command(String command) {
		this.command = command;
	}

	public JSONObject toJSON() {
		JSONObject json = null;
		try {
			json = new JSONObject().put("command", command);
		} catch (JSONException e) {
			e.printStackTrace();
		}
		return json;
	}
}
