package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

public class CoqtopState {
	private String currentTheorem;
	private int wholeStateNumber;
	private final ArrayList<String> theoremStack = new ArrayList<String>();
	private int theoremStateNumber;

	public CoqtopState(JSONObject json) {
		try {
			currentTheorem = json.getString("current_theorem");
			wholeStateNumber = json.getInt("whole_state_number");
			JSONArray arr = json.getJSONArray("theorem_stack");
			Log.d("arr", arr.toString());
			Log.d("arr.length", arr.length() + "");
			for (int i = 0; i < arr.length(); i++) {
				theoremStack.add(arr.getString(i));
			}
			theoremStateNumber = json.getInt("theorem_state_number");
		} catch(JSONException e) {
			Log.d("CoqtopState", e.toString());
		}
	}

	public String getCurrentTheorem() {
		return currentTheorem;
	}

	public int getWholeStateNumber() {
		return wholeStateNumber;
	}

	public ArrayList<String> getTheoremStack() {
		return theoremStack;
	}

	public int getTheoremStateNumber() {
		return theoremStateNumber;
	}
}
