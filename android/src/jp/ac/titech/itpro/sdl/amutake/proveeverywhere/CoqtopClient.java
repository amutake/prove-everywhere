package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import org.json.JSONException;
import org.json.JSONObject;

import android.content.Context;
import android.util.Log;

import com.android.volley.NetworkResponse;
import com.android.volley.Request.Method;
import com.android.volley.RequestQueue;
import com.android.volley.Response.ErrorListener;
import com.android.volley.Response.Listener;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;
import com.android.volley.toolbox.Volley;

public class CoqtopClient {
	private final String hostname;
	private final int port;
	private final RequestQueue queue;

	public CoqtopClient(String hostname, int port, Context context) {
		this.hostname = hostname;
		this.port = port;
		queue = Volley.newRequestQueue(context);
	}

	private void generalApi(String name, int method, JSONObject json, Listener<JSONObject> listener) {
		String url = "http://" + hostname + ":" + port + "/" + name;
		JsonObjectRequest request = new JsonObjectRequest(method, url, json, listener, errorListener);
		queue.add(request);
	}

	// FIXME
	private final ErrorListener errorListener = new ErrorListener() {
		@Override
		public void onErrorResponse(VolleyError err) {
			NetworkResponse response = err.networkResponse;
			if (response != null && response.data != null) {
				try {
					JSONObject json = new JSONObject(new String(response.data));
					CoqtopError coqtopError = new CoqtopError(json);
					Log.d("ErrorResponse", coqtopError.toString());
				} catch(JSONException e) {
					Log.d("NOT JSON", e.toString());
				}

			} else {
				String message = String.format("connection error: %1$s",  err.getMessage());
				Log.d("volley error", message);
			}
		}
	};

	public void startCoqtop(final Listener<InitialInfo> listener) {
		Listener<JSONObject> JSONListener = new Listener<JSONObject>() {
			@Override
			public void onResponse(JSONObject response) {
				listener.onResponse(new InitialInfo(response));
			}
		};
		generalApi("start", Method.POST, null, JSONListener);
	}

	public void commandCoqtop(int id, String input, final Listener<OutputDetail> listener, final Listener<JSONException> errorListener) {
		Listener<JSONObject> JSONListener = new Listener<JSONObject>() {
			@Override
			public void onResponse(JSONObject response) {
				try {
					listener.onResponse(new OutputDetail(response));
				} catch (JSONException e) {
					errorListener.onResponse(e);
				}
			}
		};
		Command command = new Command(input);
		generalApi("command/" + id, Method.POST, command.toJSON(), JSONListener);
	}

	public void terminateCoqtop(int id, final Listener<JSONObject> listener) {
		generalApi("terminate/" + id, Method.DELETE, null, listener);
	}
}

