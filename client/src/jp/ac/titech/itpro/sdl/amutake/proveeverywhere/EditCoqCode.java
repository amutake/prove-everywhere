package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.Color;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.BackgroundColorSpan;
import android.util.AttributeSet;
import android.util.Log;
import android.widget.EditText;

public class EditCoqCode extends EditText {
	private int evaluatedOffset = 0;
	private int evaluatingOffset = 0;

	private final static int EvaluatedColor = Color.argb(200, 100, 200, 200);
	private final static int EvaluatingColor = Color.argb(200, 150, 150, 200);

	public EditCoqCode(Context context) {
		super(context);
	}
	public EditCoqCode(Context context, AttributeSet attrs) {
		super(context, attrs);
	}
	public EditCoqCode(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public int getNumOfCommandsFromOffset(int offset) {
		int n = 0;
		int length = 0;
		ArrayList<String> commands = getAllCommands();
		for (String command : commands) {
			int len = command.length();
			if (length >= offset) {
				return n;
			} else {
				length += len;
				n++;
			}
		}
		return n;
	}

	public String getNextCommand() {
		ArrayList<String> commands = getAllCommands();
		int offset = evaluatedOffset;
		for (String command : commands) {
			int len = command.length();
			if (offset < len) {
				return command;
			}
			offset -= len;
		}
		return null;
	}

	public int getBackOffset() {
		ArrayList<String> commands = getAllCommands();
		int offset = evaluatedOffset;
		for (String command : commands) {
			int len = command.length();
			if (offset <= len) {
				return len;
			}
			offset -= len;
		}
		return 0;
	}

	public int getMultiBackOffset(int n) {
		ArrayList<String> commands = getAllCommands();
		int evaluated = getNumOfEvaluatedCommands();
		List<String> subCommands = commands.subList(evaluated - n, evaluated);
		return concat(subCommands).length();
	}

	public int getNumOfEvaluatedCommands() {
		return getNumOfCommandsFromOffset(evaluatedOffset);
	}

	private static String concat(List<String> list) {
		String str = "";
		for (String elem : list) {
			str += elem;
		}
		return str;
	}

	public String gotoWithEvaluating() {
		int nEvaluated = getNumOfEvaluatedCommands();
		int cursor = getSelectionStart();
		int nEvaluating = getNumOfCommandsFromOffset(cursor);
		ArrayList<String> commands = getAllCommands();
		if (nEvaluating < nEvaluated) {
			int offset = concat(commands.subList(nEvaluating, nEvaluated)).length();
			evaluating(-offset);
			return "Back " + (nEvaluated - nEvaluating) + ".";
		} else {
			List<String> subCommands = commands.subList(nEvaluated, nEvaluating);
			evaluating(concat(subCommands).length());
			return concat(subCommands);
		}
	}

	public ArrayList<String> getCommandsUntilCursor() {
		int cursor = getSelectionStart();
		if (cursor <= evaluatedOffset) {
			return new ArrayList<String>();
		}
		int offset = cursor - evaluatedOffset;
		ArrayList<String> restCommands = getRestCommands();
		ArrayList<String> commands = new ArrayList<String>();
		for (String command : restCommands) {
			int len = command.length();
			if (offset < len) {
				return commands;
			}
			commands.add(command);
			offset -= len;
		}
		return new ArrayList<String>();
	}

	private ArrayList<String> getRestCommands() {
		ArrayList<String> commands = getAllCommands();
		int offset = evaluatedOffset;
		for (;;) {
			if (commands.isEmpty()) {
				return new ArrayList<String>();
			}
			String command = commands.get(0);
			int len = command.length();
			if (offset < len) {
				return commands;
			}
			commands.remove(0);
		}
	}

	private ArrayList<String> getAllCommands() {
		String spacedText = getText().toString().replaceAll("\n", " ");
		ArrayList<String> list = new ArrayList<String>();
		for (;;) {
			int n = spacedText.indexOf(". ");
			if (n < 0) {
				list.add(spacedText);
				break;
			} else {
				list.add(spacedText.substring(0, n + 1));
				spacedText = spacedText.substring(n + 1);
			}
		}
		return list;
	}

	public int getEvaluatedOffset() {
		return evaluatedOffset;
	}
	public int getEvaluatingOffset() {
		return evaluatingOffset;
	}

	public void setEvaluatedOffset(int offset) {
		evaluatedOffset = offset;
	}
	public void setEvaluatingOffset(int offset) {
		evaluatingOffset = offset;
	}

	public void commit() {
		SpannableString span = new SpannableString(getText().toString());
		span.setSpan(new BackgroundColorSpan(EvaluatedColor), 0, evaluatingOffset, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
		setText(span);
		evaluatedOffset = evaluatingOffset;
		setSelection(evaluatedOffset);
	}

	public void rollback() {
		SpannableString span = new SpannableString(getText().toString());
		span.setSpan(new BackgroundColorSpan(EvaluatedColor), 0, evaluatedOffset, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
		setText(span);
		evaluatingOffset = evaluatedOffset;
		setSelection(evaluatedOffset);
	}

	public void evaluating(int offset) {
		evaluatingOffset = evaluatedOffset + offset;
		Log.d("evaluatedOffset", evaluatedOffset + "");
		Log.d("evaluatingOffset", evaluatingOffset + "");
		SpannableString span = new SpannableString(getText().toString());
		if (evaluatedOffset < evaluatingOffset) {
			span.setSpan(new BackgroundColorSpan(EvaluatedColor), 0, evaluatedOffset, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
			span.setSpan(new BackgroundColorSpan(EvaluatingColor), evaluatedOffset, evaluatingOffset, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
		} else {
			span.setSpan(new BackgroundColorSpan(EvaluatedColor), 0, evaluatingOffset, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
			span.setSpan(new BackgroundColorSpan(EvaluatingColor), evaluatingOffset, evaluatedOffset, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
		}
		setText(span);
	}

	public void reset() {
		setEvaluatingOffset(0);
		commit();
	}
}
