package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

public class CoqCodeDbOpenHelper extends SQLiteOpenHelper {

	static final String DATABASE_NAME = "coqcode.db";
	static final int DATABASE_VERSION = 1;

	public CoqCodeDbOpenHelper(Context context) {
		super(context, DATABASE_NAME, null, DATABASE_VERSION);
	}

	@Override
	public void onCreate(SQLiteDatabase db) {
		db.execSQL("CREATE TABLE "
				+ CoqCodeColumns.TBNAME + "("
				+ CoqCodeColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT, "
				+ CoqCodeColumns.NAME + " TEXT NOT NULL, "
				+ CoqCodeColumns.CODE + " TEXT NOT NULL, "
				+ CoqCodeColumns.CREATED_AT + " TIMESTAMP DEFAULT CURRENT_TIMESTAMP, "
				+ CoqCodeColumns.LAST_MODIFIED_AT + " TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
				+ ");");
	}

	@Override
	public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
		db.execSQL("DROP TABLE IF EXISTS " + CoqCodeColumns.TBNAME);
		onCreate(db);
	}

}
