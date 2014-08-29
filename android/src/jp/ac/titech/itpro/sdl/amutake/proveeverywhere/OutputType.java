package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

public enum OutputType {
	PROOF, INFO, ERROR;

	public static OutputType getValue(String str) {
		switch (str) {
		case "proof":
			return PROOF;
		case "info":
			return INFO;
		case "error":
			return ERROR;
		default:
			return null;
		}
	}
}
