package jp.ac.titech.itpro.sdl.amutake.proveeverywhere;

public class CoqCode {
	private final long id;
	private final String name;
	private final String code;

	public CoqCode(long id, String name, String code) {
		this.id = id;
		this.name = name;
		this.code = code;
	}

	public long getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getCode() {
		return code;
	}

	@Override
	public String toString() {
		return name;
	}
}
