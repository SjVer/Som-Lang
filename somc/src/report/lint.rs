static mut MAIN_LIST: json::JsonValue = json::JsonValue::Array(vec![]);

#[macro_export]
macro_rules! get_lint_mode {
    () => (crate::get_cli_arg!(lint).unwrap_or(crate::cli::LintMode::None))
}

#[macro_export]
macro_rules! lint_mode_is {
    ($mode:ident) => (crate::get_lint_mode!() == crate::LintMode::$mode)
}

pub fn append(value: json::JsonValue) {
	unsafe { MAIN_LIST.push(value).unwrap(); }
}

pub fn prepare_lint() {
	if crate::lint_mode_is!(Diag) {
		unsafe { MAIN_LIST = json::array![]; }
	}
}

pub fn finish_lint() {
	if crate::lint_mode_is!(Diag) {
		unsafe{ println!("{}", json::stringify_pretty(MAIN_LIST.clone(), 4)); }
	}
}