//! This module contains information (such as text) to be used by the rest of the compiler.

#![allow(dead_code)]

macro_rules! static_string {
	($name:ident, $str:expr) => {pub static $name: &str = $str;}
}

pub mod app {
	static_string!(NAME, "somc");
	static_string!(FULL_NAME, "SOMC (official)");
	static_string!(VERSION, "0.1.0");
}

pub mod cli {
	static_string!(DESCRIPTION, "The Official Som Compiler");
	static_string!(ARG_INFILE, "The file to compile");
	static_string!(ARG_MUTE, "Mute all warnings");
	static_string!(ARG_COMPACT, "Produce compact output");
	static_string!(ARG_QUIET, "Hide all output");
	static_string!(ARG_EXPLAIN, "Explain the given error code");

	pub const LINT_NONE_NAME: &str = "none";
	pub const LINT_DIAG_NAME: &str = "diag";
}

pub mod report {
	pub const ECODE_PREFIX: char = 'E';
	pub const WCODE_PREFIX: char = 'W';

	static_string!(ERROR_LABEL, "error");
	static_string!(WARNING_LABEL, "warning");
	static_string!(NOTE_LABEL, "note");
}