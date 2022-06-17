use num_enum::TryFromPrimitive;
use convert_case::{Case, Casing};
use crate::{info::report::WCODE_PREFIX, get_cli_arg};

#[repr(i16)]
#[derive(Debug, Clone, PartialEq, TryFromPrimitive)]
pub enum WarningCode {
	NoWarning = 0,
	RedefenitionOf,
	ShadowingApplication,
}

impl super::ReportableCode for WarningCode {
	fn get_name(&self) -> String {
		format!("{:?}", self).to_case(Case::Lower)
	}

	fn to_string(&self) -> String {
		format!("{}{:#03}", WCODE_PREFIX, self.clone() as u16)
	}

	fn is_useful(&self) -> bool { true /* !get_cli_arg!(mute) */ }

	fn must_hide(&self) -> bool {
		get_cli_arg!(quiet) || get_cli_arg!(mute)
	}
}

#[macro_export]
macro_rules! fmt_warning_msg {
	(NoWarning) => ("there is no warning, why did this appear?");
	(RedefenitionOf $what:tt $name:expr) => (format!("redefenition of {} '{}'", $what, $name));
	(ShadowingApplication $name:expr) => (format!("application of section '{}' shadows previous application", $name));
}