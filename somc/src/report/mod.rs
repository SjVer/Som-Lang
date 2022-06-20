mod report;
pub mod ecode;
pub mod wcode;
pub mod lint;

pub use {
	report::Report,
	ecode::ErrorCode,
	wcode::WarningCode
};

use report::Severity;
use crate::info::report::*;
use yansi::Color;

pub trait ReportableCode {
	fn get_name(&self) -> String;
	fn to_string(&self) -> String;
	fn is_useful(&self) -> bool;
	fn must_hide(&self) -> bool;
}

pub fn error(message: impl ToString, code: Option<ecode::ErrorCode>) -> Report {
	let label;
	
	if matches!(code, Some(_)) && matches!(code.as_ref().unwrap().get_type(), Some(_)) {
		label = String::from(code.as_ref().unwrap().get_type().unwrap());
	} else {
		label = String::from(ERROR_LABEL);
	}

	Report{
		label,
		message: message.to_string(),

		color: Color::Red,
		severity: Severity::Error,
		code: if let Some(c) = code { Some(Box::from(c)) } else { None },

		quote: None,
		sub_quotes: vec![],
		notes: vec![],
	}
}

#[macro_export]
macro_rules! new_formatted_error {
	($code:ident $($arg:tt)*) => {
		crate::report::error(
			crate::fmt_error_msg!($code $($arg)*),
			Some(crate::report::ecode::ErrorCode::$code)
		)
	};
}

pub fn warning(message: impl ToString, code: Option<wcode::WarningCode>) -> Report {
	Report{
		label: String::from(WARNING_LABEL),
		message: message.to_string(),

		color: Color::Yellow,
		severity: Severity::Warning,
		code: if let Some(c) = code { Some(Box::from(c)) } else { None },

		quote: None,
		sub_quotes: vec![],
		notes: vec![],
	}
}

#[macro_export]
macro_rules! new_formatted_warning {
	($code:ident $($arg:tt)*) => {
		crate::report::warning(
			crate::fmt_warning_msg!($code $($arg)*),
			Some(crate::report::wcode::WarningCode::$code)
		)
	};
}