use num_enum::TryFromPrimitive;
use convert_case::{Case, Casing};
use crate::{info::report::ECODE_PREFIX, get_cli_arg};

#[repr(i16)]
#[derive(Debug, Clone, PartialEq, TryFromPrimitive)]
pub enum ErrorCode {
	_C = -100, // codes without code
	CouldNotOpen,
	CouldNotCompile,
	CouldNotReview,
	CannotExplainCode,
	FailedToResolve,
	FailedToApply,

	NoError = 0,

	_L = 100, // lexical-error codes
	UnexpectedChar,
	InvalidDigit,

	_S = 200, // syntax-error codes
	ExpectedToken,
	UnexpectedToken,
	ExpectedTopLevel,
	ExpectedDefinition,
	ExpectedExpression,
	UseOfUndefined,
	DuplicateParameter,
}

impl super::ReportableCode for ErrorCode {
	fn get_name(&self) -> String {
		format!("{:?}", self).to_case(Case::Lower)
	}

	fn to_string(&self) -> String {
		format!("{}{:#03}", ECODE_PREFIX, self.clone() as u16)
	}

	fn is_useful(&self) -> bool {
		self.clone() as i16 >= 0
	}

	fn must_hide(&self) -> bool {
		get_cli_arg!(quiet)		
	}
}

impl ErrorCode {
	pub fn get_type(&self) -> Option<&str> {
		let rounded = (self.clone() as i16 as f32 / 100_f32).floor() * 100_f32;
		match ErrorCode::try_from(rounded as i16) {
			Err(_) => None,
			Ok(c) => match c {
				Self::_C => Some("error"),
				Self::_L => Some("lexical error"),
				Self::_S => Some("syntax error"),
				Self::_D => Some("disassembly error"),
				_ => None
			}
		}
	}
}

#[macro_export]
macro_rules! fmt_error_msg {
	(CouldNotOpen $what:expr, $file:expr, $why:expr) => (format!("could not open {} '{}': {}", $what, $file, std::io::Error::from($why)));
	(CouldNotOpen $name:expr, $why:expr) => (format!("could not open '{}': {}", $name, std::io::Error::from($why)));
	(CouldNotCompile $file:expr) => (format!("could not compile '{}' due to previous error", $file));
	(CouldNotReview $file:expr) => (format!("could not review '{}' due to previous error", $file));
	(CannotExplainCode $code:expr) => (format!("cannot explain invalid error code {:?}", $code));
	(CannotReview $what:expr, $name:expr) => (format!("cannot review {} '{}'", $what, $name));
	(FailedToResolve $name:expr, $why:expr) => (format!("failed to resolve section '{}': {}", $name, $why));
	(FailedToApply $name:expr) => (format!("failed to apply section '{}' due to an error", $name));
	(InvalidStepNumber $step:expr, $len:expr) => (format!("invalid step number '{}' not in range [1-{}]", $step, $len));
	
	(NoError) => ("there is no error, why did this appear?");
	
	(UnexpectedChar $chr:expr) => (format!("unexpected character {:?}", $chr));
	(InvalidDigit $chr:expr, $base:expr, $t:expr) => (format!("invalid digit {:?} in {} {}", $chr, $base, $t));

	(ExpectedToken $tok:expr) => (format!("expected token `{}`", $tok));
	(UnexpectedToken $tok:expr) => (format!("unexpected token `{}`", $tok));
	(ExpectedTopLevel) => ("expected a top-level statement");
	(ExpectedDeclaration) => ("expected a declaration");
	(ExpectedTheory) => ("expected a theory");
	(ExpectedExpression) => ("expected an expression");
	(ExpectedTheoryOrExpression) => ("expected a theory or expression");
	(AlreadyDefined $type:tt $name:expr) => (format!("{} `{}` already defined", $type, $name));
	(UseOfUndefined $type:tt $name:expr) => (format!("use of undefined {} `{}`", $type, $name));
	(UseOfUndefined $type:tt $name:expr, $section:expr) => (format!("use of undefined {} `{}` in section `{}`", $type, $name, $section));
	(DuplicateParameter $param:expr) => (format!("duplicate parameter `{}`", $param));

	(MissingData) => ("missing data");
	(InvalidData) => ("invalid data");
	(InvalidHeader) => ("invalid header");
	(InvalidChecksum) => ("invalid checksum");
	(InvalidIndex $of:expr) => (format!("invalid {} index", $of));
	(ExpectedNullByte) => ("expected null-byte");
	(NonexistentString) => ("use of nonexistent string");
}