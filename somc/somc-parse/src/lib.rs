mod apply;
mod ast;

use somc_lex::token::{Token, TokenKind};
use somc_report::{error, new_formatted_error, Report};

pub struct Parser {
	had_error: bool,
}

// public stuff
impl Parser {
	pub fn new() -> Self {
		Self {
			had_error: false,
		}
	}

	pub fn parse(&mut self, filename: String, tokens: Vec<Token>) -> Result<(), Report> {
		for token in tokens {
			if let TokenKind::Error(code, msg, _) = token.kind {
				error(msg, Some(code))
					.with_quote(token.span, None::<String>)
					.dispatch();
				self.had_error = true;
			}
		}

		if self.had_error {
			Err(new_formatted_error!(CouldNotCompile &filename))
		} else {
			Ok(())
		}
	}
}