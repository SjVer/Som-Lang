use super::span::Span;
use crate::report::ecode::ErrorCode;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
	// definition
	Conclusion,
	Function,
	Variable,
	Question,
	Theorem,
	Apply,

	// punctuation
	LeftBrace,
	RightBrace,
	LeftParen,
	RightParen,
	Define,
	Colon,
	Access,
	Dot,
	
	// theory operators
	Or,
	And,
	Not,
	Implies,
	NotImplies,
	DefEquals,
	DefNotEquals,
	RoughlyEquals,
	Greater,
	GreaterEqual,
	Lesser,
	LesserEqual,
	Divisible,
	Exists,

	// expression operators
	Equals,
	NotEquals,
	Plus,
	Minus,
	Multiply,
	Divide,
	Power,

	// literals
	Identifier,
	Type,
	Integer,
	Float,
	Generic,
	
	// misc.
	// Newline,
	EOF,
	Error(ErrorCode, String, Option<Box<Token>>),
}

macro_rules! __somestr {
	($s:expr) => { Some(String::from($s)) };
}

macro_rules! __somekind {
	($kind:ident) => {
		Some(Self::$kind)
	};
}

impl TokenKind {
	pub fn from_chars(chars: &[char]) -> Option<Self> {
		match chars.len() {
			3 => match (chars[0], chars[1], chars[2]) {

				('<', '=', '>') => __somekind!(Implies),
				('<', '!', '>') => __somekind!(NotImplies),

				_ => None
			}
			2 => match (chars[0], chars[1]) {

				(':', '=') => __somekind!(Define),
				(':', ':') => __somekind!(Access),
				
				('=', '=') => __somekind!(DefEquals),
				('!', '=') => __somekind!(DefNotEquals),
				('~', '=') => __somekind!(RoughlyEquals),
				('>', '=') => __somekind!(GreaterEqual),
				('<', '=') => __somekind!(LesserEqual),
				('?', '?') => __somekind!(Exists),
				
				('/', '=') => __somekind!(NotEquals),
	
				('.', '.') => __somekind!(Generic),
				
				_ => None
			}
			1 => match chars[0] {

				'&' => __somekind!(Conclusion),
				'@' => __somekind!(Function),
				'$' => __somekind!(Variable),
				'?' => __somekind!(Question),
				'!' => __somekind!(Theorem),
				'#' => __somekind!(Apply),
	
				'{' => __somekind!(LeftBrace),
				'}' => __somekind!(RightBrace),
				'(' => __somekind!(LeftParen),
				')' => __somekind!(RightParen),
				':' => __somekind!(Colon),
				'.' => __somekind!(Dot),
	
				'|' => __somekind!(Or),
				'&' => __somekind!(And),
				'~' => __somekind!(Not),
				'>' => __somekind!(Greater),
				'<' => __somekind!(Lesser),
				'%' => __somekind!(Divisible),
	
				'=' => __somekind!(Equals),
				'+' => __somekind!(Plus),
				'-' => __somekind!(Minus),
				'*' => __somekind!(Multiply),
				'/' => __somekind!(Divide),
				'^' => __somekind!(Power),
	
				_ => None
			}
			_ => unreachable!(),
		}
		
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span
}