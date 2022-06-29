use somc_span::Span;
use somc_report::ecode::ErrorCode;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
	// top-level
	Apply,
	Property,
	Define,
	Declare,

	// punctuation
	LeftBrace,
	RightBrace,
	LeftParen,
	RightParen,
	Semicolon,
	// Colon,
	Comma,
	Dot,
	Variadic,
	
	// keyword operators
	Switch,
	For,
	If,
	Else,

	// expression operators
	Cast,
	ShiftLeft,
	ShiftRight,
	Equals,
	NotEquals,
	Lesser,
	LesserEqual,
	Greater,
	GreaterEqual,
	Not,
	And,
	Or,
	XOr,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXOr,
	BitwiseNot,
	Plus,
	Minus,
	Multiply,
	Divide,
	Remainder,
	Range,

	// literals
	Identifier,
	Type,
	Integer,
	Float,
	String,
	Character,
	True,
	False,
	Null,
	
	// misc.
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

				('.', '.', '.') => __somekind!(Variadic),

				_ => None
			}
			2 => match (chars[0], chars[1]) {

				('!', '!') => __somekind!(Property),
				(':', '=') => __somekind!(Declare),
				
				('?', '?') => __somekind!(Switch),
				('=', '>') => __somekind!(For),
				
				('-', '>') => __somekind!(Cast),
				('<', '<') => __somekind!(ShiftLeft),
				('>', '>') => __somekind!(ShiftRight),
				('=', '=') => __somekind!(Equals),
				('/', '=') => __somekind!(NotEquals),
				('>', '=') => __somekind!(GreaterEqual),
				('<', '=') => __somekind!(LesserEqual),
				('&', '&') => __somekind!(And),
				('|', '|') => __somekind!(Or),
				('^', '^') => __somekind!(XOr),
				('.', '.') => __somekind!(Range),

				('(', ')') => __somekind!(Null),
				
				_ => None
			}
			1 => match chars[0] {

				'#' => __somekind!(Apply),
				'=' => __somekind!(Define),
				
				'{' => __somekind!(LeftBrace),
				'}' => __somekind!(RightBrace),
				'(' => __somekind!(LeftParen),
				')' => __somekind!(RightParen),
				';' => __somekind!(Semicolon),
				// ':' => __somekind!(Colon),
				',' => __somekind!(Comma),
				'.' => __somekind!(Dot),

				'?' => __somekind!(If),
				':' => __somekind!(Else),
				
				'>' => __somekind!(Greater),
				'<' => __somekind!(Lesser),
				'!' => __somekind!(Not),
				'|' => __somekind!(BitwiseOr),
				'&' => __somekind!(BitwiseAnd),
				'^' => __somekind!(BitwiseXOr),
				'~' => __somekind!(BitwiseNot),
				'+' => __somekind!(Plus),
				'-' => __somekind!(Minus),
				'*' => __somekind!(Multiply),
				'/' => __somekind!(Divide),
				'%' => __somekind!(Remainder),
	
	
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