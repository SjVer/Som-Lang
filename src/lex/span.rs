use crate::deref_source;
use super::source::Source;

#[derive(Clone, Debug, PartialEq)]
pub struct Location {
	pub file: String,
	pub line: Option<usize>,
	pub column: Option<usize>,

	pub source: *const Source,
}

impl Location {
	pub fn to_string(&self) -> String {
		// line and col -> file:line:col
		if matches!(self.line, Some(_)) && matches!(self.column, Some(_)) {
			format!("{}:{}:{}", self.file, self.line.unwrap(), self.column.unwrap())
		}
		// just line -> file:line
		else if matches!(self.line, Some(_)) {
			format!("{}:{}", self.file, self.line.unwrap())
		}
		// nothing -> file
		else {
			format!("{}", self.file)
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
	pub start: Location,
	pub length: usize,
}

impl Span {
	pub fn get_line_before(&self) -> Result<&str, ()> {
		match self.start.line {
			Some(line) => if line <= 1 {
				Err(())
			} else {
				deref_source!(self.start).slice_line(line - 1)
			},
			None => Err(()),
		}
	}
	
	pub fn get_line(&self) -> Result<&str, ()> {
		match self.start.line {
			Some(line) => deref_source!(self.start).slice_line(line),
			None => Err(()),
		}
	}

	pub fn get_part_before(&self) -> Result<&str, ()> {
		// get full line or return None
		let line = self.get_line()?;

		let end = self.start.column.unwrap_or(1) - 1;

		Ok(&line[..end])
	}

	pub fn get_part_after(&self) -> Result<&str, ()> {
		// get full line or return None
		let line = self.get_line()?;

		let start = self.start.column.unwrap_or(1) - 1 + self.length;

		Ok(&line[start..])
	}

	pub fn get_part(&self) -> Result<&str, ()> {
		// get full line or return None
		let line = self.get_line()?;

		let start = self.start.column.unwrap_or(1) - 1;
		let end = start + self.length;

		Ok(&line[start..end])
	}
}