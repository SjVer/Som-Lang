#![allow(deprecated)]

use std::{
	path::PathBuf,
	env::{home_dir, current_dir},
	fs::canonicalize,
};

pub static STDLIB_DIR: &str = "/usr/share/som/sections/";

#[derive(PartialEq)]
pub enum PathPrefix {
	Root, // '/'
	Home, // '~'
	Work, // '.'
	None,
}

pub struct Path {
	prefix: PathPrefix,
	segments: Vec<String>,
}

impl Path {
	pub fn new(prefix: PathPrefix) -> Self {
		Self {
			prefix,
			segments: Vec::new(),
		}
	}

	pub fn has_prefix(&self) -> bool {
		self.prefix != PathPrefix::None
	}

	pub fn append(&mut self, segment: impl ToString) {
		self.segments.push(segment.to_string());
	}

	pub fn find_file(&self) -> Result<String, String> {
		let mut path = match self.prefix {
			PathPrefix::Root => {
				// find current dir and pop until just root is left
				let pwd = match current_dir() {
					Ok(dir) => dir,
					Err(_) => { return Err(String::from("could not find root directory")); },
				};
				let mut pwd = match canonicalize(pwd) {
					Ok(dir) => dir,
					Err(_) => { return Err(String::from("could not find root directory")); },
				};
				while pwd.pop() {}
				pwd
			},
			PathPrefix::Home => {
				match home_dir() {
					Some(dir) => dir,
					None => { return Err(String::from("could not find home directory")); },
				}
			},
			PathPrefix::Work => {
				match current_dir() {
					Ok(dir) => dir,
					Err(_) => { return Err(String::from("could not find working directory")); },
				}
			},
			PathPrefix::None => PathBuf::from(STDLIB_DIR),
		};

		for segment in &self.segments {
			path.push(segment);
		}

		path.set_extension("som");

		if path.exists() {
			Ok(path.display().to_string())
		} else {
			Err(format!("file '{}' not found.", path.display()))
		}
	}

	pub fn get_ident(&self) -> String {
		self.segments[self.segments.len() - 1].clone()
	}
}

impl ToString for Path {
	fn to_string(&self) -> String {
		let prefix = match &self.prefix {
			PathPrefix::Root => "//",
			PathPrefix::Home => "~/",
			PathPrefix::Work => "./",
			PathPrefix::None => "",
		}.to_string();

		prefix + &self.segments.join("/")
	}
}
