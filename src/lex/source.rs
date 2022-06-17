use crate::{report::Report, new_formatted_error};
use std::{collections::HashMap, fs::read_to_string};

pub struct Source {
    pub newlines: Vec<usize>,
    pub buff: String
}

impl Source {
    pub fn at(&self, offset: usize) -> char {
		// pub fn at(&self, offset: usize) -> Option<(char, usize)> {
        // self.buff[offset..].chars().nth(0).map(|ch| { (ch, offset + ch.len_utf8()) })
		self.buff[offset..].chars().nth(0).unwrap_or('\0')
    }

    pub fn slice(&self, start: usize, end: usize) -> &str {
        //
        &self.buff[start..end]
    }

    pub fn slice_line(&self, line: usize) -> Result<&str, ()> {
        // actual index is `line - 1`
        match self.buff.lines().nth(line - 1) {
            Some(ln) => Ok(ln),
            None => Err(()),
        }
    }

    pub fn new(contents: String) -> Self {
        let mut this = Source {
            newlines: vec![0],
            buff: contents,
        };

        // find newlines
        for (i, c) in this.buff.chars().enumerate() {
            if c == '\n' {
                this.newlines.push(i);
            }
        }

        this
    }
}

#[macro_export]
macro_rules! deref_source {
	($self:expr) => (unsafe { $self.source.as_ref().unwrap() });
}


#[derive(Default)]
pub struct Sources {
    files: HashMap<String, Source>,
}

impl Sources {
    pub fn has_source(&self, file: String) -> bool {
        self.files.contains_key(&file)
    }

    pub fn new_raw_source(&mut self, file: String, contents: String) -> *const Source {
        //! replaces any existing source with same name
        if self.has_source(file.clone()) {
            self.remove_source(file.clone());
        }

        self.files.insert(file.clone(), Source::new(contents));
        self.files.get(&file).unwrap()
    }

    pub fn new_source(&mut self, file: String) -> Result<*const Source, Report> {
        // read file and
        let contents = match read_to_string(&file) {
            Ok(text) => text,
            Err(e) => {
                return Err(new_formatted_error!(CouldNotOpen "file", &file, e.kind()));
            }
        };

        Ok(self.new_raw_source(file, contents))
    }

    pub fn remove_source(&mut self, file: String) {
        if self.has_source(file.clone()) {
            self.files.remove(&file);
        }
    }
}


pub static mut __SOURCES: Option<Sources> = None;

#[macro_export]
macro_rules! SOURCES {
    () => { 
        unsafe { 
            if let None = crate::lex::source::__SOURCES {
                crate::lex::source::__SOURCES = Some(crate::lex::source::Sources::default());
            }
            crate::lex::source::__SOURCES.as_mut().unwrap()
        }
    };
}