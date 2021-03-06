use somc_lex::Lexer;
use somc_parse::Parser;
use somc_codegen;
use somc_span::SOURCES;
use somc_report::{Report, new_formatted_error};
use somc_global::{cli::setup, get_cli_arg};

// tmp main fn
fn main() {
	setup();

	let filename = get_cli_arg!(infile).unwrap();

	let r = || -> Result<_, Report> {
		
		// read source and parse
		let src = SOURCES!().new_source(filename.clone())
			.map_err(|e| new_formatted_error!(CouldNotOpen "file", filename, e))?;

		let tokens = Lexer::new(filename.clone(), src).lex();
		Parser::new().parse(filename.clone(), tokens)?;

		Ok(())
	};

	if let Err(r) = r() {
		r.dispatch();
		std::process::exit(1);
	}

}