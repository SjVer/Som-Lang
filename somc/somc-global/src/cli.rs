use crate::info::{cli, app};
use clap::{Parser, ArgEnum, AppSettings::DeriveDisplayOrder};
pub use clap::error as claperr;

pub static mut CLI_ARGS: Option<CliArgs> = None;


#[macro_export]
macro_rules! get_cli_arg {
    ($field:ident) => ( unsafe { somc_global::cli::CLI_ARGS.clone().unwrap().$field } )
}

/// struct containing arguments from cli
#[derive(Parser, Default, Clone, Debug)]
#[clap(name = app::NAME)]
#[clap(version, about, long_about = cli::DESCRIPTION)]
#[clap(propagate_version = true)]
#[clap(global_setting(DeriveDisplayOrder))]
pub struct CliArgs {

    #[clap(help = cli::ARG_INFILE)]
    pub infile: Option<String>,

    
    #[clap(short, long, help = cli::ARG_MUTE)]
    pub mute: bool,

    #[clap(short, long, help = cli::ARG_COMPACT)]
    pub compact: bool,

    #[clap(short, long, help = cli::ARG_QUIET)]
    pub quiet: bool,



    #[clap(long)]
    pub lint: Option<LintMode>,


    #[clap(long, help = cli::ARG_EXPLAIN, value_name = "CODE")]
    pub explain: Option<String>,
}

pub fn set_cli_args_empty() {
    unsafe {
        CLI_ARGS = Some(CliArgs{
            quiet: true,
            ..Default::default()
        });
    }
}


#[derive(Copy, Clone, Debug, ArgEnum, PartialEq)]
pub enum LintMode {
    #[clap(name = cli::LINT_NONE_NAME)] None,
    #[clap(name = cli::LINT_DIAG_NAME)] Diag,
}

impl std::str::FromStr for LintMode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            cli::LINT_NONE_NAME => Ok(LintMode::None),
            cli::LINT_DIAG_NAME => Ok(LintMode::Diag),
            _ => Err("invalid lint mode"),
        }
    }
}

impl std::fmt::Display for LintMode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LintMode::None => write!(f, "{}", cli::LINT_NONE_NAME),
            LintMode::Diag => write!(f, "{}", cli::LINT_DIAG_NAME),
        }
    }
}


pub fn setup() {
    unsafe { CLI_ARGS = Some(CliArgs::parse()); }
}
