use parser::Content;
use parser::Content::*;

use std;
use std::process;
use std::error::Error;
use std::fmt::Display;
use std::fmt;

#[derive(Debug)]
pub enum InterpreterError {
    Command(std::io::Error),
    Decoding(std::string::FromUtf8Error),
}

impl Display for InterpreterError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.cause() {
            Some(e) => write!(fmt, "interpreter error caused by {}", e),
            None => write!(fmt, "interpreter error"),
        }
    }
}

impl Error for InterpreterError {
    fn description(&self) -> &str {
        match *self {
            InterpreterError::Command(ref e) => e.description(),
            InterpreterError::Decoding(ref e) => e.description(),
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            InterpreterError::Command(ref e) => Some(e),
            InterpreterError::Decoding(ref e) => Some(e),
        }
    }
}

pub fn evaluate<'s>(content: Content<'s>) -> Result<String, InterpreterError> {
    match content {
        Literal(content) => Ok(String::from(content)),
        Command(call) => {

            let mut arg_iter = call.split_whitespace();

            let command_output = process::Command::new(arg_iter.next().unwrap())
                .args(arg_iter)
                .output()
                .map_err(InterpreterError::Command)?;

            let decoded_output = String::from_utf8(command_output.stdout)
                .map_err(InterpreterError::Decoding)?;

            Ok(String::from(decoded_output.trim_right()))

        },
        Builtin(s) => Ok(format!("({})", s)),
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    #[cfg(unix)]
    fn test_unix_commands() {

        // TODO Write tests for Unix implementation
        panic!("Not yet implemented");
    }

    #[test]
    #[cfg(windows)]
    fn test_windows_commands() {

        // Note: Most of the common Windows command line utilities are actually
        // `cmd` builtins. To call them from an outside program, you have to
        // use `cmd /c`

        // Test `cd`
        assert_eq!(
            evaluate(Command("cmd /c cd")).unwrap(),
            std::env::current_dir().unwrap().to_str().unwrap()
        );

        // Test `echo hi`
        assert_eq!(
            evaluate(Command("cmd /c echo hi")).unwrap(),
            "hi"
        );

        // Try at least one thing that's *not* cmd
        // There are only a few utilities that are always present and produce
        // deterministic output. where.exe is a good candidate, especially
        // because you can run it on itself.

        // Test `where where`
        // Note: This assumes that where hasn't been shadowed somewhere in the PATH
        assert_eq!(
            evaluate(Command("where where")).unwrap(),
            "C:\\Windows\\System32\\where.exe"
        )
    }

}
