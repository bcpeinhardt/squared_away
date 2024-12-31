import squared_away/lang/scanner
import gleam/io
import simplifile

//// The idea with this new lang module is to treat the csv source as one big
//// program as opposed to several small programs to be individually compiled.
//// We also want to compile to a byte code representation and run the sheet 
//// in a vm rather than interpreting an AST

// temporary main function for development
pub fn main() {
    let assert Ok(src) = simplifile.read("./test/testfiles/basics.csv")
    src |> scanner.scan |> io.debug
}