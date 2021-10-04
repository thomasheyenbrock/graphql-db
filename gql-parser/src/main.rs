use std::fs;

fn main() {
    let query =
        fs::read_to_string("src/example.gql").expect("Something went wrong reading the file");
    let mut lexer = gql_parser::Lexer::new(&query);
    println!("{:?}", lexer.to_vec());
}
