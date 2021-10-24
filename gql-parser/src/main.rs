use std::fs;

fn main() {
   let query =
      fs::read_to_string("src/example.gql").expect("Something went wrong reading the file");
   println!("{:?}", gql_parser::tokenize(&query));
}
