#[derive(Debug, PartialEq)]
pub struct SyntaxError {
  pub message: String,
  pub position: usize,
}
