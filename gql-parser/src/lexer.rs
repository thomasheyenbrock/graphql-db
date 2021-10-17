use std::fmt;

#[derive(Debug)]
pub struct SyntaxError {
  pub message: String,
  pub position: i32,
}

impl PartialEq for SyntaxError {
  fn eq(&self, other: &Self) -> bool {
    self.message == other.message && self.position == other.position
  }
}

#[derive(Debug)]
pub enum TokenKind {
  // Start end end of file
  SOF,
  EOF,

  // Ignored token kinds
  Comment { value: String },

  // Lexical token kinds
  ExclamationMark,
  DollarSign,
  Ampersand,
  RoundBracketOpening,
  RoundBracketClosing,
  Spread,
  Colon,
  EqualsSign,
  AtSign,
  SquareBracketOpening,
  SquareBracketClosing,
  CurlyBracketOpening,
  VerticalBar,
  CurlyBracketClosing,

  Name { value: String },
  Int { value: String },
  Float { value: String },
  String { value: String },
}

impl fmt::Display for TokenKind {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      TokenKind::Comment { .. } => write!(f, "Comment"),
      TokenKind::Name { .. } => write!(f, "Name"),
      TokenKind::Int { .. } => write!(f, "Int"),
      TokenKind::Float { .. } => write!(f, "Float"),
      TokenKind::String { .. } => write!(f, "String"),
      _ => write!(f, "{:?}", self),
    }
  }
}

impl PartialEq for TokenKind {
  fn eq(&self, other: &Self) -> bool {
    match self {
      TokenKind::SOF => match other {
        TokenKind::SOF => true,
        _ => false,
      },
      TokenKind::EOF => match other {
        TokenKind::EOF => true,
        _ => false,
      },
      TokenKind::Comment { value: value_self } => match other {
        TokenKind::Comment { value: value_other } => value_self == value_other,
        _ => false,
      },
      TokenKind::ExclamationMark => match other {
        TokenKind::ExclamationMark => true,
        _ => false,
      },
      TokenKind::DollarSign => match other {
        TokenKind::DollarSign => true,
        _ => false,
      },
      TokenKind::Ampersand => match other {
        TokenKind::Ampersand => true,
        _ => false,
      },
      TokenKind::RoundBracketOpening => match other {
        TokenKind::RoundBracketOpening => true,
        _ => false,
      },
      TokenKind::RoundBracketClosing => match other {
        TokenKind::RoundBracketClosing => true,
        _ => false,
      },
      TokenKind::Spread => match other {
        TokenKind::Spread => true,
        _ => false,
      },
      TokenKind::Colon => match other {
        TokenKind::Colon => true,
        _ => false,
      },
      TokenKind::EqualsSign => match other {
        TokenKind::EqualsSign => true,
        _ => false,
      },
      TokenKind::AtSign => match other {
        TokenKind::AtSign => true,
        _ => false,
      },
      TokenKind::SquareBracketOpening => match other {
        TokenKind::SquareBracketOpening => true,
        _ => false,
      },
      TokenKind::SquareBracketClosing => match other {
        TokenKind::SquareBracketClosing => true,
        _ => false,
      },
      TokenKind::CurlyBracketOpening => match other {
        TokenKind::CurlyBracketOpening => true,
        _ => false,
      },
      TokenKind::VerticalBar => match other {
        TokenKind::VerticalBar => true,
        _ => false,
      },
      TokenKind::CurlyBracketClosing => match other {
        TokenKind::CurlyBracketClosing => true,
        _ => false,
      },
      TokenKind::Name { value: value_self } => match other {
        TokenKind::Name { value: value_other } => value_self == value_other,
        _ => false,
      },
      TokenKind::Int { value: value_self } => match other {
        TokenKind::Int { value: value_other } => value_self == value_other,
        _ => false,
      },
      TokenKind::Float { value: value_self } => match other {
        TokenKind::Float { value: value_other } => value_self == value_other,
        _ => false,
      },
      TokenKind::String { value: value_self } => match other {
        TokenKind::String { value: value_other } => value_self == value_other,
        _ => false,
      },
    }
  }
}

#[derive(Debug)]
pub struct Token {
  pub kind: TokenKind,
  pub start: i32,
  pub end: i32,
  pub line: i32,
  pub column: i32,
}

impl PartialEq for Token {
  fn eq(&self, other: &Self) -> bool {
    self.kind == other.kind
      && self.start == other.start
      && self.end == other.end
      && self.line == other.line
      && self.column == other.column
  }
}

pub struct Lexer<'a> {
  chars: std::iter::Peekable<std::str::Chars<'a>>,
  position: i32,
  line: i32,
  column: i32,
  peeked: Option<Token>,
  error: Option<String>,
  is_done: bool,
}

impl Lexer<'_> {
  pub fn new(query: &str) -> Lexer {
    Lexer {
      chars: query.chars().peekable(),
      position: 0,
      line: 0,
      column: 0,
      peeked: None,
      error: None,
      is_done: false,
    }
  }

  fn print_character(c: &char) -> String {
    let code = *c as i16;
    if code > 0x001f && code < 0x007f {
      // Print non-control ASCII characters as is
      return format!("\"{}\"", c);
    } else if code == 0x0008 {
      return String::from("\"\\b\"");
    } else if code == 0x000c {
      return String::from("\"\\f\"");
    } else {
      // Print upper-case encoding for all other characters
      return format!("\"\\u{:0>4}\"", format!("{:X}", code));
    }
  }

  fn next_char(&mut self) -> Option<char> {
    self.position += 1;
    self.column += 1;
    return self.chars.next();
  }

  fn split_by_line_terminator(value: &str) -> Vec<&str> {
    let mut lines: Vec<&str> = Vec::new();
    for l1 in value.split("\r\n") {
      for l2 in l1.split("\r") {
        for l3 in l2.split("\n") {
          lines.push(l3)
        }
      }
    }
    return lines;
  }

  fn calculate_indent(value: &str) -> usize {
    let mut indent = 0;
    let mut chars = value.chars();
    let mut next = chars.next();
    while next == Some(' ') || next == Some('\t') {
      indent += 1;
      next = chars.next();
    }
    return indent;
  }

  fn contains_only_whitespace(value: &str) -> bool {
    return value.chars().all(|c| c == ' ' || c == '\t');
  }

  fn block_string_value(raw_value: &str) -> String {
    let mut lines = Lexer::split_by_line_terminator(raw_value);

    let mut common_indent: Option<usize> = None;

    for line in lines.iter().skip(1) {
      let length = line.len();
      let indent = Lexer::calculate_indent(line);
      if indent < length {
        if common_indent == None || indent < common_indent.unwrap() {
          common_indent = Some(indent);
        }
      }
    }

    if common_indent != None && lines.len() > 0 {
      let common_indent_value = common_indent.unwrap();
      let mut lines_without_indent = lines
        .iter()
        .skip(1)
        .map(|line| {
          if line.len() >= common_indent_value {
            &line[common_indent_value..]
          } else {
            line
          }
        })
        .collect();
      lines = vec![lines[0]];
      lines.append(&mut lines_without_indent);
    }

    while lines.len() > 0 && Lexer::contains_only_whitespace(lines[0]) {
      lines.remove(0);
    }

    while lines.len() > 0 && Lexer::contains_only_whitespace(lines[lines.len() - 1]) {
      lines.pop();
    }

    return lines.join("\n");
  }

  fn next_no_error(&mut self) -> Result<Option<Token>, String> {
    if self.line == 0 {
      let sof = Token {
        kind: TokenKind::SOF,
        start: self.position,
        end: self.position,
        line: self.line,
        column: self.column,
      };
      self.line += 1;
      self.column += 1;
      return Ok(Some(sof));
    }

    // Skip ignores tokens
    while self.chars.peek() == Some(&'\u{feff}') // UnicodeBOM
            || self.chars.peek() == Some(&'\t') // Tab
            || self.chars.peek() == Some(&' ') // Whitespace
            || self.chars.peek() == Some(&',') // Comma
            || self.chars.peek() == Some(&'\n') // Line feed & carriage return
            || self.chars.peek() == Some(&'\r')
    {
      let next = self.next_char();

      if next == Some('\n') || (next == Some('\r') && self.chars.peek() != Some(&'\n')) {
        self.line += 1;
        self.column = 1;
      }
    }

    if self.chars.peek() == None {
      // Avoid returning EOF multiple times
      if self.is_done {
        return Ok(None);
      } else {
        self.is_done = true;
        return Ok(Some(Token {
          kind: TokenKind::EOF,
          start: self.position,
          end: self.position,
          line: self.line,
          column: self.column,
        }));
      }
    }

    match self.chars.peek() {
      // ASCII controll characters are not valid source characters
      // (except for CHARACTER TABULATION, LINE FEED, and CARRIAGE RETURN)
      character
      @
      (Some(&('\u{0}'..='\u{8}'))
      | Some(&('\u{b}'..='\u{c}'))
      | Some(&('\u{e}'..='\u{1f}'))) => {
        return Err(format!(
          "Cannot contain the invalid character {}.",
          Lexer::print_character(character.unwrap())
        ));
      }
      // A comment is an ignored token, but since it does contain human
      // readable information, we append a token to the list where the
      // value contains the list of comment chars.
      Some(&'#') => {
        let start = self.position;
        let start_column = self.column;

        // Skip the '#' character
        self.next_char();

        // Combine all following characters to get the value of the
        // comment token until a line feed, carriage return, of the
        // end of file is reached.
        let mut value = String::from("");
        while (self.chars.peek() != Some(&'\u{a}'))
          && (self.chars.peek() != Some(&'\u{d}') && (self.chars.peek() != None))
        {
          value.push(self.next_char().unwrap());
        }

        let comment_token = Token {
          kind: TokenKind::Comment { value },
          start,
          end: self.position,
          line: self.line,
          column: start_column,
        };
        return Ok(Some(comment_token));
      }
      // Punctuators
      Some(&'!') => {
        let exclamation_mark_token = Token {
          kind: TokenKind::ExclamationMark,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(exclamation_mark_token));
      }
      Some(&'$') => {
        let dollar_token = Token {
          kind: TokenKind::DollarSign,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(dollar_token));
      }
      Some(&'&') => {
        let ampersand_token = Token {
          kind: TokenKind::Ampersand,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(ampersand_token));
      }
      Some(&'(') => {
        let round_bracket_opening_token = Token {
          kind: TokenKind::RoundBracketOpening,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(round_bracket_opening_token));
      }
      Some(&')') => {
        let round_bracket_closing_token = Token {
          kind: TokenKind::RoundBracketClosing,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(round_bracket_closing_token));
      }
      Some(&'.') => {
        // At this point we expect to see two more dots as the only
        // valid token is the spread token. (The only other point where
        // a dot could show up is in a float value or inside a string
        // value, both are handled separately.)

        // Skip the first dot
        self.next_char();

        // Check that the next two chars are also dots
        for index in 1..3 {
          if self.chars.peek() != Some(&'.') {
            self.position -= index;
            self.column -= index;
            return Err(String::from("Cannot parse the unexpected character \".\"."));
          }
          self.next_char();
        }

        let spread_token = Token {
          kind: TokenKind::Spread,
          start: self.position - 3,
          end: self.position,
          line: self.line,
          column: self.column - 3,
        };
        return Ok(Some(spread_token));
      }
      Some(&':') => {
        let colon_token = Token {
          kind: TokenKind::Colon,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(colon_token));
      }
      Some(&'=') => {
        let equals_sign_token = Token {
          kind: TokenKind::EqualsSign,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(equals_sign_token));
      }
      Some(&'@') => {
        let at_sign_token = Token {
          kind: TokenKind::AtSign,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(at_sign_token));
      }
      Some(&'[') => {
        let square_bracket_opening_token = Token {
          kind: TokenKind::SquareBracketOpening,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(square_bracket_opening_token));
      }
      Some(&']') => {
        let square_bracket_closing_token = Token {
          kind: TokenKind::SquareBracketClosing,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(square_bracket_closing_token));
      }
      Some(&'{') => {
        let curly_bracket_opening_token = Token {
          kind: TokenKind::CurlyBracketOpening,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(curly_bracket_opening_token));
      }
      Some(&'|') => {
        let vertical_bar_token = Token {
          kind: TokenKind::VerticalBar,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(vertical_bar_token));
      }
      Some(&'}') => {
        let curly_bracket_closing_token = Token {
          kind: TokenKind::CurlyBracketClosing,
          start: self.position,
          end: self.position + 1,
          line: self.line,
          column: self.column,
        };
        self.next_char();
        return Ok(Some(curly_bracket_closing_token));
      }
      // Name token
      Some(&('A'..='Z')) | Some(&('a'..='z')) | Some(&'_') => {
        let start = self.position;
        let start_column = self.column;

        let mut value = String::from("");
        value.push(self.next_char().unwrap());

        while (Some(&'A')..=Some(&'Z')).contains(&self.chars.peek())
          || (Some(&'a')..=Some(&'z')).contains(&self.chars.peek())
          || (Some(&'0')..=Some(&'9')).contains(&self.chars.peek())
          || self.chars.peek() == Some(&'_')
        {
          value.push(self.next_char().unwrap());
        }

        return Ok(Some(Token {
          kind: TokenKind::Name { value },
          start,
          end: self.position,
          line: self.line,
          column: start_column,
        }));
      }
      // Int or Float token
      Some(&'-') | Some(&('0'..='9')) => {
        let start = self.position;
        let start_column = self.column;

        let mut integer_part = String::from("");

        // Optional negative sign
        if self.chars.peek() == Some(&'-') {
          integer_part.push(self.next_char().unwrap());
        }

        // Leading zeros are not allowed, so if it's a zero it's the
        // only character of the IntergerPart
        if self.chars.peek() == Some(&'0') {
          integer_part.push(self.next_char().unwrap());
          if (Some(&'0')..=Some(&'9')).contains(&self.chars.peek()) {
            return Err(format!(
              "Invalid number, unexpected digit after 0: \"{}\".",
              self.chars.peek().unwrap()
            ));
          }
        } else {
          while (Some(&'0')..=Some(&'9')).contains(&self.chars.peek()) {
            integer_part.push(self.next_char().unwrap());
          }
        }

        let mut fractional_part = String::from("");
        if self.chars.peek() == Some(&'.') {
          fractional_part.push(self.next_char().unwrap());

          // Append all the following digits
          while (Some(&'0')..=Some(&'9')).contains(&self.chars.peek()) {
            fractional_part.push(self.next_char().unwrap());
          }

          if fractional_part == "." {
            return Err(format!(
              "Invalid number, expected digit but got: {}.",
              if self.chars.peek() == None {
                String::from("<EOF>")
              } else {
                format!("\"{}\"", self.chars.peek().unwrap().to_string())
              }
            ));
          }
        }

        let mut exponent_part = String::from("");
        if self.chars.peek() == Some(&'e') || self.chars.peek() == Some(&'E') {
          // ExponentIndicator
          exponent_part.push(self.next_char().unwrap());

          // Optional sign
          if self.chars.peek() == Some(&'+') || self.chars.peek() == Some(&'-') {
            exponent_part.push(self.next_char().unwrap());
          }

          // Append all the following digits
          while (Some(&'0')..=Some(&'9')).contains(&self.chars.peek()) {
            exponent_part.push(self.next_char().unwrap());
          }

          if exponent_part == "e"
            || exponent_part == "e+"
            || exponent_part == "e-"
            || exponent_part == "E"
            || exponent_part == "E+"
            || exponent_part == "E-"
          {
            return Err(format!(
              "Invalid number, expected digit but got: {}.",
              if self.chars.peek() == None {
                String::from("<EOF>")
              } else {
                format!("\"{}\"", self.chars.peek().unwrap().to_string())
              }
            ));
          }
        }

        if (Some(&'0')..=Some(&'9')).contains(&self.chars.peek())
          || (Some(&'a')..=Some(&'z')).contains(&self.chars.peek())
          || (Some(&'A')..=Some(&'Z')).contains(&self.chars.peek())
          || self.chars.peek() == Some(&'_')
          || self.chars.peek() == Some(&'.')
        {
          return Err(format!(
            "Invalid number, expected digit but got: \"{}\".",
            self.chars.peek().unwrap()
          ));
        }

        if fractional_part == "" && exponent_part == "" {
          // It's an integer
          return Ok(Some(Token {
            kind: TokenKind::Int {
              value: integer_part,
            },
            start,
            end: self.position,
            line: self.line,
            column: start_column,
          }));
        } else {
          // It's a float
          let mut value = String::from("");
          value.push_str(&integer_part);
          value.push_str(&fractional_part);
          value.push_str(&exponent_part);
          return Ok(Some(Token {
            kind: TokenKind::Float { value },
            start,
            end: self.position,
            line: self.line,
            column: start_column,
          }));
        }
      }
      // String token
      Some(&'"') => {
        let start = self.position;
        let start_line = self.line;
        let start_column = self.column;

        self.next_char();

        if self.chars.peek() == Some(&'"') {
          self.next_char();

          if self.chars.peek() == Some(&'"') {
            // Block string
            self.next_char();

            let mut value = String::from("");
            let mut is_done = false;
            let mut passes = 0;
            while !is_done {
              if self.chars.peek() == None {
                return Err(String::from("Unterminated string."));
              }

              let next = self.next_char().unwrap();
              value.push(next);

              if next == '\r' {
                self.line += 1;
                self.column = 1;
              }
              if next == '\n' && value.chars().nth_back(1) != Some('\r') {
                self.line += 1;
                self.column = 1;
              }

              if passes > 0 {
                passes -= 1
              };

              is_done = value.len() >= 3
                && value[value.len() - 3..] == String::from("\"\"\"")
                && passes == 0;

              if value.len() >= 4 && value[value.len() - 4..] == String::from("\\\"\"\"") {
                is_done = false;
                passes = 3;
                value = value[..value.len() - 4].to_string();
                value.push_str("\"\"\"");
              }
            }

            if value.len() < 3 || value[value.len() - 3..] != String::from("\"\"\"") {
              return Err(String::from("Unterminated string."));
            }

            return Ok(Some(Token {
              kind: TokenKind::String {
                value: Lexer::block_string_value(&value[..value.len() - 3]),
              },
              start,
              end: self.position,
              line: start_line,
              column: start_column,
            }));
          } else {
            // Empty string
            return Ok(Some(Token {
              kind: TokenKind::String {
                value: String::from(""),
              },
              start,
              end: self.position,
              line: self.line,
              column: start_column,
            }));
          }
        } else {
          // Non-empty non-block stirng
          let mut value = String::from("");

          while self.chars.peek() != Some(&'"') && self.chars.peek() != None {
            let next = self.next_char().unwrap();

            if next == '\\' {
              // Escaped characters & unicode
              match self.next_char() {
                character @ (Some('"') | Some('\\') | Some('/')) => value.push(character.unwrap()),
                Some('b') => value.push('\u{8}'),
                Some('f') => value.push('\u{c}'),
                Some('n') => value.push('\n'),
                Some('r') => value.push('\r'),
                Some('t') => value.push('\t'),
                Some('u') => {
                  // The next 4 characters define the unicode sequence
                  let mut unicode = String::from("");
                  for i in 1..5 {
                    let unicode_char = self.next_char();

                    if unicode_char == None {
                      self.position -= i + 2;
                      self.column -= i + 2;
                      return Err(format!(
                        "Invalid character escape sequence: \\u{}.",
                        unicode
                      ));
                    }

                    unicode.push(unicode_char.unwrap());
                  }

                  match u32::from_str_radix(&unicode, 16) {
                    Ok(unicode_int) => {
                      let unicode_char = char::from_u32(unicode_int);
                      if unicode_char == None {
                        self.position -= 6;
                        self.column -= 6;
                        return Err(format!(
                          "Invalid character escape sequence: \\u{}.",
                          unicode
                        ));
                      }
                      value.push(unicode_char.unwrap());
                    }
                    Err(_) => {
                      self.position -= 6;
                      self.column -= 6;
                      return Err(format!(
                        "Invalid character escape sequence: \\u{}.",
                        unicode
                      ));
                    }
                  }
                }
                character => {
                  self.position -= 2;
                  self.column -= 2;
                  return Err(format!(
                    "Invalid character escape sequence: \\{}.",
                    if character == None {
                      String::from("")
                    } else {
                      character.unwrap().to_string()
                    }
                  ));
                }
              }
            } else if next == '\n' || next == '\r' {
              // Line feed & carriage return
              self.position -= 1;
              self.column -= 1;
              return Err(String::from("Unterminated string."));
            } else {
              // Source character
              value.push(next);
            }
          }

          if self.chars.peek() == None {
            return Err(String::from("Unterminated string."));
          }

          // Remove closing quote
          self.next_char();

          return Ok(Some(Token {
            kind: TokenKind::String { value },
            start,
            end: self.position,
            line: self.line,
            column: start_column,
          }));
        }
      }
      Some(&'\'') => Err(String::from(
        "Unexpected single quote character ('), did you mean to use a double quote (\")?",
      )),
      character => Err(format!(
        "Cannot parse the unexpected character {}.",
        if character == None {
          String::from("<EOF>")
        } else {
          Lexer::print_character(character.unwrap())
        }
      )),
    }
  }

  pub fn next(&mut self) -> Result<Option<Token>, SyntaxError> {
    // If there already is an error, continue returning the error
    if self.error != None {
      return Err(SyntaxError {
        message: String::from((&self.error).as_ref().unwrap()),
        position: self.position,
      });
    }

    // If already peeked, then return the peeked token
    if self.peeked != None {
      let peeked = std::mem::take(&mut self.peeked);
      self.peeked = None;
      return Ok(peeked);
    }

    match self.next_no_error() {
      Ok(token) => Ok(token),
      Err(error) => {
        self.error = Some(error.to_string());
        return Err(SyntaxError {
          message: error.to_string(),
          position: self.position,
        });
      }
    }
  }

  pub fn peek(&mut self) -> Result<Option<Token>, SyntaxError> {
    // If there already is an error, continue returning the error
    if self.error != None {
      return Err(SyntaxError {
        message: String::from((&self.error).as_ref().unwrap()),
        position: self.position,
      });
    }

    // If already peeked, then return the peeked token
    if self.peeked != None {
      let peeked = std::mem::take(&mut self.peeked);
      return Ok(peeked);
    }

    match self.next_no_error() {
      Ok(mut token) => {
        self.peeked = std::mem::take(&mut token);
        return Ok(token);
      }
      Err(error) => {
        self.error = Some(error.to_string());
        return Err(SyntaxError {
          message: error.to_string(),
          position: self.position,
        });
      }
    }
  }

  pub fn has_more(&mut self) -> bool {
    match self.peek() {
      Err(_) => false,
      Ok(token) => match token {
        None => false,
        token => match token.unwrap().kind {
          TokenKind::EOF => false,
          _ => true,
        },
      },
    }
  }

  pub fn to_vec(&mut self) -> Result<Vec<Token>, SyntaxError> {
    let mut token_vec: Vec<Token> = Vec::new();
    let mut is_done = false;

    while !is_done {
      match self.next() {
        Err(error) => return Err(error),
        Ok(some_token) => match some_token {
          None => is_done = true,
          token => token_vec.push(token.unwrap()),
        },
      }
    }
    return Ok(token_vec);
  }

  pub fn get_position(&self) -> i32 {
    return self.position;
  }
}

#[cfg(test)]
mod lexer {
  use super::*;

  fn equals(v1: Result<Vec<Token>, SyntaxError>, v2: Result<Vec<Token>, SyntaxError>) {
    match v1 {
      Ok(tokens1) => match v2 {
        Ok(tokens2) => {
          if tokens1.len() != tokens2.len() {
            panic!(
              "Expected {} tokens, got {} tokens.",
              tokens2.len(),
              tokens1.len()
            )
          }
          for (index, token1) in tokens1.iter().enumerate() {
            let token2 = tokens2.get(index).unwrap();
            if token1 != token2 {
              panic!(
                "Expected {:?} in position {}, got {:?}",
                token2, index, token1
              )
            }
          }
        }
        Err(error2) => panic!("Expected {:?}, got {:?}", error2, tokens1),
      },
      Err(error1) => match v2 {
        Ok(tokens2) => {
          panic!("Expected {:?}, got {:?}", tokens2, error1);
        }
        Err(error2) => {
          if error1 != error2 {
            panic!("Expected {:?}, got {:?}", error2, error1);
          }
        }
      },
    };
  }

  #[test]
  fn should_lex_a_simple_query() {
    equals(
      Lexer::new("{ hello }").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                   start: 0, end: 0, line: 0, column:  0 },
                Token { kind: TokenKind::CurlyBracketOpening,                   start: 0, end: 1, line: 1, column:  1 },
                Token { kind: TokenKind::Name { value: String::from("hello") }, start: 2, end: 7, line: 1, column:  3 },
                Token { kind: TokenKind::CurlyBracketClosing,                   start: 8, end: 9, line: 1, column:  9 },
                Token { kind: TokenKind::EOF,                                   start: 9, end: 9, line: 1, column: 10 },
            ]),
    );
  }

  #[test]
  fn should_skip_all_ignored_tokens() {
    equals(
      Lexer::new("\u{feff} \t\n,,,\r,\r\n,,").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF, start:  0, end:  0, line: 0, column: 0 },
                Token { kind: TokenKind::EOF, start: 13, end: 13, line: 4, column: 3 },
            ]),
    );
  }

  #[test]
  fn should_count_lines_correctly() {
    equals(
      Lexer::new("\n").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF, start: 0, end: 0, line: 0, column: 0 },
                Token { kind: TokenKind::EOF, start: 1, end: 1, line: 2, column: 1 },
            ]),
    );
    equals(
      Lexer::new("\r").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF, start: 0, end: 0, line: 0, column: 0 },
                Token { kind: TokenKind::EOF, start: 1, end: 1, line: 2, column: 1 },
            ]),
    );
    equals(
      Lexer::new("\r\n").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF, start: 0, end: 0, line: 0, column: 0 },
                Token { kind: TokenKind::EOF, start: 2, end: 2, line: 2, column: 1 },
            ]),
    );
    equals(
      Lexer::new("\n\r").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF, start: 0, end: 0, line: 0, column: 0 },
                Token { kind: TokenKind::EOF, start: 2, end: 2, line: 3, column: 1 },
            ]),
    );
  }

  #[test]
  fn should_include_comment_tokens() {
    equals(
      Lexer::new("\t,,#this is a comment\n,#until the end of file").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                                      start:  0, end:  0, line: 0, column:  0 },
                Token { kind: TokenKind::Comment { value: String::from("this is a comment") },     start:  3, end: 21, line: 1, column:  4 },
                Token { kind: TokenKind::Comment { value: String::from("until the end of file") }, start: 23, end: 45, line: 2, column:  2 },
                Token { kind: TokenKind::EOF,                                                      start: 45, end: 45, line: 2, column: 24 },
            ]),
    );
  }

  #[test]
  fn should_return_an_error_for_invalid_source_chars() {
    equals(
      Lexer::new("Hello\u{2}world").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot contain the invalid character \"\\u0002\"."),
        position: 5,
      }),
    );
    equals(
      Lexer::new("Hello\u{8}world").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot contain the invalid character \"\\b\"."),
        position: 5,
      }),
    );
    equals(
      Lexer::new("Hello\u{c}world").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot contain the invalid character \"\\f\"."),
        position: 5,
      }),
    );
  }

  #[test]
  fn should_parse_punctuators() {
    equals(
      Lexer::new("!$&()...:=@[]{|}").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                  start:  0, end:  0, line: 0, column:  0 },
                Token { kind: TokenKind::ExclamationMark,      start:  0, end:  1, line: 1, column:  1 },
                Token { kind: TokenKind::DollarSign,           start:  1, end:  2, line: 1, column:  2 },
                Token { kind: TokenKind::Ampersand,            start:  2, end:  3, line: 1, column:  3 },
                Token { kind: TokenKind::RoundBracketOpening,  start:  3, end:  4, line: 1, column:  4 },
                Token { kind: TokenKind::RoundBracketClosing,  start:  4, end:  5, line: 1, column:  5 },
                Token { kind: TokenKind::Spread,               start:  5, end:  8, line: 1, column:  6 },
                Token { kind: TokenKind::Colon,                start:  8, end:  9, line: 1, column:  9 },
                Token { kind: TokenKind::EqualsSign,           start:  9, end: 10, line: 1, column: 10 },
                Token { kind: TokenKind::AtSign,               start: 10, end: 11, line: 1, column: 11 },
                Token { kind: TokenKind::SquareBracketOpening, start: 11, end: 12, line: 1, column: 12 },
                Token { kind: TokenKind::SquareBracketClosing, start: 12, end: 13, line: 1, column: 13 },
                Token { kind: TokenKind::CurlyBracketOpening,  start: 13, end: 14, line: 1, column: 14 },
                Token { kind: TokenKind::VerticalBar,          start: 14, end: 15, line: 1, column: 15 },
                Token { kind: TokenKind::CurlyBracketClosing,  start: 15, end: 16, line: 1, column: 16 },
                Token { kind: TokenKind::EOF,                  start: 16, end: 16, line: 1, column: 17 },
            ]),
    );
  }

  #[test]
  fn should_return_an_error_for_incomplete_spread() {
    equals(
      Lexer::new("Hello.world").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot parse the unexpected character \".\"."),
        position: 5,
      }),
    );
    equals(
      Lexer::new("Hello..world").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot parse the unexpected character \".\"."),
        position: 5,
      }),
    );
  }

  #[test]
  fn should_parse_name_tokens() {
    equals(
      Lexer::new("HELLO hello _HELLO __hello hello123 hello_123 _123 __123").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                       start:  0, end:  0, line: 0, column:  0 },
                Token { kind: TokenKind::Name { value: String::from("HELLO") },     start:  0, end:  5, line: 1, column:  1 },
                Token { kind: TokenKind::Name { value: String::from("hello") },     start:  6, end: 11, line: 1, column:  7 },
                Token { kind: TokenKind::Name { value: String::from("_HELLO") },    start: 12, end: 18, line: 1, column: 13 },
                Token { kind: TokenKind::Name { value: String::from("__hello") },   start: 19, end: 26, line: 1, column: 20 },
                Token { kind: TokenKind::Name { value: String::from("hello123") },  start: 27, end: 35, line: 1, column: 28 },
                Token { kind: TokenKind::Name { value: String::from("hello_123") }, start: 36, end: 45, line: 1, column: 37 },
                Token { kind: TokenKind::Name { value: String::from("_123") },      start: 46, end: 50, line: 1, column: 47 },
                Token { kind: TokenKind::Name { value: String::from("__123") },     start: 51, end: 56, line: 1, column: 52 },
                Token { kind: TokenKind::EOF,                                       start: 56, end: 56, line: 1, column: 57 },
            ]),
    );
  }

  #[test]
  fn should_return_an_error_when_starting_with_a_number() {
    equals(
      Lexer::new("Hello 42world").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, expected digit but got: \"w\"."),
        position: 8,
      }),
    );
  }

  #[test]
  fn should_parse_int_tokens() {
    equals(
      Lexer::new("0 -0 42 -42").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                start:  0, end:  0, line: 0, column:  0 },
                Token { kind: TokenKind::Int { value: String::from("0") },   start:  0, end:  1, line: 1, column:  1 },
                Token { kind: TokenKind::Int { value: String::from("-0") },  start:  2, end:  4, line: 1, column:  3 },
                Token { kind: TokenKind::Int { value: String::from("42") },  start:  5, end:  7, line: 1, column:  6 },
                Token { kind: TokenKind::Int { value: String::from("-42") }, start:  8, end: 11, line: 1, column:  9 },
                Token { kind: TokenKind::EOF,                                start: 11, end: 11, line: 1, column: 12 },
            ]),
    );
  }

  #[test]
  fn should_return_an_error_for_leading_plus_for_ints() {
    equals(
      Lexer::new("+0").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot parse the unexpected character \"+\"."),
        position: 0,
      }),
    );
    equals(
      Lexer::new("+42").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot parse the unexpected character \"+\"."),
        position: 0,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_leading_zeros_for_ints() {
    equals(
      Lexer::new("042").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, unexpected digit after 0: \"4\"."),
        position: 1,
      }),
    );
    equals(
      Lexer::new("0042").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, unexpected digit after 0: \"0\"."),
        position: 1,
      }),
    );
  }

  #[test]
  fn should_parse_float_tokens_with_a_fractional_part() {
    equals(
      Lexer::new("0.43 -0.43 42.43 -42.43").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                     start:  0, end:  0, line: 0, column:  0 },
                Token { kind: TokenKind::Float { value: String::from("0.43") },   start:  0, end:  4, line: 1, column:  1 },
                Token { kind: TokenKind::Float { value: String::from("-0.43") },  start:  5, end: 10, line: 1, column:  6 },
                Token { kind: TokenKind::Float { value: String::from("42.43") },  start: 11, end: 16, line: 1, column: 12 },
                Token { kind: TokenKind::Float { value: String::from("-42.43") }, start: 17, end: 23, line: 1, column: 18 },
                Token { kind: TokenKind::EOF,                                     start: 23, end: 23, line: 1, column: 24 },
            ]),
    );
  }

  #[test]
  fn should_parse_float_tokens_with_an_exponent_part() {
    equals(
            Lexer::new("0e44 0E44 0e+44 0E+44 0e-44 0E-44 -0e44 -0E44 -0e+44 -0E+44 -0e-44 -0E-44 42e44 42E44 42e+44 42E+44 42e-44 42E-44 -42e44 -42E44 -42e+44 -42E+44 -42e-44 -42E-44").to_vec(),
            #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                      start:   0, end:   0, line: 0, column:   0 },
                Token { kind: TokenKind::Float { value: String::from("0e44") },    start:   0, end:   4, line: 1, column:   1 },
                Token { kind: TokenKind::Float { value: String::from("0E44") },    start:   5, end:   9, line: 1, column:   6 },
                Token { kind: TokenKind::Float { value: String::from("0e+44") },   start:  10, end:  15, line: 1, column:  11 },
                Token { kind: TokenKind::Float { value: String::from("0E+44") },   start:  16, end:  21, line: 1, column:  17 },
                Token { kind: TokenKind::Float { value: String::from("0e-44") },   start:  22, end:  27, line: 1, column:  23 },
                Token { kind: TokenKind::Float { value: String::from("0E-44") },   start:  28, end:  33, line: 1, column:  29 },
                Token { kind: TokenKind::Float { value: String::from("-0e44") },   start:  34, end:  39, line: 1, column:  35 },
                Token { kind: TokenKind::Float { value: String::from("-0E44") },   start:  40, end:  45, line: 1, column:  41 },
                Token { kind: TokenKind::Float { value: String::from("-0e+44") },  start:  46, end:  52, line: 1, column:  47 },
                Token { kind: TokenKind::Float { value: String::from("-0E+44") },  start:  53, end:  59, line: 1, column:  54 },
                Token { kind: TokenKind::Float { value: String::from("-0e-44") },  start:  60, end:  66, line: 1, column:  61 },
                Token { kind: TokenKind::Float { value: String::from("-0E-44") },  start:  67, end:  73, line: 1, column:  68 },
                Token { kind: TokenKind::Float { value: String::from("42e44") },   start:  74, end:  79, line: 1, column:  75 },
                Token { kind: TokenKind::Float { value: String::from("42E44") },   start:  80, end:  85, line: 1, column:  81 },
                Token { kind: TokenKind::Float { value: String::from("42e+44") },  start:  86, end:  92, line: 1, column:  87 },
                Token { kind: TokenKind::Float { value: String::from("42E+44") },  start:  93, end:  99, line: 1, column:  94 },
                Token { kind: TokenKind::Float { value: String::from("42e-44") },  start: 100, end: 106, line: 1, column: 101 },
                Token { kind: TokenKind::Float { value: String::from("42E-44") },  start: 107, end: 113, line: 1, column: 108 },
                Token { kind: TokenKind::Float { value: String::from("-42e44") },  start: 114, end: 120, line: 1, column: 115 },
                Token { kind: TokenKind::Float { value: String::from("-42E44") },  start: 121, end: 127, line: 1, column: 122 },
                Token { kind: TokenKind::Float { value: String::from("-42e+44") }, start: 128, end: 135, line: 1, column: 129 },
                Token { kind: TokenKind::Float { value: String::from("-42E+44") }, start: 136, end: 143, line: 1, column: 137 },
                Token { kind: TokenKind::Float { value: String::from("-42e-44") }, start: 144, end: 151, line: 1, column: 145 },
                Token { kind: TokenKind::Float { value: String::from("-42E-44") }, start: 152, end: 159, line: 1, column: 153 },
                Token { kind: TokenKind::EOF,                                      start: 159, end: 159, line: 1, column: 160 },
            ]),
        );
  }

  #[test]
  fn should_parse_float_tokens_with_fractional_and_exponent_part() {
    equals(
            Lexer::new("0.43e44 0.43E44 0.43e+44 0.43E+44 0.43e-44 0.43E-44 -0.43e44 -0.43E44 -0.43e+44 -0.43E+44 -0.43e-44 -0.43E-44 42.43e44 42.43E44 42.43e+44 42.43E+44 42.43e-44 42.43E-44 -42.43e44 -42.43E44 -42.43e+44 -42.43E+44 -42.43e-44 -42.43E-44").to_vec(),
            #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                         start:   0, end:   0, line: 0, column:   0 },
                Token { kind: TokenKind::Float { value: String::from("0.43e44") },    start:   0, end:   7, line: 1, column:   1 },
                Token { kind: TokenKind::Float { value: String::from("0.43E44") },    start:   8, end:  15, line: 1, column:   9 },
                Token { kind: TokenKind::Float { value: String::from("0.43e+44") },   start:  16, end:  24, line: 1, column:  17 },
                Token { kind: TokenKind::Float { value: String::from("0.43E+44") },   start:  25, end:  33, line: 1, column:  26 },
                Token { kind: TokenKind::Float { value: String::from("0.43e-44") },   start:  34, end:  42, line: 1, column:  35 },
                Token { kind: TokenKind::Float { value: String::from("0.43E-44") },   start:  43, end:  51, line: 1, column:  44 },
                Token { kind: TokenKind::Float { value: String::from("-0.43e44") },   start:  52, end:  60, line: 1, column:  53 },
                Token { kind: TokenKind::Float { value: String::from("-0.43E44") },   start:  61, end:  69, line: 1, column:  62 },
                Token { kind: TokenKind::Float { value: String::from("-0.43e+44") },  start:  70, end:  79, line: 1, column:  71 },
                Token { kind: TokenKind::Float { value: String::from("-0.43E+44") },  start:  80, end:  89, line: 1, column:  81 },
                Token { kind: TokenKind::Float { value: String::from("-0.43e-44") },  start:  90, end:  99, line: 1, column:  91 },
                Token { kind: TokenKind::Float { value: String::from("-0.43E-44") },  start: 100, end: 109, line: 1, column: 101 },
                Token { kind: TokenKind::Float { value: String::from("42.43e44") },   start: 110, end: 118, line: 1, column: 111 },
                Token { kind: TokenKind::Float { value: String::from("42.43E44") },   start: 119, end: 127, line: 1, column: 120 },
                Token { kind: TokenKind::Float { value: String::from("42.43e+44") },  start: 128, end: 137, line: 1, column: 129 },
                Token { kind: TokenKind::Float { value: String::from("42.43E+44") },  start: 138, end: 147, line: 1, column: 139 },
                Token { kind: TokenKind::Float { value: String::from("42.43e-44") },  start: 148, end: 157, line: 1, column: 149 },
                Token { kind: TokenKind::Float { value: String::from("42.43E-44") },  start: 158, end: 167, line: 1, column: 159 },
                Token { kind: TokenKind::Float { value: String::from("-42.43e44") },  start: 168, end: 177, line: 1, column: 169 },
                Token { kind: TokenKind::Float { value: String::from("-42.43E44") },  start: 178, end: 187, line: 1, column: 179 },
                Token { kind: TokenKind::Float { value: String::from("-42.43e+44") }, start: 188, end: 198, line: 1, column: 189 },
                Token { kind: TokenKind::Float { value: String::from("-42.43E+44") }, start: 199, end: 209, line: 1, column: 200 },
                Token { kind: TokenKind::Float { value: String::from("-42.43e-44") }, start: 210, end: 220, line: 1, column: 211 },
                Token { kind: TokenKind::Float { value: String::from("-42.43E-44") }, start: 221, end: 231, line: 1, column: 222 },
                Token { kind: TokenKind::EOF,                                         start: 231, end: 231, line: 1, column: 232 },
            ]),
        );
  }

  #[test]
  fn should_return_an_error_for_leading_plus_for_floats() {
    equals(
      Lexer::new("+0").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot parse the unexpected character \"+\"."),
        position: 0,
      }),
    );
    equals(
      Lexer::new("+42.43").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot parse the unexpected character \"+\"."),
        position: 0,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_leading_zeros_for_floats() {
    equals(
      Lexer::new("042.43").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, unexpected digit after 0: \"4\"."),
        position: 1,
      }),
    );
    equals(
      Lexer::new("0042.43").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, unexpected digit after 0: \"0\"."),
        position: 1,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_empty_fractional_part() {
    equals(
      Lexer::new("0.").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, expected digit but got: <EOF>."),
        position: 2,
      }),
    );
    equals(
      Lexer::new("42.e44").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, expected digit but got: \"e\"."),
        position: 3,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_empty_exponent_part() {
    equals(
      Lexer::new("0e").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, expected digit but got: <EOF>."),
        position: 2,
      }),
    );
    equals(
      Lexer::new("42e ").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, expected digit but got: \" \"."),
        position: 3,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_name_following_int() {
    equals(
      Lexer::new("42hello").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, expected digit but got: \"h\"."),
        position: 2,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_name_following_float() {
    equals(
      Lexer::new("42.43hello").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid number, expected digit but got: \"h\"."),
        position: 5,
      }),
    );
  }

  #[test]
  fn should_parse_string_tokens() {
    equals(
      Lexer::new("\"Hello\" \"\" \"\"\"world\"\"\" \"\"\"\"\"\"").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                     start:  0, end:  0, line: 0, column:  0 },
                Token { kind: TokenKind::String { value: String::from("Hello") }, start:  0, end:  7, line: 1, column:  1 },
                Token { kind: TokenKind::String { value: String::from("") },      start:  8, end: 10, line: 1, column:  9 },
                Token { kind: TokenKind::String { value: String::from("world") }, start: 11, end: 22, line: 1, column: 12 },
                Token { kind: TokenKind::String { value: String::from("") },      start: 23, end: 29, line: 1, column: 24 },
                Token { kind: TokenKind::EOF,                                     start: 29, end: 29, line: 1, column: 30 },
            ]),
    );
  }

  #[test]
  fn should_parse_escaped_characters_in_strings() {
    equals(
      Lexer::new("\"Hello \\\" \\\\ \\/ \\b \\f \\n \\r \\t \\u1234\"").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                                                           start:  0, end:  0, line: 0, column:  0 },
                Token { kind: TokenKind::String { value: String::from("Hello \" \\ / \u{8} \u{c} \n \r \t \u{1234}") }, start:  0, end: 38, line: 1, column:  1 },
                Token { kind: TokenKind::EOF,                                                                           start: 38, end: 38, line: 1, column: 39 },
            ]),
    );
  }

  #[test]
  fn should_parse_escaped_triple_quotes_in_block_strings() {
    equals(
            Lexer::new("\"\"\"\\\"\"\"\"\"\" \"\"\"escaped \\\"\"\"\"\"\" \"\"\"\\\"\"\" escaped\"\"\" \"\"\"escaped \\\"\"\" escaped\"\"\"").to_vec(),
            #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                                      start:  0, end:  0, line: 0, column:  0 },
                Token { kind: TokenKind::String { value: String::from("\"\"\"") },                 start:  0, end: 10, line: 1, column:  1 },
                Token { kind: TokenKind::String { value: String::from("escaped \"\"\"") },         start: 11, end: 29, line: 1, column: 12 },
                Token { kind: TokenKind::String { value: String::from("\"\"\" escaped") },         start: 30, end: 48, line: 1, column: 31 },
                Token { kind: TokenKind::String { value: String::from("escaped \"\"\" escaped") }, start: 49, end: 75, line: 1, column: 50 },
                Token { kind: TokenKind::EOF,                                                      start: 75, end: 75, line: 1, column: 76 },
            ]),
        );
  }

  #[test]
  fn should_format_block_string_values() {
    equals(
      Lexer::new("\"\"\"\n \t Hello\r\n\r    \tworld\r  \n\t\"\"\"").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                                 start:  0, end:  0, line: 0, column: 0 },
                Token { kind: TokenKind::String { value: String::from("Hello\n\n \tworld") }, start:  0, end: 33, line: 1, column: 1 },
                Token { kind: TokenKind::EOF,                                                 start: 33, end: 33, line: 6, column: 5 },
            ]),
    );
    equals(
      Lexer::new("\"\"\"\n  Hello,\n    World!\n\n  Yours,\n    GraphQL.\n  \"\"\"").to_vec(),
      #[cfg_attr(rustfmt, rustfmt_skip)]
            Ok(vec![
                Token { kind: TokenKind::SOF,                                                                      start:  0, end:  0, line: 0, column: 0 },
                Token { kind: TokenKind::String { value: String::from("Hello,\n  World!\n\nYours,\n  GraphQL.") }, start:  0, end: 52, line: 1, column: 1 },
                Token { kind: TokenKind::EOF,                                                                      start: 52, end: 52, line: 7, column: 6 },
            ]),
    );
  }

  #[test]
  fn should_return_an_error_for_invalid_character_escapes() {
    equals(
      Lexer::new("\"\\q\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid character escape sequence: \\q."),
        position: 1,
      }),
    );
    equals(
      Lexer::new("\"\\u\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid character escape sequence: \\u\"."),
        position: 1,
      }),
    );
    equals(
      Lexer::new("\"\\u1\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid character escape sequence: \\u1\"."),
        position: 1,
      }),
    );
    equals(
      Lexer::new("\"\\u12\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid character escape sequence: \\u12\"."),
        position: 1,
      }),
    );
    equals(
      Lexer::new("\"\\u123\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid character escape sequence: \\u123\"."),
        position: 1,
      }),
    );
    equals(
      Lexer::new("\"\\u123z\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Invalid character escape sequence: \\u123z."),
        position: 1,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_unterminated_string() {
    equals(
      Lexer::new("\"Hello").to_vec(),
      Err(SyntaxError {
        message: String::from("Unterminated string."),
        position: 6,
      }),
    );
    equals(
      Lexer::new("\"Hello\nworld\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Unterminated string."),
        position: 6,
      }),
    );
    equals(
      Lexer::new("\"Hello\rworld\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Unterminated string."),
        position: 6,
      }),
    );
    equals(
      Lexer::new("\"Hello\r\nworld\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Unterminated string."),
        position: 6,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_unterminated_block_string() {
    equals(
      Lexer::new("\"\"\"Hello").to_vec(),
      Err(SyntaxError {
        message: String::from("Unterminated string."),
        position: 8,
      }),
    );
    equals(
      Lexer::new("\"\"\"Hello\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Unterminated string."),
        position: 9,
      }),
    );
    equals(
      Lexer::new("\"\"\"Hello\"\"").to_vec(),
      Err(SyntaxError {
        message: String::from("Unterminated string."),
        position: 10,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_unexpected_characters() {
    equals(
      Lexer::new("Hello\u{25}world").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot parse the unexpected character \"%\"."),
        position: 5,
      }),
    );
    equals(
      Lexer::new("Hello\u{1234}world").to_vec(),
      Err(SyntaxError {
        message: String::from("Cannot parse the unexpected character \"\\u1234\"."),
        position: 5,
      }),
    );
  }

  #[test]
  fn should_return_an_error_for_single_quote() {
    equals(
      Lexer::new("'").to_vec(),
      Err(SyntaxError {
        message: String::from(
          "Unexpected single quote character ('), did you mean to use a double quote (\")?",
        ),
        position: 0,
      }),
    );
  }
}
