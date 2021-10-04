use std::fmt;

#[derive(Debug)]
pub struct SyntaxError {
    pub message: String,
    pub position: i32,
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

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub start: i32,
    pub end: i32,
    pub line: i32,
    pub column: i32,
}

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    position: i32,
    line: i32,
    column: i32,
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
            error: None,
            is_done: false,
        }
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
            let next = self.chars.next();
            self.column += 1;
            self.position += 1;

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
            Some(&('\u{0}'..='\u{8}'))
            | Some(&('\u{b}'..='\u{c}'))
            | Some(&('\u{e}'..='\u{1f}')) => {
                self.chars.next();
                return Err(String::from("Not a valid source character"));
            }
            // A comment is an ignored token, but since it does contain human
            // readable information, we append a token to the list where the
            // value contains the list of comment chars.
            Some(&'#') => {
                let start = self.position;
                let start_column = self.column;

                // Skip the '#' character
                self.chars.next();
                self.column += 1;
                self.position += 1;

                // Combine all following characters to get the value of the
                // comment token until a line feed, carriage return, of the
                // end of file is reached.
                let mut value = String::from("");
                while (self.chars.peek() != Some(&'\u{a}'))
                    && (self.chars.peek() != Some(&'\u{d}') && (self.chars.peek() != None))
                {
                    value.push(self.chars.next().unwrap());
                    self.column += 1;
                    self.position += 1;
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
                self.chars.next();
                let exclamation_mark_token = Token {
                    kind: TokenKind::ExclamationMark,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(exclamation_mark_token));
            }
            Some(&'$') => {
                self.chars.next();
                let dollar_token = Token {
                    kind: TokenKind::DollarSign,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(dollar_token));
            }
            Some(&'&') => {
                self.chars.next();
                let ampersand_token = Token {
                    kind: TokenKind::Ampersand,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(ampersand_token));
            }
            Some(&'(') => {
                self.chars.next();
                let round_bracket_opening_token = Token {
                    kind: TokenKind::RoundBracketOpening,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(round_bracket_opening_token));
            }
            Some(&')') => {
                self.chars.next();
                let round_bracket_closing_token = Token {
                    kind: TokenKind::RoundBracketClosing,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(round_bracket_closing_token));
            }
            Some(&'.') => {
                // At this point we expect to see two more dots as the only
                // valid token is the spread token. (The only other point where
                // a dot could show up is in a float value or inside a string
                // value, both are handled separately.)

                // Skip the first dot
                self.chars.next();
                self.column += 1;
                self.position += 1;

                // Check that the next two chars are also dots
                for _ in 0..2 {
                    if self.chars.next() != Some('.') {
                        return Err(String::from("Cannot parse the unexpected character '.'"));
                    }
                    self.column += 1;
                    self.position += 1;
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
                self.chars.next();
                let colon_token = Token {
                    kind: TokenKind::Colon,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(colon_token));
            }
            Some(&'=') => {
                self.chars.next();
                let equals_sign_token = Token {
                    kind: TokenKind::EqualsSign,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(equals_sign_token));
            }
            Some(&'@') => {
                self.chars.next();
                let at_sign_token = Token {
                    kind: TokenKind::AtSign,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(at_sign_token));
            }
            Some(&'[') => {
                self.chars.next();
                let square_bracket_opening_token = Token {
                    kind: TokenKind::SquareBracketOpening,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(square_bracket_opening_token));
            }
            Some(&']') => {
                self.chars.next();
                let square_bracket_closing_token = Token {
                    kind: TokenKind::SquareBracketClosing,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(square_bracket_closing_token));
            }
            Some(&'{') => {
                self.chars.next();
                let curly_bracket_opening_token = Token {
                    kind: TokenKind::CurlyBracketOpening,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(curly_bracket_opening_token));
            }
            Some(&'|') => {
                self.chars.next();
                let vertical_bar_token = Token {
                    kind: TokenKind::VerticalBar,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(vertical_bar_token));
            }
            Some(&'}') => {
                self.chars.next();
                let curly_bracket_closing_token = Token {
                    kind: TokenKind::CurlyBracketClosing,
                    start: self.position,
                    end: self.position + 1,
                    line: self.line,
                    column: self.column,
                };
                self.column += 1;
                self.position += 1;
                return Ok(Some(curly_bracket_closing_token));
            }
            // Name token
            Some(&('A'..='Z')) | Some(&('a'..='z')) | Some(&'_') => {
                let start = self.position;
                let start_column = self.column;

                let mut value = String::from("");
                value.push(self.chars.next().unwrap());
                self.column += 1;
                self.position += 1;

                while (Some(&'A')..=Some(&'Z')).contains(&self.chars.peek())
                    || (Some(&'a')..=Some(&'z')).contains(&self.chars.peek())
                    || (Some(&'0')..=Some(&'9')).contains(&self.chars.peek())
                    || self.chars.peek() == Some(&'_')
                {
                    value.push(self.chars.next().unwrap());
                    self.column += 1;
                    self.position += 1;
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
                    integer_part.push(self.chars.next().unwrap());
                    self.column += 1;
                    self.position += 1;
                }

                // Leading zeros are not allowed, so if it's a zero it's the
                // only character of the IntergerPart
                if self.chars.peek() == Some(&'0') {
                    integer_part.push(self.chars.next().unwrap());
                    self.column += 1;
                    self.position += 1;
                } else {
                    while (Some(&'0')..=Some(&'9')).contains(&self.chars.peek()) {
                        integer_part.push(self.chars.next().unwrap());
                        self.column += 1;
                        self.position += 1;
                    }
                }

                let mut fractional_part = String::from("");
                if self.chars.peek() == Some(&'.') {
                    fractional_part.push(self.chars.next().unwrap());
                    self.column += 1;
                    self.position += 1;

                    // Append all the following digits
                    while (Some(&'0')..=Some(&'9')).contains(&self.chars.peek()) {
                        fractional_part.push(self.chars.next().unwrap());
                        self.column += 1;
                        self.position += 1;
                    }

                    if fractional_part == "." {
                        let mut next = String::from("");
                        if self.chars.peek() == None {
                            next.push_str("end of file");
                        } else {
                            next.push(self.chars.next().unwrap());
                        }
                        return Err(format!(
                            "Invalid number, expected a digit but got: '{}'",
                            next
                        ));
                    }
                }

                let mut exponent_part = String::from("");
                if self.chars.peek() == Some(&'e') || self.chars.peek() == Some(&'E') {
                    // ExponentIndicator
                    exponent_part.push(self.chars.next().unwrap());
                    self.column += 1;
                    self.position += 1;

                    // Optional sign
                    if self.chars.peek() == Some(&'+') || self.chars.peek() == Some(&'+') {
                        exponent_part.push(self.chars.next().unwrap());
                        self.column += 1;
                        self.position += 1;
                    }

                    // Append all the following digits
                    while (Some(&'0')..=Some(&'9')).contains(&self.chars.peek()) {
                        exponent_part.push(self.chars.next().unwrap());
                        self.column += 1;
                        self.position += 1;
                    }

                    if exponent_part == "." || exponent_part == ".+" || exponent_part == ".-" {
                        let mut next = String::from("");
                        if self.chars.peek() == None {
                            next.push_str("end of file");
                        } else {
                            next.push(self.chars.next().unwrap());
                        }
                        return Err(format!(
                            "Invalid number, expected a digit but got: '{}'",
                            next
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
                        "Invalid number, expected digit but got: '{}'",
                        self.chars.next().unwrap()
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

                self.chars.next();
                self.position += 1;
                self.column += 1;

                if self.chars.peek() == Some(&'"') {
                    self.chars.next();
                    self.position += 1;
                    self.column += 1;

                    if self.chars.peek() == Some(&'"') {
                        // Block string
                        self.chars.next();
                        self.position += 1;
                        self.column += 1;

                        let mut value = String::from("");
                        let mut is_done = false;
                        let mut passes = 0;
                        while !is_done {
                            let next = self.chars.next();

                            if next == None {
                                return Err(String::from("Unterminated string"));
                            }

                            if next == Some('\r') {
                                self.line += 1;
                            }
                            if next == Some('\n') && value.chars().last() != Some('\r') {
                                self.line += 1;
                            }

                            value.push(next.unwrap());
                            self.position += 1;
                            self.column += 1;
                            if passes > 0 {
                                passes -= 1
                            };

                            is_done = value.len() >= 3
                                && value[value.len() - 3..] == String::from("\"\"\"")
                                && passes == 0;

                            if value.len() >= 4
                                && value[value.len() - 4..] == String::from("\\\"\"\"")
                            {
                                is_done = false;
                                passes = 3;
                                value = value[..value.len() - 4].to_string();
                                value.push_str("\"\"\"");
                            }
                        }

                        if value.len() < 3 || value[value.len() - 3..] != String::from("\"\"\"") {
                            return Err(String::from("Unterminated string"));
                        }

                        return Ok(Some(Token {
                            kind: TokenKind::String { value },
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
                        let next = self.chars.next().unwrap();
                        self.position += 1;
                        self.column += 1;

                        if next == '\\' {
                            // Escaped characters & unicode
                            match self.chars.next() {
                                character
                                @
                                (Some('"') | Some('\\') | Some('/') | Some('b')
                                | Some('f') | Some('n') | Some('r') | Some('t')) => {
                                    value.push(character.unwrap())
                                }
                                Some('u') => {
                                    // The next 4 characters define the unicode sequence
                                    let mut unicode = String::from("");
                                    for _ in 0..4 {
                                        let unicode_char = self.chars.next();
                                        self.position += 1;
                                        self.column += 1;

                                        if unicode_char == None {
                                            return Err(format!(
                                                "Invalid character escape sequence: \\u{}",
                                                unicode
                                            ));
                                        }

                                        unicode.push(unicode_char.unwrap());
                                    }

                                    match u32::from_str_radix(&unicode, 16) {
                                        Ok(unicode_int) => {
                                            let unicode_char = char::from_u32(unicode_int);
                                            if unicode_char == None {
                                                return Err(format!(
                                                    "Invalid character escape sequence: \\u{}",
                                                    unicode
                                                ));
                                            }
                                            value.push(unicode_char.unwrap());
                                        }
                                        Err(_) => {
                                            return Err(format!(
                                                "Invalid character escape sequence: \\u{}",
                                                unicode
                                            ));
                                        }
                                    }
                                }
                                character => {
                                    return Err(format!(
                                        "Invalid character escape sequence: \\{}",
                                        if character == None {
                                            String::from("")
                                        } else {
                                            character.unwrap().to_string()
                                        }
                                    ));
                                }
                            }
                            self.position += 1;
                            self.column += 1;
                        } else if next == '\n' || next == '\r' {
                            // Line feed & carriage return
                            return Err(String::from("Unterminated string"));
                        } else {
                            // Source character
                            value.push(next);
                        }
                    }

                    if self.chars.peek() == None {
                        return Err(String::from("Unterminated string"));
                    }

                    // Remove closing quote
                    self.chars.next();
                    self.position += 1;
                    self.column += 1;

                    return Ok(Some(Token {
                        kind: TokenKind::String { value },
                        start,
                        end: self.position,
                        line: self.line,
                        column: start_column,
                    }));
                }
            }
            None => Err(String::from("Unexpected end of query string")),
            character => Err(format!("Unexpected character {}", character.unwrap())),
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>, SyntaxError> {
        match &self.error {
            // Continue looking for the next token if there is no error
            None => match self.next_no_error() {
                Ok(token) => Ok(token),
                Err(error) => {
                    self.error = Some(error.to_string());
                    return Err(SyntaxError {
                        message: error.to_string(),
                        position: self.position,
                    });
                }
            },
            // If there already is an error, continue returning the error
            err => Err(SyntaxError {
                message: String::from(err.as_ref().unwrap()),
                position: self.position,
            }),
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
}
