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

pub fn lexer(query: String) -> Result<Vec<Token>, SyntaxError> {
    let mut position = 0;
    let mut line = 0;
    let mut column = 0;

    let mut token_list: Vec<Token> = Vec::new();
    token_list.push(Token {
        kind: TokenKind::SOF,
        start: position,
        end: position,
        line,
        column,
    });

    line += 1;
    column += 1;

    let mut chars = query.chars().peekable();
    while chars.peek() != None {
        match chars.peek() {
            // ASCII controll characters are not valid source characters
            // (except for CHARACTER TABULATION, LINE FEED, and CARRIAGE RETURN)
            Some(&('\u{0}'..='\u{8}'))
            | Some(&('\u{b}'..='\u{c}'))
            | Some(&('\u{e}'..='\u{1f}')) => {
                chars.next();
                return Err(SyntaxError {
                    message: String::from("Not a valid source character"),
                    position,
                });
            }
            // UnicodeBOM, Whitespace (space and tab), and commas are all ignored token
            Some(&'\u{feff}') | Some(&'\u{9}') | Some(&' ') | Some(&',') => {
                chars.next();
                column += 1;
                position += 1;
            }
            // Line feed is an ignored token
            Some(&'\u{a}') => {
                chars.next();
                line += 1;
                column = 1;
                position += 1;
            }
            // Carriage return is an ignored token
            Some(&'\u{d}') => {
                chars.next();
                line += 1;
                column = 1;
                position += 1;

                // If a carriage return is followed by a line feed, both chars
                // together are interpreted as one line break character.
                if chars.peek() == Some(&'\u{a}') {
                    chars.next();
                }
            }
            // A comment is an ignored token, but since it does contain human
            // readable information, we append a token to the list where the
            // value contains the list of comment chars.
            Some(&'#') => {
                let start = position;
                let start_column = column;

                // Skip the '#' character
                chars.next();
                column += 1;
                position += 1;

                // Combine all following characters to get the value of the
                // comment token until a line feed, carriage return, of the
                // end of file is reached.
                let mut value = String::from("");
                while (chars.peek() != Some(&'\u{a}'))
                    && (chars.peek() != Some(&'\u{d}') && (chars.peek() != None))
                {
                    value.push(chars.next().unwrap());
                    column += 1;
                    position += 1;
                }

                let comment_token = Token {
                    kind: TokenKind::Comment { value },
                    start,
                    end: position,
                    line,
                    column: start_column,
                };
                token_list.push(comment_token);
            }
            // Punctuators
            Some(&'!') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::ExclamationMark,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'$') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::DollarSign,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'&') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::Ampersand,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'(') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::RoundBracketOpening,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&')') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::RoundBracketClosing,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'.') => {
                // At this point we expect to see two more dots as the only
                // valid token is the spread token. (The only other point where
                // a dot could show up is in a float value or inside a string
                // value, both are handled separately.)

                // Skip the first dot
                chars.next();
                column += 1;
                position += 1;

                // Check that the next two chars are also dots
                for _ in 0..2 {
                    let next = chars.next();
                    if next != Some('.') {
                        return Err(SyntaxError {
                            message: format!("Expected '.' but got {}", next.unwrap()),
                            position,
                        });
                    }
                    column += 1;
                    position += 1;
                }

                token_list.push(Token {
                    kind: TokenKind::Spread,
                    start: position - 3,
                    end: position,
                    line,
                    column: column - 3,
                });
            }
            Some(&':') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::Colon,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'=') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::EqualsSign,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'@') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::AtSign,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'[') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::SquareBracketOpening,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&']') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::SquareBracketClosing,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'{') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::CurlyBracketOpening,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'|') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::VerticalBar,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            Some(&'}') => {
                chars.next();
                token_list.push(Token {
                    kind: TokenKind::CurlyBracketClosing,
                    start: position,
                    end: position + 1,
                    line,
                    column,
                });
                column += 1;
                position += 1;
            }
            // Name token
            Some(&('A'..='Z')) | Some(&('a'..='z')) | Some(&'_') => {
                let start = position;
                let start_column = column;

                let mut value = String::from("");
                value.push(chars.next().unwrap());
                column += 1;
                position += 1;

                while (Some(&'A')..=Some(&'Z')).contains(&chars.peek())
                    || (Some(&'a')..=Some(&'z')).contains(&chars.peek())
                    || (Some(&'0')..=Some(&'9')).contains(&chars.peek())
                    || chars.peek() == Some(&'_')
                {
                    value.push(chars.next().unwrap());
                    column += 1;
                    position += 1;
                }

                token_list.push(Token {
                    kind: TokenKind::Name { value },
                    start,
                    end: position,
                    line,
                    column: start_column,
                })
            }
            // Int or Float token
            Some(&'-') | Some(&('0'..='9')) => {
                let start = position;
                let start_column = column;

                let mut integer_part = String::from("");

                // Optional negative sign
                if chars.peek() == Some(&'-') {
                    integer_part.push(chars.next().unwrap());
                    column += 1;
                    position += 1;
                }

                // Leading zeros are not allowed, so if it's a zero it's the
                // only character of the IntergerPart
                if chars.peek() == Some(&'0') {
                    integer_part.push(chars.next().unwrap());
                    column += 1;
                    position += 1;
                } else {
                    while (Some(&'0')..=Some(&'9')).contains(&chars.peek()) {
                        integer_part.push(chars.next().unwrap());
                        column += 1;
                        position += 1;
                    }
                }

                let mut fractional_part = String::from("");
                if chars.peek() == Some(&'.') {
                    fractional_part.push(chars.next().unwrap());
                    column += 1;
                    position += 1;

                    // Append all the following digits
                    while (Some(&'0')..=Some(&'9')).contains(&chars.peek()) {
                        fractional_part.push(chars.next().unwrap());
                        column += 1;
                        position += 1;
                    }

                    if fractional_part == "." {
                        let mut next = String::from("");
                        if chars.peek() == None {
                            next.push_str("end of file");
                        } else {
                            next.push(chars.next().unwrap());
                        }
                        return Err(SyntaxError {
                            message: format!("Expected a digit but got {}", next),
                            position,
                        });
                    }
                }

                let mut exponent_part = String::from("");
                if chars.peek() == Some(&'e') || chars.peek() == Some(&'E') {
                    // ExponentIndicator
                    exponent_part.push(chars.next().unwrap());
                    column += 1;
                    position += 1;

                    // Optional sign
                    if chars.peek() == Some(&'+') || chars.peek() == Some(&'+') {
                        exponent_part.push(chars.next().unwrap());
                        column += 1;
                        position += 1;
                    }

                    // Append all the following digits
                    while (Some(&'0')..=Some(&'9')).contains(&chars.peek()) {
                        exponent_part.push(chars.next().unwrap());
                        column += 1;
                        position += 1;
                    }

                    if exponent_part == "." || exponent_part == ".+" || exponent_part == ".-" {
                        let mut next = String::from("");
                        if chars.peek() == None {
                            next.push_str("end of file");
                        } else {
                            next.push(chars.next().unwrap());
                        }
                        return Err(SyntaxError {
                            message: format!("Expected a digit but got {}", next),
                            position,
                        });
                    }
                }

                if (Some(&'0')..=Some(&'9')).contains(&chars.peek())
                    || (Some(&'a')..=Some(&'z')).contains(&chars.peek())
                    || (Some(&'A')..=Some(&'Z')).contains(&chars.peek())
                    || chars.peek() == Some(&'_')
                    || chars.peek() == Some(&'.')
                {
                    return Err(SyntaxError {
                        message: format!("Unexpected character {}", chars.next().unwrap()),
                        position,
                    });
                }

                if fractional_part == "" && exponent_part == "" {
                    // It's an integer
                    token_list.push(Token {
                        kind: TokenKind::Int {
                            value: integer_part,
                        },
                        start,
                        end: position,
                        line,
                        column: start_column,
                    })
                } else {
                    // It's a float
                    let mut value = String::from("");
                    value.push_str(&integer_part);
                    value.push_str(&fractional_part);
                    value.push_str(&exponent_part);
                    token_list.push(Token {
                        kind: TokenKind::Float { value },
                        start,
                        end: position,
                        line,
                        column: start_column,
                    })
                }
            }
            None => {
                return Err(SyntaxError {
                    message: String::from("Unexpected end of query string"),
                    position,
                })
            }
            _ => {
                // TODO: throw an error, this should never happen
                chars.next();
                position += 1;
                column += 1;
            }
        }
    }

    token_list.push(Token {
        kind: TokenKind::EOF,
        start: position,
        end: position,
        line,
        column,
    });
    return Ok(token_list);
}
