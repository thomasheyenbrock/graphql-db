use crate::error::SyntaxError;
use crate::lexer::{Lexer, Token, TokenKind};
use vec1::Vec1;

pub struct Parser<'a> {
  lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
  fn new(query: &str) -> Parser {
    Parser {
      lexer: Lexer::new(query),
    }
  }

  fn error(&mut self, message: String) -> SyntaxError {
    SyntaxError {
      message,
      position: self.lexer.get_position(),
    }
  }

  fn next_token(&mut self, expected: Option<TokenKind>) -> Result<Token, SyntaxError> {
    match self.lexer.next()? {
      None => Err(self.error(match expected {
        None => format!("Unexpected {}.", TokenKind::EOF),
        Some(expected) => {
          format!("Expected {}, found {}.", expected, TokenKind::EOF)
        }
      })),
      Some(token) => match token.kind {
        TokenKind::Comment => self.next_token(expected),
        _ => Ok(token),
      },
    }
  }

  fn peek_token(&mut self, expected: Option<TokenKind>) -> Result<Token, SyntaxError> {
    match self.lexer.peek()? {
      None => Err(self.error(match expected {
        None => format!("Unexpected {}.", TokenKind::EOF),
        Some(expected) => format!("Expected {}, found {}.", expected, TokenKind::EOF),
      })),
      Some(token) => match token.kind {
        TokenKind::Comment => {
          self.lexer.next()?;
          self.peek_token(expected)
        }
        _ => Ok(token),
      },
    }
  }

  fn parse_token(&mut self, token_kind: TokenKind) -> Result<Token, SyntaxError> {
    let token = self.next_token(Some(token_kind.clone()))?;
    let cloned_token = token.clone();
    if token_kind.clone().equals(token.kind) {
      Ok(cloned_token)
    } else {
      Err(SyntaxError {
        message: format!("Expected {}, found {}.", token_kind, cloned_token.kind),
        position: token.start,
      })
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct Loc {
  pub start_token: Token,
  pub end_token: Token,
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
pub enum OperationType {
  query,
  mutation,
  subscription,
}

impl std::fmt::Display for OperationType {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      OperationType::query => write!(f, "query"),
      OperationType::mutation => write!(f, "mutation"),
      OperationType::subscription => write!(f, "subscription"),
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct Name {
  pub value: String,
  pub loc: Loc,
}

impl Name {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let token = p.parse_token(TokenKind::Name)?;
    let loc = Loc {
      start_token: token.clone(),
      end_token: token.clone(),
    };
    Ok(Name {
      value: token.value,
      loc,
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct NamedType {
  pub name: Name,
  pub loc: Loc,
}

impl NamedType {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let name = Name::parse(p)?;
    let start_token = name.loc.start_token.clone();
    let end_token = name.loc.end_token.clone();
    Ok(NamedType {
      name,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct ListType {
  pub gql_type: Box<Type>,
  pub loc: Loc,
}

impl ListType {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::SquareBracketOpening)?;
    let gql_type = Type::parse(p)?;
    let end_token = p.parse_token(TokenKind::SquareBracketClosing)?;
    Ok(ListType {
      gql_type: Box::new(gql_type),
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct NonNullType {
  pub gql_type: NullableType,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub enum NullableType {
  NamedType(NamedType),
  ListType(ListType),
}

#[derive(Debug, PartialEq)]
pub enum Type {
  NamedType(NamedType),
  ListType(ListType),
  NonNullType(NonNullType),
}

impl Type {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let peeked = p.peek_token(Some(TokenKind::Name))?;
    match peeked.kind {
      TokenKind::SquareBracketOpening => {
        let list_type = ListType::parse(p)?;
        if p.peek_token(None)?.kind == TokenKind::ExclamationMark {
          let start_token = list_type.loc.start_token.clone();
          let end_token = p.parse_token(TokenKind::ExclamationMark)?;
          Ok(Type::NonNullType(NonNullType {
            gql_type: NullableType::ListType(list_type),
            loc: Loc {
              start_token,
              end_token,
            },
          }))
        } else {
          Ok(Type::ListType(list_type))
        }
      }
      TokenKind::Name => {
        let named_type = NamedType::parse(p)?;
        if p.peek_token(None)?.kind == TokenKind::ExclamationMark {
          let start_token = named_type.loc.start_token.clone();
          let end_token = p.parse_token(TokenKind::ExclamationMark)?;
          Ok(Type::NonNullType(NonNullType {
            gql_type: NullableType::NamedType(named_type),
            loc: Loc {
              start_token,
              end_token,
            },
          }))
        } else {
          Ok(Type::NamedType(named_type))
        }
      }
      _ => Err(p.error(format!("Expected {}, found {}.", TokenKind::Name, peeked))),
    }
  }

  fn get_end_token(&self) -> Token {
    match self {
      Type::NamedType(named_type) => named_type.loc.end_token.clone(),
      Type::ListType(list_type) => list_type.loc.end_token.clone(),
      Type::NonNullType(non_null_type) => non_null_type.loc.end_token.clone(),
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct Variable {
  pub name: Name,
  pub loc: Loc,
}

impl Variable {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::DollarSign)?;
    let name = Name::parse(p)?;
    let end_token = name.loc.end_token.clone();
    Ok(Variable {
      name,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct IntValue {
  pub value: String,
  pub loc: Loc,
}

impl IntValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let int_token = p.parse_token(TokenKind::Int)?;
    let start_token = int_token.clone();
    let end_token = int_token.clone();
    Ok(IntValue {
      value: int_token.value,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct FloatValue {
  pub value: String,
  pub loc: Loc,
}

impl FloatValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let float_token = p.parse_token(TokenKind::Float)?;
    let start_token = float_token.clone();
    let end_token = float_token.clone();
    Ok(FloatValue {
      value: float_token.value,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct StringValue {
  pub value: String,
  pub block: bool,
  pub loc: Loc,
}

impl StringValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let string_token = p.parse_token(TokenKind::String { block: false })?;
    let start_token = string_token.clone();
    let end_token = string_token.clone();
    Ok(StringValue {
      value: string_token.value,
      // We know that the token kind is actually a stirng, but Rust can't infer this
      block: match string_token.kind {
        TokenKind::String { block } => block,
        // This can never happen
        kind => panic!("Expected \"String\", found {}.", kind),
      },
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct BooleanValue {
  pub value: bool,
  pub loc: Loc,
}

impl BooleanValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let token = p.parse_token(TokenKind::Name)?;
    if token.value != "true" && token.value != "false" {
      Err(SyntaxError {
        message: format!("Expected {}, found {}.", TokenKind::Name, token),
        position: token.start,
      })
    } else {
      let start_token = token.clone();
      let end_token = token.clone();
      Ok(BooleanValue {
        value: token.value == "true",
        loc: Loc {
          start_token,
          end_token,
        },
      })
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct NullValue {
  pub loc: Loc,
}

impl NullValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let token = p.parse_token(TokenKind::Name)?;
    if token.value != "null" {
      Err(SyntaxError {
        message: format!("Expected {}, found {}.", TokenKind::Name, token),
        position: token.start,
      })
    } else {
      Ok(NullValue {
        loc: Loc {
          start_token: token.clone(),
          end_token: token,
        },
      })
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct EnumValue {
  pub value: String,
  pub loc: Loc,
}

impl EnumValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let token = p.parse_token(TokenKind::Name)?;
    if token.value == "true" || token.value == "false" || token.value == "null" {
      Err(SyntaxError {
        message: format!("Expected {}, found {}.", TokenKind::Name, token),
        position: token.start,
      })
    } else {
      let start_token = token.clone();
      let end_token = token.clone();
      Ok(EnumValue {
        value: token.value,
        loc: Loc {
          start_token,
          end_token,
        },
      })
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct ListValue {
  pub values: Vec<Value>,
  pub loc: Loc,
}

impl ListValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::SquareBracketOpening)?;

    let mut values = vec![];
    let mut peeked = p.peek_token(None)?;
    while peeked.kind != TokenKind::SquareBracketClosing {
      values.push(Value::parse(p)?);
      peeked = p.peek_token(None)?
    }

    let end_token = p.parse_token(TokenKind::SquareBracketClosing)?;
    Ok(ListValue {
      values,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct ObjectField {
  pub name: Name,
  pub value: Value,
  pub loc: Loc,
}

impl ObjectField {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let name = Name::parse(p)?;
    p.parse_token(TokenKind::Colon)?;
    let value = Value::parse(p)?;

    let start_token = name.loc.start_token.clone();
    let end_token = value.get_end_token();

    Ok(ObjectField {
      name,
      value,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct ObjectValue {
  pub fields: Vec<ObjectField>,
  pub loc: Loc,
}

impl ObjectValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::CurlyBracketOpening)?;

    let mut fields = vec![];
    let mut peeked = p.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::CurlyBracketClosing {
      fields.push(ObjectField::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::Name))?;
    }

    let end_token = p.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok(ObjectValue {
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub enum Value {
  Variable(Variable),
  IntValue(IntValue),
  FloatValue(FloatValue),
  StringValue(StringValue),
  BooleanValue(BooleanValue),
  NullValue(NullValue),
  EnumValue(EnumValue),
  ListValue(ListValue),
  ObjectValue(ObjectValue),
}

impl Value {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let peeked = p.peek_token(None)?;
    match peeked.kind {
      TokenKind::DollarSign => Ok(Value::Variable(Variable::parse(p)?)),
      TokenKind::Int => Ok(Value::IntValue(IntValue::parse(p)?)),
      TokenKind::Float => Ok(Value::FloatValue(FloatValue::parse(p)?)),
      TokenKind::String { .. } => Ok(Value::StringValue(StringValue::parse(p)?)),
      TokenKind::Name => {
        if peeked.value == "true" || peeked.value == "false" {
          Ok(Value::BooleanValue(BooleanValue::parse(p)?))
        } else if peeked.value == "null" {
          Ok(Value::NullValue(NullValue::parse(p)?))
        } else {
          Ok(Value::EnumValue(EnumValue::parse(p)?))
        }
      }
      TokenKind::SquareBracketOpening => Ok(Value::ListValue(ListValue::parse(p)?)),
      TokenKind::CurlyBracketOpening => Ok(Value::ObjectValue(ObjectValue::parse(p)?)),
      _ => Err(p.error(format!("Unexpected {}.", peeked))),
    }
  }

  fn get_end_token(&self) -> Token {
    match self {
      Value::Variable(variable) => variable.loc.end_token.clone(),
      Value::IntValue(int_value) => int_value.loc.end_token.clone(),
      Value::FloatValue(float_value) => float_value.loc.end_token.clone(),
      Value::StringValue(string_value) => string_value.loc.end_token.clone(),
      Value::BooleanValue(boolean_value) => boolean_value.loc.end_token.clone(),
      Value::NullValue(null_value) => null_value.loc.end_token.clone(),
      Value::EnumValue(enum_value) => enum_value.loc.end_token.clone(),
      Value::ListValue(list_value) => list_value.loc.end_token.clone(),
      Value::ObjectValue(object_value) => object_value.loc.end_token.clone(),
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct ConstListValue {
  pub values: Vec<ConstValue>,
  pub loc: Loc,
}

impl ConstListValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::SquareBracketOpening)?;

    let mut values = vec![];
    let mut peeked = p.peek_token(None)?;
    while peeked.kind != TokenKind::SquareBracketClosing {
      values.push(ConstValue::parse(p)?);
      peeked = p.peek_token(None)?
    }

    let end_token = p.parse_token(TokenKind::SquareBracketClosing)?;

    Ok(ConstListValue {
      values,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct ConstObjectField {
  pub name: Name,
  pub value: ConstValue,
  pub loc: Loc,
}

impl ConstObjectField {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let name = Name::parse(p)?;
    p.parse_token(TokenKind::Colon)?;
    let value = ConstValue::parse(p)?;

    let start_token = name.loc.start_token.clone();
    let end_token = value.get_end_token();

    Ok(ConstObjectField {
      name,
      value,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct ConstObjectValue {
  pub fields: Vec<ConstObjectField>,
  pub loc: Loc,
}

impl ConstObjectValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::CurlyBracketOpening)?;

    let mut fields = vec![];
    let mut peeked = p.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::CurlyBracketClosing {
      fields.push(ConstObjectField::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::Name))?;
    }

    let end_token = p.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok(ConstObjectValue {
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub enum ConstValue {
  IntValue(IntValue),
  FloatValue(FloatValue),
  StringValue(StringValue),
  BooleanValue(BooleanValue),
  NullValue(NullValue),
  EnumValue(EnumValue),
  ListValue(ConstListValue),
  ObjectValue(ConstObjectValue),
}

impl ConstValue {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let peeked = p.peek_token(None)?;
    match peeked.kind {
      TokenKind::Int => Ok(ConstValue::IntValue(IntValue::parse(p)?)),
      TokenKind::Float => Ok(ConstValue::FloatValue(FloatValue::parse(p)?)),
      TokenKind::String { .. } => Ok(ConstValue::StringValue(StringValue::parse(p)?)),
      TokenKind::Name => {
        if peeked.value == "true" || peeked.value == "false" {
          Ok(ConstValue::BooleanValue(BooleanValue::parse(p)?))
        } else if peeked.value == "null" {
          Ok(ConstValue::NullValue(NullValue::parse(p)?))
        } else {
          Ok(ConstValue::EnumValue(EnumValue::parse(p)?))
        }
      }
      TokenKind::SquareBracketOpening => Ok(ConstValue::ListValue(ConstListValue::parse(p)?)),
      TokenKind::CurlyBracketOpening => Ok(ConstValue::ObjectValue(ConstObjectValue::parse(p)?)),
      _ => Err(p.error(format!("Unexpected {}.", peeked))),
    }
  }

  fn get_end_token(&self) -> Token {
    match self {
      ConstValue::IntValue(int_value) => int_value.loc.end_token.clone(),
      ConstValue::FloatValue(float_value) => float_value.loc.end_token.clone(),
      ConstValue::StringValue(string_value) => string_value.loc.end_token.clone(),
      ConstValue::BooleanValue(boolean_value) => boolean_value.loc.end_token.clone(),
      ConstValue::NullValue(null_value) => null_value.loc.end_token.clone(),
      ConstValue::EnumValue(enum_value) => enum_value.loc.end_token.clone(),
      ConstValue::ListValue(list_value) => list_value.loc.end_token.clone(),
      ConstValue::ObjectValue(object_value) => object_value.loc.end_token.clone(),
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct Argument {
  pub name: Name,
  pub value: Value,
  pub loc: Loc,
}

impl Argument {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let name = Name::parse(p)?;
    p.parse_token(TokenKind::Colon)?;
    let value = Value::parse(p)?;

    let start_token = name.loc.start_token.clone();
    let end_token = value.get_end_token();

    Ok(Argument {
      name,
      value,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_many(
    p: &mut Parser,
    expected: Option<TokenKind>,
  ) -> Result<(Vec<Self>, Option<Token>), SyntaxError> {
    if p.peek_token(expected)?.kind != TokenKind::RoundBracketOpening {
      return Ok((vec![], None));
    }

    p.parse_token(TokenKind::RoundBracketOpening)?;

    let mut arguments = vec![];
    let mut peeked = p.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::RoundBracketClosing {
      arguments.push(Argument::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::Name))?;
    }

    let round_bracket_closing_token = p.parse_token(TokenKind::RoundBracketClosing)?;

    Ok((arguments, Some(round_bracket_closing_token)))
  }
}

#[derive(Debug, PartialEq)]
pub struct ConstArgument {
  pub name: Name,
  pub value: ConstValue,
  pub loc: Loc,
}

impl ConstArgument {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let name = Name::parse(p)?;
    p.parse_token(TokenKind::Colon)?;
    let value = ConstValue::parse(p)?;

    let start_token = name.loc.start_token.clone();
    let end_token = value.get_end_token();

    Ok(ConstArgument {
      name,
      value,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_many(
    p: &mut Parser,
    expected: Option<TokenKind>,
  ) -> Result<(Vec<Self>, Option<Token>), SyntaxError> {
    if p.peek_token(expected)?.kind != TokenKind::RoundBracketOpening {
      return Ok((vec![], None));
    }

    p.parse_token(TokenKind::RoundBracketOpening)?;

    let mut arguments = vec![];
    let mut peeked = p.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::RoundBracketClosing {
      arguments.push(ConstArgument::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::Name))?;
    }

    let round_bracket_closing_token = p.parse_token(TokenKind::RoundBracketClosing)?;

    Ok((arguments, Some(round_bracket_closing_token)))
  }
}

#[derive(Debug, PartialEq)]
pub struct Directive {
  pub name: Name,
  pub arguments: Vec<Argument>,
  pub loc: Loc,
}

impl Directive {
  fn parse(p: &mut Parser, expected: Option<TokenKind>) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::AtSign)?;
    let name = Name::parse(p)?;

    let (arguments, arguments_end_token) = Argument::parse_many(p, expected)?;

    let end_token = if arguments_end_token != None {
      arguments_end_token.unwrap()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Directive {
      name,
      arguments,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_many(p: &mut Parser, expected: Option<TokenKind>) -> Result<Vec<Self>, SyntaxError> {
    let expected_clone = expected.clone();

    let mut directives = vec![];
    let mut peeked = p.peek_token(expected)?;
    while peeked.kind == TokenKind::AtSign {
      directives.push(Directive::parse(p, expected_clone.clone())?);
      peeked = p.peek_token(expected_clone.clone())?;
    }
    Ok(directives)
  }
}

#[derive(Debug, PartialEq)]
pub struct ConstDirective {
  pub name: Name,
  pub arguments: Vec<ConstArgument>,
  pub loc: Loc,
}

impl ConstDirective {
  fn parse(p: &mut Parser, expected: Option<TokenKind>) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::AtSign)?;
    let name = Name::parse(p)?;

    let (arguments, arguments_end_token) = ConstArgument::parse_many(p, expected)?;

    let end_token = if arguments_end_token != None {
      arguments_end_token.unwrap()
    } else {
      name.loc.end_token.clone()
    };

    Ok(ConstDirective {
      name,
      arguments,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_many(p: &mut Parser, expected: Option<TokenKind>) -> Result<Vec<Self>, SyntaxError> {
    let expected_clone = expected.clone();

    let mut directives = vec![];
    let mut peeked = p.peek_token(expected)?;
    while peeked.kind == TokenKind::AtSign {
      directives.push(ConstDirective::parse(p, expected_clone.clone())?);
      peeked = p.peek_token(expected_clone.clone())?;
    }
    Ok(directives)
  }
}

#[derive(Debug, PartialEq)]
pub struct VariableDefinition {
  pub variable: Variable,
  pub gql_type: Type,
  pub default_value: Option<ConstValue>,
  pub directives: Vec<Directive>,
  pub loc: Loc,
}

impl VariableDefinition {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let variable = Variable::parse(p)?;
    p.parse_token(TokenKind::Colon)?;
    let gql_type = Type::parse(p)?;

    let default_value = if p.peek_token(Some(TokenKind::DollarSign))?.kind == TokenKind::EqualsSign
    {
      p.parse_token(TokenKind::EqualsSign)?;
      Some(ConstValue::parse(p)?)
    } else {
      None
    };

    let directives = Directive::parse_many(p, Some(TokenKind::DollarSign))?;

    let start_token = variable.loc.start_token.clone();
    let end_token = if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else if default_value != None {
      default_value.as_ref().unwrap().get_end_token()
    } else {
      gql_type.get_end_token()
    };

    Ok(VariableDefinition {
      variable,
      gql_type,
      default_value,
      directives,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_many(p: &mut Parser) -> Result<Vec<Self>, SyntaxError> {
    if p.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind != TokenKind::RoundBracketOpening {
      return Ok(vec![]);
    }

    p.parse_token(TokenKind::RoundBracketOpening)?;
    // There must be at least one variable
    let mut variable_definitions = vec![VariableDefinition::parse(p)?];
    let mut peeked = p.peek_token(Some(TokenKind::DollarSign))?;
    while peeked.kind == TokenKind::DollarSign {
      variable_definitions.push(VariableDefinition::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::DollarSign))?;
    }

    p.parse_token(TokenKind::RoundBracketClosing)?;

    Ok(variable_definitions)
  }
}

#[derive(Debug, PartialEq)]
pub enum Selection {
  Field {
    name: Name,
    alias: Option<Name>,
    arguments: Vec<Argument>,
    directives: Vec<Directive>,
    selection_set: Option<SelectionSet>,
    loc: Loc,
  },
  FragmentSpread {
    name: Name,
    directives: Vec<Directive>,
    loc: Loc,
  },
  InlineFragment {
    type_condition: Option<NamedType>,
    directives: Vec<Directive>,
    selection_set: SelectionSet,
    loc: Loc,
  },
}

impl Selection {
  fn parse_field(p: &mut Parser) -> Result<Self, SyntaxError> {
    let alias_or_name = Name::parse(p)?;
    let start_token = alias_or_name.loc.start_token.clone();

    let peeked = p.peek_token(None)?;
    let (alias, name) = if peeked.kind == TokenKind::Colon {
      p.parse_token(TokenKind::Colon)?;
      (Some(alias_or_name), Name::parse(p)?)
    } else {
      (None, alias_or_name)
    };

    let (arguments, arguments_end_token) = Argument::parse_many(p, Some(TokenKind::Name))?;

    let directives = Directive::parse_many(p, Some(TokenKind::Name))?;

    let selection_set =
      if p.peek_token(Some(TokenKind::Name))?.kind == TokenKind::CurlyBracketOpening {
        Some(SelectionSet::parse(p)?)
      } else {
        None
      };

    let end_token = if selection_set != None {
      selection_set.as_ref().unwrap().loc.end_token.clone()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else if arguments_end_token != None {
      arguments_end_token.unwrap()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Selection::Field {
      name,
      alias,
      arguments,
      directives,
      selection_set,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_fragment_spread(p: &mut Parser, start_token: Token) -> Result<Self, SyntaxError> {
    let name = Name::parse(p)?;

    let directives = Directive::parse_many(p, Some(TokenKind::Name))?;

    let end_token = if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Selection::FragmentSpread {
      name,
      directives,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_inline_fragment(p: &mut Parser, start_token: Token) -> Result<Self, SyntaxError> {
    let peeked = p.peek_token(Some(TokenKind::Name))?;
    let type_condition = if peeked.kind == TokenKind::Name && peeked.value == "on" {
      p.parse_token(TokenKind::Name)?;
      Some(NamedType::parse(p)?)
    } else {
      None
    };

    let directives = Directive::parse_many(p, Some(TokenKind::Name))?;

    let selection_set = SelectionSet::parse(p)?;

    let end_token = selection_set.loc.end_token.clone();

    Ok(Selection::InlineFragment {
      type_condition,
      directives,
      selection_set,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let peeked = p.peek_token(Some(TokenKind::Name))?;
    match peeked.kind {
      TokenKind::Name => Selection::parse_field(p),
      TokenKind::Spread => {
        let start_token = p.parse_token(TokenKind::Spread)?;
        let peeked = p.peek_token(Some(TokenKind::CurlyBracketOpening))?;
        match peeked.kind {
          TokenKind::Name => {
            if p.peek_token(Some(TokenKind::Name))?.value == "on" {
              Selection::parse_inline_fragment(p, start_token)
            } else {
              Selection::parse_fragment_spread(p, start_token)
            }
          }
          TokenKind::AtSign => Selection::parse_inline_fragment(p, start_token),
          TokenKind::CurlyBracketOpening => Selection::parse_inline_fragment(p, start_token),
          _ => Err(p.error(format!(
            "Expected {}, found {}.",
            TokenKind::CurlyBracketOpening,
            peeked
          ))),
        }
      }
      _ => Err(p.error(format!("Expected {}, found {}.", TokenKind::Name, peeked))),
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct SelectionSet {
  pub selections: Vec1<Selection>,
  pub loc: Loc,
}

impl SelectionSet {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::CurlyBracketOpening)?;
    let mut selections = vec1![Selection::parse(p)?];

    let mut peeked = p.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::CurlyBracketClosing {
      selections.push(Selection::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::Name))?;
    }

    let end_token = p.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok(SelectionSet {
      selections,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct OperationTypeDefinition {
  pub operation: OperationType,
  pub gql_type: NamedType,
  pub loc: Loc,
}

impl OperationTypeDefinition {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::Name)?;

    let operation = if start_token.value == "query" {
      OperationType::query
    } else if start_token.value == "mutation" {
      OperationType::mutation
    } else if start_token.value == "subscription" {
      OperationType::subscription
    } else {
      return Err(SyntaxError {
        message: format!("Unexpected name \"{}\".", start_token.value),
        position: start_token.start,
      });
    };

    p.parse_token(TokenKind::Colon)?;

    let gql_type = NamedType::parse(p)?;

    let end_token = gql_type.loc.end_token.clone();

    Ok(OperationTypeDefinition {
      operation,
      gql_type,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

#[derive(Debug, PartialEq)]
pub struct InputValueDefinition {
  pub description: Option<StringValue>,
  pub name: Name,
  pub gql_type: Type,
  pub default_value: Option<ConstValue>,
  pub directives: Vec<ConstDirective>,
  pub loc: Loc,
}

impl InputValueDefinition {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let description = if p
      .peek_token(Some(TokenKind::Name))?
      .kind
      .equals(TokenKind::String { block: false })
    {
      Some(StringValue::parse(p)?)
    } else {
      None
    };

    let name = Name::parse(p)?;

    p.parse_token(TokenKind::Colon)?;

    let gql_type = Type::parse(p)?;

    let default_value = if p.peek_token(Some(TokenKind::Name))?.kind == TokenKind::EqualsSign {
      p.parse_token(TokenKind::EqualsSign)?;
      Some(ConstValue::parse(p)?)
    } else {
      None
    };

    let directives = ConstDirective::parse_many(p, Some(TokenKind::Name))?;

    let start_token = if description != None {
      description.as_ref().unwrap().loc.start_token.clone()
    } else {
      name.loc.start_token.clone()
    };
    let end_token = if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else if default_value != None {
      default_value.as_ref().unwrap().get_end_token()
    } else {
      gql_type.get_end_token()
    };

    Ok(InputValueDefinition {
      description,
      name,
      gql_type,
      default_value,
      directives,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_many(
    p: &mut Parser,
    opening: TokenKind,
    closing: TokenKind,
  ) -> Result<(Vec<Self>, Option<Token>), SyntaxError> {
    if p.peek_token(None)?.kind != opening {
      return Ok((vec![], None));
    }

    p.parse_token(opening)?;

    let mut argument_definitions = vec![];
    let mut peeked = p.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != closing {
      argument_definitions.push(InputValueDefinition::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::Name))?;
    }

    let closing_token = p.parse_token(closing)?;

    Ok((argument_definitions, Some(closing_token)))
  }
}

#[derive(Debug, PartialEq)]
pub struct FieldDefinition {
  pub description: Option<StringValue>,
  pub name: Name,
  pub arguments: Vec<InputValueDefinition>,
  pub gql_type: Type,
  pub directives: Vec<ConstDirective>,
  pub loc: Loc,
}

impl FieldDefinition {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let description = if p
      .peek_token(Some(TokenKind::Name))?
      .kind
      .equals(TokenKind::String { block: false })
    {
      Some(StringValue::parse(p)?)
    } else {
      None
    };

    let name = Name::parse(p)?;

    let (arguments, _) = InputValueDefinition::parse_many(
      p,
      TokenKind::RoundBracketOpening,
      TokenKind::RoundBracketClosing,
    )?;

    p.parse_token(TokenKind::Colon)?;

    let gql_type = Type::parse(p)?;

    let directives = ConstDirective::parse_many(p, None)?;

    let start_token = if description != None {
      description.as_ref().unwrap().loc.start_token.clone()
    } else {
      name.loc.start_token.clone()
    };
    let end_token = if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      gql_type.get_end_token()
    };

    Ok(FieldDefinition {
      description,
      name,
      arguments,
      gql_type,
      directives,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_many(p: &mut Parser) -> Result<(Vec<Self>, Option<Token>), SyntaxError> {
    if p.peek_token(None)?.kind != TokenKind::CurlyBracketOpening {
      return Ok((vec![], None));
    }

    p.parse_token(TokenKind::CurlyBracketOpening)?;

    let mut field_definitions = vec![FieldDefinition::parse(p)?];
    let mut peeked = p.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::CurlyBracketClosing {
      field_definitions.push(FieldDefinition::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::Name))?;
    }

    let curly_bracket_closing_token = p.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok((field_definitions, Some(curly_bracket_closing_token)))
  }
}

#[derive(Debug, PartialEq)]
pub struct EnumValueDefinition {
  pub description: Option<StringValue>,
  pub enum_value: EnumValue,
  pub directives: Vec<ConstDirective>,
  pub loc: Loc,
}

impl EnumValueDefinition {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let description = if p
      .peek_token(Some(TokenKind::Name))?
      .kind
      .equals(TokenKind::String { block: false })
    {
      Some(StringValue::parse(p)?)
    } else {
      None
    };

    let enum_value = EnumValue::parse(p)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::Name))?;

    let start_token = if description != None {
      description.as_ref().unwrap().loc.start_token.clone()
    } else {
      enum_value.loc.start_token.clone()
    };
    let end_token = if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      enum_value.loc.end_token.clone()
    };

    Ok(EnumValueDefinition {
      description,
      enum_value,
      directives,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_many(p: &mut Parser) -> Result<(Vec<Self>, Option<Token>), SyntaxError> {
    if p.peek_token(None)?.kind != TokenKind::CurlyBracketOpening {
      return Ok((vec![], None));
    }

    p.parse_token(TokenKind::CurlyBracketOpening)?;

    let mut values = vec![];
    let mut peeked = p.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::CurlyBracketClosing {
      values.push(EnumValueDefinition::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::Name))?;
    }

    let curly_bracket_closing_token = p.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok((values, Some(curly_bracket_closing_token)))
  }
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
pub enum DirectiveLocationName {
  // Executable
  QUERY,
  MUTATION,
  SUBSCRIPTION,
  FIELD,
  FRAGMENT_DEFINITION,
  FRAGMENT_SPREAD,
  INLINE_FRAGMENT,
  VARIABLE_DEFINITION,
  // Type system
  SCHEMA,
  SCALAR,
  OBJECT,
  FIELD_DEFINITION,
  ARGUMENT_DEFINITION,
  INTERFACE,
  UNION,
  ENUM,
  ENUM_VALUE,
  INPUT_OBJECT,
  INPUT_FIELD_DEFINITION,
}

#[derive(Debug, PartialEq)]
pub struct DirectiveLocation {
  pub name: DirectiveLocationName,
  pub loc: Loc,
}

impl DirectiveLocation {
  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let location = p.parse_token(TokenKind::Name)?;
    let loc = Loc {
      start_token: location.clone(),
      end_token: location.clone(),
    };
    let name = if location.value == "QUERY" {
      DirectiveLocationName::QUERY
    } else if location.value == "MUTATION" {
      DirectiveLocationName::MUTATION
    } else if location.value == "SUBSCRIPTION" {
      DirectiveLocationName::SUBSCRIPTION
    } else if location.value == "FIELD" {
      DirectiveLocationName::FIELD
    } else if location.value == "FRAGMENT_DEFINITION" {
      DirectiveLocationName::FRAGMENT_DEFINITION
    } else if location.value == "FRAGMENT_SPREAD" {
      DirectiveLocationName::FRAGMENT_SPREAD
    } else if location.value == "INLINE_FRAGMENT" {
      DirectiveLocationName::INLINE_FRAGMENT
    } else if location.value == "VARIABLE_DEFINITION" {
      DirectiveLocationName::VARIABLE_DEFINITION
    } else if location.value == "SCHEMA" {
      DirectiveLocationName::SCHEMA
    } else if location.value == "SCALAR" {
      DirectiveLocationName::SCALAR
    } else if location.value == "OBJECT" {
      DirectiveLocationName::OBJECT
    } else if location.value == "FIELD_DEFINITION" {
      DirectiveLocationName::FIELD_DEFINITION
    } else if location.value == "ARGUMENT_DEFINITION" {
      DirectiveLocationName::ARGUMENT_DEFINITION
    } else if location.value == "INTERFACE" {
      DirectiveLocationName::INTERFACE
    } else if location.value == "UNION" {
      DirectiveLocationName::UNION
    } else if location.value == "ENUM" {
      DirectiveLocationName::ENUM
    } else if location.value == "ENUM_VALUE" {
      DirectiveLocationName::ENUM_VALUE
    } else if location.value == "INPUT_OBJECT" {
      DirectiveLocationName::INPUT_OBJECT
    } else if location.value == "INPUT_FIELD_DEFINITION" {
      DirectiveLocationName::INPUT_FIELD_DEFINITION
    } else {
      return Err(SyntaxError {
        message: format!("Unexpected {}.", location),
        position: location.start,
      });
    };
    Ok(DirectiveLocation { name, loc })
  }

  fn parse_many(p: &mut Parser) -> Result<Vec1<Self>, SyntaxError> {
    if p.peek_token(Some(TokenKind::Name))?.kind == TokenKind::VerticalBar {
      p.parse_token(TokenKind::VerticalBar)?;
    }

    let mut locations = vec1![DirectiveLocation::parse(p)?];
    let mut peeked = p.peek_token(None)?;
    while peeked.kind == TokenKind::VerticalBar {
      p.parse_token(TokenKind::VerticalBar)?;
      locations.push(DirectiveLocation::parse(p)?);
      peeked = p.peek_token(None)?;
    }

    Ok(locations)
  }
}

#[derive(Debug, PartialEq)]
pub enum Definition {
  OperationDefinition {
    operation: OperationType,
    name: Option<Name>,
    variable_definitions: Vec<VariableDefinition>,
    directives: Vec<Directive>,
    selection_set: SelectionSet,
    loc: Loc,
  },
  FragmentDefinition {
    name: Name,
    type_condition: NamedType,
    directives: Vec<Directive>,
    selection_set: SelectionSet,
    loc: Loc,
  },
  SchemaDefinition {
    description: Option<StringValue>,
    directives: Vec<ConstDirective>,
    operation_types: Vec1<OperationTypeDefinition>,
    loc: Loc,
  },
  ScalarTypeDefinition {
    description: Option<StringValue>,
    name: Name,
    directives: Vec<ConstDirective>,
    loc: Loc,
  },
  ObjectTypeDefinition {
    description: Option<StringValue>,
    name: Name,
    interfaces: Vec<NamedType>,
    directives: Vec<ConstDirective>,
    fields: Vec<FieldDefinition>,
    loc: Loc,
  },
  InterfaceTypeDefinition {
    description: Option<StringValue>,
    name: Name,
    interfaces: Vec<NamedType>,
    directives: Vec<ConstDirective>,
    fields: Vec<FieldDefinition>,
    loc: Loc,
  },
  UnionTypeDefinition {
    description: Option<StringValue>,
    name: Name,
    directives: Vec<ConstDirective>,
    types: Vec<NamedType>,
    loc: Loc,
  },
  EnumTypeDefinition {
    description: Option<StringValue>,
    name: Name,
    directives: Vec<ConstDirective>,
    values: Vec<EnumValueDefinition>,
    loc: Loc,
  },
  InputObjectTypeDefinition {
    description: Option<StringValue>,
    name: Name,
    directives: Vec<ConstDirective>,
    fields: Vec<InputValueDefinition>,
    loc: Loc,
  },
  DirectiveDefinition {
    description: Option<StringValue>,
    name: Name,
    arguments: Vec<InputValueDefinition>,
    repeatable: bool,
    locations: Vec1<DirectiveLocation>,
    loc: Loc,
  },
  SchemaExtension {
    directives: Vec<ConstDirective>,
    operation_types: Vec<OperationTypeDefinition>,
    loc: Loc,
  },
  ScalarTypeExtension {
    name: Name,
    directives: Vec1<ConstDirective>,
    loc: Loc,
  },
  ObjectTypeExtension {
    name: Name,
    interfaces: Vec<NamedType>,
    directives: Vec<ConstDirective>,
    fields: Vec<FieldDefinition>,
    loc: Loc,
  },
  InterfaceTypeExtension {
    name: Name,
    interfaces: Vec<NamedType>,
    directives: Vec<ConstDirective>,
    fields: Vec<FieldDefinition>,
    loc: Loc,
  },
  UnionTypeExtension {
    name: Name,
    directives: Vec<ConstDirective>,
    types: Vec<NamedType>,
    loc: Loc,
  },
  EnumTypeExtension {
    name: Name,
    directives: Vec<ConstDirective>,
    values: Vec<EnumValueDefinition>,
    loc: Loc,
  },
  InputObjectTypeExtension {
    name: Name,
    directives: Vec<ConstDirective>,
    fields: Vec<InputValueDefinition>,
    loc: Loc,
  },
}

impl Definition {
  fn parse_implements_interface(p: &mut Parser) -> Result<Vec<NamedType>, SyntaxError> {
    let peeked = p.peek_token(None)?;
    if !(peeked.kind == TokenKind::Name && peeked.value == "implements") {
      return Ok(vec![]);
    }

    p.parse_token(TokenKind::Name)?;
    if p.peek_token(Some(TokenKind::Name))?.kind == TokenKind::Ampersand {
      p.parse_token(TokenKind::Ampersand)?;
    }
    let mut interfaces = vec![NamedType::parse(p)?];

    while p.peek_token(None)?.kind == TokenKind::Ampersand {
      p.parse_token(TokenKind::Ampersand)?;
      interfaces.push(NamedType::parse(p)?);
    }

    Ok(interfaces)
  }

  fn parse_union_member_types(p: &mut Parser) -> Result<Vec<NamedType>, SyntaxError> {
    if p.peek_token(None)?.kind != TokenKind::EqualsSign {
      return Ok(vec![]);
    }

    p.parse_token(TokenKind::EqualsSign)?;
    if p.peek_token(None)?.kind == TokenKind::VerticalBar {
      p.parse_token(TokenKind::VerticalBar)?;
    }

    let mut types = vec![NamedType::parse(p)?];
    let mut peeked = p.peek_token(None)?;
    while peeked.kind == TokenKind::VerticalBar {
      p.parse_token(TokenKind::VerticalBar)?;
      types.push(NamedType::parse(p)?);
      peeked = p.peek_token(None)?;
    }

    Ok(types)
  }

  fn parse_operation_definition(p: &mut Parser) -> Result<Self, SyntaxError> {
    let peeked = p.peek_token(None)?;
    match peeked.kind {
      TokenKind::CurlyBracketOpening => {
        let selection_set = SelectionSet::parse(p)?;

        let start_token = selection_set.loc.start_token.clone();
        let end_token = selection_set.loc.end_token.clone();

        Ok(Definition::OperationDefinition {
          operation: OperationType::query,
          name: None,
          variable_definitions: vec![],
          directives: vec![],
          selection_set,
          loc: Loc {
            start_token,
            end_token,
          },
        })
      }
      TokenKind::Name => {
        let start_token = p.parse_token(TokenKind::Name)?;
        let operation = if start_token.value == "query" {
          OperationType::query
        } else if start_token.value == "mutation" {
          OperationType::mutation
        } else if start_token.value == "subscription" {
          OperationType::subscription
        } else {
          return Err(SyntaxError {
            message: format!("Unexpected {}.", start_token),
            position: start_token.start,
          });
        };

        let name = if p.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind == TokenKind::Name {
          Some(Name::parse(p)?)
        } else {
          None
        };

        let variable_definitions = VariableDefinition::parse_many(p)?;

        let directives = Directive::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

        let selection_set = SelectionSet::parse(p)?;
        let end_token = selection_set.loc.end_token.clone();

        Ok(Definition::OperationDefinition {
          operation,
          name,
          variable_definitions,
          directives,
          selection_set,
          loc: Loc {
            start_token,
            end_token,
          },
        })
      }
      _ => Err(p.error(format!("Unexpected {}.", peeked))),
    }
  }

  fn parse_fragment_definition(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let on = p.next_token(None)?;
    match on.kind {
      TokenKind::Name => {
        if on.value != "on" {
          return Err(SyntaxError {
            message: format!("Expected \"on\", found {}.", on),
            position: on.start,
          });
        }
      }
      _ => {
        return Err(SyntaxError {
          message: format!("Expected \"on\", found {}.", on),
          position: on.start,
        })
      }
    }

    let type_condition = NamedType::parse(p)?;

    let directives = Directive::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let selection_set = SelectionSet::parse(p)?;

    let end_token = selection_set.loc.end_token.clone();

    Ok(Definition::FragmentDefinition {
      name,
      type_condition,
      directives,
      selection_set,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_schema_definition(
    p: &mut Parser,
    description: Option<StringValue>,
  ) -> Result<Self, SyntaxError> {
    let schema = p.parse_token(TokenKind::Name)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    p.parse_token(TokenKind::CurlyBracketOpening)?;

    let mut operation_types = vec1![OperationTypeDefinition::parse(p)?];
    let mut peeked = p.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::CurlyBracketClosing {
      operation_types.push(OperationTypeDefinition::parse(p)?);
      peeked = p.peek_token(Some(TokenKind::Name))?;
    }

    let start_token = match description {
      None => schema,
      Some(ref description) => description.loc.start_token.clone(),
    };
    let end_token = p.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok(Definition::SchemaDefinition {
      description,
      directives,
      operation_types,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_scalar_type_definition(
    p: &mut Parser,
    description: Option<StringValue>,
  ) -> Result<Self, SyntaxError> {
    let scalar = p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let start_token = match description {
      None => scalar,
      Some(ref description) => description.loc.start_token.clone(),
    };
    let end_token = if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Definition::ScalarTypeDefinition {
      description,
      name,
      directives,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_object_type_definition(
    p: &mut Parser,
    description: Option<StringValue>,
  ) -> Result<Self, SyntaxError> {
    let name_token = p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let interfaces = Definition::parse_implements_interface(p)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let (fields, curly_bracket_closing_token) = FieldDefinition::parse_many(p)?;

    let start_token = match description {
      None => name_token,
      Some(ref description) => description.loc.start_token.clone(),
    };
    let end_token = if curly_bracket_closing_token != None {
      curly_bracket_closing_token.unwrap()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else if interfaces.len() > 0 {
      interfaces.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Definition::ObjectTypeDefinition {
      description,
      name,
      interfaces,
      directives,
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_interface_type_definition(
    p: &mut Parser,
    description: Option<StringValue>,
  ) -> Result<Self, SyntaxError> {
    let name_token = p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let interfaces = Definition::parse_implements_interface(p)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let (fields, curly_bracket_closing_token) = FieldDefinition::parse_many(p)?;

    let start_token = match description {
      None => name_token,
      Some(ref description) => description.loc.start_token.clone(),
    };
    let end_token = if curly_bracket_closing_token != None {
      curly_bracket_closing_token.unwrap()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else if interfaces.len() > 0 {
      interfaces.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Definition::InterfaceTypeDefinition {
      description,
      name,
      interfaces,
      directives,
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_union_type_definition(
    p: &mut Parser,
    description: Option<StringValue>,
  ) -> Result<Self, SyntaxError> {
    let name_token = p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let start_token = match description {
      None => name_token,
      Some(ref description) => description.loc.start_token.clone(),
    };

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let types = Definition::parse_union_member_types(p)?;

    let end_token = if types.len() > 0 {
      types.last().unwrap().loc.end_token.clone()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Definition::UnionTypeDefinition {
      description,
      name,
      directives,
      types,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_enum_type_definition(
    p: &mut Parser,
    description: Option<StringValue>,
  ) -> Result<Self, SyntaxError> {
    let name_token = p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let start_token = match description {
      None => name_token,
      Some(ref description) => description.loc.start_token.clone(),
    };

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let (values, curly_bracket_closing_token) = EnumValueDefinition::parse_many(p)?;

    let end_token = if curly_bracket_closing_token != None {
      curly_bracket_closing_token.unwrap()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Definition::EnumTypeDefinition {
      description,
      name,
      directives,
      values,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_input_object_type_definition(
    p: &mut Parser,
    description: Option<StringValue>,
  ) -> Result<Self, SyntaxError> {
    let name_token = p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let interfaces = Definition::parse_implements_interface(p)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let (fields, curly_bracket_closing_token) = InputValueDefinition::parse_many(
      p,
      TokenKind::CurlyBracketOpening,
      TokenKind::CurlyBracketClosing,
    )?;

    let start_token = match description {
      None => name_token,
      Some(ref description) => description.loc.start_token.clone(),
    };
    let end_token = if curly_bracket_closing_token != None {
      curly_bracket_closing_token.unwrap()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else if interfaces.len() > 0 {
      interfaces.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Definition::InputObjectTypeDefinition {
      description,
      name,
      directives,
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_directive_definition(
    p: &mut Parser,
    description: Option<StringValue>,
  ) -> Result<Self, SyntaxError> {
    let name_token = p.parse_token(TokenKind::Name)?;

    p.parse_token(TokenKind::AtSign)?;

    let name = Name::parse(p)?;

    let (arguments, _) = InputValueDefinition::parse_many(
      p,
      TokenKind::RoundBracketOpening,
      TokenKind::RoundBracketClosing,
    )?;

    let peeked = p.peek_token(Some(TokenKind::Name))?;
    let repeatable = if peeked.kind == TokenKind::Name && peeked.value == "repeatable" {
      p.parse_token(TokenKind::Name)?;
      true
    } else {
      false
    };

    let on = p.parse_token(TokenKind::Name)?;
    if on.value != "on" {
      return Err(SyntaxError {
        message: format!("Expected \"on\", found {}.", on),
        position: on.start,
      });
    }

    let locations = DirectiveLocation::parse_many(p)?;

    let start_token = match description {
      None => name_token,
      Some(ref description) => description.loc.start_token.clone(),
    };
    let end_token = locations.last().loc.end_token.clone();

    Ok(Definition::DirectiveDefinition {
      description,
      name,
      arguments,
      repeatable,
      locations,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_schema_extension(p: &mut Parser, start_token: Token) -> Result<Self, SyntaxError> {
    p.parse_token(TokenKind::Name)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let (operation_types, closing_curly_bracket_token) =
      if directives.len() == 0 || p.peek_token(None)?.kind == TokenKind::CurlyBracketOpening {
        p.next_token(None)?;
        let mut operation_type_definitions = vec![OperationTypeDefinition::parse(p)?];
        let mut peeked = p.peek_token(Some(TokenKind::Name))?;
        while peeked.kind != TokenKind::CurlyBracketClosing {
          operation_type_definitions.push(OperationTypeDefinition::parse(p)?);
          peeked = p.peek_token(Some(TokenKind::Name))?;
        }
        (
          operation_type_definitions,
          Some(p.parse_token(TokenKind::CurlyBracketClosing)?),
        )
      } else {
        (vec![], None)
      };

    let end_token = if closing_curly_bracket_token != None {
      closing_curly_bracket_token.unwrap()
    } else if operation_types.len() > 0 {
      operation_types.last().unwrap().loc.end_token.clone()
    } else {
      directives.last().unwrap().loc.end_token.clone()
    };

    Ok(Definition::SchemaExtension {
      directives,
      operation_types,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_scalar_extension(p: &mut Parser, start_token: Token) -> Result<Self, SyntaxError> {
    p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let directives = ConstDirective::parse_many(p, None)?;

    if directives.len() == 0 {
      let peeked = p.peek_token(None)?;
      return Err(p.error(format!("Unexpected {}.", peeked)));
    }

    let end_token = directives.last().unwrap().loc.end_token.clone();

    Ok(Definition::ScalarTypeExtension {
      name,
      directives: Vec1::try_from_vec(directives).unwrap(),
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_object_type_extension(p: &mut Parser, start_token: Token) -> Result<Self, SyntaxError> {
    p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let interfaces = Definition::parse_implements_interface(p)?;

    let directives = ConstDirective::parse_many(p, None)?;

    let (fields, curly_bracket_closing_token) = FieldDefinition::parse_many(p)?;

    let end_token = if curly_bracket_closing_token != None {
      curly_bracket_closing_token.unwrap()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      interfaces.last().unwrap().loc.end_token.clone()
    };

    Ok(Definition::ObjectTypeExtension {
      name,
      interfaces,
      directives,
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_interface_type_extension(
    p: &mut Parser,
    start_token: Token,
  ) -> Result<Self, SyntaxError> {
    p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let interfaces = Definition::parse_implements_interface(p)?;

    let directives = ConstDirective::parse_many(p, None)?;

    let (fields, curly_bracket_closing_token) = FieldDefinition::parse_many(p)?;

    let end_token = if curly_bracket_closing_token != None {
      curly_bracket_closing_token.unwrap()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      interfaces.last().unwrap().loc.end_token.clone()
    };

    Ok(Definition::InterfaceTypeExtension {
      name,
      interfaces,
      directives,
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_union_type_extension(p: &mut Parser, start_token: Token) -> Result<Self, SyntaxError> {
    p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let types = Definition::parse_union_member_types(p)?;

    let end_token = if types.len() > 0 {
      types.last().unwrap().loc.end_token.clone()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Definition::UnionTypeExtension {
      name,
      directives,
      types,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_enum_type_extension(p: &mut Parser, start_token: Token) -> Result<Self, SyntaxError> {
    p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let (values, curly_bracket_closing_token) = EnumValueDefinition::parse_many(p)?;

    let end_token = if curly_bracket_closing_token != None {
      curly_bracket_closing_token.unwrap()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Definition::EnumTypeExtension {
      name,
      directives,
      values,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_input_object_type_extension(
    p: &mut Parser,
    start_token: Token,
  ) -> Result<Self, SyntaxError> {
    p.parse_token(TokenKind::Name)?;

    let name = Name::parse(p)?;

    let interfaces = Definition::parse_implements_interface(p)?;

    let directives = ConstDirective::parse_many(p, Some(TokenKind::CurlyBracketOpening))?;

    let (fields, curly_bracket_closing_token) = InputValueDefinition::parse_many(
      p,
      TokenKind::CurlyBracketOpening,
      TokenKind::CurlyBracketClosing,
    )?;

    let end_token = if curly_bracket_closing_token != None {
      curly_bracket_closing_token.unwrap()
    } else if directives.len() > 0 {
      directives.last().unwrap().loc.end_token.clone()
    } else if interfaces.len() > 0 {
      interfaces.last().unwrap().loc.end_token.clone()
    } else {
      name.loc.end_token.clone()
    };

    Ok(Definition::InputObjectTypeExtension {
      name,
      directives,
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let peeked = p.peek_token(None)?;
    if peeked.kind == TokenKind::Name && peeked.value == "extend" {
      // Skip the "extend" keyword
      let start_token = p.parse_token(TokenKind::Name)?;

      // An extension must be followed by a type keyworkd
      let peeked = p.peek_token(None)?;
      return match peeked.kind {
        TokenKind::Name => {
          if peeked.value == "schema" {
            Definition::parse_schema_extension(p, start_token)
          } else if peeked.value == "scalar" {
            Definition::parse_scalar_extension(p, start_token)
          } else if peeked.value == "type" {
            Definition::parse_object_type_extension(p, start_token)
          } else if peeked.value == "interface" {
            Definition::parse_interface_type_extension(p, start_token)
          } else if peeked.value == "union" {
            Definition::parse_union_type_extension(p, start_token)
          } else if peeked.value == "enum" {
            Definition::parse_enum_type_extension(p, start_token)
          } else if peeked.value == "input" {
            Definition::parse_input_object_type_extension(p, start_token)
          } else {
            Err(p.error(format!("Unexpected {}", peeked)))
          }
        }
        _ => Err(p.error(format!("Unexpected {}", peeked))),
      };
    }

    let description = if p
      .peek_token(None)?
      .kind
      .equals(TokenKind::String { block: false })
    {
      let string_value = StringValue::parse(p)?;

      // A description must be followed by a type system definition
      let peeked = p.peek_token(None)?;
      match peeked.kind {
        TokenKind::Name => {
          if peeked.value == "schema"
            || peeked.value == "scalar"
            || peeked.value == "type"
            || peeked.value == "interface"
            || peeked.value == "union"
            || peeked.value == "enum"
            || peeked.value == "input"
            || peeked.value == "directive"
          {
            Some(string_value)
          } else {
            return Err(p.error(format!("Unexpected {}", peeked)));
          }
        }
        _ => return Err(p.error(format!("Unexpected {}", peeked))),
      }
    } else {
      None
    };

    let peeked = p.peek_token(None)?;
    match peeked.kind {
      TokenKind::CurlyBracketOpening => Definition::parse_operation_definition(p),
      TokenKind::Name => {
        if peeked.value == "query" || peeked.value == "mutation" || peeked.value == "subscription" {
          Definition::parse_operation_definition(p)
        } else if peeked.value == "fragment" {
          Definition::parse_fragment_definition(p)
        } else if peeked.value == "schema" {
          Definition::parse_schema_definition(p, description)
        } else if peeked.value == "scalar" {
          Definition::parse_scalar_type_definition(p, description)
        } else if peeked.value == "type" {
          Definition::parse_object_type_definition(p, description)
        } else if peeked.value == "interface" {
          Definition::parse_interface_type_definition(p, description)
        } else if peeked.value == "union" {
          Definition::parse_union_type_definition(p, description)
        } else if peeked.value == "enum" {
          Definition::parse_enum_type_definition(p, description)
        } else if peeked.value == "input" {
          Definition::parse_input_object_type_definition(p, description)
        } else if peeked.value == "directive" {
          Definition::parse_directive_definition(p, description)
        } else {
          Err(p.error(format!("Unexpected {}.", peeked)))
        }
      }
      _ => Err(p.error(format!("Unexpected {}.", peeked))),
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct Document {
  pub definitions: Vec1<Definition>,
  pub loc: Loc,
}

impl Document {
  pub fn parse(p: &mut Parser) -> Result<Self, SyntaxError> {
    let start_token = p.parse_token(TokenKind::SOF)?;
    let mut definitions = vec1![Definition::parse(p)?];
    while p.lexer.has_more() {
      definitions.push(Definition::parse(p)?);
    }

    let end_token = p.parse_token(TokenKind::EOF)?;
    Ok(Document {
      definitions,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }
}

pub fn parse(query: &str) -> Result<Document, SyntaxError> {
  Document::parse(&mut Parser::new(query))
}

#[cfg(test)]
mod parser {
  use super::*;

  #[test]
  fn should_parse_example_query() {
    let query = std::fs::read_to_string("src/example.gql").unwrap();
    assert_eq!(
      parse(&query),
      Ok(Document {
        definitions: vec1![
          Definition::OperationDefinition {
            operation: OperationType::query,
            name: Some(Name {
              value: String::from("IgnoredTokens"),
              #[cfg_attr(rustfmt, rustfmt_skip)]
              loc: Loc {
                start_token: Token { kind: TokenKind::Name, value: String::from("IgnoredTokens"), line: 6, column: 7, start: 172, end: 185 },
                end_token:   Token { kind: TokenKind::Name, value: String::from("IgnoredTokens"), line: 6, column: 7, start: 172, end: 185 },
              }
            }),
            variable_definitions: vec![],
            directives: vec![],
            selection_set: SelectionSet {
              selections: vec1![Selection::Field {
                name: Name {
                  value: String::from("myField"),
                  #[cfg_attr(rustfmt, rustfmt_skip)]
                  loc: Loc {
                    start_token: Token { kind: TokenKind::Name, value: String::from("myField"), line: 6, column: 22, start: 187, end: 194 },
                    end_token:   Token { kind: TokenKind::Name, value: String::from("myField"), line: 6, column: 22, start: 187, end: 194 },
                  }
                },
                alias: None,
                arguments: vec![Argument {
                  name: Name {
                    value: String::from("myArg"),
                    #[cfg_attr(rustfmt, rustfmt_skip)]
                    loc: Loc {
                      start_token: Token { kind: TokenKind::Name, value: String::from("myArg"), line: 6, column: 34, start: 199, end: 204 },
                      end_token:   Token { kind: TokenKind::Name, value: String::from("myArg"), line: 6, column: 34, start: 199, end: 204 },
                    },
                  },
                  value: Value::StringValue(StringValue {
                    value: String::from("myVal"),
                    block: false,
                    #[cfg_attr(rustfmt, rustfmt_skip)]
                    loc: Loc {
                      start_token: Token { kind: TokenKind::String { block: false }, value: String::from("myVal"), line: 7, column: 1, start: 210, end: 217 },
                      end_token:   Token { kind: TokenKind::String { block: false }, value: String::from("myVal"), line: 7, column: 1, start: 210, end: 217 },
                    },
                  }),
                  #[cfg_attr(rustfmt, rustfmt_skip)]
                  loc: Loc {
                    start_token: Token { kind: TokenKind::Name,                    value: String::from("myArg"), line: 6, column: 34, start: 199, end: 204 },
                    end_token:   Token { kind: TokenKind::String { block: false }, value: String::from("myVal"), line: 7, column:  1, start: 210, end: 217 },
                  },
                }],
                directives: vec![],
                selection_set: None,
                #[cfg_attr(rustfmt, rustfmt_skip)]
                loc: Loc {
                  start_token: Token { kind: TokenKind::Name,                value: String::from("myField"), line: 6, column: 22, start: 187, end: 194 },
                  end_token:   Token { kind: TokenKind::RoundBracketClosing, value: String::from(")"),       line: 7, column:  8, start: 217, end: 218 },
                },
              }],
              #[cfg_attr(rustfmt, rustfmt_skip)]
              loc: Loc {
                start_token: Token { kind: TokenKind::CurlyBracketOpening, value: String::from("{"), line: 6, column: 21, start: 186, end: 187 },
                end_token:   Token { kind: TokenKind::CurlyBracketClosing, value: String::from("}"), line: 8, column:  2, start: 240, end: 241 },
              },
            },
            #[cfg_attr(rustfmt, rustfmt_skip)]
            loc: Loc {
              start_token: Token { kind: TokenKind::Name,                value: String::from("query"), line: 6, column: 1, start: 166, end: 171 },
              end_token:   Token { kind: TokenKind::CurlyBracketClosing, value: String::from("}"),     line: 8, column: 2, start: 240, end: 241 },
            },
          },
          Definition::OperationDefinition {
            operation: OperationType::query,
            name: None,
            variable_definitions: vec![],
            directives: vec![],
            selection_set: SelectionSet {
              selections: vec1![
                Selection::Field {
                  name: Name {
                    value: String::from("myField"),
                    #[cfg_attr(rustfmt, rustfmt_skip)]
                    loc: Loc {
                      start_token: Token { kind: TokenKind::Name, value: String::from("myField"), line: 15, column: 3, start: 395, end: 402 },
                      end_token:   Token { kind: TokenKind::Name, value: String::from("myField"), line: 15, column: 3, start: 395, end: 402 },
                    }
                  },
                  alias: None,
                  arguments: vec![
                    Argument {
                      name: Name {
                        value: String::from("intArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("intArg"), line: 16, column: 5, start: 408, end: 414 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("intArg"), line: 16, column: 5, start: 408, end: 414 },
                        }
                      },
                      value: Value::IntValue(IntValue {
                        value: String::from("0"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Int, value: String::from("0"), line: 16, column: 13, start: 416, end: 417 },
                          end_token:   Token { kind: TokenKind::Int, value: String::from("0"), line: 16, column: 13, start: 416, end: 417 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name, value: String::from("intArg"), line: 16, column:  5, start: 408, end: 414 },
                        end_token:   Token { kind: TokenKind::Int,  value: String::from("0"),      line: 16, column: 13, start: 416, end: 417 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("intArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("intArg"), line: 17, column: 5, start: 422, end: 428 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("intArg"), line: 17, column: 5, start: 422, end: 428 },
                        }
                      },
                      value: Value::IntValue(IntValue {
                        value: String::from("-0"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Int, value: String::from("-0"), line: 17, column: 13, start: 430, end: 432 },
                          end_token:   Token { kind: TokenKind::Int, value: String::from("-0"), line: 17, column: 13, start: 430, end: 432 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name, value: String::from("intArg"), line: 17, column:  5, start: 422, end: 428 },
                        end_token:   Token { kind: TokenKind::Int,  value: String::from("-0"),     line: 17, column: 13, start: 430, end: 432 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("intArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("intArg"), line: 18, column: 5, start: 437, end: 443 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("intArg"), line: 18, column: 5, start: 437, end: 443 },
                        }
                      },
                      value: Value::IntValue(IntValue {
                        value: String::from("42"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Int, value: String::from("42"), line: 18, column: 13, start: 445, end: 447 },
                          end_token:   Token { kind: TokenKind::Int, value: String::from("42"), line: 18, column: 13, start: 445, end: 447 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name, value: String::from("intArg"), line: 18, column:  5, start: 437, end: 443 },
                        end_token:   Token { kind: TokenKind::Int,  value: String::from("42"),     line: 18, column: 13, start: 445, end: 447 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("intArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("intArg"), line: 19, column: 5, start: 452, end: 458 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("intArg"), line: 19, column: 5, start: 452, end: 458 },
                        }
                      },
                      value: Value::IntValue(IntValue {
                        value: String::from("-42"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Int, value: String::from("-42"), line: 19, column: 13, start: 460, end: 463 },
                          end_token:   Token { kind: TokenKind::Int, value: String::from("-42"), line: 19, column: 13, start: 460, end: 463 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name, value: String::from("intArg"), line: 19, column:  5, start: 452, end: 458 },
                        end_token:   Token { kind: TokenKind::Int,  value: String::from("-42"),    line: 19, column: 13, start: 460, end: 463 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("floatArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 20, column: 5, start: 468, end: 476 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 20, column: 5, start: 468, end: 476 },
                        }
                      },
                      value: Value::FloatValue(FloatValue {
                        value: String::from("0.43e44"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Float, value: String::from("0.43e44"), line: 20, column: 15, start: 478, end: 485 },
                          end_token:   Token { kind: TokenKind::Float, value: String::from("0.43e44"), line: 20, column: 15, start: 478, end: 485 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name,   value: String::from("floatArg"), line: 20, column:  5, start: 468, end: 476 },
                        end_token:   Token { kind: TokenKind::Float,  value: String::from("0.43e44"),  line: 20, column: 15, start: 478, end: 485 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("floatArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 21, column: 5, start: 490, end: 498 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 21, column: 5, start: 490, end: 498 },
                        }
                      },
                      value: Value::FloatValue(FloatValue {
                        value: String::from("0.43E44"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Float, value: String::from("0.43E44"), line: 21, column: 15, start: 500, end: 507 },
                          end_token:   Token { kind: TokenKind::Float, value: String::from("0.43E44"), line: 21, column: 15, start: 500, end: 507 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name,   value: String::from("floatArg"), line: 21, column:  5, start: 490, end: 498 },
                        end_token:   Token { kind: TokenKind::Float,  value: String::from("0.43E44"),  line: 21, column: 15, start: 500, end: 507 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("floatArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 22, column: 5, start: 512, end: 520 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 22, column: 5, start: 512, end: 520 },
                        }
                      },
                      value: Value::FloatValue(FloatValue {
                        value: String::from("0.43e+44"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Float, value: String::from("0.43e+44"), line: 22, column: 15, start: 522, end: 530 },
                          end_token:   Token { kind: TokenKind::Float, value: String::from("0.43e+44"), line: 22, column: 15, start: 522, end: 530 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name,   value: String::from("floatArg"), line: 22, column:  5, start: 512, end: 520 },
                        end_token:   Token { kind: TokenKind::Float,  value: String::from("0.43e+44"), line: 22, column: 15, start: 522, end: 530 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("floatArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 23, column: 5, start: 535, end: 543 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 23, column: 5, start: 535, end: 543 },
                        }
                      },
                      value: Value::FloatValue(FloatValue {
                        value: String::from("0.43E+44"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Float, value: String::from("0.43E+44"), line: 23, column: 15, start: 545, end: 553 },
                          end_token:   Token { kind: TokenKind::Float, value: String::from("0.43E+44"), line: 23, column: 15, start: 545, end: 553 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name,   value: String::from("floatArg"), line: 23, column:  5, start: 535, end: 543 },
                        end_token:   Token { kind: TokenKind::Float,  value: String::from("0.43E+44"), line: 23, column: 15, start: 545, end: 553 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("floatArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 24, column: 5, start: 558, end: 566 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 24, column: 5, start: 558, end: 566 },
                        }
                      },
                      value: Value::FloatValue(FloatValue {
                        value: String::from("0.43e-44"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Float, value: String::from("0.43e-44"), line: 24, column: 15, start: 568, end: 576 },
                          end_token:   Token { kind: TokenKind::Float, value: String::from("0.43e-44"), line: 24, column: 15, start: 568, end: 576 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name,   value: String::from("floatArg"), line: 24, column:  5, start: 558, end: 566 },
                        end_token:   Token { kind: TokenKind::Float,  value: String::from("0.43e-44"), line: 24, column: 15, start: 568, end: 576 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("floatArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 25, column: 5, start: 581, end: 589 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 25, column: 5, start: 581, end: 589 },
                        }
                      },
                      value: Value::FloatValue(FloatValue {
                        value: String::from("0.43E-44"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Float, value: String::from("0.43E-44"), line: 25, column: 15, start: 591, end: 599 },
                          end_token:   Token { kind: TokenKind::Float, value: String::from("0.43E-44"), line: 25, column: 15, start: 591, end: 599 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name,   value: String::from("floatArg"), line: 25, column:  5, start: 581, end: 589 },
                        end_token:   Token { kind: TokenKind::Float,  value: String::from("0.43E-44"), line: 25, column: 15, start: 591, end: 599 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("floatArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 26, column: 5, start: 604, end: 612 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 26, column: 5, start: 604, end: 612 },
                        }
                      },
                      value: Value::FloatValue(FloatValue {
                        value: String::from("0e44"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Float, value: String::from("0e44"), line: 26, column: 15, start: 614, end: 618 },
                          end_token:   Token { kind: TokenKind::Float, value: String::from("0e44"), line: 26, column: 15, start: 614, end: 618 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name,   value: String::from("floatArg"), line: 26, column:  5, start: 604, end: 612 },
                        end_token:   Token { kind: TokenKind::Float,  value: String::from("0e44"),     line: 26, column: 15, start: 614, end: 618 },
                      }
                    },
                    Argument {
                      name: Name {
                        value: String::from("floatArg"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 27, column: 5, start: 623, end: 631 },
                          end_token:   Token { kind: TokenKind::Name, value: String::from("floatArg"), line: 27, column: 5, start: 623, end: 631 },
                        }
                      },
                      value: Value::FloatValue(FloatValue {
                        value: String::from("0E44"),
                        #[cfg_attr(rustfmt, rustfmt_skip)]
                        loc: Loc {
                          start_token: Token { kind: TokenKind::Float, value: String::from("0E44"), line: 27, column: 15, start: 633, end: 637 },
                          end_token:   Token { kind: TokenKind::Float, value: String::from("0E44"), line: 27, column: 15, start: 633, end: 637 },
                        }
                      }),
                      #[cfg_attr(rustfmt, rustfmt_skip)]
                      loc: Loc {
                        start_token: Token { kind: TokenKind::Name,   value: String::from("floatArg"), line: 27, column:  5, start: 623, end: 631 },
                        end_token:   Token { kind: TokenKind::Float,  value: String::from("0E44"),     line: 27, column: 15, start: 633, end: 637 },
                      }
                    }
                  ],
                  directives: vec![],
                  selection_set: None,
                  #[cfg_attr(rustfmt, rustfmt_skip)]
                  loc: Loc {
                    start_token: Token { kind: TokenKind::Name,                value: String::from("myField"), line: 15, column: 3, start: 395, end: 402 },
                    end_token:   Token { kind: TokenKind::RoundBracketClosing, value: String::from(")"),       line: 21, column: 3, start: 488, end: 489 },
                  },
                },
                // Selection::Field {
                //   name: Name {
                //     value: String::from("my123Name"),
                //     #[cfg_attr(rustfmt, rustfmt_skip)]
                //     loc: Loc {
                //       start_token: Token { kind: TokenKind::Name, value: String::from("my123Name"), line: 94, column: 3, start: 2103, end: 2112 },
                //       end_token:   Token { kind: TokenKind::Name, value: String::from("my123Name"), line: 94, column: 3, start: 2103, end: 2112 },
                //     }
                //   },
                //   alias: None,
                //   arguments: vec![],
                //   directives: vec![],
                //   selection_set: None,
                //   #[cfg_attr(rustfmt, rustfmt_skip)]
                //   loc: Loc {
                //     start_token: Token { kind: TokenKind::Name, value: String::from("my123Name"), line: 94, column: 3, start: 2103, end: 2112 },
                //     end_token:   Token { kind: TokenKind::Name, value: String::from("my123Name"), line: 94, column: 3, start: 2103, end: 2112 },
                //   },
                // },
                // Selection::Field {
                //   name: Name {
                //     value: String::from("_myName"),
                //     #[cfg_attr(rustfmt, rustfmt_skip)]
                //     loc: Loc {
                //       start_token: Token { kind: TokenKind::Name, value: String::from("_myName"), line: 95, column: 3, start: 2115, end: 2122 },
                //       end_token:   Token { kind: TokenKind::Name, value: String::from("_myName"), line: 95, column: 3, start: 2115, end: 2122 },
                //     }
                //   },
                //   alias: None,
                //   arguments: vec![],
                //   directives: vec![],
                //   selection_set: None,
                //   #[cfg_attr(rustfmt, rustfmt_skip)]
                //   loc: Loc {
                //     start_token: Token { kind: TokenKind::Name, value: String::from("_myName"), line: 95, column: 3, start: 2115, end: 2122 },
                //     end_token:   Token { kind: TokenKind::Name, value: String::from("_myName"), line: 95, column: 3, start: 2115, end: 2122 },
                //   },
                // },
                // Selection::Field {
                //   name: Name {
                //     value: String::from("_my_name"),
                //     #[cfg_attr(rustfmt, rustfmt_skip)]
                //     loc: Loc {
                //       start_token: Token { kind: TokenKind::Name, value: String::from("_my_name"), line: 96, column: 3, start: 2125, end: 2133 },
                //       end_token:   Token { kind: TokenKind::Name, value: String::from("_my_name"), line: 96, column: 3, start: 2125, end: 2133 },
                //     }
                //   },
                //   alias: None,
                //   arguments: vec![],
                //   directives: vec![],
                //   selection_set: None,
                //   #[cfg_attr(rustfmt, rustfmt_skip)]
                //   loc: Loc {
                //     start_token: Token { kind: TokenKind::Name, value: String::from("_my_name"), line: 96, column: 3, start: 2125, end: 2133 },
                //     end_token:   Token { kind: TokenKind::Name, value: String::from("_my_name"), line: 96, column: 3, start: 2125, end: 2133 },
                //   },
                // }
              ],
              #[cfg_attr(rustfmt, rustfmt_skip)]
              loc: Loc {
                start_token: Token { kind: TokenKind::CurlyBracketOpening, value: String::from("{"), line: 14, column: 1, start: 391, end: 392 },
                end_token:   Token { kind: TokenKind::CurlyBracketClosing, value: String::from("}"), line: 22, column: 1, start: 490, end: 491 },
              },
            },
            #[cfg_attr(rustfmt, rustfmt_skip)]
            loc: Loc {
              start_token: Token { kind: TokenKind::CurlyBracketOpening, value: String::from("{"), line: 14, column: 1, start: 391, end: 392 },
              end_token:   Token { kind: TokenKind::CurlyBracketClosing, value: String::from("}"), line: 22, column: 1, start: 490, end: 491 },
            },
          },
        ],
        #[cfg_attr(rustfmt, rustfmt_skip)]
        loc: Loc {
          start_token: Token { kind: TokenKind::SOF, value: String::from("<SOF>"), line: 0,  column: 0, start:   0, end:   0 },
          end_token:   Token { kind: TokenKind::EOF, value: String::from("<EOF>"), line: 23, column: 1, start: 492, end: 492 },
        }
      })
    )
  }
}
