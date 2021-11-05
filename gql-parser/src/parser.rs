use crate::error::SyntaxError;
use crate::lexer::{Lexer, Token, TokenKind};
use vec1::Vec1;

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

#[derive(Debug, PartialEq)]
pub struct NamedType {
  pub name: Name,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct ListType {
  pub gql_type: Box<Type>,
  pub loc: Loc,
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

#[derive(Debug, PartialEq)]
pub struct IntValue {
  pub value: String,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct FloatValue {
  pub value: String,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct StringValue {
  pub value: String,
  pub block: bool,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct BooleanValue {
  pub value: bool,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct NullValue {
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct EnumValue {
  pub value: String,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct ListValue {
  pub values: Vec<Value>,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct ObjectField {
  pub name: Name,
  pub value: Value,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct ObjectValue {
  pub fields: Vec<ObjectField>,
  pub loc: Loc,
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

#[derive(Debug, PartialEq)]
pub struct ConstObjectField {
  pub name: Name,
  pub value: ConstValue,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct ConstObjectValue {
  pub fields: Vec<ConstObjectField>,
  pub loc: Loc,
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

#[derive(Debug, PartialEq)]
pub struct ConstArgument {
  pub name: Name,
  pub value: ConstValue,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct Directive {
  pub name: Name,
  pub arguments: Vec<Argument>,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct ConstDirective {
  pub name: Name,
  pub arguments: Vec<ConstArgument>,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct VariableDefinition {
  pub variable: Variable,
  pub gql_type: Type,
  pub default_value: Option<ConstValue>,
  pub directives: Vec<Directive>,
  pub loc: Loc,
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

#[derive(Debug, PartialEq)]
pub struct SelectionSet {
  pub selections: Vec1<Selection>,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct OperationTypeDefinition {
  pub operation: OperationType,
  pub gql_type: NamedType,
  pub loc: Loc,
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

#[derive(Debug, PartialEq)]
pub struct FieldDefinition {
  pub description: Option<StringValue>,
  pub name: Name,
  pub arguments: Vec<InputValueDefinition>,
  pub gql_type: Type,
  pub directives: Vec<ConstDirective>,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct EnumValueDefinition {
  pub description: Option<StringValue>,
  pub enum_value: EnumValue,
  pub directives: Vec<ConstDirective>,
  pub loc: Loc,
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
pub enum DirectiveLocation {
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

#[derive(Debug, PartialEq)]
pub struct Document {
  pub definitions: Vec1<Definition>,
  pub loc: Loc,
}

pub struct Parser<'a> {
  lexer: Lexer<'a>,
}

impl Parser<'_> {
  pub fn new(query: &str) -> Parser {
    Parser {
      lexer: Lexer::new(query),
    }
  }

  fn next_token(&mut self, expected: Option<TokenKind>) -> Result<Token, SyntaxError> {
    match self.lexer.next()? {
      None => Err(SyntaxError {
        message: match expected {
          None => format!("Unexpected {}.", TokenKind::EOF),
          Some(expected) => {
            format!("Expected {}, found {}.", expected, TokenKind::EOF)
          }
        },
        position: self.lexer.get_position(),
      }),
      Some(token) => match token.kind {
        TokenKind::Comment => self.next_token(expected),
        _ => Ok(token),
      },
    }
  }

  fn peek_token(&mut self, expected: Option<TokenKind>) -> Result<Token, SyntaxError> {
    match self.lexer.peek()? {
      None => Err(SyntaxError {
        message: match expected {
          None => format!("Unexpected {}.", TokenKind::EOF),
          Some(expected) => format!("Expected {}, found {}.", expected, TokenKind::EOF),
        },
        position: self.lexer.get_position(),
      }),
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

  fn parse_name(&mut self) -> Result<Name, SyntaxError> {
    let token = self.parse_token(TokenKind::Name)?;
    let loc = Loc {
      start_token: token.clone(),
      end_token: token.clone(),
    };
    Ok(Name {
      value: token.value,
      loc,
    })
  }

  fn parse_named_type(&mut self) -> Result<NamedType, SyntaxError> {
    let name = self.parse_name()?;
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

  fn parse_list_type(&mut self) -> Result<ListType, SyntaxError> {
    let start_token = self.parse_token(TokenKind::SquareBracketOpening)?;
    let gql_type = self.parse_type()?;
    let end_token = self.parse_token(TokenKind::SquareBracketClosing)?;
    Ok(ListType {
      gql_type: Box::new(gql_type),
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_type(&mut self) -> Result<Type, SyntaxError> {
    let peeked = self.peek_token(Some(TokenKind::Name))?;
    match peeked.kind {
      TokenKind::SquareBracketOpening => {
        let list_type = self.parse_list_type()?;
        // We don't use self.peek_token because we don't know what token to expect
        let peeked = self.lexer.peek()?;
        if peeked != None && peeked.unwrap().kind == TokenKind::ExclamationMark {
          let start_token = list_type.loc.start_token.clone();
          let end_token = self.parse_token(TokenKind::ExclamationMark)?;
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
        let named_type = self.parse_named_type()?;
        // We don't use self.peek_token because we don't know what token to expect
        let peeked = self.lexer.peek()?;
        if peeked != None && peeked.unwrap().kind == TokenKind::ExclamationMark {
          let start_token = named_type.loc.start_token.clone();
          let end_token = self.parse_token(TokenKind::ExclamationMark)?;
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
      _ => Err(SyntaxError {
        message: format!("Expected {}, found {}.", TokenKind::Name, peeked),
        position: self.lexer.get_position(),
      }),
    }
  }

  fn parse_variable(&mut self) -> Result<Variable, SyntaxError> {
    let start_token = self.parse_token(TokenKind::DollarSign)?;
    let name = self.parse_name()?;
    let end_token = name.loc.end_token.clone();
    Ok(Variable {
      name,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_int_value(&mut self) -> Result<IntValue, SyntaxError> {
    let int_token = self.parse_token(TokenKind::Int)?;
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
  fn parse_float_value(&mut self) -> Result<FloatValue, SyntaxError> {
    let float_token = self.parse_token(TokenKind::Float)?;
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

  fn parse_string_value(&mut self) -> Result<StringValue, SyntaxError> {
    let string_token = self.parse_token(TokenKind::String { block: false })?;
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

  fn parse_boolean_value(&mut self) -> Result<BooleanValue, SyntaxError> {
    let token = self.parse_token(TokenKind::Name)?;
    if token.value != "true" && token.value != "false" {
      Err(SyntaxError {
        message: format!("Expected {}, found {}.", TokenKind::Name, token),
        position: self.lexer.get_position(),
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

  fn parse_null_value(&mut self) -> Result<NullValue, SyntaxError> {
    let token = self.parse_token(TokenKind::Name)?;
    if token.value != "null" {
      Err(SyntaxError {
        message: format!("Expected {}, found {}.", TokenKind::Name, token),
        position: self.lexer.get_position(),
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

  fn parse_enum_value(&mut self) -> Result<EnumValue, SyntaxError> {
    let token = self.parse_token(TokenKind::Name)?;
    if token.value == "true" || token.value == "false" || token.value == "null" {
      Err(SyntaxError {
        message: format!("Expected {}, found {}.", TokenKind::Name, token),
        position: self.lexer.get_position(),
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

  fn parse_list_value(&mut self) -> Result<ListValue, SyntaxError> {
    let start_token = self.parse_token(TokenKind::SquareBracketOpening)?;
    let mut values = vec![];
    let mut next = self.peek_token(None)?;
    while next.kind != TokenKind::SquareBracketClosing {
      values.push(self.parse_value()?);
      next = self.peek_token(None)?
    }

    let end_token = self.parse_token(TokenKind::SquareBracketClosing)?;
    Ok(ListValue {
      values,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_object_field(&mut self) -> Result<ObjectField, SyntaxError> {
    let name = self.parse_name()?;
    self.parse_token(TokenKind::Colon)?;
    let value = self.parse_value()?;

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

  fn parse_object_value(&mut self) -> Result<ObjectValue, SyntaxError> {
    let start_token = self.parse_token(TokenKind::CurlyBracketOpening)?;
    let mut fields = vec![];

    let mut next = self.peek_token(Some(TokenKind::Name))?;
    while next.kind != TokenKind::CurlyBracketClosing {
      fields.push(self.parse_object_field()?);
      next = self.peek_token(Some(TokenKind::Name))?;
    }

    if next.kind != TokenKind::CurlyBracketClosing {
      return Err(SyntaxError {
        // Align with graphql-js: The error message always expects another
        // field instead of a closing bracket.
        message: format!("Expected {}, found {}.", TokenKind::Name, next),
        position: self.lexer.get_position(),
      });
    }
    let end_token = self.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok(ObjectValue {
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_value(&mut self) -> Result<Value, SyntaxError> {
    let peeked = self.peek_token(None)?;
    match peeked.kind {
      TokenKind::DollarSign => Ok(Value::Variable(self.parse_variable()?)),
      TokenKind::Int => Ok(Value::IntValue(self.parse_int_value()?)),
      TokenKind::Float => Ok(Value::FloatValue(self.parse_float_value()?)),
      TokenKind::String { .. } => Ok(Value::StringValue(self.parse_string_value()?)),
      TokenKind::Name => {
        if peeked.value == "true" || peeked.value == "false" {
          Ok(Value::BooleanValue(self.parse_boolean_value()?))
        } else if peeked.value == "null" {
          Ok(Value::NullValue(self.parse_null_value()?))
        } else {
          Ok(Value::EnumValue(self.parse_enum_value()?))
        }
      }
      TokenKind::SquareBracketOpening => Ok(Value::ListValue(self.parse_list_value()?)),
      TokenKind::CurlyBracketOpening => Ok(Value::ObjectValue(self.parse_object_value()?)),
      _ => Err(SyntaxError {
        message: format!("Unexpected {}.", peeked),
        position: self.lexer.get_position(),
      }),
    }
  }

  fn parse_const_list_value(&mut self) -> Result<ConstListValue, SyntaxError> {
    let start_token = self.parse_token(TokenKind::SquareBracketOpening)?;
    let mut values = vec![];
    let mut next = self.peek_token(None)?;
    while next.kind != TokenKind::SquareBracketClosing {
      values.push(self.parse_const_value()?);
      next = self.peek_token(None)?
    }

    let end_token = self.parse_token(TokenKind::SquareBracketClosing)?;
    Ok(ConstListValue {
      values,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_const_object_field(&mut self) -> Result<ConstObjectField, SyntaxError> {
    let name = self.parse_name()?;
    self.parse_token(TokenKind::Colon)?;
    let value = self.parse_const_value()?;

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

  fn parse_const_object_value(&mut self) -> Result<ConstObjectValue, SyntaxError> {
    let start_token = self.parse_token(TokenKind::CurlyBracketOpening)?;
    let mut fields = vec![];

    let mut next = self.peek_token(Some(TokenKind::Name))?;
    while next.kind != TokenKind::CurlyBracketClosing {
      fields.push(self.parse_const_object_field()?);
      next = self.peek_token(Some(TokenKind::Name))?;
    }

    if next.kind != TokenKind::CurlyBracketClosing {
      return Err(SyntaxError {
        // Align with graphql-js: The error message always expects another
        // field instead of a closing bracket.
        message: format!("Expected {}, found {}.", TokenKind::Name, next),
        position: self.lexer.get_position(),
      });
    }
    let end_token = self.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok(ConstObjectValue {
      fields,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_const_value(&mut self) -> Result<ConstValue, SyntaxError> {
    let peeked = self.peek_token(None)?;
    match peeked.kind {
      TokenKind::Int => Ok(ConstValue::IntValue(self.parse_int_value()?)),
      TokenKind::Float => Ok(ConstValue::FloatValue(self.parse_float_value()?)),
      TokenKind::String { .. } => Ok(ConstValue::StringValue(self.parse_string_value()?)),
      TokenKind::Name => {
        if peeked.value == "true" || peeked.value == "false" {
          Ok(ConstValue::BooleanValue(self.parse_boolean_value()?))
        } else if peeked.value == "null" {
          Ok(ConstValue::NullValue(self.parse_null_value()?))
        } else {
          Ok(ConstValue::EnumValue(self.parse_enum_value()?))
        }
      }
      TokenKind::SquareBracketOpening => Ok(ConstValue::ListValue(self.parse_const_list_value()?)),
      TokenKind::CurlyBracketOpening => {
        Ok(ConstValue::ObjectValue(self.parse_const_object_value()?))
      }
      _ => Err(SyntaxError {
        message: format!("Unexpected {}.", peeked),
        position: self.lexer.get_position(),
      }),
    }
  }

  fn parse_description(&mut self) -> Result<StringValue, SyntaxError> {
    let token = self.next_token(None)?;
    let loc = Loc {
      start_token: token.clone(),
      end_token: token.clone(),
    };
    match token.kind {
      TokenKind::String { block } => Ok(StringValue {
        value: token.value,
        block,
        loc,
      }),
      _ => Err(SyntaxError {
        message: format!(
          "Expected {}, found {}",
          TokenKind::String { block: false },
          token
        ),
        position: self.lexer.get_position(),
      }),
    }
  }

  fn parse_arguments(
    &mut self,
    expected: Option<TokenKind>,
  ) -> Result<(Vec<Argument>, Option<Token>), SyntaxError> {
    if self.peek_token(expected)?.kind != TokenKind::RoundBracketOpening {
      return Ok((vec![], None));
    }

    self.parse_token(TokenKind::RoundBracketOpening)?;

    let mut arguments = vec![];
    let mut next = self.peek_token(Some(TokenKind::Name))?;
    while next.kind != TokenKind::RoundBracketClosing {
      let name = self.parse_name()?;
      self.parse_token(TokenKind::Colon)?;
      let value = self.parse_value()?;

      let start_token = name.loc.start_token.clone();
      let end_token = value.get_end_token();

      arguments.push(Argument {
        name,
        value,
        loc: Loc {
          start_token,
          end_token,
        },
      });

      next = self.peek_token(Some(TokenKind::Name))?;
    }

    let round_bracket_closing_token = self.parse_token(TokenKind::RoundBracketClosing)?;

    Ok((arguments, Some(round_bracket_closing_token)))
  }

  fn parse_const_arguments(
    &mut self,
    expected: Option<TokenKind>,
  ) -> Result<(Vec<ConstArgument>, Option<Token>), SyntaxError> {
    if self.peek_token(expected)?.kind != TokenKind::RoundBracketOpening {
      return Ok((vec![], None));
    }

    self.parse_token(TokenKind::RoundBracketOpening)?;

    let mut arguments = vec![];
    let mut next = self.peek_token(Some(TokenKind::Name))?;
    while next.kind != TokenKind::RoundBracketClosing {
      let name = self.parse_name()?;
      self.parse_token(TokenKind::Colon)?;
      let value = self.parse_const_value()?;

      let start_token = name.loc.start_token.clone();
      let end_token = value.get_end_token();

      arguments.push(ConstArgument {
        name,
        value,
        loc: Loc {
          start_token,
          end_token,
        },
      });

      next = self.peek_token(Some(TokenKind::Name))?;
    }

    let round_bracket_closing_token = self.parse_token(TokenKind::RoundBracketClosing)?;

    Ok((arguments, Some(round_bracket_closing_token)))
  }

  fn parse_directives(
    &mut self,
    expected: Option<TokenKind>,
  ) -> Result<Vec<Directive>, SyntaxError> {
    let expected_clone = expected.clone();

    let mut directives = vec![];
    let mut next = self.peek_token(expected)?;
    while next.kind == TokenKind::AtSign {
      let start_token = self.parse_token(TokenKind::AtSign)?;
      let name = self.parse_name()?;

      let (arguments, arguments_end_token) = self.parse_arguments(expected_clone.clone())?;

      let end_token = if arguments_end_token != None {
        arguments_end_token.unwrap()
      } else {
        name.loc.end_token.clone()
      };

      directives.push(Directive {
        name,
        arguments,
        loc: Loc {
          start_token,
          end_token,
        },
      });
      next = self.peek_token(expected_clone.clone())?;
    }
    Ok(directives)
  }

  fn parse_const_directives(
    &mut self,
    expected: Option<TokenKind>,
  ) -> Result<Vec<ConstDirective>, SyntaxError> {
    let expected_clone = expected.clone();

    let mut directives = vec![];
    let mut next = self.peek_token(expected)?;
    while next.kind == TokenKind::AtSign {
      let start_token = self.parse_token(TokenKind::AtSign)?;
      let name = self.parse_name()?;

      let (arguments, arguments_end_token) = self.parse_const_arguments(expected_clone.clone())?;

      let end_token = if arguments_end_token != None {
        arguments_end_token.unwrap()
      } else {
        name.loc.end_token.clone()
      };

      directives.push(ConstDirective {
        name,
        arguments,
        loc: Loc {
          start_token,
          end_token,
        },
      });
      next = self.peek_token(expected_clone.clone())?;
    }
    Ok(directives)
  }

  fn parse_variable_definitions(&mut self) -> Result<Vec<VariableDefinition>, SyntaxError> {
    self.parse_token(TokenKind::RoundBracketOpening)?;
    let mut variable_definitions = vec![];
    let mut next = self.peek_token(Some(TokenKind::DollarSign))?;

    // There must be at least one variable
    if next.kind != TokenKind::DollarSign {
      return Err(SyntaxError {
        message: format!("Expected {}, found {}.", TokenKind::DollarSign, next),
        position: self.lexer.get_position(),
      });
    }

    while next.kind == TokenKind::DollarSign {
      let variable = self.parse_variable()?;
      self.parse_token(TokenKind::Colon)?;
      let gql_type = self.parse_type()?;

      let default_value =
        if self.peek_token(Some(TokenKind::DollarSign))?.kind == TokenKind::EqualsSign {
          self.parse_token(TokenKind::EqualsSign)?;
          Some(self.parse_const_value()?)
        } else {
          None
        };

      let directives = if self.peek_token(Some(TokenKind::AtSign))?.kind == TokenKind::AtSign {
        self.parse_directives(Some(TokenKind::DollarSign))?
      } else {
        vec![]
      };

      let start_token = variable.loc.start_token.clone();
      let end_token = if directives.len() > 0 {
        directives.last().unwrap().loc.end_token.clone()
      } else if default_value != None {
        default_value.as_ref().unwrap().get_end_token()
      } else {
        gql_type.get_end_token()
      };

      variable_definitions.push(VariableDefinition {
        variable,
        gql_type,
        default_value,
        directives,
        loc: Loc {
          start_token,
          end_token,
        },
      });
      next = self.peek_token(Some(TokenKind::DollarSign))?;
    }

    if next.kind != TokenKind::RoundBracketClosing {
      return Err(SyntaxError {
        // Align with graphql-js: The error message always expects another
        // variable instead of a closing bracket.
        message: format!("Expected {}, found {}.", TokenKind::DollarSign, next),
        position: self.lexer.get_position(),
      });
    }
    self.next_token(Some(TokenKind::DollarSign))?;

    Ok(variable_definitions)
  }

  fn parse_implements_interface(&mut self) -> Result<Vec<NamedType>, SyntaxError> {
    let peeked = self.peek_token(None)?;
    if !(peeked.kind == TokenKind::Name && peeked.value == "implements") {
      return Ok(vec![]);
    }

    self.parse_token(TokenKind::Name)?;
    if self.peek_token(Some(TokenKind::Name))?.kind == TokenKind::Ampersand {
      self.parse_token(TokenKind::Ampersand)?;
    }
    let mut interfaces = vec![self.parse_named_type()?];

    while self.peek_token(None)?.kind == TokenKind::Ampersand {
      self.parse_token(TokenKind::Ampersand)?;
      interfaces.push(self.parse_named_type()?);
    }

    Ok(interfaces)
  }

  fn parse_arguments_definition(&mut self) -> Result<Vec<InputValueDefinition>, SyntaxError> {
    if self.peek_token(None)?.kind != TokenKind::RoundBracketOpening {
      return Ok(vec![]);
    }

    self.parse_token(TokenKind::RoundBracketOpening)?;

    let mut argument_definitions = vec![];
    let mut peeked = self.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::RoundBracketClosing {
      let description = if peeked.kind.equals(TokenKind::String { block: false }) {
        Some(self.parse_string_value()?)
      } else {
        None
      };

      let name = self.parse_name()?;

      self.parse_token(TokenKind::Colon)?;

      let gql_type = self.parse_type()?;

      let default_value = if self.peek_token(Some(TokenKind::Name))?.kind == TokenKind::EqualsSign {
        self.parse_token(TokenKind::EqualsSign)?;
        Some(self.parse_const_value()?)
      } else {
        None
      };

      let directives = self.parse_const_directives(Some(TokenKind::Name))?;

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

      argument_definitions.push(InputValueDefinition {
        description,
        name,
        gql_type,
        default_value,
        directives,
        loc: Loc {
          start_token,
          end_token,
        },
      });
      peeked = self.peek_token(Some(TokenKind::Name))?;
    }

    self.parse_token(TokenKind::RoundBracketClosing)?;

    Ok(argument_definitions)
  }

  fn parse_field_definitions(
    &mut self,
  ) -> Result<(Vec<FieldDefinition>, Option<Token>), SyntaxError> {
    if self.peek_token(None)?.kind != TokenKind::CurlyBracketOpening {
      return Ok((vec![], None));
    }

    self.parse_token(TokenKind::CurlyBracketOpening)?;

    let mut field_definitions = vec![];
    let mut peeked = self.peek_token(Some(TokenKind::Name))?;
    while peeked.kind != TokenKind::CurlyBracketClosing {
      let description = if peeked.kind.equals(TokenKind::String { block: false }) {
        Some(self.parse_string_value()?)
      } else {
        None
      };

      let name = self.parse_name()?;

      let arguments = self.parse_arguments_definition()?;

      self.parse_token(TokenKind::Colon)?;

      let gql_type = self.parse_type()?;

      let directives = self.parse_const_directives(None)?;

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

      field_definitions.push(FieldDefinition {
        description,
        name,
        arguments,
        gql_type,
        directives,
        loc: Loc {
          start_token,
          end_token,
        },
      });
      peeked = self.peek_token(Some(TokenKind::Name))?;
    }

    if field_definitions.len() == 0 {
      return Err(SyntaxError {
        message: format!(
          "Expected Name, found {}.",
          self.peek_token(Some(TokenKind::Name))?.kind
        ),
        position: self.lexer.get_position(),
      });
    }

    let curly_bracket_closing_token = self.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok((field_definitions, Some(curly_bracket_closing_token)))
  }

  fn parse_selection(&mut self) -> Result<Selection, SyntaxError> {
    let peeked = self.peek_token(Some(TokenKind::Name))?;
    match peeked.kind {
      TokenKind::Name => {
        let alias_or_name = self.parse_name()?;
        let start_token = alias_or_name.loc.start_token.clone();

        let peeked = self.peek_token(None)?;
        let (alias, name) = if peeked.kind == TokenKind::Colon {
          self.parse_token(TokenKind::Colon)?;
          (Some(alias_or_name), self.parse_name()?)
        } else {
          (None, alias_or_name)
        };

        let (arguments, arguments_end_token) = self.parse_arguments(Some(TokenKind::Name))?;

        let directives = if self.peek_token(Some(TokenKind::Name))?.kind == TokenKind::AtSign {
          self.parse_directives(Some(TokenKind::Name))?
        } else {
          vec![]
        };

        let selection_set =
          if self.peek_token(Some(TokenKind::Name))?.kind == TokenKind::CurlyBracketOpening {
            Some(self.parse_selection_set()?)
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
      TokenKind::Spread => {
        let start_token = self.parse_token(TokenKind::Spread)?;
        let peeked = self.peek_token(Some(TokenKind::CurlyBracketOpening))?;
        match peeked.kind {
          TokenKind::Name => {
            let name = self.parse_name()?;
            if name.value == "on" {
              let type_condition = Some(self.parse_named_type()?);

              let directives = if self.peek_token(Some(TokenKind::Name))?.kind == TokenKind::AtSign
              {
                self.parse_directives(Some(TokenKind::Name))?
              } else {
                vec![]
              };

              let selection_set = self.parse_selection_set()?;
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
            } else {
              let directives = if self.peek_token(Some(TokenKind::Name))?.kind == TokenKind::AtSign
              {
                self.parse_directives(Some(TokenKind::Name))?
              } else {
                vec![]
              };

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
          }
          TokenKind::AtSign => {
            let directives = if self.peek_token(Some(TokenKind::Name))?.kind == TokenKind::AtSign {
              self.parse_directives(Some(TokenKind::Name))?
            } else {
              vec![]
            };

            let selection_set = self.parse_selection_set()?;
            let end_token = selection_set.loc.end_token.clone();

            Ok(Selection::InlineFragment {
              type_condition: None,
              directives,
              selection_set,
              loc: Loc {
                start_token,
                end_token,
              },
            })
          }
          TokenKind::CurlyBracketOpening => {
            let selection_set = self.parse_selection_set()?;
            let end_token = selection_set.loc.end_token.clone();

            Ok(Selection::InlineFragment {
              type_condition: None,
              directives: vec![],
              selection_set,
              loc: Loc {
                start_token,
                end_token,
              },
            })
          }
          _ => Err(SyntaxError {
            message: format!(
              "Expected {}, found {}.",
              TokenKind::CurlyBracketOpening,
              peeked
            ),
            position: self.lexer.get_position(),
          }),
        }
      }
      _ => Err(SyntaxError {
        message: format!("Expected {}, found {}.", TokenKind::Name, peeked),
        position: self.lexer.get_position(),
      }),
    }
  }

  fn parse_selection_set(&mut self) -> Result<SelectionSet, SyntaxError> {
    let start_token = self.parse_token(TokenKind::CurlyBracketOpening)?;
    let mut selections = vec1![self.parse_selection()?];

    let mut next = self.peek_token(Some(TokenKind::Name))?;
    while next.kind != TokenKind::CurlyBracketClosing {
      selections.push(self.parse_selection()?);
      next = self.peek_token(Some(TokenKind::Name))?;
    }

    let end_token = self.parse_token(TokenKind::CurlyBracketClosing)?;

    Ok(SelectionSet {
      selections,
      loc: Loc {
        start_token,
        end_token,
      },
    })
  }

  fn parse_operation_type_definition(&mut self) -> Result<OperationTypeDefinition, SyntaxError> {
    let start_token = self.parse_token(TokenKind::Name)?;

    let operation = if start_token.value == "query" {
      OperationType::query
    } else if start_token.value == "mutation" {
      OperationType::mutation
    } else if start_token.value == "subscription" {
      OperationType::subscription
    } else {
      return Err(SyntaxError {
        message: format!("Unexpected name \"{}\".", start_token.value),
        position: self.lexer.get_position(),
      });
    };

    self.parse_token(TokenKind::Colon)?;

    let gql_type = self.parse_named_type()?;

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

  fn parse_operation_definition(&mut self) -> Result<Definition, SyntaxError> {
    let peeked = self.peek_token(None)?;
    match peeked.kind {
      TokenKind::CurlyBracketOpening => {
        let selection_set = self.parse_selection_set()?;

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
        let start_token = self.parse_token(TokenKind::Name)?;
        let operation = if start_token.value == "query" {
          OperationType::query
        } else if start_token.value == "mutation" {
          OperationType::mutation
        } else if start_token.value == "subscription" {
          OperationType::subscription
        } else {
          return Err(SyntaxError {
            message: format!("Unexpected {}.", start_token),
            position: self.lexer.get_position(),
          });
        };

        let name = if self.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind == TokenKind::Name
        {
          Some(self.parse_name()?)
        } else {
          None
        };

        let variable_definitions = if self.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind
          == TokenKind::RoundBracketOpening
        {
          self.parse_variable_definitions()?
        } else {
          vec![]
        };

        let directives =
          if self.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind == TokenKind::AtSign {
            self.parse_directives(Some(TokenKind::CurlyBracketOpening))?
          } else {
            vec![]
          };

        let selection_set = self.parse_selection_set()?;
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
      _ => Err(SyntaxError {
        message: format!("Unexpected {}.", peeked),
        position: self.lexer.get_position(),
      }),
    }
  }

  fn parse_fragment_definition(&mut self) -> Result<Definition, SyntaxError> {
    let start_token = self.parse_token(TokenKind::Name)?;

    let name = self.parse_name()?;

    let on = self.next_token(None)?;
    match on.kind {
      TokenKind::Name => {
        if on.value != "on" {
          return Err(SyntaxError {
            message: format!("Expected \"on\", found {}.", on),
            position: self.lexer.get_position(),
          });
        }
      }
      _ => {
        return Err(SyntaxError {
          message: format!("Expected \"on\", found {}.", on),
          position: self.lexer.get_position(),
        })
      }
    }

    let type_condition = self.parse_named_type()?;

    let directives =
      if self.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind == TokenKind::AtSign {
        self.parse_directives(Some(TokenKind::CurlyBracketOpening))?
      } else {
        vec![]
      };

    let selection_set = self.parse_selection_set()?;

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
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    let schema = self.parse_token(TokenKind::Name)?;

    let directives =
      if self.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind == TokenKind::AtSign {
        self.parse_const_directives(Some(TokenKind::CurlyBracketOpening))?
      } else {
        vec![]
      };

    self.parse_token(TokenKind::CurlyBracketOpening)?;

    let mut operation_types = vec1![self.parse_operation_type_definition()?];
    let mut next = self.peek_token(Some(TokenKind::Name))?;
    while next.kind != TokenKind::CurlyBracketClosing {
      operation_types.push(self.parse_operation_type_definition()?);
      next = self.peek_token(Some(TokenKind::Name))?;
    }

    let start_token = match description {
      None => schema,
      Some(ref description) => description.loc.start_token.clone(),
    };
    let end_token = self.parse_token(TokenKind::CurlyBracketClosing)?;

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
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    let scalar = self.parse_token(TokenKind::Name)?;

    let name = self.parse_name()?;

    let directives =
      if self.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind == TokenKind::AtSign {
        self.parse_const_directives(Some(TokenKind::CurlyBracketOpening))?
      } else {
        vec![]
      };

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
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    let name_token = self.parse_token(TokenKind::Name)?;

    let name = self.parse_name()?;

    let interfaces = self.parse_implements_interface()?;

    let directives =
      if self.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind == TokenKind::AtSign {
        self.parse_const_directives(Some(TokenKind::CurlyBracketOpening))?
      } else {
        vec![]
      };

    let (fields, curly_bracket_closing_token) = self.parse_field_definitions()?;

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
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_union_type_definition(
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_enum_type_definition(
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_input_object_type_definition(
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_directive_definition(
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_schema_extension(&mut self, start_token: Token) -> Result<Definition, SyntaxError> {
    self.parse_token(TokenKind::Name)?;

    let directives =
      if self.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind == TokenKind::AtSign {
        self.parse_const_directives(Some(TokenKind::CurlyBracketOpening))?
      } else {
        vec![]
      };

    let mut end_token = None;

    let operation_types =
      if directives.len() == 0 || self.peek_token(None)?.kind == TokenKind::CurlyBracketOpening {
        self.next_token(None)?;
        let mut operation_type_definitions = vec![self.parse_operation_type_definition()?];
        let mut next = self.peek_token(Some(TokenKind::Name))?;
        while next.kind != TokenKind::CurlyBracketClosing {
          operation_type_definitions.push(self.parse_operation_type_definition()?);
          next = self.peek_token(Some(TokenKind::Name))?;
        }
        end_token = Some(self.parse_token(TokenKind::CurlyBracketClosing)?);
        operation_type_definitions
      } else {
        vec![]
      };

    if end_token == None {
      end_token = if operation_types.len() > 0 {
        Some(operation_types.last().unwrap().loc.end_token.clone())
      } else {
        Some(directives.last().unwrap().loc.end_token.clone())
      };
    }

    Ok(Definition::SchemaExtension {
      directives,
      operation_types,
      loc: Loc {
        start_token,
        end_token: end_token.unwrap(),
      },
    })
  }

  fn parse_scalar_extension(&mut self, start_token: Token) -> Result<Definition, SyntaxError> {
    self.parse_token(TokenKind::Name)?;

    let name = self.parse_name()?;

    let directives = self.parse_const_directives(None)?;

    if directives.len() == 0 {
      return Err(SyntaxError {
        message: format!(""),
        position: self.lexer.get_position(),
      });
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

  fn parse_object_type_extension(&mut self, start_token: Token) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_interface_type_extension(
    &mut self,
    start_token: Token,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_union_type_extension(&mut self, start_token: Token) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_enum_type_extension(&mut self, start_token: Token) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_input_object_type_extension(
    &mut self,
    start_token: Token,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_definition(&mut self) -> Result<Definition, SyntaxError> {
    let peeked = self.peek_token(None)?;
    let is_extension = peeked.kind == TokenKind::Name && peeked.value == "extend";
    if is_extension {
      // Skip the "extend" keyword
      let start_token = self.parse_token(TokenKind::Name)?;

      // An extension must be followed by a type keyworkd
      let peeked = self.peek_token(None)?;
      return match peeked.kind {
        TokenKind::Name => {
          if peeked.value == "schema" {
            self.parse_schema_extension(start_token)
          } else if peeked.value == "scalar" {
            self.parse_scalar_extension(start_token)
          } else if peeked.value == "type" {
            self.parse_object_type_extension(start_token)
          } else if peeked.value == "interface" {
            self.parse_interface_type_extension(start_token)
          } else if peeked.value == "union" {
            self.parse_union_type_extension(start_token)
          } else if peeked.value == "enum" {
            self.parse_enum_type_extension(start_token)
          } else if peeked.value == "input" {
            self.parse_input_object_type_extension(start_token)
          } else {
            Err(SyntaxError {
              message: format!("Unexpected {}", peeked),
              position: self.lexer.get_position(),
            })
          }
        }
        _ => Err(SyntaxError {
          message: format!("Unexpected {}", peeked),
          position: self.lexer.get_position(),
        }),
      };
    }

    let description = if (self.peek_token(None)?.kind == (TokenKind::String { block: true }))
      || (self.peek_token(None)?.kind == (TokenKind::String { block: false }))
    {
      // Parse the description
      let string_value = self.parse_description()?;

      // A description must be followed by a type system definition
      let peeked = self.peek_token(None)?;
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
            return Err(SyntaxError {
              message: format!("Unexpected {}", peeked),
              position: self.lexer.get_position(),
            });
          }
        }
        _ => {
          return Err(SyntaxError {
            message: format!("Unexpected {}", peeked),
            position: self.lexer.get_position(),
          })
        }
      }
    } else {
      None
    };

    let token = self.peek_token(None)?;
    match token.kind {
      TokenKind::CurlyBracketOpening => self.parse_operation_definition(),
      TokenKind::Name => {
        if token.value == "query" || token.value == "mutation" || token.value == "subscription" {
          self.parse_operation_definition()
        } else if token.value == "fragment" {
          self.parse_fragment_definition()
        } else if token.value == "schema" {
          self.parse_schema_definition(description)
        } else if token.value == "scalar" {
          self.parse_scalar_type_definition(description)
        } else if token.value == "type" {
          self.parse_object_type_definition(description)
        } else if token.value == "interface" {
          self.parse_interface_type_definition(description)
        } else if token.value == "union" {
          self.parse_union_type_definition(description)
        } else if token.value == "enum" {
          self.parse_enum_type_definition(description)
        } else if token.value == "input" {
          self.parse_input_object_type_definition(description)
        } else if token.value == "directive" {
          self.parse_directive_definition(description)
        } else {
          Err(SyntaxError {
            message: format!("Unexpected {}.", token),
            position: token.start,
          })
        }
      }
      _ => Err(SyntaxError {
        message: format!("Unexpected {}.", token),
        position: token.start,
      }),
    }
  }

  pub fn parse_document(&mut self) -> Result<Document, SyntaxError> {
    let start_token = self.parse_token(TokenKind::SOF)?;
    let mut definitions = vec1![self.parse_definition()?];
    while self.lexer.has_more() {
      definitions.push(self.parse_definition()?);
    }

    let end_token = self.parse_token(TokenKind::EOF)?;
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
  Parser::new(query).parse_document()
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
