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
          None => String::from("Unexpected <EOF>."),
          Some(expected) => format!("Expected {}, found <EOF>.", expected),
        },
        position: self.lexer.get_position(),
      }),
      Some(token) => Ok(token),
    }
  }

  fn peek_token(&mut self, expected: Option<TokenKind>) -> Result<Token, SyntaxError> {
    match self.lexer.peek()? {
      None => Err(SyntaxError {
        message: match expected {
          None => String::from("Unexpected <EOF>."),
          Some(expected) => format!("Expected {}, found <EOF>.", expected),
        },
        position: self.lexer.get_position(),
      }),
      Some(token) => Ok(token),
    }
  }

  fn parse_token(&mut self, token_kind: TokenKind) -> Result<Token, SyntaxError> {
    let token = self.next_token(Some(token_kind.clone()))?;
    let cloned_token = token.clone();
    if matches!(token.kind, token_kind) {
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
    let end_token = self.next_token(Some(TokenKind::Name))?;

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

  fn parse_directives(&mut self) -> Result<Vec<Directive>, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_variable_definition(&mut self) -> Result<VariableDefinition, SyntaxError> {
    let variable = self.parse_variable()?;
    self.parse_token(TokenKind::Colon)?;
    let gql_type = self.parse_type()?;

    let default_value =
      if self.peek_token(Some(TokenKind::DollarSign))?.kind == TokenKind::EqualsSign {
        Some(self.parse_const_value()?)
      } else {
        None
      };

    let directives = if self.peek_token(Some(TokenKind::AtSign))?.kind == TokenKind::AtSign {
      self.parse_directives()?
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
      variable_definitions.push(self.parse_variable_definition()?);
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

  fn parse_selection_set(&mut self) -> Result<SelectionSet, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_operation_definition(&mut self) -> Result<Definition, SyntaxError> {
    let start_token = self.next_token(None)?;
    match start_token.kind {
      TokenKind::CurlyBracketOpening => Ok(Definition::OperationDefinition {
        operation: OperationType::query,
        name: None,
        variable_definitions: vec![],
        directives: vec![],
        selection_set: self.parse_selection_set()?,
        loc: Loc {
          start_token,
          end_token: self.parse_token(TokenKind::CurlyBracketClosing)?,
        },
      }),
      TokenKind::Name => {
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
          if self.peek_token(Some(TokenKind::CurlyBracketOpening))?.kind == TokenKind::Name {
            self.parse_directives()?
          } else {
            vec![]
          };

        Ok(Definition::OperationDefinition {
          operation,
          name,
          variable_definitions,
          directives,
          selection_set: self.parse_selection_set()?,
          loc: Loc {
            start_token,
            end_token: self.parse_token(TokenKind::CurlyBracketClosing)?,
          },
        })
      }
      _ => Err(SyntaxError {
        message: format!("Unexpected {}.", start_token),
        position: self.lexer.get_position(),
      }),
    }
  }

  fn parse_fragment_definition(&mut self) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_schema_definition(
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_scalar_type_definition(
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_object_type_definition(
    &mut self,
    description: Option<StringValue>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
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

  fn parse_schema_extension(&mut self) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_scalar_extension(&mut self) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_object_type_extension(&mut self) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_interface_type_extension(&mut self) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_union_type_extension(&mut self) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_enum_type_extension(&mut self) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_input_object_type_extension(&mut self) -> Result<Definition, SyntaxError> {
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
      self.next_token(None)?;

      // An extension must be followed by a type keyworkd
      let peeked = self.peek_token(None)?;
      return match peeked.kind {
        TokenKind::Name => {
          if peeked.value == "schema" {
            self.parse_schema_extension()
          } else if peeked.value == "scalar" {
            self.parse_scalar_extension()
          } else if peeked.value == "type" {
            self.parse_object_type_extension()
          } else if peeked.value == "interface" {
            self.parse_interface_type_extension()
          } else if peeked.value == "union" {
            self.parse_union_type_extension()
          } else if peeked.value == "enum" {
            self.parse_enum_type_extension()
          } else if peeked.value == "input" {
            self.parse_input_object_type_extension()
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
  fn should_work() {
    assert_eq!(
      parse("query ($foo: Int) { hello }"),
      Ok(Document {
        definitions: vec1![Definition::OperationDefinition {
          operation: OperationType::query,
          name: None,
          variable_definitions: vec![],
          directives: vec![],
          selection_set: SelectionSet {
            selections: vec1![Selection::Field {
              name: Name {
                value: String::from("hello"),
                loc: Loc {
                  start_token: Token {
                    kind: TokenKind::Name,
                    value: String::from("hello"),
                    start: 2,
                    end: 7,
                    line: 1,
                    column: 3
                  },
                  end_token: Token {
                    kind: TokenKind::Name,
                    value: String::from("hello"),
                    start: 2,
                    end: 7,
                    line: 1,
                    column: 3
                  },
                },
              },
              alias: None,
              arguments: vec![],
              directives: vec![],
              selection_set: None,
              loc: Loc {
                start_token: Token {
                  kind: TokenKind::Name,
                  value: String::from("hello"),
                  start: 2,
                  end: 7,
                  line: 1,
                  column: 3
                },
                end_token: Token {
                  kind: TokenKind::Name,
                  value: String::from("hello"),
                  start: 2,
                  end: 7,
                  line: 1,
                  column: 3
                },
              },
            }],
            loc: Loc {
              start_token: Token {
                kind: TokenKind::CurlyBracketOpening,
                value: String::from("{"),
                start: 0,
                end: 1,
                line: 1,
                column: 1
              },
              end_token: Token {
                kind: TokenKind::CurlyBracketClosing,
                value: String::from("}"),
                start: 8,
                end: 9,
                line: 1,
                column: 9
              },
            },
          },
          loc: Loc {
            start_token: Token {
              kind: TokenKind::CurlyBracketOpening,
              value: String::from("{"),
              start: 0,
              end: 1,
              line: 1,
              column: 1
            },
            end_token: Token {
              kind: TokenKind::CurlyBracketClosing,
              value: String::from("}"),
              start: 8,
              end: 9,
              line: 1,
              column: 9
            },
          },
        }],
        loc: Loc {
          start_token: Token {
            kind: TokenKind::SOF,
            value: String::from("<SOF>"),
            line: 0,
            column: 0,
            start: 0,
            end: 0,
          },
          end_token: Token {
            kind: TokenKind::EOF,
            value: String::from("<EOF>"),
            start: 9,
            end: 9,
            line: 1,
            column: 10
          }
        }
      })
    )
  }
}
