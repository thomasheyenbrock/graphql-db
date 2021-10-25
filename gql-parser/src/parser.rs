use crate::error::SyntaxError;
use crate::lexer;
use vec1::Vec1;

#[derive(Debug, PartialEq)]
pub struct Loc {
  pub start_token: lexer::Token,
  pub end_token: lexer::Token,
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
  fn get_end_token(&self) -> lexer::Token {
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
  fn get_end_token(&self) -> lexer::Token {
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
  lexer: lexer::Lexer<'a>,
}

impl Parser<'_> {
  pub fn new(query: &str) -> Parser {
    Parser {
      lexer: lexer::Lexer::new(query),
    }
  }

  fn next_token(
    &mut self,
    expected: Option<lexer::TokenKind>,
  ) -> Result<lexer::Token, SyntaxError> {
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

  fn peek_token(
    &mut self,
    expected: Option<lexer::TokenKind>,
  ) -> Result<lexer::Token, SyntaxError> {
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

  fn parse_token(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::Token, SyntaxError> {
    let token = self.next_token(Some(token_kind.clone()))?;
    if token.kind == token_kind {
      Ok(token)
    } else {
      Err(SyntaxError {
        message: format!("Expected {}, found {}.", token_kind, token.kind),
        position: token.start,
      })
    }
  }

  fn parse_name(&mut self) -> Result<Name, SyntaxError> {
    let token = self.parse_token(lexer::TokenKind::Name)?;
    let loc = Loc {
      start_token: token.clone(),
      end_token: token.clone(),
    };
    Ok(Name {
      value: token.value,
      loc,
    })
  }

  fn parse_type(&mut self) -> Result<Type, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_variable(&mut self) -> Result<Variable, SyntaxError> {
    let start_token = self.parse_token(lexer::TokenKind::DollarSign)?;
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

  fn parse_const_value(&mut self) -> Result<ConstValue, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_description(&mut self) -> Result<StringValue, SyntaxError> {
    let token = self.next_token(None)?;
    let loc = Loc {
      start_token: token.clone(),
      end_token: token.clone(),
    };
    match token.kind {
      lexer::TokenKind::String { block } => Ok(StringValue {
        value: token.value,
        block,
        loc,
      }),
      _ => Err(SyntaxError {
        message: format!(
          "Expected {}, found {}",
          lexer::TokenKind::String { block: false },
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
    self.parse_token(lexer::TokenKind::Colon)?;
    let gql_type = self.parse_type()?;

    let default_value = if self.peek_token(Some(lexer::TokenKind::DollarSign))?.kind
      == lexer::TokenKind::EqualsSign
    {
      Some(self.parse_const_value()?)
    } else {
      None
    };

    let directives =
      if self.peek_token(Some(lexer::TokenKind::AtSign))?.kind == lexer::TokenKind::AtSign {
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
    self.parse_token(lexer::TokenKind::RoundBracketOpening)?;
    let mut variable_definitions = vec![];
    let mut next = self.peek_token(Some(lexer::TokenKind::DollarSign))?;

    // There must be at least one variable
    if next.kind != lexer::TokenKind::DollarSign {
      return Err(SyntaxError {
        message: format!("Expected {}, found {}.", lexer::TokenKind::DollarSign, next),
        position: self.lexer.get_position(),
      });
    }

    while next.kind == lexer::TokenKind::DollarSign {
      variable_definitions.push(self.parse_variable_definition()?);
      next = self.peek_token(Some(lexer::TokenKind::DollarSign))?;
    }

    if next.kind != lexer::TokenKind::RoundBracketClosing {
      return Err(SyntaxError {
        // Align with graphql-js: The error message always expects another
        // variable instead of a closing bracket.
        message: format!("Expected {}, found {}.", lexer::TokenKind::DollarSign, next),
        position: self.lexer.get_position(),
      });
    }
    self.next_token(Some(lexer::TokenKind::DollarSign))?;

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
      lexer::TokenKind::CurlyBracketOpening => Ok(Definition::OperationDefinition {
        operation: OperationType::query,
        name: None,
        variable_definitions: vec![],
        directives: vec![],
        selection_set: self.parse_selection_set()?,
        loc: Loc {
          start_token,
          end_token: self.parse_token(lexer::TokenKind::CurlyBracketClosing)?,
        },
      }),
      lexer::TokenKind::Name => {
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

        let name = if self
          .peek_token(Some(lexer::TokenKind::CurlyBracketOpening))?
          .kind
          == lexer::TokenKind::Name
        {
          Some(self.parse_name()?)
        } else {
          None
        };

        let variable_definitions = if self
          .peek_token(Some(lexer::TokenKind::CurlyBracketOpening))?
          .kind
          == lexer::TokenKind::RoundBracketOpening
        {
          self.parse_variable_definitions()?
        } else {
          vec![]
        };

        let directives = if self
          .peek_token(Some(lexer::TokenKind::CurlyBracketOpening))?
          .kind
          == lexer::TokenKind::Name
        {
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
            end_token: self.parse_token(lexer::TokenKind::CurlyBracketClosing)?,
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
    let is_extension = peeked.kind == lexer::TokenKind::Name && peeked.value == "extend";
    if is_extension {
      // Skip the "extend" keyword
      self.next_token(None)?;

      // An extension must be followed by a type keyworkd
      let peeked = self.peek_token(None)?;
      return match peeked.kind {
        lexer::TokenKind::Name => {
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

    let description = if (self.peek_token(None)?.kind == (lexer::TokenKind::String { block: true }))
      || (self.peek_token(None)?.kind == (lexer::TokenKind::String { block: false }))
    {
      // Parse the description
      let string_value = self.parse_description()?;

      // A description must be followed by a type system definition
      let peeked = self.peek_token(None)?;
      match peeked.kind {
        lexer::TokenKind::Name => {
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
      lexer::TokenKind::CurlyBracketOpening => self.parse_operation_definition(),
      lexer::TokenKind::Name => {
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
    let start_token = self.parse_token(lexer::TokenKind::SOF)?;
    let mut definitions = vec1![self.parse_definition()?];
    while self.lexer.has_more() {
      definitions.push(self.parse_definition()?);
    }

    let end_token = self.parse_token(lexer::TokenKind::EOF)?;
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
                  start_token: lexer::Token {
                    kind: lexer::TokenKind::Name,
                    value: String::from("hello"),
                    start: 2,
                    end: 7,
                    line: 1,
                    column: 3
                  },
                  end_token: lexer::Token {
                    kind: lexer::TokenKind::Name,
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
                start_token: lexer::Token {
                  kind: lexer::TokenKind::Name,
                  value: String::from("hello"),
                  start: 2,
                  end: 7,
                  line: 1,
                  column: 3
                },
                end_token: lexer::Token {
                  kind: lexer::TokenKind::Name,
                  value: String::from("hello"),
                  start: 2,
                  end: 7,
                  line: 1,
                  column: 3
                },
              },
            }],
            loc: Loc {
              start_token: lexer::Token {
                kind: lexer::TokenKind::CurlyBracketOpening,
                value: String::from("{"),
                start: 0,
                end: 1,
                line: 1,
                column: 1
              },
              end_token: lexer::Token {
                kind: lexer::TokenKind::CurlyBracketClosing,
                value: String::from("}"),
                start: 8,
                end: 9,
                line: 1,
                column: 9
              },
            },
          },
          loc: Loc {
            start_token: lexer::Token {
              kind: lexer::TokenKind::CurlyBracketOpening,
              value: String::from("{"),
              start: 0,
              end: 1,
              line: 1,
              column: 1
            },
            end_token: lexer::Token {
              kind: lexer::TokenKind::CurlyBracketClosing,
              value: String::from("}"),
              start: 8,
              end: 9,
              line: 1,
              column: 9
            },
          },
        }],
        loc: Loc {
          start_token: lexer::Token {
            kind: lexer::TokenKind::SOF,
            value: String::from("<SOF>"),
            line: 0,
            column: 0,
            start: 0,
            end: 0,
          },
          end_token: lexer::Token {
            kind: lexer::TokenKind::EOF,
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
