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

enum OperationTypeWithShorthand {
  Shorthand,
  NonShorthand { operation_type: OperationType },
}

enum TypeModifier {
  Description { token: lexer::Token },
  Extension,
  None,
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

  fn parse_token(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::Token, SyntaxError> {
    let maybe_token = self.lexer.next()?;
    if maybe_token == None {
      return Err(SyntaxError {
        message: format!("Expected {}, got no token", token_kind),
        position: self.lexer.get_position(),
      });
    }

    let token = maybe_token.unwrap();
    if token.kind == token_kind {
      Ok(token)
    } else {
      Err(SyntaxError {
        message: format!("Expected {}, got {}", token_kind, token.kind),
        position: token.start,
      })
    }
  }

  fn parse_operation_definition(
    &mut self,
    operation_type: OperationTypeWithShorthand,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_fragment_definition(&mut self) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_schema_definition(
    &mut self,
    description: Option<lexer::Token>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_scalar_type_definition(
    &mut self,
    description: Option<lexer::Token>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_object_type_definition(
    &mut self,
    description: Option<lexer::Token>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_interface_type_definition(
    &mut self,
    description: Option<lexer::Token>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_union_type_definition(
    &mut self,
    description: Option<lexer::Token>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_enum_type_definition(
    &mut self,
    description: Option<lexer::Token>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_input_object_type_definition(
    &mut self,
    description: Option<lexer::Token>,
  ) -> Result<Definition, SyntaxError> {
    Err(SyntaxError {
      message: String::from("TODO:"),
      position: 999,
    })
  }

  fn parse_directive_definition(
    &mut self,
    description: Option<lexer::Token>,
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

  fn parse_definition(&mut self, modifier: TypeModifier) -> Result<Definition, SyntaxError> {
    let maybe_token = self.lexer.next()?;
    if maybe_token == None {
      return Err(SyntaxError {
        message: String::from("Expected definition, got no token"),
        position: self.lexer.get_position(),
      });
    }

    let token = maybe_token.unwrap();
    match token.kind {
      lexer::TokenKind::CurlyBracketOpening => {
        self.parse_operation_definition(OperationTypeWithShorthand::Shorthand)
      }
      lexer::TokenKind::Name => {
        if token.value == "query" && matches!(modifier, TypeModifier::None) {
          self.parse_operation_definition(OperationTypeWithShorthand::NonShorthand {
            operation_type: OperationType::query,
          })
        } else if token.value == "mutation" && matches!(modifier, TypeModifier::None) {
          self.parse_operation_definition(OperationTypeWithShorthand::NonShorthand {
            operation_type: OperationType::mutation,
          })
        } else if token.value == "subscription" && matches!(modifier, TypeModifier::None) {
          self.parse_operation_definition(OperationTypeWithShorthand::NonShorthand {
            operation_type: OperationType::subscription,
          })
        } else if token.value == "fragment" && matches!(modifier, TypeModifier::None) {
          self.parse_fragment_definition()
        } else if token.value == "schema" {
          match modifier {
            TypeModifier::None => self.parse_schema_definition(None),
            TypeModifier::Description { token } => self.parse_schema_definition(Some(token)),
            TypeModifier::Extension => self.parse_schema_extension(),
          }
        } else if token.value == "scalar" && !matches!(modifier, TypeModifier::Extension) {
          match modifier {
            TypeModifier::None => self.parse_scalar_type_definition(None),
            TypeModifier::Description { token } => self.parse_scalar_type_definition(Some(token)),
            TypeModifier::Extension => self.parse_scalar_extension(),
          }
        } else if token.value == "type" && !matches!(modifier, TypeModifier::Extension) {
          match modifier {
            TypeModifier::None => self.parse_object_type_definition(None),
            TypeModifier::Description { token } => self.parse_object_type_definition(Some(token)),
            TypeModifier::Extension => self.parse_object_type_extension(),
          }
        } else if token.value == "interface" && !matches!(modifier, TypeModifier::Extension) {
          match modifier {
            TypeModifier::None => self.parse_interface_type_definition(None),
            TypeModifier::Description { token } => {
              self.parse_interface_type_definition(Some(token))
            }
            TypeModifier::Extension => self.parse_interface_type_extension(),
          }
        } else if token.value == "union" && !matches!(modifier, TypeModifier::Extension) {
          match modifier {
            TypeModifier::None => self.parse_union_type_definition(None),
            TypeModifier::Description { token } => self.parse_union_type_definition(Some(token)),
            TypeModifier::Extension => self.parse_union_type_extension(),
          }
        } else if token.value == "enum" && !matches!(modifier, TypeModifier::Extension) {
          match modifier {
            TypeModifier::None => self.parse_enum_type_definition(None),
            TypeModifier::Description { token } => self.parse_enum_type_definition(Some(token)),
            TypeModifier::Extension => self.parse_enum_type_extension(),
          }
        } else if token.value == "input" && !matches!(modifier, TypeModifier::Extension) {
          match modifier {
            TypeModifier::None => self.parse_input_object_type_definition(None),
            TypeModifier::Description { token } => {
              self.parse_input_object_type_definition(Some(token))
            }
            TypeModifier::Extension => self.parse_input_object_type_extension(),
          }
        } else if token.value == "directive" && !matches!(modifier, TypeModifier::Extension) {
          match modifier {
            TypeModifier::None => self.parse_directive_definition(None),
            TypeModifier::Description { token } => self.parse_directive_definition(Some(token)),
            TypeModifier::Extension => Err(SyntaxError {
              message: String::from("Unexpected name \"directive\""),
              position: token.start,
            }),
          }
        } else if token.value == "extend" && matches!(modifier, TypeModifier::None) {
          self.parse_definition(TypeModifier::Extension)
        } else {
          Err(SyntaxError {
            message: format!("Unexpected name \"{}\"", token.value),
            position: token.start,
          })
        }
      }
      lexer::TokenKind::String => self.parse_definition(TypeModifier::Description { token }),
      lexer::TokenKind::Int => Err(SyntaxError {
        message: format!("Unexpected Int \"{}\".", token.value),
        position: token.start,
      }),
      lexer::TokenKind::Float => Err(SyntaxError {
        message: format!("Unexpected Float \"{}\".", token.value),
        position: token.start,
      }),
      kind => Err(SyntaxError {
        message: format!("Unexpected \"{}\".", kind),
        position: token.start,
      }),
    }
  }

  pub fn parse_document(&mut self) -> Result<Document, SyntaxError> {
    let start_token = self.parse_token(lexer::TokenKind::SOF)?;
    let mut definitions = vec1![self.parse_definition(TypeModifier::None)?];
    while self.lexer.has_more() {
      definitions.push(self.parse_definition(TypeModifier::None)?);
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
