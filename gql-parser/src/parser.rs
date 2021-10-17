use super::lexer;
use vec1::Vec1;

#[derive(Debug)]
pub struct StructureError {
  pub message: String,
  pub position: i32,
}

#[derive(Debug)]
pub struct Loc {
  pub start_token: lexer::Token,
  pub end_token: lexer::Token,
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum OperationType {
  query,
  mutation,
  subscription,
}

#[derive(Debug)]
pub struct Name {
  pub value: String,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct NamedType {
  pub name: Name,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct ListType {
  pub gql_type: Box<Type>,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct NonNullType {
  pub gql_type: NullableType,
  pub loc: Loc,
}

#[derive(Debug)]
pub enum NullableType {
  NamedType(NamedType),
  ListType(ListType),
}

#[derive(Debug)]
pub enum Type {
  NamedType(NamedType),
  ListType(ListType),
  NonNullType(NonNullType),
}

#[derive(Debug)]
pub struct Variable {
  pub name: Name,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct IntValue {
  pub value: String,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct FloatValue {
  pub value: String,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct StringValue {
  pub value: String,
  pub block: bool,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct BooleanValue {
  pub value: bool,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct NullValue {
  pub loc: Loc,
}

#[derive(Debug)]
pub struct EnumValue {
  pub value: String,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct ListValue {
  pub values: Vec<Value>,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct ObjectField {
  pub name: Name,
  pub value: Value,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct ObjectValue {
  pub fields: Vec<ObjectField>,
  pub loc: Loc,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ConstListValue {
  pub values: Vec<ConstValue>,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct ConstObjectField {
  pub name: Name,
  pub value: ConstValue,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct ConstObjectValue {
  pub fields: Vec<ConstObjectField>,
  pub loc: Loc,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Argument {
  pub name: Name,
  pub value: Value,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct ConstArgument {
  pub name: Name,
  pub value: ConstValue,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct Directive {
  pub name: Name,
  pub arguments: Vec<Argument>,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct ConstDirective {
  pub name: Name,
  pub arguments: Vec<ConstArgument>,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct VariableDefinition {
  pub variable: Variable,
  pub gql_type: Type,
  pub default_value: Option<ConstValue>,
  pub directives: Vec<Directive>,
  pub loc: Loc,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct SelectionSet {
  pub selections: Vec1<Selection>,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct OperationTypeDefinition {
  pub operation: OperationType,
  pub gql_type: NamedType,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct InputValueDefinition {
  pub description: Option<StringValue>,
  pub name: Name,
  pub gql_type: Type,
  pub default_value: Option<ConstValue>,
  pub directives: Vec<ConstDirective>,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct FieldDefinition {
  pub description: Option<StringValue>,
  pub name: Name,
  pub arguments: Vec<InputValueDefinition>,
  pub gql_type: Type,
  pub directives: Vec<ConstDirective>,
  pub loc: Loc,
}

#[derive(Debug)]
pub struct EnumValueDefinition {
  pub description: Option<StringValue>,
  pub enum_value: EnumValue,
  pub directives: Vec<ConstDirective>,
  pub loc: Loc,
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Document {
  pub definitions: Vec1<Definition>,
  pub loc: Loc,
}
