use gql_parser::*;
use neon::prelude::*;

fn transform_name<'a>(cx: &mut CallContext<'a, JsObject>, name: &Name) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("Name");
  obj.set(cx, "kind", kind)?;

  let value = cx.string(name.value.clone());
  obj.set(cx, "value", value)?;

  let loc = cx.empty_object();
  let start = cx.number(name.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(name.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_named_type<'a>(
  cx: &mut CallContext<'a, JsObject>,
  named_type: &NamedType,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("NamedType");
  obj.set(cx, "kind", kind)?;

  let name = transform_name(cx, &named_type.name)?;
  obj.set(cx, "name", name)?;

  let loc = cx.empty_object();
  let start = cx.number(named_type.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(named_type.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_type<'a>(
  cx: &mut CallContext<'a, JsObject>,
  gql_type: &Type,
) -> JsResult<'a, JsObject> {
  match gql_type {
    Type::NamedType(named_type) => transform_named_type(cx, named_type),
    _ => Ok(cx.empty_object()), // TODO: remove this
  }
}

fn transform_variable<'a>(
  cx: &mut CallContext<'a, JsObject>,
  variable: &Variable,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("Variable");
  obj.set(cx, "kind", kind)?;

  let name = transform_name(cx, &variable.name)?;
  obj.set(cx, "name", name)?;

  let loc = cx.empty_object();
  let start = cx.number(variable.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(variable.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_int_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  int_value: &gql_parser::IntValue,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("IntValue");
  obj.set(cx, "kind", kind)?;

  let value = cx.string(int_value.value.clone());
  obj.set(cx, "value", value)?;

  let loc = cx.empty_object();
  let start = cx.number(int_value.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(int_value.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_float_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  float_value: &gql_parser::FloatValue,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("FloatValue");
  obj.set(cx, "kind", kind)?;

  let value = cx.string(float_value.value.clone());
  obj.set(cx, "value", value)?;

  let loc = cx.empty_object();
  let start = cx.number(float_value.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(float_value.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_string_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  string_value: &gql_parser::StringValue,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("StringValue");
  obj.set(cx, "kind", kind)?;

  let value = cx.string(string_value.value.clone());
  obj.set(cx, "value", value)?;

  let block = cx.boolean(string_value.block);
  obj.set(cx, "block", block)?;

  let loc = cx.empty_object();
  let start = cx.number(string_value.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(string_value.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_boolean_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  boolean_value: &gql_parser::BooleanValue,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("BooleanValue");
  obj.set(cx, "kind", kind)?;

  let value = cx.boolean(boolean_value.value);
  obj.set(cx, "value", value)?;

  let loc = cx.empty_object();
  let start = cx.number(boolean_value.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(boolean_value.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_null_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  null_value: &gql_parser::NullValue,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("NullValue");
  obj.set(cx, "kind", kind)?;

  let loc = cx.empty_object();
  let start = cx.number(null_value.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(null_value.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_enum_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  enum_value: &gql_parser::EnumValue,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("EnumValue");
  obj.set(cx, "kind", kind)?;

  let value = cx.string(enum_value.value.clone());
  obj.set(cx, "value", value)?;

  let loc = cx.empty_object();
  let start = cx.number(enum_value.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(enum_value.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_list_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  list_value: &gql_parser::ListValue,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("ListValue");
  obj.set(cx, "kind", kind)?;

  let values = cx.empty_array();
  for (index, value) in list_value.values.iter().enumerate() {
    let transformed_value = transform_value(cx, value)?;
    values.set(cx, index as u32, transformed_value)?;
  }
  obj.set(cx, "values", values)?;

  let loc = cx.empty_object();
  let start = cx.number(list_value.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(list_value.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_object_field<'a>(
  cx: &mut CallContext<'a, JsObject>,
  object_field: &gql_parser::ObjectField,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("ObjectField");
  obj.set(cx, "kind", kind)?;

  let name = transform_name(cx, &object_field.name)?;
  obj.set(cx, "name", name)?;

  let value = transform_value(cx, &object_field.value)?;
  obj.set(cx, "value", value)?;

  let loc = cx.empty_object();
  let start = cx.number(object_field.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(object_field.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_object_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  object_value: &gql_parser::ObjectValue,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("ObjectValue");
  obj.set(cx, "kind", kind)?;

  let fields = cx.empty_array();
  for (index, object_field) in object_value.fields.iter().enumerate() {
    let transformed_field = transform_object_field(cx, object_field)?;
    fields.set(cx, index as u32, transformed_field)?;
  }
  obj.set(cx, "fields", fields)?;

  let loc = cx.empty_object();
  let start = cx.number(object_value.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(object_value.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  value: &gql_parser::Value,
) -> JsResult<'a, JsObject> {
  match value {
    gql_parser::Value::Variable(variable) => transform_variable(cx, variable),
    gql_parser::Value::IntValue(int_value) => transform_int_value(cx, int_value),
    gql_parser::Value::FloatValue(float_value) => transform_float_value(cx, float_value),
    gql_parser::Value::StringValue(string_value) => transform_string_value(cx, string_value),
    gql_parser::Value::BooleanValue(boolean_value) => transform_boolean_value(cx, boolean_value),
    gql_parser::Value::NullValue(null_value) => transform_null_value(cx, null_value),
    gql_parser::Value::EnumValue(enum_value) => transform_enum_value(cx, enum_value),
    gql_parser::Value::ListValue(list_value) => transform_list_value(cx, list_value),
    gql_parser::Value::ObjectValue(object_value) => transform_object_value(cx, object_value),
  }
}

fn transform_const_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  value: &gql_parser::ConstValue,
) -> JsResult<'a, JsObject> {
  match value {
    _ => Ok(cx.empty_object()), // TODO: remove this
  }
}

fn transform_argument<'a>(
  cx: &mut CallContext<'a, JsObject>,
  argument: &Argument,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("Argument");
  obj.set(cx, "kind", kind)?;

  let name = transform_name(cx, &argument.name)?;
  obj.set(cx, "name", name)?;

  let value = transform_value(cx, &argument.value)?;
  obj.set(cx, "value", value)?;

  let loc = cx.empty_object();
  let start = cx.number(argument.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(argument.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_variable_definition<'a>(
  cx: &mut CallContext<'a, JsObject>,
  variable_definition: &VariableDefinition,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("VariableDefinition");
  obj.set(cx, "kind", kind)?;

  let variable = transform_variable(cx, &variable_definition.variable)?;
  obj.set(cx, "variable", variable)?;

  let gql_type = transform_type(cx, &variable_definition.gql_type)?;
  obj.set(cx, "type", gql_type)?;

  match &variable_definition.default_value {
    None => {
      let default_value = cx.undefined();
      obj.set(cx, "defaultValue", default_value)?;
    }
    Some(default_value) => {
      let transformed_default_value = transform_const_value(cx, default_value)?;
      obj.set(cx, "defaultValue", transformed_default_value)?;
    }
  }

  let directives = cx.empty_array();
  for (index, argument) in variable_definition.directives.iter().enumerate() {
    let transformed_directive = transform_directive(cx, argument)?;
    directives.set(cx, index as u32, transformed_directive)?;
  }
  obj.set(cx, "directives", directives)?;

  let loc = cx.empty_object();
  let start = cx.number(variable_definition.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(variable_definition.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_directive<'a>(
  cx: &mut CallContext<'a, JsObject>,
  directive: &Directive,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("Directive");
  obj.set(cx, "kind", kind)?;

  let name = transform_name(cx, &directive.name)?;
  obj.set(cx, "name", name)?;

  let arguments = cx.empty_array();
  for (index, argument) in directive.arguments.iter().enumerate() {
    let transformed_argument = transform_argument(cx, argument)?;
    arguments.set(cx, index as u32, transformed_argument)?;
  }
  obj.set(cx, "arguments", arguments)?;

  let loc = cx.empty_object();
  let start = cx.number(directive.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(directive.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_selection<'a>(
  cx: &mut CallContext<'a, JsObject>,
  selection: &Selection,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();

  match selection {
    Selection::Field {
      name,
      alias,
      arguments,
      directives,
      selection_set,
      loc,
    } => {
      let kind = cx.string("Field");
      obj.set(cx, "kind", kind)?;

      let t_name = transform_name(cx, &name)?;
      obj.set(cx, "name", t_name)?;

      match alias {
        None => {
          let t_alias = cx.undefined();
          obj.set(cx, "alias", t_alias)?;
        }
        Some(alias) => {
          let t_alias = transform_name(cx, &alias)?;
          obj.set(cx, "alias", t_alias)?;
        }
      }

      let t_arguments = cx.empty_array();
      for (index, argument) in arguments.iter().enumerate() {
        let transformed_argument = transform_argument(cx, argument)?;
        t_arguments.set(cx, index as u32, transformed_argument)?;
      }
      obj.set(cx, "arguments", t_arguments)?;

      let t_directives = cx.empty_array();
      for (index, directive) in directives.iter().enumerate() {
        let transformed_directive = transform_directive(cx, directive)?;
        t_directives.set(cx, index as u32, transformed_directive)?;
      }
      obj.set(cx, "directives", t_directives)?;

      match selection_set {
        None => {
          let t_selection_set = cx.undefined();
          obj.set(cx, "selectionSet", t_selection_set)?;
        }
        Some(selection_set) => {
          let t_selection_set = transform_selection_set(cx, &selection_set)?;
          obj.set(cx, "selectionSet", t_selection_set)?;
        }
      }

      let t_loc = cx.empty_object();
      let start = cx.number(loc.start_token.start as u32);
      t_loc.set(cx, "start", start)?;
      let end = cx.number(loc.end_token.end as u32);
      t_loc.set(cx, "end", end)?;
      obj.set(cx, "loc", t_loc)?;
    }
    Selection::FragmentSpread {
      name,
      directives,
      loc,
    } => {
      let kind = cx.string("FragmentSpread");
      obj.set(cx, "kind", kind)?;

      let t_name = transform_name(cx, &name)?;
      obj.set(cx, "name", t_name)?;

      let t_directives = cx.empty_array();
      for (index, directive) in directives.iter().enumerate() {
        let transformed_directive = transform_directive(cx, directive)?;
        t_directives.set(cx, index as u32, transformed_directive)?;
      }
      obj.set(cx, "directives", t_directives)?;

      let t_loc = cx.empty_object();
      let start = cx.number(loc.start_token.start as u32);
      t_loc.set(cx, "start", start)?;
      let end = cx.number(loc.end_token.end as u32);
      t_loc.set(cx, "end", end)?;
      obj.set(cx, "loc", t_loc)?;
    }
    Selection::InlineFragment {
      type_condition,
      directives,
      selection_set,
      loc,
    } => {
      let kind = cx.string("InlineFragment");
      obj.set(cx, "kind", kind)?;

      match type_condition {
        None => {
          let t_type_condition = cx.undefined();
          obj.set(cx, "typeCondition", t_type_condition)?;
        }
        Some(type_condition) => {
          let t_type_condition = transform_named_type(cx, type_condition)?;
          obj.set(cx, "typeCondition", t_type_condition)?;
        }
      }
      let t_directives = cx.empty_array();
      for (index, directive) in directives.iter().enumerate() {
        let transformed_directive = transform_directive(cx, directive)?;
        t_directives.set(cx, index as u32, transformed_directive)?;
      }
      obj.set(cx, "directives", t_directives)?;

      let t_selection_set = transform_selection_set(cx, selection_set)?;
      obj.set(cx, "selectionSet", t_selection_set)?;

      let t_loc = cx.empty_object();
      let start = cx.number(loc.start_token.start as u32);
      t_loc.set(cx, "start", start)?;
      let end = cx.number(loc.end_token.end as u32);
      t_loc.set(cx, "end", end)?;
      obj.set(cx, "loc", t_loc)?;
    }
  }

  Ok(obj)
}

fn transform_selection_set<'a>(
  cx: &mut CallContext<'a, JsObject>,
  selection_set: &SelectionSet,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();
  let kind = cx.string("SelectionSet");
  obj.set(cx, "kind", kind)?;

  let selections = cx.empty_array();
  for (index, selection) in selection_set.selections.iter().enumerate() {
    let transformed_selection = transform_selection(cx, selection)?;
    selections.set(cx, index as u32, transformed_selection)?;
  }
  obj.set(cx, "selections", selections)?;

  let loc = cx.empty_object();
  let start = cx.number(selection_set.loc.start_token.start as u32);
  loc.set(cx, "start", start)?;
  let end = cx.number(selection_set.loc.end_token.end as u32);
  loc.set(cx, "end", end)?;
  obj.set(cx, "loc", loc)?;

  Ok(obj)
}

fn transform_definition<'a>(
  cx: &mut CallContext<'a, JsObject>,
  definition: &Definition,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();

  match definition {
    Definition::OperationDefinition {
      operation,
      name,
      variable_definitions,
      directives,
      selection_set,
      loc,
    } => {
      let kind = cx.string("OperationDefinition");
      obj.set(cx, "kind", kind)?;

      let t_operation = cx.string(format!("{}", operation));
      obj.set(cx, "operation", t_operation)?;

      match name {
        None => {
          let t_name = cx.undefined();
          obj.set(cx, "name", t_name)?;
        }
        Some(name) => {
          let t_name = transform_name(cx, &name)?;
          obj.set(cx, "name", t_name)?;
        }
      }

      let t_variable_definitions = cx.empty_array();
      for (index, variable_definition) in variable_definitions.iter().enumerate() {
        let transformed_variable_definition =
          transform_variable_definition(cx, variable_definition)?;
        t_variable_definitions.set(cx, index as u32, transformed_variable_definition)?;
      }
      obj.set(cx, "variableDefinitions", t_variable_definitions)?;

      let t_directives = cx.empty_array();
      for (index, directive) in directives.iter().enumerate() {
        let transformed_directive = transform_directive(cx, directive)?;
        t_directives.set(cx, index as u32, transformed_directive)?;
      }
      obj.set(cx, "directives", t_directives)?;

      let t_selection_set = transform_selection_set(cx, &selection_set)?;
      obj.set(cx, "selectionSet", t_selection_set)?;

      let t_loc = cx.empty_object();
      let start = cx.number(loc.start_token.start as u32);
      t_loc.set(cx, "start", start)?;
      let end = cx.number(loc.end_token.end as u32);
      t_loc.set(cx, "end", end)?;
      obj.set(cx, "loc", t_loc)?;
    }
    Definition::FragmentDefinition {
      name,
      type_condition,
      directives,
      selection_set,
      loc,
    } => {
      let kind = cx.string("FragmentDefinition");
      obj.set(cx, "kind", kind)?;

      let t_name = transform_name(cx, name)?;
      obj.set(cx, "name", t_name)?;

      let t_type_condition = transform_named_type(cx, type_condition)?;
      obj.set(cx, "typeCondition", t_type_condition)?;

      let t_directives = cx.empty_array();
      for (index, directive) in directives.iter().enumerate() {
        let transformed_directive = transform_directive(cx, directive)?;
        t_directives.set(cx, index as u32, transformed_directive)?;
      }
      obj.set(cx, "directives", t_directives)?;

      let t_selection_set = transform_selection_set(cx, selection_set)?;
      obj.set(cx, "selectionSet", t_selection_set)?;

      let t_loc = cx.empty_object();
      let start = cx.number(loc.start_token.start as u32);
      t_loc.set(cx, "start", start)?;
      let end = cx.number(loc.end_token.end as u32);
      t_loc.set(cx, "end", end)?;
      obj.set(cx, "loc", t_loc)?;
    }
    _ => {} // TODO: remove this
  }

  Ok(obj)
}

fn parse_to_js(mut cx: FunctionContext) -> JsResult<JsObject> {
  let query = cx.argument::<JsString>(0)?.value(&mut cx);
  match parse(&query) {
    Ok(document) => {
      let obj = cx.empty_object();
      let kind = cx.string("Document");
      obj.set(&mut cx, "kind", kind)?;

      let definitions = cx.empty_array();
      for (index, definition) in document.definitions.iter().enumerate() {
        let transformed_definition = transform_definition(&mut cx, definition)?;
        definitions.set(&mut cx, index as u32, transformed_definition)?;
      }
      obj.set(&mut cx, "definitions", definitions)?;

      let loc = cx.empty_object();
      let start = cx.number(document.loc.start_token.start as u32);
      loc.set(&mut cx, "start", start)?;
      let end = cx.number(document.loc.end_token.end as u32);
      loc.set(&mut cx, "end", end)?;
      obj.set(&mut cx, "loc", loc)?;

      return Ok(obj);
    }
    Err(syntax_error) => {
      let error_type = cx.string("error");
      let error_message = cx.string(syntax_error.message);
      let error_position = cx.number(syntax_error.position as u32);
      let error = cx.empty_object();
      error.set(&mut cx, "type", error_type)?;
      error.set(&mut cx, "message", error_message)?;
      error.set(&mut cx, "position", error_position)?;
      // TODO: Throw
      return Ok(error);
    }
  }
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
  cx.export_function("parse", parse_to_js)?;
  Ok(())
}