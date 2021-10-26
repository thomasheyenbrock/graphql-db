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

fn transform_value<'a>(
  cx: &mut CallContext<'a, JsObject>,
  value: &gql_parser::Value,
) -> JsResult<'a, JsObject> {
  match value {
    gql_parser::Value::StringValue(string_value) => transform_string_value(cx, string_value),
    _ => Ok(cx.empty_object()),
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

  Ok(obj)
}

fn transform_directive<'a>(
  cx: &mut CallContext<'a, JsObject>,
  directive: &Directive,
) -> JsResult<'a, JsObject> {
  let obj = cx.empty_object();

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
    _ => {}
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
    _ => {}
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
