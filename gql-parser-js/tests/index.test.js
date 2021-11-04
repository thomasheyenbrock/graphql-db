const fs = require("fs");
const path = require("path");
const { parse: parse_reference } = require("graphql/language");
const { parse } = require("..");

function jsonifyLocs(node) {
  return { ...node, loc: node.loc.toJSON() };
}

function transformNode(node) {
  switch (node.kind) {
    case "Argument":
      return jsonifyLocs({
        ...node,
        name: transformNode(node.name),
        value: transformNode(node.value),
      });
    case "BooleanValue":
      return jsonifyLocs(node);
    case "Directive":
      return jsonifyLocs({
        ...node,
        name: transformNode(node.name),
        arguments: node.arguments.map(transformNode),
      });
    case "Document":
      return jsonifyLocs({
        ...node,
        definitions: node.definitions.map(transformNode),
      });
    case "EnumValue":
      return jsonifyLocs(node);
    case "Field":
      return jsonifyLocs({
        ...node,
        alias: node.alias ? transformNode(node.alias) : node.alias,
        name: transformNode(node.name),
        arguments: node.arguments.map(transformNode),
        directives: node.directives.map(transformNode),
        selectionSet: node.selectionSet
          ? transformNode(node.selectionSet)
          : node.selectionSet,
      });
    case "FloatValue":
      return jsonifyLocs(node);
    case "FragmentDefinition":
      return jsonifyLocs({
        ...node,
        name: transformNode(node.name),
        typeCondition: transformNode(node.typeCondition),
        directives: node.directives.map(transformNode),
        selectionSet: transformNode(node.selectionSet),
      });
    case "FragmentSpread":
      return jsonifyLocs({
        ...node,
        name: transformNode(node.name),
        directives: node.directives.map(transformNode),
      });
    case "InlineFragment":
      return jsonifyLocs({
        ...node,
        typeCondition: node.typeCondition
          ? transformNode(node.typeCondition)
          : node.typeCondition,
        directives: node.directives.map(transformNode),
        selectionSet: transformNode(node.selectionSet),
      });
    case "IntValue":
      return jsonifyLocs(node);
    case "ListType":
      return jsonifyLocs({ ...node, type: transformNode(node.type) });
    case "ListValue":
      return jsonifyLocs({ ...node, values: node.values.map(transformNode) });
    case "Name":
      return jsonifyLocs(node);
    case "NamedType":
      return jsonifyLocs({ ...node, name: transformNode(node.name) });
    case "NonNullType":
      return jsonifyLocs({ ...node, type: transformNode(node.type) });
    case "NullValue":
      return jsonifyLocs(node);
    case "ObjectField":
      return jsonifyLocs({
        ...node,
        name: transformNode(node.name),
        value: transformNode(node.value),
      });
    case "ObjectValue":
      return jsonifyLocs({ ...node, fields: node.fields.map(transformNode) });
    case "OperationDefinition":
      return jsonifyLocs({
        ...node,
        name: node.name ? transformNode(node.name) : node.name,
        variableDefinitions: node.variableDefinitions.map(transformNode),
        directives: node.directives.map(transformNode),
        selectionSet: transformNode(node.selectionSet),
      });
    case "SelectionSet":
      return jsonifyLocs({
        ...node,
        selections: node.selections.map(transformNode),
      });
    case "StringValue":
      return jsonifyLocs(node);
    case "Variable":
      return jsonifyLocs({ ...node, name: transformNode(node.name) });
    case "VariableDefinition":
      return jsonifyLocs({
        ...node,
        variable: transformNode(node.variable),
        type: transformNode(node.type),
        defaultValue: node.defaultValue
          ? transformNode(node.defaultValue)
          : node.defaultValue,
        directives: node.directives.map(transformNode),
      });
    default:
      throw new Error(`Unknown kind "${node.kind}"`);
  }
}

describe("parse", () => {
  it("should work", async () => {
    const query = await fs.promises.readFile(
      path.join(__dirname, "example.gql"),
      "utf8"
    );
    expect(parse(query)).toEqual(transformNode(parse_reference(query)));
  });
});
