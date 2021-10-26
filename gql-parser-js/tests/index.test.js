const fs = require("fs");
const path = require("path");
const { parse: parse_reference } = require("graphql/language");
const { parse } = require("..");

describe("parse", () => {
  it("should work", async () => {
    const query = await fs.promises.readFile(
      path.join(__dirname, "example.gql"),
      "utf8"
    );
    expect(parse(query)).toEqual(parse_reference(query));
  });
});
