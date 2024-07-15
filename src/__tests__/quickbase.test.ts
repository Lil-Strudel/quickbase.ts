import { describe, expect, it, test } from "vitest";
import { Quickbase } from "..";

describe("example test", () => {
  const sdk = new Quickbase({
    realm: "1234",
    userToken: "asdf",
    userAgent: "asdf",
    app: {
      id: "",
      name: "",
      tables: [],
    },
  });

  it("should pass", () => {
    expect(3).toBe(3);
  });
});
