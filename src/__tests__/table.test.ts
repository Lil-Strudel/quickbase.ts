import { describe, expect, it } from "vitest";
import { Table } from "../table";
import axios from "axios";
import { TableDefinition } from "../types";

describe("Table Class", async () => {
  const myAxios = axios.create({
    baseURL: "https://api.quickbase.com/v1/",
    headers: {
      Authorization: `QB-USER-TOKEN ${process.env.TESTING_QB_USER_TOKEN}`,
      "QB-Realm-Hostname": process.env.TESTING_QB_REALM,
      "User-Agent": "Quickbase.ts Test Suite",
    },
  });

  const tableDef = {
    id: "budd82t8w",
    accessor: "table",
    keyField: { id: 3, type: "Numeric" },
    // prettier-ignore
    fields: [
      { id: 1, key: "dateCreated", type: "DateTime", label: "Date Created", required: false, },
      { id: 2, key: "dateModified", type: "DateTime", label: "Date Modified", required: false, },
      { id: 3, key: "id", type: "Numeric", label: "Record ID#", required: false, },
      { id: 4, key: "recordOwner", type: "User", label: "Record Owner", required: false, },
      { id: 5, key: "lastModifiedBy", type: "User", label: "Last Modified By", required: false, },
      { id: 6, key: "name", type: "Text", label: "Name", required: false }, 
      { id: 7, key: "phone", type: "Phone Number", label: "Phone", required: false, },
      { id: 8, key: "email", type: "Email Address", label: "Email", required: false, },
      { id: 9, key: "date", type: "DateTime", label: "Date", required: false },
      { id: 10, key: "favoriteNumber", type: "Numeric", label: "Favorite Number", required: false, },
      { id: 11, key: "favoriteCheckbox", type: "Checkbox", label: "Favorite Checkbox", required: false, },
    ],
  } as const satisfies TableDefinition;

  const table = new Table(myAxios, tableDef);

  let testRecordId: number;
  it("should create one", async () => {
    const recordId = await table.createOne({
      name: "Test Name",
    });
    expect(recordId).toBeDefined();

    testRecordId = recordId;
  });

  it("should get one by id", async () => {
    const doc = await table.getOneById({
      id: testRecordId,
    });

    expect(doc).toBeDefined();
  });

  it("should get many", async () => {
    const docs = await table.getMany({
      page: 1,
      limit: 10,
    });
    expect(docs).toBeDefined();
  });

  it("should only include the fields for name and email", async () => {
    const docs = await table.getMany({
      select: ["name", "email"],
      page: 1,
      limit: 10,
    });
    expect(docs[0]).toHaveProperty("name");
    expect(docs[0]).not.toHaveProperty("phone");
    expect(docs[0]).toHaveProperty("email");
  });

  it("should update one", async () => {
    const res = await table.updateOne(testRecordId, { email: "test@test.com" });
    expect(res).toBeDefined();
  });

  it("should get one", async () => {
    const res = await table.getOne({
      query: {
        field: "name",
        op: "EX",
        value: "Test Name",
      },
    });
    expect(res).toBeDefined();
  });

  it("should delete one", async () => {
    const res = await table.deleteOne(testRecordId);
    expect(res).toBeDefined();
  });

  it("should create a where string", async () => {
    const where = table.createWhere({
      AND: [
        { field: "name", op: "EX", value: "Test Name" },
        { field: "date", op: "OAF", value: "2000-01-01" },
        {
          OR: [
            { field: "favoriteNumber", op: "GT", value: 7 },
            { field: "favoriteNumber", op: "LT", value: 2 },
          ],
        },
      ],
    });

    expect(where).toBe(
      "({'6'.EX.'Test Name'}AND{'9'.OAF.'2000-01-01'}AND({'10'.GT.'7'}OR{'10'.LT.'2'}))",
    );
  });

  it("should throw error when an invalid field is passed to a query", async () => {
    expect(() => {
      table.createWhere({
        field: "notreal",
        op: "EX",
        value: "",
      });
    }).toThrowError("Field not found in table definition");
  });

  it("should limit the type to the parameters passed in", async () => {
    type Test<T> 

  });
});
export type QBFile = NonNullable<
  Awaited<ReturnType<typeof grandeurAgain.tables.file.getOne>>
>;

