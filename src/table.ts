import { AxiosInstance } from "axios";
import {
  DocumentFromTable,
  Field,
  FieldTypeMapping,
  QuickbaseField,
  QuickbaseInsertResponse,
  QuickbaseOperators,
  QuickbaseQueryResponse,
  QuickbaseRecord,
  TableDefinition,
} from "./types";
interface GetOneByIdRequest<
  T extends TableDefinition,
  K extends DocumentFromTable<T>,
> {
  id: FieldTypeMapping[T["keyField"]["type"]];
  select?: SelectBuilder<K>;
}
interface GetOneRequest<T> {
  select?: SelectBuilder<T>;
  query: QueryBuilder<T>;
}
interface GetManyRequest<T> {
  page: number;
  limit: number;
  select?: SelectBuilder<T>;
  query?: QueryBuilder<T>;
}

type FieldQuery<T> = {
  [K in keyof T]: {
    field: K;
    op: QuickbaseOperators;
    value: T[K];
  };
}[keyof T];

type QueryBuilder<T> =
  | { AND: QueryBuilder<T>[] }
  | { OR: QueryBuilder<T>[] }
  | FieldQuery<T>;

type SelectBuilder<T> = (keyof T)[];

export class Table<TableDef extends TableDefinition> {
  private fieldIds: number[];
  private fieldKeyMap: { [K in TableDef["fields"][number] as K["key"]]: K };

  constructor(
    private axios: AxiosInstance,
    private table: TableDef,
  ) {
    this.fieldIds = table.fields.map((field) => field.id);

    type FieldKeyMap = typeof this.fieldKeyMap;
    this.fieldKeyMap = table.fields.reduce((acc, field) => {
      const fieldKey = field.key as keyof FieldKeyMap;
      acc[fieldKey] = field as FieldKeyMap[typeof fieldKey];

      return acc;
    }, {} as FieldKeyMap);
  }

  createWhere(query: QueryBuilder<DocumentFromTable<TableDef>>): string {
    if ("AND" in query) {
      const where = query.AND.map((subQuery) => this.createWhere(subQuery));
      return "(" + where.join("AND") + ")";
    }

    if ("OR" in query) {
      const where = query.OR.map((subQuery) => this.createWhere(subQuery));
      return "(" + where.join("OR") + ")";
    }

    type FieldKeyMap = typeof this.fieldKeyMap;
    const field = this.fieldKeyMap[query.field as keyof FieldKeyMap] as Field;
    if (!field) {
      throw new Error(`Field not found in table definition`);
    }
    return `{'${field.id}'.${query.op}.'${query.value}'}`;
  }

  private getSelectedFieldIds<K extends (keyof DocumentFromTable<TableDef>)[]>(
    selectedKeys: K | undefined,
  ) {
    return selectedKeys
      ? selectedKeys.map((field) => {
          const fieldDef = this.table.fields.find((f) => f.key === field);
          if (!fieldDef) throw new Error(`Field not found in table definition`);
          return fieldDef.id;
        })
      : this.table.fields.map((field) => field.id);
  }

  private extractFields<
    K extends undefined | (keyof DocumentFromTable<TableDef>)[],
  >(record: QuickbaseRecord<TableDef>, selectedKeys: K = undefined as K) {
    const fields = selectedKeys
      ? (selectedKeys.map((key) => {
          const field = this.fieldKeyMap[key];
          return field;
        }) as Field[])
      : (this.table.fields as Field[]);
    type Document = Pick<
      DocumentFromTable<TableDef>,
      K extends (keyof DocumentFromTable<TableDef>)[]
        ? K[number]
        : keyof DocumentFromTable<TableDef>
    >;
    const newItem = {} as Document;

    for (let i = 0; i < fields.length; i++) {
      const field = fields[i];
      const key = field.key as keyof Document;
      const qbRecordKey = field.id as keyof typeof record;
      const qbField = record[qbRecordKey] as QuickbaseField<
        Document[keyof Document]
      >;
      newItem[key] = qbField.value;
    }

    return newItem;
  }

  private buildQBData(body: Partial<DocumentFromTable<TableDef>>) {
    type FieldKeyMap = typeof this.fieldKeyMap;
    const data = Object.entries(body).reduce(
      (acc, [key, value]) => {
        const fieldKey = key as keyof FieldKeyMap;
        const field = this.fieldKeyMap[fieldKey] as Field | undefined;
        if (!field) throw new Error(`Field not found in table definition`);

        acc[field.id] = { value };

        return acc;
      },
      {} as Record<string, { value: unknown }>,
    );

    return data;
  }

  async getOne<
    T extends GetOneRequest<DocumentFromTable<TableDef>>,
    K extends T["select"],
  >(data: T) {
    const selectedFieldIds = this.getSelectedFieldIds(data.select);

    const res = await this.axios.post<QuickbaseQueryResponse<TableDef>>(
      "/records/query",
      {
        from: this.table.id,
        select: selectedFieldIds,
        options: { top: 1, skip: 0 },
        where: this.createWhere(data.query),
      },
    );

    return res.data.data?.[0]
      ? this.extractFields(res.data.data[0], data.select as K)
      : null;
  }

  async getOneById<
    T extends GetOneByIdRequest<TableDef, DocumentFromTable<TableDef>>,
    K extends T["select"],
  >(data: T) {
    const fieldIds = this.getSelectedFieldIds(data.select);

    const res = await this.axios.post<QuickbaseQueryResponse<TableDef>>(
      "/records/query",
      {
        from: this.table.id,
        select: fieldIds,
        options: { top: 1, skip: 0 },
        where: `{'${this.table.keyField.id}'.EX.'${data.id}'}`,
      },
    );

    return res.data.data?.[0]
      ? this.extractFields(res.data.data[0], data.select as K)
      : null;
  }

  async getMany<
    T extends GetManyRequest<DocumentFromTable<TableDef>>,
    K extends T["select"],
  >(data: T) {
    const selectedFieldIds = this.getSelectedFieldIds(data.select);

    const res = await this.axios.post<QuickbaseQueryResponse<TableDef>>(
      "/records/query",
      {
        from: this.table.id,
        select: selectedFieldIds,
        sortBy: [{ fieldId: this.table.keyField.id, order: "DESC" }],
        options: { top: data.limit, skip: (data.page - 1) * data.limit },
        ...(data.query ? { where: this.createWhere(data.query) } : {}),
      },
    );

    return res.data.data.map((qbRecord) =>
      this.extractFields(qbRecord, data.select as K),
    );
  }

  async createOne(body: DocumentFromTable<TableDef>) {
    const data = this.buildQBData(body);

    const res = await this.axios.post<QuickbaseInsertResponse<TableDef>>(
      "/records",
      {
        to: this.table.id,
        data: [data],
        fieldsToReturn: [],
      },
    );

    const createdId = res.data.metadata.createdRecordIds[0];
    if (!createdId && createdId !== 0) throw new Error("Record not created");

    return createdId;
  }

  async updateOne(
    id: FieldTypeMapping[TableDef["keyField"]["type"]],
    update: Partial<DocumentFromTable<TableDef>>,
  ) {
    const data = this.buildQBData(update);

    data[this.table.keyField.id] = { value: id };

    const res = await this.axios.post<QuickbaseInsertResponse<TableDef>>(
      "/records",
      {
        to: this.table.id,
        data: [data],
        fieldsToReturn: [],
        mergeFieldId: this.table.keyField.id,
      },
    );

    const updatedId =
      res.data.metadata.updatedRecordIds[0] ||
      res.data.metadata.unchangedRecordIds[0];

    if (!updatedId && updatedId !== 0) throw new Error("Record not updated");

    return true;
  }

  async deleteOne(id: FieldTypeMapping[TableDef["keyField"]["type"]]) {
    const res = await this.axios.delete<{ numberDeleted: number }>("/records", {
      data: {
        from: this.table.id,
        where: `{'${this.table.keyField.id}'.EX.'${id}'}`,
      },
    });

    if (res.data.numberDeleted === 0) throw new Error("Record not found");

    return true;
  }
}
