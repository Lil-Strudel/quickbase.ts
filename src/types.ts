export interface AppDefinition {
  id: string;
  name: string;
  tables: TableDefinition[];
}

export interface TableDefinition {
  id: string;
  accessor: string;
  keyField: { id: number; type: keyof FieldTypeMappingQuery };
  fields: Field[];
}

export interface Field {
  id: number;
  key: string;
  type: keyof FieldTypeMappingQuery;
  label: string;
  required?: boolean;
}

export interface GetManyRequest<T> {
  page: number;
  limit: number;
  query?: QueryBuilder<T>;
}

export type FieldQuery<T> = {
  [K in keyof T]: {
    field: K;
    op: QuickbaseOperators;
    value: T[K];
  };
}[keyof T];

export type QueryBuilder<T> =
  | { AND: QueryBuilder<T>[] }
  | { OR: QueryBuilder<T>[] }
  | FieldQuery<T>;

interface FileVersion {
  versionNumber: number;
  fileName: string;
  uploaded: string; // ISO 8601 date string
  creator: {
    email: string;
    id: string;
    name: string;
    userName: string;
  };
}

interface ReservedBy {
  email: string;
  id: string;
  name: string;
  userName: string;
}

declare interface FileData {
  url: string;
  reservedBy: ReservedBy;
  versions: FileVersion[];
}
declare interface UploadFileData {
  fileName: string;
  data: string;
}

interface User {
  id: string;
  name: string;
  userName: string;
  email: string;
}

interface ListUser {
  id: string;
  name: string;
  email: string;
}

export interface FieldTypeMappingQuery {
  Text: string;
  "Rich-Text": string;
  "Multi-Line-Text": string;
  "Multiple-Choice-Text": string;
  "List-Text": string[];
  "Multi-Select": string[];
  Address: string;
  "Email Address": string;
  URL: string;
  Numeric: number;
  "Numeric-Percent": number;
  "Numeric-Rating": number;
  "Numeric-Currency": number;
  Duration: number;
  Date: string;
  DateTime: string;
  "Time of day": string;
  Checkbox: boolean;
  "Phone Number": string;
  User: User;
  "List-User": ListUser[];
  "File Attachment": FileData;
  "Record ID": number;
}

export interface FieldTypeMappingUpsert
  extends Omit<FieldTypeMappingQuery, "File Attachment"> {
  "File Attachment": UploadFileData;
}

export type QuickbaseOperators =
  | "CT"
  | "XCT"
  | "HAS"
  | "XHAS"
  | "EX"
  | "TV"
  | "XTV"
  | "XEX"
  | "SW"
  | "XSW"
  | "BF"
  | "OBF"
  | "AF"
  | "OAF"
  | "IR"
  | "XIR"
  | "LT"
  | "LTE"
  | "GT"
  | "GTE";

export type QueryDocumentFromTable<T extends { fields: Field[] }> = {
  [K in T["fields"][number] as K["key"]]?: FieldTypeMappingQuery[K["type"]];
} & {
  [K in T["fields"][number] as K["required"] extends true
    ? K["key"]
    : never]: FieldTypeMappingQuery[K["type"]];
};

export type UpsertDocumentFromTable<T extends { fields: Field[] }> = {
  [K in T["fields"][number] as K["key"]]?: FieldTypeMappingUpsert[K["type"]];
} & {
  [K in T["fields"][number] as K["required"] extends true
    ? K["key"]
    : never]: FieldTypeMappingUpsert[K["type"]];
};

export type SelectForTable<T extends { fields: Field[] }> =
  T["fields"][number]["key"][];

export type QuickbaseField<T> = {
  value: T;
};

export type QuickbaseRecord<T extends { fields: Field[] }> = {
  [K in T["fields"][number] as K["id"]]: QuickbaseField<
    FieldTypeMappingQuery[K["type"]]
  >;
};

export interface QuickbaseQueryResponse<T extends { fields: Field[] }> {
  data: QuickbaseRecord<T>[];
  fields: {
    id: number;
    label: string;
    type: string;
    labelOverride?: string;
  }[];
  metadata: {
    skip: number;
    numFields: number;
    top: number;
    totalRecords: number;
    numRecords: number;
  };
}

export interface QuickbaseInsertResponse<T extends { fields: Field[] }> {
  data: QuickbaseRecord<T>[];
  fields: {
    id: number;
    label: string;
    type: string;
    labelOverride?: string;
  }[];
  metadata: {
    createdRecordIds: number[];
    totalNumberOfRecordsProcessed: number;
    unchangedRecordIds: number[];
    updatedRecordIds: number[];
  };
}

export interface QuickbaseConfig {
  realm: string;
  userToken: string;
  userAgent?: string;
}
