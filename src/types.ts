export interface AppDefinition {
  id: string;
  name: string;
  tables: TableDefinition[];
}

export interface TableDefinition {
  id: string;
  accessor: string;
  keyField: { id: number; type: keyof FieldTypeMapping };
  fields: Field[];
}

export interface Field {
  id: number;
  key: string;
  type: keyof FieldTypeMapping;
  label: string;
  required?: boolean;
}

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

export interface FieldTypeMapping {
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

export type DocumentFromTable<T extends { fields: Field[] }> = {
  [K in T["fields"][number] as K["key"]]?: FieldTypeMapping[K["type"]];
} & {
  [K in T["fields"][number] as K["required"] extends true
    ? K["key"]
    : never]: FieldTypeMapping[K["type"]];
};

export type QuickbaseField<T> = {
  value: T;
};

export type QuickbaseRecord<T extends { fields: Field[] }> = {
  [K in T["fields"][number] as K["id"]]: QuickbaseField<
    FieldTypeMapping[K["type"]]
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
