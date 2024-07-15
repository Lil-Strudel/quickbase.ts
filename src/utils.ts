export interface AppDefinition {
  id: string;
  name: string;
  tables: TableDefinition[];
}

export interface TableDefinition {
  id: string;
  keyFieldId: number;
  fields: Field[];
}

export interface Field {
  id: number;
  key: string;
  type: keyof FieldTypeMapping;
  label: string;
}

type FieldTypeMapping = {
  string: string;
  number: number;
  boolean: boolean;
};
