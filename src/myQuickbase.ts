import { CreateAxiosDefaults } from "axios";
import { Quickbase } from "./quickbase";
import { Table } from "./table";
import { AppDefinition, QuickbaseConfig } from "./types";

export class MyQuickbase<AppDef extends AppDefinition> extends Quickbase {
  tables: {
    [K in AppDef["tables"][number] as K["accessor"]]: Table<K>;
  };

  constructor(
    config: QuickbaseConfig,
    app: AppDef,
    axiosOverrides?: CreateAxiosDefaults,
  ) {
    super(config, axiosOverrides);

    type Tables = typeof this.tables;
    this.tables = app.tables.reduce((acc, table) => {
      const accessor = table.accessor as keyof typeof acc;
      acc[accessor] = new Table(this.axios, table) as Tables[typeof accessor];

      return acc;
    }, {} as Tables);
  }
}
