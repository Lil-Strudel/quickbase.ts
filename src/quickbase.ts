import vanillaAxios, { AxiosInstance, CreateAxiosDefaults } from "axios";
import { AppDefinition } from "./types";

export interface QuickbaseConfig {
  realm: string;
  userToken: string;
  userAgent?: string;
  app?: AppDefinition;
}

export class Quickbase {
  axios: AxiosInstance;

  constructor(config: QuickbaseConfig, axiosOverrides?: CreateAxiosDefaults) {
    this.axios = vanillaAxios.create({
      baseURL: "https://api.quickbase.com/v1/",
      ...(axiosOverrides ? axiosOverrides : {}),
      headers: {
        Authorization: `QB-USER-TOKEN ${config.userToken}`,
        ...(config.userAgent ? { "User-Agent": config.userAgent } : {}),
        ...(axiosOverrides?.headers ? axiosOverrides.headers : {}),
      },
    });
  }
}
