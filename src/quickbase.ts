import axios, { AxiosInstance, CreateAxiosDefaults } from "axios";
import { QuickbaseConfig } from "./types";

export class Quickbase {
  axios: AxiosInstance;

  constructor(config: QuickbaseConfig, axiosOverrides?: CreateAxiosDefaults) {
    this.axios = axios.create({
      baseURL: "https://api.quickbase.com/v1/",
      ...(axiosOverrides ? axiosOverrides : {}),
      headers: {
        Authorization: `QB-USER-TOKEN ${config.userToken}`,
        "QB-Realm-Hostname": config.realm,
        ...(config.userAgent ? { "User-Agent": config.userAgent } : {}),
        ...(axiosOverrides?.headers ? axiosOverrides.headers : {}),
      },
    });
  }
}
