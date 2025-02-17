import { AxiosResponse } from "axios";

class QBError extends Error {
  statusCode: number;
  description: string;

  constructor(message: string, description: string, statusCode: number) {
    super(message);
    this.description = description;
    this.statusCode = statusCode;
    this.name = "QBError";
    Object.setPrototypeOf(this, new.target.prototype);
  }
}

export function checkForQBErrors(res: AxiosResponse) {
  console.log(res.status);
  console.log(res.data);
  if (res.status > 299) {
    console.log(res.data.description);
    throw new QBError(res.data.message, res.data.description, res.status);
  }
}
