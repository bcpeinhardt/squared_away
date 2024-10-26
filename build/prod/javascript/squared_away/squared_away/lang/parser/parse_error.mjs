import { CustomType as $CustomType } from "../../../gleam.mjs";

export class ParseError extends $CustomType {
  constructor(context) {
    super();
    this.context = context;
  }
}

export function to_string(pe) {
  let c = pe.context;
  return c;
}
