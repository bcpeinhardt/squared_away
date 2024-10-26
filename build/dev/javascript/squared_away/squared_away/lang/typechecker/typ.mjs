import { CustomType as $CustomType } from "../../../gleam.mjs";

export class TNil extends $CustomType {}

export class TFloat extends $CustomType {}

export class TString extends $CustomType {}

export class TInt extends $CustomType {}

export class TBool extends $CustomType {}

export function to_string(typ) {
  if (typ instanceof TNil) {
    return "Empty";
  } else if (typ instanceof TFloat) {
    return "Floating Point Number";
  } else if (typ instanceof TString) {
    return "Text";
  } else if (typ instanceof TInt) {
    return "Integer";
  } else {
    return "Boolean (True or False)";
  }
}
