import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import { Spec } from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import { coerce as unsafe_coerce } from "../../lustre-escape.ffi.mjs";
import * as $effect from "../../lustre/effect.mjs";
import * as $element from "../../lustre/element.mjs";
import * as $patch from "../../lustre/internals/patch.mjs";
import { Diff, Init } from "../../lustre/internals/patch.mjs";
import * as $vdom from "../../lustre/internals/vdom.mjs";

class State extends $CustomType {
  constructor(self, model, update, view, html, renderers, handlers, on_attribute_change) {
    super();
    this.self = self;
    this.model = model;
    this.update = update;
    this.view = view;
    this.html = html;
    this.renderers = renderers;
    this.handlers = handlers;
    this.on_attribute_change = on_attribute_change;
  }
}

export class Attrs extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Batch extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class Debug extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Dispatch extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Emit extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class Event extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class SetSelector extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Shutdown extends $CustomType {}

export class Subscribe extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class Unsubscribe extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class ForceModel extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Model extends $CustomType {
  constructor(reply) {
    super();
    this.reply = reply;
  }
}

export class View extends $CustomType {
  constructor(reply) {
    super();
    this.reply = reply;
  }
}
