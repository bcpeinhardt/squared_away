import * as $actor from "../../gleam_otp/gleam/otp/actor.mjs";
import * as $package_interface from "../../gleam_package_interface/gleam/package_interface.mjs";
import { Fn, Named, Tuple, Variable } from "../../gleam_package_interface/gleam/package_interface.mjs";
import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $glisten from "../../glisten/glisten.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import {
  CustomType as $CustomType,
  makeError,
  remainderInt,
  divideInt,
  toBitArray,
} from "../gleam.mjs";

export class BuildError extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class BundleError extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class CannotCreateDirectory extends $CustomType {
  constructor(reason, path) {
    super();
    this.reason = reason;
    this.path = path;
  }
}

export class CannotReadFile extends $CustomType {
  constructor(reason, path) {
    super();
    this.reason = reason;
    this.path = path;
  }
}

export class CannotSetPermissions extends $CustomType {
  constructor(reason, path) {
    super();
    this.reason = reason;
    this.path = path;
  }
}

export class CannotStartDevServer extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class CannotStartFileWatcher extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class CannotWriteFile extends $CustomType {
  constructor(reason, path) {
    super();
    this.reason = reason;
    this.path = path;
  }
}

export class ComponentMissing extends $CustomType {
  constructor(module) {
    super();
    this.module = module;
  }
}

export class IncompleteProxy extends $CustomType {
  constructor(missing) {
    super();
    this.missing = missing;
  }
}

export class InternalError extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class InvalidProxyTarget extends $CustomType {
  constructor(to) {
    super();
    this.to = to;
  }
}

export class MainBadAppType extends $CustomType {
  constructor(module, flags, model, msg) {
    super();
    this.module = module;
    this.flags = flags;
    this.model = model;
    this.msg = msg;
  }
}

export class MainMissing extends $CustomType {
  constructor(module) {
    super();
    this.module = module;
  }
}

export class MainTakesAnArgument extends $CustomType {
  constructor(module, got) {
    super();
    this.module = module;
    this.got = got;
  }
}

export class ModuleMissing extends $CustomType {
  constructor(module) {
    super();
    this.module = module;
  }
}

export class NameIncorrectType extends $CustomType {
  constructor(module, got) {
    super();
    this.module = module;
    this.got = got;
  }
}

export class NameMissing extends $CustomType {
  constructor(module) {
    super();
    this.module = module;
  }
}

export class NetworkError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class TemplateMissing extends $CustomType {
  constructor(name, reason) {
    super();
    this.name = name;
    this.reason = reason;
  }
}

export class UnknownPlatform extends $CustomType {
  constructor(binary, os, cpu) {
    super();
    this.binary = binary;
    this.os = os;
    this.cpu = cpu;
  }
}

export class OtpTooOld extends $CustomType {
  constructor(version) {
    super();
    this.version = version;
  }
}

export class UnzipError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class InvalidEsbuildBinary extends $CustomType {}

export class InvalidTailwindBinary extends $CustomType {}

function build_error(reason) {
  let message = "\nIt looks like your project has some compilation errors that need to be addressed\nbefore I can do anything. Here's the error message I got:\n\n{reason}\n";
  let _pipe = message;
  return $string.replace(_pipe, "{reason}", reason);
}

function bundle_error(reason) {
  let message = "\nI ran into an unexpected issue while trying to bundle your project with esbuild.\nHere's the error message I got:\n\n    {reason}\n\nIf you think this is a bug, please open an issue at\nhttps://github.com/lustre-labs/dev-tools/issues/new with some details about what\nyou were trying to do when you ran into this issue.\n";
  let _pipe = message;
  return $string.replace(_pipe, "{reason}", reason);
}

function cannot_create_directory(reason, path) {
  let message = "\nI ran into an error while trying to create the following directory:\n\n    {path}\n\nHere's the error message I got:\n\n    {reason}\n\nIf you think this is a bug, please open an issue at\nhttps://github.com/lustre-labs/dev-tools/issues/new with some details about what\nyou were trying to do when you ran into this issue.\n";
  let _pipe = message;
  let _pipe$1 = $string.replace(_pipe, "{path}", path);
  return $string.replace(_pipe$1, "{reason}", $string.inspect(reason));
}

function cannot_read_file(reason, path) {
  let message = "\nI ran into an error while trying to read the following file:\n\n    {path}\n\nHere's the error message I got:\n\n    {reason}\n\nIf you think this is a bug, please open an issue at\nhttps://github.com/lustre-labs/dev-tools/issues/new with some details about what\nyou were trying to do when you ran into this issue.\n";
  let _pipe = message;
  let _pipe$1 = $string.replace(_pipe, "{path}", path);
  return $string.replace(_pipe$1, "{reason}", $string.inspect(reason));
}

function cannot_set_permissions(reason, path) {
  let message = "\nI ran into an error while trying to set the permissions on the following file:\n\n    {path}\n\nHere's the error message I got:\n\n    {reason}\n\nIf you think this is a bug, please open an issue at\nhttps://github.com/lustre-labs/dev-tools/issues/new with some details about what\nyou were trying to do when you ran into this issue.\n";
  let _pipe = message;
  let _pipe$1 = $string.replace(_pipe, "{path}", path);
  return $string.replace(_pipe$1, "{reason}", $string.inspect(reason));
}

function cannot_start_dev_server(reason) {
  let message = "\nI ran into an error while trying to start the development server. Here's the\nerror message I got:\n\n    {reason}\n\nPlease open an issue at https://github.com/lustre-labs/dev-tools/issues/new with\nsome details about what you were trying to do when you ran into this issue.\n";
  let _pipe = message;
  return $string.replace(_pipe, "{reason}", $string.inspect(reason));
}

function cannot_start_file_watcher(reason) {
  let message = "\nI ran into an error while trying to start the file watcher used for live reloading.\nHere's the error message I got:\n\n    {reason}\n\nPlease open an issue at https://github.com/lustre-labs/dev-tools/issues/new with\nsome details about what you were trying to do when you ran into this issue.\n";
  let _pipe = message;
  return $string.replace(_pipe, "{reason}", $string.inspect(reason));
}

function cannot_write_file(reason, path) {
  let message = "\nI ran into an error while trying to write the following file:\n\n    {path}\n\nHere's the error message I got:\n\n    {reason}\n\nIf you think this is a bug, please open an issue at\nhttps://github.com/lustre-labs/dev-tools/issues/new with some details about what\nyou were trying to do when you ran into this issue.\n";
  let _pipe = message;
  let _pipe$1 = $string.replace(_pipe, "{path}", path);
  return $string.replace(_pipe$1, "{reason}", $string.inspect(reason));
}

function component_missing(module) {
  let message = "\nI couldn't find a valid component definition in the following module:\n\n    {module}\n\nTo bundle a component, the module should have a public function that returns a\nLustre `App`. Try adding a function like this:\n\n    pub const name: String = \"my-component\"\n\n    pub fn component() -> App(Nil, Model, Msg) {\n      lustre.component(init, update, view, on_attribute_change())\n    }\n";
  let _pipe = message;
  return $string.replace(_pipe, "{module}", module);
}

function incomplete_proxy(missing) {
  let message = "\nI'm missing some information needed to proxy requests from the development server.\nThe following keys are missing:\n\n    {missing}\n\nYou can provide the missing information either as flags when starting the\ndevelopment server, or by adding a `proxy` key to the `lustre-dev` section of\nyour `gleam.toml`.\n\nTo pass the information as flags, you should start the development server like\nthis:\n\n    gleam run -m lustre/dev start --proxy-from=/api --proxy-to=http://localhost:4000/api\n\nTo add the information to your `gleam.toml`, make sure it looks something like\nthis:\n\n    [lustre-dev.start]\n    proxy = { from = \"/api\", to = \"http://localhost:4000/api\" }\n";
  let _pipe = message;
  return $string.replace(_pipe, "{missing}", $string.join(missing, ", "));
}

function internal_error(info) {
  let message = "\nOops, it looks like I ran into an unexpected error while trying to do something.\nPlease open an issue at https://github.com/lustre-labs/dev-tools/issues/new with\nthe following message:\n\n    {info}\n";
  let _pipe = message;
  return $string.replace(_pipe, "{info}", info);
}

export function invalid_proxy_target(to) {
  let message = "\nI ran into an issue reading your proxy configuration. The URI you provided as the\ntarget for the proxy is invalid:\n\n    {to}\n\nPlease make sure the URI is valid and try again. If you think this is a bug,\nplease open an issue at https://github.com/lustre-labs/dev-tools/issues/new\n";
  let _pipe = message;
  return $string.replace(_pipe, "{to}", to);
}

function main_missing(module) {
  let message = "\nI couldn't find a `main` function in the following module:\n\n    {module}\n\nIs the module path correct? Your app's `main` function is the entry point we use\nto build and start your app. It should look something like this:\n\n    pub fn main() -> App(Nil, Model, Msg) {\n      lustre.application(init, update, view)\n    }\n";
  let _pipe = message;
  return $string.replace(_pipe, "{module}", module);
}

function module_missing(module) {
  let message = "\nI couldn't find the following module:\n\n    {module}\n\nMake sure the module path is correct and also the module is not included in the\n`internal_modules` list in your `gleam.toml`.\n\nIf you think this is a bug, please open an issue at\nhttps://github.com/lustre-labs/dev-tools/issues/new with some details about what\nyou were trying to do when you ran into this issue.\n";
  let _pipe = message;
  return $string.replace(_pipe, "{module}", module);
}

function name_missing(module) {
  let message = "\nI couldn't find a valid component definition in the following module:\n\n    {module}\n\nTo bundle a component, the module should have a public function that returns a\nLustre `App`. Try adding a function like this:\n\n    pub const name: String = \"my-component\"\n\n    pub fn component() -> App(Nil, Model, Msg) {\n      lustre.component(init, update, view, on_attribute_change())\n    }\n";
  let _pipe = message;
  return $string.replace(_pipe, "{module}", module);
}

function network_error(error) {
  let message = "\nI ran into an unexpected network error while trying to do something. Here's the\nerror message I got:\n\n    {error}\n\nPlease check your internet connection and try again. If you think this is a bug,\nplease open an issue at https://github.com/lustre-labs/dev-tools/issues/new with\nsome details about what you were trying to do when you ran into this issue.\n";
  let _pipe = message;
  return $string.replace(_pipe, "{error}", $string.inspect(error));
}

function template_missing(name, reason) {
  let message = "\nI ran into an unexpected error trying to read an internal template file. This\nshould never happen! The template file I was looking for is:\n\n    {name}\n\nThe error message I got was:\n\n    {reason}\n\nPlease open an issue at https://github.com/lustre-labs/dev-tools/issues/new with\nthe above information and some details about what you were trying to do when you\nran into this issue.\n}\n";
  let _pipe = message;
  let _pipe$1 = $string.replace(_pipe, "{name}", name);
  return $string.replace(_pipe$1, "{reason}", $string.inspect(reason));
}

function unknown_platform(binary, os, cpu) {
  let path = "./build/.lustre/bin/" + binary;
  let message = "\nI ran into a problem trying to download the {binary} binary. I couldn't find a\ncompatible binary for the following platform:\n\n    OS: {os}\n    CPU: {cpu}\n\nYou may be able to build the binary from source and place it at the following\npath:\n\n    {path}\n\nIf you think this is a bug, please open an issue at\nhttps://github.com/lustre-labs/dev-tools/issues/new with some details about what\nyou were trying to do when you ran into this issue.\n";
  let _pipe = message;
  let _pipe$1 = $string.replace(_pipe, "{binary}", binary);
  let _pipe$2 = $string.replace(_pipe$1, "{os}", os);
  let _pipe$3 = $string.replace(_pipe$2, "{cpu}", cpu);
  return $string.replace(_pipe$3, "{path}", path);
}

function otp_too_old(version) {
  let message = "\nIt looks like you're running an OTP version that is not supported by the dev\ntools: {version}.\n\nYou should upgrade to OTP 26 or newer to run this command:\nhttps://gleam.run/getting-started/installing/#installing-erlang\n";
  let _pipe = message;
  return $string.replace(_pipe, "{version}", $int.to_string(version));
}

function unzip_error(error) {
  let message = "\nI ran into an unexpected error while trying to unzip a file. Here's the error\nmessage I got:\n\n    {error}\n\nIf you think this is a bug, please open an issue at\nhttps://github.com/lustre-labs/dev-tools/issues/new with some details about what\nyou were trying to do when you ran into this issue.\n";
  let _pipe = message;
  return $string.replace(_pipe, "{error}", $string.inspect(error));
}

function invalid_esbuild_binary() {
  return "\nIt looks like the downloaded Esbuild tarball has a different hash from what I\nexpected.\n";
}

function invalid_tailwind_binary() {
  return "\nIt looks like the downloaded Tailwind binary has a different hash from what I\nexpected.\n";
}

function pretty_var(id) {
  let $ = id >= 26;
  if ($) {
    return pretty_var((divideInt(id, 26)) - 1) + pretty_var(
      remainderInt(id, 26),
    );
  } else {
    let id$1 = id + 97;
    let $1 = $bit_array.to_string(toBitArray([id$1]));
    if (!$1.isOk()) {
      throw makeError(
        "assignment_no_match",
        "lustre_dev_tools/error",
        608,
        "pretty_var",
        "Assignment pattern did not match",
        { value: $1 }
      )
    }
    let var$ = $1[0];
    return var$;
  }
}

function pretty_type(t) {
  if (t instanceof Tuple) {
    let elements = t.elements;
    let message = "#({elements})";
    let elements$1 = $list.map(elements, pretty_type);
    let _pipe = message;
    return $string.replace(_pipe, "{elements}", $string.join(elements$1, ", "));
  } else if (t instanceof Fn) {
    let params = t.parameters;
    let return$ = t.return;
    let message = "fn({params}) -> {return}";
    let params$1 = $list.map(params, pretty_type);
    let return$1 = pretty_type(return$);
    let _pipe = message;
    let _pipe$1 = $string.replace(
      _pipe,
      "{params}",
      $string.join(params$1, ", "),
    );
    return $string.replace(_pipe$1, "{return}", return$1);
  } else if (t instanceof Named && t.parameters.hasLength(0)) {
    let name = t.name;
    return name;
  } else if (t instanceof Named) {
    let name = t.name;
    let params = t.parameters;
    let message = "{name}({params})";
    let params$1 = $list.map(params, pretty_type);
    let _pipe = message;
    let _pipe$1 = $string.replace(_pipe, "{name}", name);
    return $string.replace(_pipe$1, "{params}", $string.join(params$1, ", "));
  } else {
    let id = t.id;
    return pretty_var(id);
  }
}

function main_bad_app_type(module, flags, model, msg) {
  let message = "\nI don't know how to serve the Lustre app returned from the `main` function in the\nfollowing module:\n\n    {module}\n\nI worked out your app type to be:\n\n    App({flags}, {model}, {msg})\n\nI need your app's flags type to either be `Nil` or a type variable like `a`. Your\n`main` function should look something like this:\n\n    pub fn main() -> App(Nil, {model}, {msg}) {\n      lustre.application(init, update, view)\n    }\n\nI don't know how to produce flags of type `{flags}`! If this is intentional and\nyou want to provide your own flags, try modifying your `main` function to look\nlike this:\n\n    pub fn main() -> Nil {\n      let app = lustre.application(init, update, view)\n      let flags = todo // provide your flags here\n      let assert Ok() = lustre.run(app, \"#app\", flags)\n\n      Nil\n    }\n";
  let _pipe = message;
  let _pipe$1 = $string.replace(_pipe, "{module}", module);
  let _pipe$2 = $string.replace(_pipe$1, "{flags}", pretty_type(flags));
  let _pipe$3 = $string.replace(_pipe$2, "{model}", pretty_type(model));
  return $string.replace(_pipe$3, "{msg}", pretty_type(msg));
}

function main_takes_an_argument(module, got) {
  let message = "\nI ran into a problem trying to serve your Lustre app in the following module:\n\n    {module}\n\nI worked out the type of your `main` function to be:\n\n    {got}\n\nThe `main` function should not take any arguments because I don't know how to\nprovide them! Your `main` function should look something like this:\n\n    pub fn main() -> App(Nil, Model, Msg) {\n      lustre.application(init, update, view)\n    }\n";
  let _pipe = message;
  let _pipe$1 = $string.replace(_pipe, "{module}", module);
  return $string.replace(_pipe$1, "{got}", pretty_type(got));
}

function name_incorrect_type(module, got) {
  let message = "\nI ran into a problem trying to bundle the component in the following module:\n\n    {module}\n\nThe type of the `name` constant isn't what I expected. I worked out the type to\nbe:\n\n    {got}\n\nThe `name` constant should be a string. Make sure it's defined like this:\n\n    pub const name: String = \"my-component\"\n\nIf you think this is a bug, please open an issue at\nhttps://github.com/lustre-labs/dev-tools/issues/new with some details about what\nyou were trying to do when you ran into this issue.\n";
  let _pipe = message;
  let _pipe$1 = $string.replace(_pipe, "{module}", module);
  return $string.replace(_pipe$1, "{got}", pretty_type(got));
}

export function explain(error) {
  if (error instanceof BuildError) {
    let reason = error.reason;
    return build_error(reason);
  } else if (error instanceof BundleError) {
    let reason = error.reason;
    return bundle_error(reason);
  } else if (error instanceof CannotCreateDirectory) {
    let reason = error.reason;
    let path = error.path;
    return cannot_create_directory(reason, path);
  } else if (error instanceof CannotReadFile) {
    let reason = error.reason;
    let path = error.path;
    return cannot_read_file(reason, path);
  } else if (error instanceof CannotSetPermissions) {
    let reason = error.reason;
    let path = error.path;
    return cannot_set_permissions(reason, path);
  } else if (error instanceof CannotStartDevServer) {
    let reason = error.reason;
    return cannot_start_dev_server(reason);
  } else if (error instanceof CannotStartFileWatcher) {
    let reason = error.reason;
    return cannot_start_file_watcher(reason);
  } else if (error instanceof CannotWriteFile) {
    let reason = error.reason;
    let path = error.path;
    return cannot_write_file(reason, path);
  } else if (error instanceof ComponentMissing) {
    let module = error.module;
    return component_missing(module);
  } else if (error instanceof IncompleteProxy) {
    let missing = error.missing;
    return incomplete_proxy(missing);
  } else if (error instanceof InternalError) {
    let message = error.message;
    return internal_error(message);
  } else if (error instanceof InvalidProxyTarget) {
    let to = error.to;
    return invalid_proxy_target(to);
  } else if (error instanceof MainBadAppType) {
    let module = error.module;
    let flags = error.flags;
    let model = error.model;
    let msg = error.msg;
    return main_bad_app_type(module, flags, model, msg);
  } else if (error instanceof MainMissing) {
    let module = error.module;
    return main_missing(module);
  } else if (error instanceof MainTakesAnArgument) {
    let module = error.module;
    let got = error.got;
    return main_takes_an_argument(module, got);
  } else if (error instanceof ModuleMissing) {
    let module = error.module;
    return module_missing(module);
  } else if (error instanceof NameIncorrectType) {
    let module = error.module;
    let got = error.got;
    return name_incorrect_type(module, got);
  } else if (error instanceof NameMissing) {
    let module = error.module;
    return name_missing(module);
  } else if (error instanceof NetworkError) {
    let error$1 = error[0];
    return network_error(error$1);
  } else if (error instanceof TemplateMissing) {
    let name = error.name;
    let reason = error.reason;
    return template_missing(name, reason);
  } else if (error instanceof UnknownPlatform) {
    let binary = error.binary;
    let os = error.os;
    let cpu = error.cpu;
    return unknown_platform(binary, os, cpu);
  } else if (error instanceof OtpTooOld) {
    let version = error.version;
    return otp_too_old(version);
  } else if (error instanceof UnzipError) {
    let error$1 = error[0];
    return unzip_error(error$1);
  } else if (error instanceof InvalidEsbuildBinary) {
    return invalid_esbuild_binary();
  } else {
    return invalid_tailwind_binary();
  }
}
