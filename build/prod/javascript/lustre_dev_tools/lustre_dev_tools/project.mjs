import * as $filepath from "../../filepath/filepath.mjs";
import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $package_interface from "../../gleam_package_interface/gleam/package_interface.mjs";
import { Fn, Named, Tuple, Variable } from "../../gleam_package_interface/gleam/package_interface.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import { DecodeError } from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $pair from "../../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import * as $tom from "../../tom/tom.mjs";
import { Ok, toList, CustomType as $CustomType, makeError } from "../gleam.mjs";
import * as $cmd from "../lustre_dev_tools/cmd.mjs";
import * as $error from "../lustre_dev_tools/error.mjs";
import { BuildError } from "../lustre_dev_tools/error.mjs";

export class Config extends $CustomType {
  constructor(name, version, toml) {
    super();
    this.name = name;
    this.version = version;
    this.toml = toml;
  }
}

export class Interface extends $CustomType {
  constructor(name, version, modules) {
    super();
    this.name = name;
    this.version = version;
    this.modules = modules;
  }
}

export class Module extends $CustomType {
  constructor(constants, functions) {
    super();
    this.constants = constants;
    this.functions = functions;
  }
}

export class Function extends $CustomType {
  constructor(parameters, return$) {
    super();
    this.parameters = parameters;
    this.return = return$;
  }
}

function find_root(loop$path) {
  while (true) {
    let path = loop$path;
    let toml = $filepath.join(path, "gleam.toml");
    let $ = $simplifile.is_file(toml);
    if ($.isOk() && !$[0]) {
      loop$path = $filepath.join("..", path);
    } else if (!$.isOk()) {
      loop$path = $filepath.join("..", path);
    } else {
      return path;
    }
  }
}

export function root() {
  return find_root(".");
}

export function config() {
  let configuration_path = $filepath.join(root(), "gleam.toml");
  let $ = $simplifile.read(configuration_path);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre_dev_tools/project",
      105,
      "config",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let configuration = $[0];
  let $1 = $tom.parse(configuration);
  if (!$1.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre_dev_tools/project",
      106,
      "config",
      "Assignment pattern did not match",
      { value: $1 }
    )
  }
  let toml = $1[0];
  let $2 = $tom.get_string(toml, toList(["name"]));
  if (!$2.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre_dev_tools/project",
      107,
      "config",
      "Assignment pattern did not match",
      { value: $2 }
    )
  }
  let name = $2[0];
  let $3 = $tom.get_string(toml, toList(["version"]));
  if (!$3.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre_dev_tools/project",
      108,
      "config",
      "Assignment pattern did not match",
      { value: $3 }
    )
  }
  let version = $3[0];
  return new Ok(new Config(name, version, toml));
}

export function type_to_string(type_) {
  if (type_ instanceof Tuple) {
    let elements = type_.elements;
    let elements$1 = $list.map(elements, type_to_string);
    return ("#(" + $string.join(elements$1, ", ")) + ")";
  } else if (type_ instanceof Fn) {
    let params = type_.parameters;
    let return$ = type_.return;
    let params$1 = $list.map(params, type_to_string);
    let return$1 = type_to_string(return$);
    return (("fn(" + $string.join(params$1, ", ")) + ") -> ") + return$1;
  } else if (type_ instanceof Named && type_.parameters.hasLength(0)) {
    let name = type_.name;
    return name;
  } else if (type_ instanceof Named) {
    let name = type_.name;
    let params = type_.parameters;
    let params$1 = $list.map(params, type_to_string);
    return ((name + "(") + $string.join(params$1, ", ")) + ")";
  } else {
    let id = type_.id;
    return "a_" + $int.to_string(id);
  }
}

function labelled_argument_decoder(dyn) {
  return $dynamic.field("type", $package_interface.type_decoder)(dyn);
}

function function_decoder(dyn) {
  return $dynamic.decode2(
    (var0, var1) => { return new Function(var0, var1); },
    $dynamic.field("parameters", $dynamic.list(labelled_argument_decoder)),
    $dynamic.field("return", $package_interface.type_decoder),
  )(dyn);
}

function string_dict(values) {
  return $dynamic.dict($dynamic.string, values);
}

function module_decoder(dyn) {
  return $dynamic.decode2(
    (var0, var1) => { return new Module(var0, var1); },
    $dynamic.field(
      "constants",
      string_dict($dynamic.field("type", $package_interface.type_decoder)),
    ),
    $dynamic.field("functions", string_dict(function_decoder)),
  )(dyn);
}

function interface_decoder(dyn) {
  return $dynamic.decode3(
    (var0, var1, var2) => { return new Interface(var0, var1, var2); },
    $dynamic.field("name", $dynamic.string),
    $dynamic.field("version", $dynamic.string),
    $dynamic.field("modules", string_dict(module_decoder)),
  )(dyn);
}
