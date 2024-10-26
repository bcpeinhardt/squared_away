import * as $filepath from "../../../filepath/filepath.mjs";
import * as $package_interface from "../../../gleam_package_interface/gleam/package_interface.mjs";
import { Named, Variable } from "../../../gleam_package_interface/gleam/package_interface.mjs";
import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $glint from "../../../glint/glint.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import { Ok, Error } from "../../gleam.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import { do$, try$ } from "../../lustre_dev_tools/cli.mjs";
import * as $flag from "../../lustre_dev_tools/cli/flag.mjs";
import * as $cmd from "../../lustre_dev_tools/cmd.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import {
  BundleError,
  ComponentMissing,
  MainMissing,
  ModuleMissing,
  NameIncorrectType,
  NameMissing,
} from "../../lustre_dev_tools/error.mjs";
import * as $esbuild from "../../lustre_dev_tools/esbuild.mjs";
import * as $project from "../../lustre_dev_tools/project.mjs";
import * as $tailwind from "../../lustre_dev_tools/tailwind.mjs";

function check_main_function(module_path, module) {
  let $ = $dict.has_key(module.functions, "main");
  if ($) {
    return new Ok(undefined);
  } else {
    return new Error(new MainMissing(module_path));
  }
}

function is_string_type(t) {
  if (t instanceof Named &&
  t.name === "String" &&
  t.package === "" &&
  t.module === "gleam" &&
  t.parameters.hasLength(0)) {
    return true;
  } else {
    return false;
  }
}

function check_component_name(module_path, module) {
  let _pipe = $dict.get(module.constants, "name");
  let _pipe$1 = $result.replace_error(_pipe, new NameMissing(module_path));
  return $result.then$(
    _pipe$1,
    (component_name) => {
      let $ = is_string_type(component_name);
      if ($) {
        return new Ok(undefined);
      } else {
        return new Error(new NameIncorrectType(module_path, component_name));
      }
    },
  );
}

function is_nil_type(t) {
  if (t instanceof Named &&
  t.name === "Nil" &&
  t.package === "" &&
  t.module === "gleam" &&
  t.parameters.hasLength(0)) {
    return true;
  } else {
    return false;
  }
}

function is_type_variable(t) {
  if (t instanceof Variable) {
    return true;
  } else {
    return false;
  }
}

function is_compatible_app_type(t) {
  if (t instanceof Named &&
  t.name === "App" &&
  t.package === "lustre" &&
  t.module === "lustre" &&
  t.parameters.atLeastLength(1)) {
    let flags = t.parameters.head;
    return is_nil_type(flags) || is_type_variable(flags);
  } else {
    return false;
  }
}

function find_component(module_path, module) {
  let functions = $dict.to_list(module.functions);
  let error = new Error(new ComponentMissing(module_path));
  return $list.fold_until(
    functions,
    error,
    (_, _use1) => {
      let name = _use1[0];
      let t = _use1[1];
      let $ = t.parameters;
      let $1 = is_compatible_app_type(t.return);
      if ($.hasLength(0) && $1) {
        return new $list.Stop(new Ok(name));
      } else {
        return new $list.Continue(error);
      }
    },
  );
}

function is_reserved_keyword(name) {
  if (name === "await") {
    return true;
  } else if (name === "arguments") {
    return true;
  } else if (name === "break") {
    return true;
  } else if (name === "case") {
    return true;
  } else if (name === "catch") {
    return true;
  } else if (name === "class") {
    return true;
  } else if (name === "const") {
    return true;
  } else if (name === "continue") {
    return true;
  } else if (name === "debugger") {
    return true;
  } else if (name === "default") {
    return true;
  } else if (name === "delete") {
    return true;
  } else if (name === "do") {
    return true;
  } else if (name === "else") {
    return true;
  } else if (name === "enum") {
    return true;
  } else if (name === "export") {
    return true;
  } else if (name === "extends") {
    return true;
  } else if (name === "eval") {
    return true;
  } else if (name === "false") {
    return true;
  } else if (name === "finally") {
    return true;
  } else if (name === "for") {
    return true;
  } else if (name === "function") {
    return true;
  } else if (name === "if") {
    return true;
  } else if (name === "implements") {
    return true;
  } else if (name === "import") {
    return true;
  } else if (name === "in") {
    return true;
  } else if (name === "instanceof") {
    return true;
  } else if (name === "interface") {
    return true;
  } else if (name === "let") {
    return true;
  } else if (name === "new") {
    return true;
  } else if (name === "null") {
    return true;
  } else if (name === "package") {
    return true;
  } else if (name === "private") {
    return true;
  } else if (name === "protected") {
    return true;
  } else if (name === "public") {
    return true;
  } else if (name === "return") {
    return true;
  } else if (name === "static") {
    return true;
  } else if (name === "super") {
    return true;
  } else if (name === "switch") {
    return true;
  } else if (name === "this") {
    return true;
  } else if (name === "throw") {
    return true;
  } else if (name === "true") {
    return true;
  } else if (name === "try") {
    return true;
  } else if (name === "typeof") {
    return true;
  } else if (name === "var") {
    return true;
  } else if (name === "void") {
    return true;
  } else if (name === "while") {
    return true;
  } else if (name === "with") {
    return true;
  } else if (name === "yield") {
    return true;
  } else if (name === "undefined") {
    return true;
  } else if (name === "then") {
    return true;
  } else {
    return false;
  }
}

function importable_name(identifier) {
  let $ = is_reserved_keyword(identifier);
  if ($) {
    return identifier + "$";
  } else {
    return identifier;
  }
}

export const description = "\nCommands to build different kinds of Lustre application. These commands go beyond\njust running `gleam build` and handle features like bundling, minification, and\nintegration with other build tools.\n";
