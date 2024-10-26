import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { Error, toList, CustomType as $CustomType } from "../gleam.mjs";

export class Package extends $CustomType {
  constructor(name, version, gleam_version_constraint, modules) {
    super();
    this.name = name;
    this.version = version;
    this.gleam_version_constraint = gleam_version_constraint;
    this.modules = modules;
  }
}

export class Module extends $CustomType {
  constructor(documentation, type_aliases, types, constants, functions) {
    super();
    this.documentation = documentation;
    this.type_aliases = type_aliases;
    this.types = types;
    this.constants = constants;
    this.functions = functions;
  }
}

export class TypeAlias extends $CustomType {
  constructor(documentation, deprecation, parameters, alias) {
    super();
    this.documentation = documentation;
    this.deprecation = deprecation;
    this.parameters = parameters;
    this.alias = alias;
  }
}

export class TypeDefinition extends $CustomType {
  constructor(documentation, deprecation, parameters, constructors) {
    super();
    this.documentation = documentation;
    this.deprecation = deprecation;
    this.parameters = parameters;
    this.constructors = constructors;
  }
}

export class TypeConstructor extends $CustomType {
  constructor(documentation, name, parameters) {
    super();
    this.documentation = documentation;
    this.name = name;
    this.parameters = parameters;
  }
}

export class Parameter extends $CustomType {
  constructor(label, type_) {
    super();
    this.label = label;
    this.type_ = type_;
  }
}

export class Constant extends $CustomType {
  constructor(documentation, deprecation, implementations, type_) {
    super();
    this.documentation = documentation;
    this.deprecation = deprecation;
    this.implementations = implementations;
    this.type_ = type_;
  }
}

export class Function extends $CustomType {
  constructor(documentation, deprecation, implementations, parameters, return$) {
    super();
    this.documentation = documentation;
    this.deprecation = deprecation;
    this.implementations = implementations;
    this.parameters = parameters;
    this.return = return$;
  }
}

export class Deprecation extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class Implementations extends $CustomType {
  constructor(gleam, uses_erlang_externals, uses_javascript_externals) {
    super();
    this.gleam = gleam;
    this.uses_erlang_externals = uses_erlang_externals;
    this.uses_javascript_externals = uses_javascript_externals;
  }
}

export class Tuple extends $CustomType {
  constructor(elements) {
    super();
    this.elements = elements;
  }
}

export class Fn extends $CustomType {
  constructor(parameters, return$) {
    super();
    this.parameters = parameters;
    this.return = return$;
  }
}

export class Variable extends $CustomType {
  constructor(id) {
    super();
    this.id = id;
  }
}

export class Named extends $CustomType {
  constructor(name, package$, module, parameters) {
    super();
    this.name = name;
    this.package = package$;
    this.module = module;
    this.parameters = parameters;
  }
}

export function deprecation_decoder(dynamic) {
  return $dynamic.decode1(
    (var0) => { return new Deprecation(var0); },
    $dynamic.field("message", $dynamic.string),
  )(dynamic);
}

export function implementations_decoder(dynamic) {
  return $dynamic.decode3(
    (var0, var1, var2) => { return new Implementations(var0, var1, var2); },
    $dynamic.field("gleam", $dynamic.bool),
    $dynamic.field("uses-erlang-externals", $dynamic.bool),
    $dynamic.field("uses-javascript-externals", $dynamic.bool),
  )(dynamic);
}

export function type_decoder(dynamic) {
  return $result.try$(
    $dynamic.field("kind", $dynamic.string)(dynamic),
    (kind) => {
      if (kind === "variable") {
        return $dynamic.decode1(
          (var0) => { return new Variable(var0); },
          $dynamic.field("id", $dynamic.int),
        )(dynamic);
      } else if (kind === "tuple") {
        return $dynamic.decode1(
          (var0) => { return new Tuple(var0); },
          $dynamic.field("elements", $dynamic.list(type_decoder)),
        )(dynamic);
      } else if (kind === "named") {
        return $dynamic.decode4(
          (var0, var1, var2, var3) => {
            return new Named(var0, var1, var2, var3);
          },
          $dynamic.field("name", $dynamic.string),
          $dynamic.field("package", $dynamic.string),
          $dynamic.field("module", $dynamic.string),
          $dynamic.field("parameters", $dynamic.list(type_decoder)),
        )(dynamic);
      } else if (kind === "fn") {
        return $dynamic.decode2(
          (var0, var1) => { return new Fn(var0, var1); },
          $dynamic.field("parameters", $dynamic.list(type_decoder)),
          $dynamic.field("return", type_decoder),
        )(dynamic);
      } else {
        let unknown_tag = kind;
        return new Error(
          toList([
            new $dynamic.DecodeError(
              "one of variable, tuple, named, fn",
              unknown_tag,
              toList(["kind"]),
            ),
          ]),
        );
      }
    },
  );
}

export function type_alias_decoder(dynamic) {
  return $dynamic.decode4(
    (var0, var1, var2, var3) => { return new TypeAlias(var0, var1, var2, var3); },
    $dynamic.field("documentation", $dynamic.optional($dynamic.string)),
    $dynamic.field("deprecation", $dynamic.optional(deprecation_decoder)),
    $dynamic.field("parameters", $dynamic.int),
    $dynamic.field("alias", type_decoder),
  )(dynamic);
}

export function constant_decoder(dynamic) {
  return $dynamic.decode4(
    (var0, var1, var2, var3) => { return new Constant(var0, var1, var2, var3); },
    $dynamic.field("documentation", $dynamic.optional($dynamic.string)),
    $dynamic.field("deprecation", $dynamic.optional(deprecation_decoder)),
    $dynamic.field("implementations", implementations_decoder),
    $dynamic.field("type", type_decoder),
  )(dynamic);
}

export function parameter_decoder(dynamic) {
  return $dynamic.decode2(
    (var0, var1) => { return new Parameter(var0, var1); },
    $dynamic.field("label", $dynamic.optional($dynamic.string)),
    $dynamic.field("type", type_decoder),
  )(dynamic);
}

export function function_decoder(dynamic) {
  return $dynamic.decode5(
    (var0, var1, var2, var3, var4) => {
      return new Function(var0, var1, var2, var3, var4);
    },
    $dynamic.field("documentation", $dynamic.optional($dynamic.string)),
    $dynamic.field("deprecation", $dynamic.optional(deprecation_decoder)),
    $dynamic.field("implementations", implementations_decoder),
    $dynamic.field("parameters", $dynamic.list(parameter_decoder)),
    $dynamic.field("return", type_decoder),
  )(dynamic);
}

export function constructor_decoder(dynamic) {
  return $dynamic.decode3(
    (var0, var1, var2) => { return new TypeConstructor(var0, var1, var2); },
    $dynamic.field("documentation", $dynamic.optional($dynamic.string)),
    $dynamic.field("name", $dynamic.string),
    $dynamic.field("parameters", $dynamic.list(parameter_decoder)),
  )(dynamic);
}

export function type_definition_decoder(dynamic) {
  return $dynamic.decode4(
    (var0, var1, var2, var3) => {
      return new TypeDefinition(var0, var1, var2, var3);
    },
    $dynamic.field("documentation", $dynamic.optional($dynamic.string)),
    $dynamic.field("deprecation", $dynamic.optional(deprecation_decoder)),
    $dynamic.field("parameters", $dynamic.int),
    $dynamic.field("constructors", $dynamic.list(constructor_decoder)),
  )(dynamic);
}

function string_dict(values) {
  return $dynamic.dict($dynamic.string, values);
}

export function module_decoder(dynamic) {
  return $dynamic.decode5(
    (var0, var1, var2, var3, var4) => {
      return new Module(var0, var1, var2, var3, var4);
    },
    $dynamic.field("documentation", $dynamic.list($dynamic.string)),
    $dynamic.field("type-aliases", string_dict(type_alias_decoder)),
    $dynamic.field("types", string_dict(type_definition_decoder)),
    $dynamic.field("constants", string_dict(constant_decoder)),
    $dynamic.field("functions", string_dict(function_decoder)),
  )(dynamic);
}

export function decoder(dynamic) {
  return $dynamic.decode4(
    (var0, var1, var2, var3) => { return new Package(var0, var1, var2, var3); },
    $dynamic.field("name", $dynamic.string),
    $dynamic.field("version", $dynamic.string),
    $dynamic.field(
      "gleam-version-constraint",
      $dynamic.optional($dynamic.string),
    ),
    $dynamic.field("modules", string_dict(module_decoder)),
  )(dynamic);
}
