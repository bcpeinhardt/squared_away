import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeErrors, type Decoder, type Dynamic}
import gleam/option.{type Option}
import gleam/result

// --- TYPES -------------------------------------------------------------------

/// A Gleam package.
///
pub type Package {
  Package(
    name: String,
    version: String,
    /// The Gleam version constraint that the package specifies in its
    /// `gleam.toml`.
    gleam_version_constraint: Option(String),
    modules: Dict(String, Module),
  )
}

/// A Gleam module.
///
pub type Module {
  Module(
    /// All the lines composing the module's documentation (that is every line
    /// preceded by a `////`).
    documentation: List(String),
    /// The public type aliases defined in the module.
    type_aliases: Dict(String, TypeAlias),
    /// The public custom types defined in the module.
    types: Dict(String, TypeDefinition),
    /// The public constants defined in the module.
    constants: Dict(String, Constant),
    /// The public functions defined in the module.
    functions: Dict(String, Function),
  )
}

/// A Gleam type alias.
///
/// ```gleam
/// // This is a type alias.
/// type Ints = List(Int)
/// ```
///
pub type TypeAlias {
  TypeAlias(
    /// The type alias' documentation comment (that is every line preceded by
    /// `///`).
    ///
    documentation: Option(String),
    /// If the type alias is deprecated this will hold the reason of the
    /// deprecation.
    ///
    deprecation: Option(Deprecation),
    /// The number of type variables of the type alias.
    ///
    /// ```gleam
    /// type Results(a, b) = List(Result(a, b))
    /// //   ^^^^^^^^^^^^^ This type alias has 2 type variables.
    ///
    /// type Ints = List(Int)
    /// //   ^^^^ This type alias has 0 type variables.
    /// ```
    ///
    parameters: Int,
    /// The aliased type.
    ///
    /// ```gleam
    /// type Ints = List(Int)
    /// //          ^^^^^^^^^ This is the aliased type.
    /// ```
    ///
    alias: Type,
  )
}

/// A Gleam custom type definition.
///
/// ```gleam
/// // This is a custom type definition.
/// pub type Result(a, b) {
///   Ok(a)
///   Error(b)
/// }
/// ```
///
pub type TypeDefinition {
  TypeDefinition(
    /// The type definition's documentation comment (that is every line preceded
    /// by `///`).
    ///
    documentation: Option(String),
    /// If the type definition is deprecated this will hold the reason of the
    /// deprecation.
    ///
    deprecation: Option(Deprecation),
    /// The number of type variables of the type definition.
    ///
    /// ```gleam
    /// type Result(a, b) { ... }
    /// //   ^^^^^^^^^^^^ This type definition has 2 type variables.
    ///
    /// type Person { ... }
    /// //   ^^^^^^ This type alias has 0 type variables.
    /// ```
    ///
    parameters: Int,
    /// The type constructors. If the type is opaque this list will be empty as
    /// the type doesn't have any public constructor.
    ///
    /// ```gleam
    /// type Result(a, b) {
    ///   Ok(a)
    ///   Error(b)
    /// }
    /// // `Ok` and `Error` are the type constructors
    /// // of the `Error` type.
    /// ```
    ///
    constructors: List(TypeConstructor),
  )
}

/// A Gleam type constructor.
///
/// ```gleam
/// type Result(a, b) {
///   Ok(a)
///   Error(b)
/// }
/// // `Ok` and `Error` are the type constructors
/// // of the `Error` type.
/// ```
///
pub type TypeConstructor {
  TypeConstructor(
    /// The type constructor's documentation comment (that is every line
    /// preceded by `///`).
    ///
    documentation: Option(String),
    name: String,
    /// The parameters required by the constructor.
    ///
    /// ```gleam
    /// type Box(a) {
    ///   Box(content: a)
    /// //    ^^^^^^^^^^ The `Box` constructor has a single
    /// //               labelled argument.
    /// }
    /// ```
    ///
    parameters: List(Parameter),
  )
}

/// A parameter (that might be labelled) of a module function or type
/// constructor.
///
/// ```gleam
/// pub fn map(over list: List(a), with fun: fn(a) -> b) -> b { todo }
/// //         ^^^^^^^^^^^^^^^^^^ A labelled parameter.
/// ```
///
pub type Parameter {
  Parameter(label: Option(String), type_: Type)
}

/// A Gleam constant.
///
/// ```gleam
/// pub const my_favourite_number = 11
/// ```
///
pub type Constant {
  Constant(
    /// The constant's documentation comment (that is every line preceded by
    /// `///`).
    ///
    documentation: Option(String),
    /// If the constant is deprecated this will hold the reason of the
    /// deprecation.
    ///
    deprecation: Option(Deprecation),
    implementations: Implementations,
    type_: Type,
  )
}

/// A Gleam function definition.
///
/// ```gleam
/// pub fn reverse(list: List(a)) -> List(a) { todo }
/// ```
pub type Function {
  Function(
    /// The function's documentation comment (that is every line preceded by
    /// `///`).
    ///
    documentation: Option(String),
    /// If the function is deprecated this will hold the reason of the
    /// deprecation.
    ///
    deprecation: Option(Deprecation),
    implementations: Implementations,
    parameters: List(Parameter),
    return: Type,
  )
}

/// A deprecation notice that can be added to definition using the
/// `@deprecated` annotation.
///
pub type Deprecation {
  Deprecation(message: String)
}

/// Metadata about how a value is implemented and the targets it supports.
///
pub type Implementations {
  Implementations(
    /// Set to `True` if the const/function has a pure Gleam implementation
    /// (that is, it never uses external code).
    /// Being pure Gleam means that the function will support all Gleam
    /// targets, even future ones that are not present to this day.
    ///
    /// Consider the following function:
    ///
    /// ```gleam
    /// @external(erlang, "foo", "bar")
    /// pub fn a_random_number() -> Int {
    ///   4
    ///   // This is a default implementation.
    /// }
    /// ```
    ///
    /// The implementations for this function will look like this:
    ///
    /// ```gleam
    /// Implementations(
    ///   gleam: True,
    ///   uses_erlang_externals: True,
    ///   uses_javascript_externals: False,
    /// )
    /// ```
    ///
    /// - `gleam: True` means that the function has a pure Gleam implementation
    ///   and thus it can be used on all Gleam targets with no problems.
    /// - `uses_erlang_externals: True` means that the function will use Erlang
    ///   external code when compiled to the Erlang target.
    /// - `uses_javascript_externals: False` means that the function won't use
    ///   JavaScript external code when compiled to JavaScript. The function can
    ///   still be used on the JavaScript target since it has a pure Gleam
    ///   implementation.
    ///
    gleam: Bool,
    /// Set to `True` if the const/function is defined using Erlang external
    /// code. That means that the function will use Erlang code through FFI when
    /// compiled for the Erlang target.
    ///
    uses_erlang_externals: Bool,
    /// Set to `True` if the const/function is defined using JavaScript external
    /// code. That means that the function will use JavaScript code through FFI
    /// when compiled for the JavaScript target.
    ///
    /// Let's have a look at an example:
    ///
    /// ```gleam
    /// @external(javascript, "foo", "bar")
    /// pub fn javascript_only() -> Int
    /// ```
    ///
    /// It's implementations field will look like this:
    ///
    /// ```gleam
    /// Implementations(
    ///   gleam: False,
    ///   uses_erlang_externals: False,
    ///   uses_javascript_externals: True,
    /// )
    /// ```
    ///
    /// - `gleam: False` means that the function doesn't have a pure Gleam
    ///   implementations. This means that the function is only defined using
    ///   externals and can only be used on some targets.
    /// - `uses_erlang_externals: False` the function is not using external
    ///   Erlang code. So, since the function doesn't have a fallback pure Gleam
    ///   implementation, you won't be able to compile it on this target.
    /// - `uses_javascript_externals: True` the function is using JavaScript
    ///   external code. This means that you will be able to use it on the
    ///   JavaScript target with no problems.
    ///
    uses_javascript_externals: Bool,
  )
}

/// A Gleam type.
///
pub type Type {
  /// A tuple type like `#(Int, Float)`.
  ///
  Tuple(elements: List(Type))
  /// A function type like `fn(Int, a) -> List(a)`.
  ///
  Fn(parameters: List(Type), return: Type)
  /// A type variable.
  ///
  /// ```gleam
  /// pub fn foo(value: a) -> a { todo }
  /// //                ^ This is a type variable.
  /// ```
  ///
  Variable(id: Int)
  /// A custom named type.
  /// ```gleam
  /// let value: Bool = True
  /// //         ^^^^ Bool is a named type coming from Gleam's prelude
  /// ```
  ///
  /// All prelude types - like Bool, String, etc. - are named types as well.
  /// In that case, their package is an empty string `""` and their module
  /// name is the string `"gleam"`.
  ///
  Named(
    name: String,
    /// The package the type comes from.
    ///
    package: String,
    /// The module the type is defined in.
    ///
    module: String,
    /// The concrete type's type parameters
    /// .
    /// ```gleam
    /// let result: Result(Int, e) = Ok(1)
    /// //                ^^^^^^^^ The `Result` named type has 2 parameters.
    /// //                         In this case it's the `Int` type and a
    /// //                         type variable.
    /// ```
    ///
    parameters: List(Type),
  )
}

// --- DECODERS ----------------------------------------------------------------

pub fn decoder(dynamic: Dynamic) -> Result(Package, DecodeErrors) {
  dynamic.decode4(
    Package,
    dynamic.field("name", dynamic.string),
    dynamic.field("version", dynamic.string),
    dynamic.field("gleam-version-constraint", dynamic.optional(dynamic.string)),
    dynamic.field("modules", string_dict(module_decoder)),
  )(dynamic)
}

pub fn module_decoder(dynamic: Dynamic) -> Result(Module, DecodeErrors) {
  dynamic.decode5(
    Module,
    dynamic.field("documentation", dynamic.list(dynamic.string)),
    dynamic.field("type-aliases", string_dict(type_alias_decoder)),
    dynamic.field("types", string_dict(type_definition_decoder)),
    dynamic.field("constants", string_dict(constant_decoder)),
    dynamic.field("functions", string_dict(function_decoder)),
  )(dynamic)
}

pub fn type_alias_decoder(dynamic: Dynamic) -> Result(TypeAlias, DecodeErrors) {
  dynamic.decode4(
    TypeAlias,
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("deprecation", dynamic.optional(deprecation_decoder)),
    dynamic.field("parameters", dynamic.int),
    dynamic.field("alias", type_decoder),
  )(dynamic)
}

pub fn type_definition_decoder(
  dynamic: Dynamic,
) -> Result(TypeDefinition, DecodeErrors) {
  dynamic.decode4(
    TypeDefinition,
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("deprecation", dynamic.optional(deprecation_decoder)),
    dynamic.field("parameters", dynamic.int),
    dynamic.field("constructors", dynamic.list(constructor_decoder)),
  )(dynamic)
}

pub fn constant_decoder(dynamic: Dynamic) -> Result(Constant, DecodeErrors) {
  dynamic.decode4(
    Constant,
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("deprecation", dynamic.optional(deprecation_decoder)),
    dynamic.field("implementations", implementations_decoder),
    dynamic.field("type", type_decoder),
  )(dynamic)
}

pub fn function_decoder(dynamic: Dynamic) -> Result(Function, DecodeErrors) {
  dynamic.decode5(
    Function,
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("deprecation", dynamic.optional(deprecation_decoder)),
    dynamic.field("implementations", implementations_decoder),
    dynamic.field("parameters", dynamic.list(parameter_decoder)),
    dynamic.field("return", type_decoder),
  )(dynamic)
}

pub fn deprecation_decoder(
  dynamic: Dynamic,
) -> Result(Deprecation, DecodeErrors) {
  dynamic.decode1(Deprecation, dynamic.field("message", dynamic.string))(
    dynamic,
  )
}

pub fn constructor_decoder(
  dynamic: Dynamic,
) -> Result(TypeConstructor, DecodeErrors) {
  dynamic.decode3(
    TypeConstructor,
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("name", dynamic.string),
    dynamic.field("parameters", dynamic.list(parameter_decoder)),
  )(dynamic)
}

pub fn implementations_decoder(
  dynamic: Dynamic,
) -> Result(Implementations, DecodeErrors) {
  dynamic.decode3(
    Implementations,
    dynamic.field("gleam", dynamic.bool),
    dynamic.field("uses-erlang-externals", dynamic.bool),
    dynamic.field("uses-javascript-externals", dynamic.bool),
  )(dynamic)
}

pub fn parameter_decoder(dynamic: Dynamic) -> Result(Parameter, DecodeErrors) {
  dynamic.decode2(
    Parameter,
    dynamic.field("label", dynamic.optional(dynamic.string)),
    dynamic.field("type", type_decoder),
  )(dynamic)
}

pub fn type_decoder(dynamic: Dynamic) -> Result(Type, DecodeErrors) {
  use kind <- result.try(dynamic.field("kind", dynamic.string)(dynamic))
  case kind {
    "variable" ->
      dynamic.decode1(Variable, dynamic.field("id", dynamic.int))(dynamic)

    "tuple" ->
      dynamic.decode1(
        Tuple,
        dynamic.field("elements", dynamic.list(type_decoder)),
      )(dynamic)

    "named" ->
      dynamic.decode4(
        Named,
        dynamic.field("name", dynamic.string),
        dynamic.field("package", dynamic.string),
        dynamic.field("module", dynamic.string),
        dynamic.field("parameters", dynamic.list(type_decoder)),
      )(dynamic)

    "fn" ->
      dynamic.decode2(
        Fn,
        dynamic.field("parameters", dynamic.list(type_decoder)),
        dynamic.field("return", type_decoder),
      )(dynamic)

    unknown_tag ->
      Error([
        dynamic.DecodeError(
          expected: "one of variable, tuple, named, fn",
          found: unknown_tag,
          path: ["kind"],
        ),
      ])
  }
}

// --- UTILITY FUNCTIONS -------------------------------------------------------

fn string_dict(values: Decoder(a)) -> Decoder(Dict(String, a)) {
  dynamic.dict(dynamic.string, values)
}
