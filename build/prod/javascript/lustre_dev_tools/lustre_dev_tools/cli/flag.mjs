import * as $glint from "../../../glint/glint.mjs";
import * as $constraint from "../../../glint/glint/constraint.mjs";
import { toList } from "../../gleam.mjs";

export function esbuild_os() {
  let description = "Override the automatic OS detection.";
  let allowed = toList([
    "android",
    "darwin",
    "freebsd",
    "linux",
    "win32",
    "netbsd",
    "openbsd",
    "sunos",
  ]);
  let _pipe = $glint.string_flag("os");
  let _pipe$1 = $glint.flag_help(_pipe, description);
  return $glint.flag_constraint(_pipe$1, $constraint.one_of(allowed));
}

export function esbuild_cpu() {
  let description = "Override the automatic CPU architecture detection.";
  let allowed = toList([
    "aarch64",
    "amd64",
    "arm",
    "arm64",
    "ia32",
    "x64",
    "x86_64",
  ]);
  let _pipe = $glint.string_flag("cpu");
  let _pipe$1 = $glint.flag_help(_pipe, description);
  return $glint.flag_constraint(_pipe$1, $constraint.one_of(allowed));
}

export function tailwind_os() {
  let description = "Override the automatic OS detection.";
  let allowed = toList(["linux", "win32", "darwin"]);
  let _pipe = $glint.string_flag("os");
  let _pipe$1 = $glint.flag_help(_pipe, description);
  return $glint.flag_constraint(_pipe$1, $constraint.one_of(allowed));
}

export function tailwind_cpu() {
  let description = "Override the automatic CPU architecture detection.";
  let allowed = toList(["armv7", "arm64", "x64", "x86_64", "aarch64"]);
  let _pipe = $glint.string_flag("cpu");
  let _pipe$1 = $glint.flag_help(_pipe, description);
  return $glint.flag_constraint(_pipe$1, $constraint.one_of(allowed));
}

export function minify() {
  let description = "Minify the output, renaming variables and removing whitespace.";
  let _pipe = $glint.bool_flag("minify");
  return $glint.flag_help(_pipe, description);
}

export function tailwind_entry() {
  let description = "Use a custom CSS file as the entry to a Tailwind CSS bundle.";
  let _pipe = $glint.string_flag("tailwind-entry");
  return $glint.flag_help(_pipe, description);
}

export function outdir() {
  let description = "Use a custom directory as the destination for any built files.";
  let _pipe = $glint.string_flag("outdir");
  return $glint.flag_help(_pipe, description);
}

export function ext() {
  let description = "Use a file extension other than 'mjs' for the built JavaScript.";
  let _pipe = $glint.string_flag("ext");
  return $glint.flag_help(_pipe, description);
}

export function detect_tailwind() {
  let description = "Detect and build Tailwind styles automatically.";
  let _pipe = $glint.bool_flag("detect-tailwind");
  return $glint.flag_help(_pipe, description);
}

export function port() {
  let description = "Specify server port. If the port is taken the dev server will not start.";
  let _pipe = $glint.int_flag("port");
  return $glint.flag_help(_pipe, description);
}

export function proxy_from() {
  let description = "Proxy requests that start with this path to the URL specified by the --proxy-to flag.";
  let _pipe = $glint.string_flag("proxy-from");
  return $glint.flag_help(_pipe, description);
}

export function proxy_to() {
  let description = "Proxy requests that start with the path specified by the --proxy-from flag to this URL.";
  let _pipe = $glint.string_flag("proxy-to");
  return $glint.flag_help(_pipe, description);
}
