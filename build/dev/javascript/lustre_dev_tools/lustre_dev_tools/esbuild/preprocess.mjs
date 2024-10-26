import * as $filepath from "../../../filepath/filepath.mjs";
import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $regex from "../../../gleam_stdlib/gleam/regex.mjs";
import { Match, Options } from "../../../gleam_stdlib/gleam/regex.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import { Ok, makeError } from "../../gleam.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import { CannotCreateDirectory, CannotReadFile, CannotWriteFile } from "../../lustre_dev_tools/error.mjs";
import * as $project from "../../lustre_dev_tools/project.mjs";

function can_resolve_relative_gleam_imports(path) {
  let $ = $filepath.extension(path);
  if ($.isOk() && $[0] === "js") {
    return true;
  } else if ($.isOk() && $[0] === "mjs") {
    return true;
  } else if ($.isOk() && $[0] === "ts") {
    return true;
  } else if ($.isOk() && $[0] === "mts") {
    return true;
  } else {
    return false;
  }
}

function resolve_relative_gleam_imports(path, source) {
  return $bool.guard(
    !can_resolve_relative_gleam_imports(path),
    source,
    () => {
      let options = new Options(false, true);
      let $ = $regex.compile("^import.+\"(\\..+)\";$", options);
      if (!$.isOk()) {
        throw makeError(
          "assignment_no_match",
          "lustre_dev_tools/esbuild/preprocess",
          65,
          "",
          "Assignment pattern did not match",
          { value: $ }
        )
      }
      let re = $[0];
      return $list.fold(
        $regex.scan(re, source),
        source,
        (source, match) => {
          if (
            !(match instanceof Match) ||
            !match.submatches.hasLength(1) ||
            !(match.submatches.head instanceof Some)
          ) {
            throw makeError(
              "assignment_no_match",
              "lustre_dev_tools/esbuild/preprocess",
              68,
              "",
              "Assignment pattern did not match",
              { value: match }
            )
          }
          let match$1 = match.content;
          let import_path = match.submatches.head[0];
          let resolved_import_path = $string.replace(
            import_path,
            ".gleam",
            ".mjs",
          );
          let resolved_import = $string.replace(
            match$1,
            import_path,
            resolved_import_path,
          );
          return $string.replace(source, match$1, resolved_import);
        },
      );
    },
  );
}

export function copy_deep_ffi() {
  return $cli.try$(
    $project.config(),
    (config) => {
      let root = $project.root();
      let src = $filepath.join(root, "src");
      let out = $filepath.join(root, "build/dev/javascript/" + config.name);
      let $ = $simplifile.get_files(src);
      if (!$.isOk()) {
        throw makeError(
          "assignment_no_match",
          "lustre_dev_tools/esbuild/preprocess",
          25,
          "",
          "Assignment pattern did not match",
          { value: $ }
        )
      }
      let files = $[0];
      return $cli.from_result(
        $list.try_each(
          files,
          (path) => {
            return $bool.guard(
              $string.ends_with(path, ".gleam"),
              new Ok(undefined),
              () => {
                return $result.try$(
                  (() => {
                    let _pipe = $simplifile.read(path);
                    return $result.map_error(
                      _pipe,
                      (_capture) => {
                        return new CannotReadFile(_capture, path);
                      },
                    );
                  })(),
                  (source) => {
                    if (!path.startsWith("./src/")) {
                      throw makeError(
                        "assignment_no_match",
                        "lustre_dev_tools/esbuild/preprocess",
                        35,
                        "",
                        "Assignment pattern did not match",
                        { value: path }
                      )
                    }
                    let module_path = path.slice(6);
                    let out_path = $filepath.join(out, module_path);
                    let out_dir = $filepath.directory_name(out_path);
                    let source$1 = resolve_relative_gleam_imports(path, source);
                    return $result.try$(
                      (() => {
                        let _pipe = $simplifile.create_directory_all(out_dir);
                        return $result.map_error(
                          _pipe,
                          (_capture) => {
                            return new CannotCreateDirectory(_capture, out_dir);
                          },
                        );
                      })(),
                      (_) => {
                        return $result.try$(
                          (() => {
                            let _pipe = $simplifile.write(out_path, source$1);
                            return $result.map_error(
                              _pipe,
                              (_capture) => {
                                return new CannotWriteFile(_capture, out_path);
                              },
                            );
                          })(),
                          (_) => { return new Ok(undefined); },
                        );
                      },
                    );
                  },
                );
              },
            );
          },
        ),
      );
    },
  );
}
