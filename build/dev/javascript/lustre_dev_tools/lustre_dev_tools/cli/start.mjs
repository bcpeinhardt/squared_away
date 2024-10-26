import * as $filepath from "../../../filepath/filepath.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $glint from "../../../glint/glint.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import { do$, try$ } from "../../lustre_dev_tools/cli.mjs";
import * as $build from "../../lustre_dev_tools/cli/build.mjs";
import * as $flag from "../../lustre_dev_tools/cli/flag.mjs";
import * as $cmd from "../../lustre_dev_tools/cmd.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import { CannotWriteFile } from "../../lustre_dev_tools/error.mjs";
import * as $project from "../../lustre_dev_tools/project.mjs";
import * as $server from "../../lustre_dev_tools/server.mjs";

function write_html(path, source) {
  let _pipe = $simplifile.write(path, source);
  return $result.map_error(
    _pipe,
    (_capture) => { return new CannotWriteFile(_capture, path); },
  );
}
