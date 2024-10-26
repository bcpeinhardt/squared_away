import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $glint from "../../../glint/glint.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import { do$ } from "../../lustre_dev_tools/cli.mjs";
import * as $flag from "../../lustre_dev_tools/cli/flag.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import * as $esbuild from "../../lustre_dev_tools/esbuild.mjs";
import * as $tailwind from "../../lustre_dev_tools/tailwind.mjs";

export const description = "\nCommands for adding external binaries to your project. These are run and managed\nby Lustre, and while not typically intended to be run manually, they can be found\ninside `build/.lustre/bin`.\n  ";
