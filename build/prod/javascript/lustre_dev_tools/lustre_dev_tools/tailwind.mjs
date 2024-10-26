import * as $filepath from "../../filepath/filepath.mjs";
import * as $ansi from "../../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $crypto from "../../gleam_crypto/gleam/crypto.mjs";
import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import { Execute, FilePermissions, Read, Write } from "../../simplifile/simplifile.mjs";
import { Ok, Error, toList } from "../gleam.mjs";
import * as $cli from "../lustre_dev_tools/cli.mjs";
import * as $error from "../lustre_dev_tools/error.mjs";
import { CannotSetPermissions, CannotWriteFile, NetworkError, UnknownPlatform } from "../lustre_dev_tools/error.mjs";
import * as $project from "../lustre_dev_tools/project.mjs";
import * as $utils from "../lustre_dev_tools/utils.mjs";

function display_next_steps() {
  return $cli.notify(
    $ansi.bold("\nNext Steps:\n"),
    () => {
      return $cli.notify(
        "1. Be sure to update your root `index.html` file to include \n   `<link rel='stylesheet' type='text/css' href='./priv/static/your_app.css' />`",
        () => { return $cli.return$(undefined); },
      );
    },
  );
}

function check_tailwind_exists(path) {
  let $ = $simplifile.is_file(path);
  if ($.isOk() && $[0]) {
    return true;
  } else if ($.isOk() && !$[0]) {
    return false;
  } else {
    return false;
  }
}

function get_download_url_and_hash(os, cpu) {
  let base = "https://github.com/tailwindlabs/tailwindcss/releases/download/v3.4.1/tailwindcss-";
  if (os === "linux" && cpu === "armv7") {
    return new Ok(
      [
        base + "linux-armv7",
        "38E004B144004495CD148621ADB852C21D5D350E66308C8FF9E2FD90A15726F5",
      ],
    );
  } else if (os === "linux" && cpu === "arm64") {
    return new Ok(
      [
        base + "linux-arm64",
        "1178C3E8B44B9EB43F40E786EE25664C93D83F6D05B062C0D9CAF410D64D5587",
      ],
    );
  } else if (os === "linux" && cpu === "aarch64") {
    return new Ok(
      [
        base + "linux-arm64",
        "1178C3E8B44B9EB43F40E786EE25664C93D83F6D05B062C0D9CAF410D64D5587",
      ],
    );
  } else if (os === "linux" && cpu === "x64") {
    return new Ok(
      [
        base + "linux-x64",
        "A6814CC8FB6E573DD637352093F3B8E927C5C8628B1FF87826652935AF1430B1",
      ],
    );
  } else if (os === "linux" && cpu === "x86_64") {
    return new Ok(
      [
        base + "linux-x64",
        "A6814CC8FB6E573DD637352093F3B8E927C5C8628B1FF87826652935AF1430B1",
      ],
    );
  } else if (os === "win32" && cpu === "arm64") {
    return new Ok(
      [
        base + "windows-arm64.exe",
        "AC06BC274FAED7A0C9C0C7E9058D87A428B574BCF8FCE85330F576DE4568BB81",
      ],
    );
  } else if (os === "win32" && cpu === "x64") {
    return new Ok(
      [
        base + "windows-x64.exe",
        "EF2FE367DAA8204CB186796C1833FAEE81A5B20E4C80E533F7A3B3DCC7DB6C54",
      ],
    );
  } else if (os === "win32" && cpu === "x86_64") {
    return new Ok(
      [
        base + "windows-x64.exe",
        "EF2FE367DAA8204CB186796C1833FAEE81A5B20E4C80E533F7A3B3DCC7DB6C54",
      ],
    );
  } else if (os === "darwin" && cpu === "arm64") {
    return new Ok(
      [
        base + "macos-arm64",
        "40738E59ECEF06F955243154E7D1C6EAF11370037CBEEE4A32C3138387E2DA5D",
      ],
    );
  } else if (os === "darwin" && cpu === "aarch64") {
    return new Ok(
      [
        base + "macos-arm64",
        "40738E59ECEF06F955243154E7D1C6EAF11370037CBEEE4A32C3138387E2DA5D",
      ],
    );
  } else if (os === "darwin" && cpu === "x64") {
    return new Ok(
      [
        base + "macos-x64",
        "594D01B032125199DB105C661FE23DE4C069006921B96F7FEE98EE4FBC15F800",
      ],
    );
  } else if (os === "darwin" && cpu === "x86_64") {
    return new Ok(
      [
        base + "macos-x64",
        "594D01B032125199DB105C661FE23DE4C069006921B96F7FEE98EE4FBC15F800",
      ],
    );
  } else {
    return new Error(new UnknownPlatform("tailwind", os, cpu));
  }
}

function check_tailwind_integrity(bin, expected_hash) {
  let hash = $crypto.hash(new $crypto.Sha256(), bin);
  let hash_string = $bit_array.base16_encode(hash);
  let $ = hash_string === expected_hash;
  if ($) {
    return new Ok(undefined);
  } else {
    return new Error(new $error.InvalidTailwindBinary());
  }
}

function write_tailwind(bin, outdir, outfile) {
  let $ = $simplifile.create_directory_all(outdir);
  
  let _pipe = $simplifile.write_bits(outfile, bin);
  return $result.map_error(
    _pipe,
    (_capture) => {
      return new CannotWriteFile(_capture, $filepath.join(outdir, outfile));
    },
  );
}

function set_file_permissions(file) {
  let permissions = new FilePermissions(
    $set.from_list(toList([new Read(), new Write(), new Execute()])),
    $set.from_list(toList([new Read(), new Execute()])),
    $set.from_list(toList([new Read(), new Execute()])),
  );
  let _pipe = $simplifile.set_permissions(file, permissions);
  return $result.map_error(
    _pipe,
    (_capture) => { return new CannotSetPermissions(_capture, file); },
  );
}

function write_config(path, content) {
  let _pipe = $simplifile.write(path, content);
  return $result.map_error(
    _pipe,
    (_capture) => { return new CannotWriteFile(_capture, path); },
  );
}
