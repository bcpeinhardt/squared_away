import * as $filepath from "../../filepath/filepath.mjs";
import * as $crypto from "../../gleam_crypto/gleam/crypto.mjs";
import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import { Execute, FilePermissions, Read, Write } from "../../simplifile/simplifile.mjs";
import { Ok, Error, toList } from "../gleam.mjs";
import * as $cli from "../lustre_dev_tools/cli.mjs";
import * as $cmd from "../lustre_dev_tools/cmd.mjs";
import * as $error from "../lustre_dev_tools/error.mjs";
import {
  BundleError,
  CannotSetPermissions,
  CannotWriteFile,
  NetworkError,
  UnknownPlatform,
  UnzipError,
} from "../lustre_dev_tools/error.mjs";
import * as $preprocess from "../lustre_dev_tools/esbuild/preprocess.mjs";
import * as $project from "../lustre_dev_tools/project.mjs";
import * as $utils from "../lustre_dev_tools/utils.mjs";

function check_esbuild_exists(path) {
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
  let base = "https://registry.npmjs.org/@esbuild/";
  if (os === "android" && cpu === "arm") {
    return new Ok(
      [
        base + "android-arm/-/android-arm-0.19.10.tgz",
        "545CF157B0E42E407AC1412F73876119414314D9E31982EBD1E9073336DA5365",
      ],
    );
  } else if (os === "android" && cpu === "arm64") {
    return new Ok(
      [
        base + "android-arm64/-/android-arm64-0.19.10.tgz",
        "DFB0A873B1BB9698EF42561B9513FC4A7D8392CE84FBD44FC276883B82AB087E",
      ],
    );
  } else if (os === "android" && cpu === "x64") {
    return new Ok(
      [
        base + "android-x64/-/android-x64-0.19.10.tgz",
        "B5D9D170F469BE483F3E16DA6033DFD064ED3DF788C6DC238BA6FE3232BF5653",
      ],
    );
  } else if (os === "darwin" && cpu === "aarch64") {
    return new Ok(
      [
        base + "darwin-arm64/-/darwin-arm64-0.19.10.tgz",
        "8CEC451CBD47E228D2D0B740B62870471C22813348398061C06D829BC6610EA9",
      ],
    );
  } else if (os === "darwin" && cpu === "amd64") {
    return new Ok(
      [
        base + "darwin-arm64/-/darwin-arm64-0.19.10.tgz",
        "8CEC451CBD47E228D2D0B740B62870471C22813348398061C06D829BC6610EA9",
      ],
    );
  } else if (os === "darwin" && cpu === "arm64") {
    return new Ok(
      [
        base + "darwin-arm64/-/darwin-arm64-0.19.10.tgz",
        "8CEC451CBD47E228D2D0B740B62870471C22813348398061C06D829BC6610EA9",
      ],
    );
  } else if (os === "darwin" && cpu === "x86_64") {
    return new Ok(
      [
        base + "darwin-x64/-/darwin-x64-0.19.10.tgz",
        "4AEC252E72CD56FD31341D960D8DCF0BEBFB858587061FE36B9047FD591C68A3",
      ],
    );
  } else if (os === "freebsd" && cpu === "aarch64") {
    return new Ok(
      [
        base + "freebsd-arm64/-/freebsd-arm64-0.19.10.tgz",
        "739E9F6DD3121DB0CD03B70B14C4D17A1854970272B0F988BD035AA876BE254B",
      ],
    );
  } else if (os === "freebsd" && cpu === "amd64") {
    return new Ok(
      [
        base + "freebsd-x64/-/freebsd-x64-0.19.10.tgz",
        "3E937C849B21B89244A8A62B473E36EEFE793F5BDF602BEDBB314DD33DDBE7EE",
      ],
    );
  } else if (os === "linux" && cpu === "aarch64") {
    return new Ok(
      [
        base + "linux-arm64/-/linux-arm64-0.19.10.tgz",
        "D3523B8F7B2540BA5A15C4EE4C747B31DFDC496C7A8A3F3FB0ECCB3008647DB7",
      ],
    );
  } else if (os === "linux" && cpu === "arm") {
    return new Ok(
      [
        base + "linux-arm/-/linux-arm-0.19.10.tgz",
        "99EEB37F5C1AB8750D9CAB6AB04469EF5CA32847B25E1461215276920AFB01B2",
      ],
    );
  } else if (os === "linux" && cpu === "arm64") {
    return new Ok(
      [
        base + "linux-arm64/-/linux-arm64-0.19.10.tgz",
        "D3523B8F7B2540BA5A15C4EE4C747B31DFDC496C7A8A3F3FB0ECCB3008647DB7",
      ],
    );
  } else if (os === "linux" && cpu === "ia32") {
    return new Ok(
      [
        base + "linux-ia32/-/linux-ia32-0.19.10.tgz",
        "3D69F7B90C62E6D94140355A92EDB15B8BFB934096C6E518BE41DAD6249BF38E",
      ],
    );
  } else if (os === "linux" && cpu === "x64") {
    return new Ok(
      [
        base + "linux-x64/-/linux-x64-0.19.10.tgz",
        "73CA82A3C9049315027E60A50AF53C2ABFDE678BF66562B407FACA7FD3FAD6F4",
      ],
    );
  } else if (os === "linux" && cpu === "x86_64") {
    return new Ok(
      [
        base + "linux-x64/-/linux-x64-0.19.10.tgz",
        "73CA82A3C9049315027E60A50AF53C2ABFDE678BF66562B407FACA7FD3FAD6F4",
      ],
    );
  } else if (os === "win32" && cpu === "arm64") {
    return new Ok(
      [
        base + "win32-arm64/-/win32-arm64-0.19.10.tgz",
        "2D0EC6ED7C5BA6F2D99CBB1428C1FAABFA7D42E7435BC40474C5787DCD1FF37C",
      ],
    );
  } else if (os === "win32" && cpu === "ia32") {
    return new Ok(
      [
        base + "win32-ia32/-/win32-ia32-0.19.10.tgz",
        "5BFBF08A8EDC16D53FE2103C68705DC3B4ABDFA6C44919B9602495ABA523BA46",
      ],
    );
  } else if (os === "win32" && cpu === "x64") {
    return new Ok(
      [
        base + "win32-x64/-/win32-x64-0.19.10.tgz",
        "03EFF9A74ED7C72C8E4ACE85F6BFD2D097169D8D6E7D691AE1D7959F2912B785",
      ],
    );
  } else if (os === "win32" && cpu === "x86_64") {
    return new Ok(
      [
        base + "win32-x64/-/win32-x64-0.19.10.tgz",
        "03EFF9A74ED7C72C8E4ACE85F6BFD2D097169D8D6E7D691AE1D7959F2912B785",
      ],
    );
  } else if (os === "netbsd" && cpu === "x64") {
    return new Ok(
      [
        base + "netbsd-x64/-/netbsd-x64-0.19.10.tgz",
        "C8F6E2CB79B1DDC2AD42C0AE25FB2A769A989E36B917B231CF9847B683D6DD8D",
      ],
    );
  } else if (os === "openbsd" && cpu === "x64") {
    return new Ok(
      [
        base + "openbsd-x64/-/openbsd-x64-0.19.10.tgz",
        "AFEBEAD35BB5A1B921C126E70E0D76CF04DB64FA53C60E0779816CFA9E1F9A11",
      ],
    );
  } else if (os === "sunos" && cpu === "x64") {
    return new Ok(
      [
        base + "sunos-x64/-/sunos-x64-0.19.10.tgz",
        "B1E9F969433574BD43A293FA3A3C71C88B8C4CF841957DAAA2CF83A90ADAAB7E",
      ],
    );
  } else {
    return new Error(new UnknownPlatform("esbuild", os, cpu));
  }
}

function check_esbuild_integrity(bin, expected_hash) {
  let hash = $crypto.hash(new $crypto.Sha256(), bin);
  let hash_string = $bit_array.base16_encode(hash);
  let $ = hash_string === expected_hash;
  if ($) {
    return new Ok(undefined);
  } else {
    return new Error(new $error.InvalidEsbuildBinary());
  }
}

function write_esbuild(bin, outdir, outfile) {
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
