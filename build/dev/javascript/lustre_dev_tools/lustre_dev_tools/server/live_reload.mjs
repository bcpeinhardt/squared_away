import * as $filepath from "../../../filepath/filepath.mjs";
import * as $ansi from "../../../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $erlang from "../../../gleam_erlang/gleam/erlang.mjs";
import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $request from "../../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../../gleam_http/gleam/http/response.mjs";
import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $set from "../../../gleam_stdlib/gleam/set.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $glint from "../../../glint/glint.mjs";
import * as $mist from "../../../mist/mist.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import { Ok, Error, toList, CustomType as $CustomType, isEqual } from "../../gleam.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import * as $build from "../../lustre_dev_tools/cli/build.mjs";
import * as $flag from "../../lustre_dev_tools/cli/flag.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import { CannotStartFileWatcher } from "../../lustre_dev_tools/error.mjs";

export class Add extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Remove extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Broadcast extends $CustomType {}

export class Unknown extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Reload extends $CustomType {}

export class ShowError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class NoFileWatcherSupportedForOs extends $CustomType {}

class NoFileWatcherInstalled extends $CustomType {
  constructor(watcher) {
    super();
    this.watcher = watcher;
  }
}

class Created extends $CustomType {}

class Modified extends $CustomType {}

class Deleted extends $CustomType {}

function is_interesting_event(event) {
  return ((isEqual(event, $dynamic.from(new Created()))) || (isEqual(
    event,
    $dynamic.from(new Modified())
  ))) || (isEqual(event, $dynamic.from(new Deleted())));
}

function change_decoder(dyn) {
  let events_decoder = $dynamic.element(1, $dynamic.list($dynamic.dynamic));
  return $result.try$(
    $dynamic.element(2, events_decoder)(dyn),
    (events) => {
      let $ = $list.any(events, is_interesting_event);
      if ($) {
        return new Ok(new Broadcast());
      } else {
        return new Error(toList([]));
      }
    },
  );
}
