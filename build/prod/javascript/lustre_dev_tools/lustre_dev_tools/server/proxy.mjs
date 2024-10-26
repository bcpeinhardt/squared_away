import * as $filepath from "../../../filepath/filepath.mjs";
import * as $request from "../../../gleam_http/gleam/http/request.mjs";
import { Request } from "../../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../../gleam_http/gleam/http/response.mjs";
import * as $httpc from "../../../gleam_httpc/gleam/httpc.mjs";
import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $bytes_builder from "../../../gleam_stdlib/gleam/bytes_builder.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $uri from "../../../gleam_stdlib/gleam/uri.mjs";
import * as $glint from "../../../glint/glint.mjs";
import * as $mist from "../../../mist/mist.mjs";
import * as $tom from "../../../tom/tom.mjs";
import { Error, toList, CustomType as $CustomType, makeError, isEqual } from "../../gleam.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import { do$ } from "../../lustre_dev_tools/cli.mjs";
import * as $flag from "../../lustre_dev_tools/cli/flag.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import { IncompleteProxy, InvalidProxyTarget } from "../../lustre_dev_tools/error.mjs";

export class Proxy extends $CustomType {
  constructor(from, to) {
    super();
    this.from = from;
    this.to = to;
  }
}

function get_proxy_from() {
  return do$(
    $cli.get_flags(),
    (flags) => {
      return do$(
        $cli.get_config(),
        (config) => {
          let flag = $result.nil_error(
            $glint.get_flag(flags, $flag.proxy_from()),
          );
          let toml = $result.nil_error(
            $tom.get_string(
              config.toml,
              toList(["lustre-dev", "start", "proxy", "from"]),
            ),
          );
          let _pipe = $result.or(flag, toml);
          let _pipe$1 = $option.from_result(_pipe);
          return $cli.return$(_pipe$1);
        },
      );
    },
  );
}

function get_proxy_to() {
  return do$(
    $cli.get_flags(),
    (flags) => {
      return do$(
        $cli.get_config(),
        (config) => {
          let flag = $result.nil_error($glint.get_flag(flags, $flag.proxy_to()));
          let toml = $result.nil_error(
            $tom.get_string(
              config.toml,
              toList(["lustre-dev", "start", "proxy", "to"]),
            ),
          );
          let from = $result.or(flag, toml);
          return $bool.guard(
            isEqual(from, new Error(undefined)),
            $cli.return$(new None()),
            () => {
              if (!from.isOk()) {
                throw makeError(
                  "assignment_no_match",
                  "lustre_dev_tools/server/proxy",
                  94,
                  "",
                  "Assignment pattern did not match",
                  { value: from }
                )
              }
              let from$1 = from[0];
              let $ = $uri.parse(from$1);
              if ($.isOk()) {
                let from$2 = $[0];
                return $cli.return$(new Some(from$2));
              } else {
                return $cli.throw$(new InvalidProxyTarget(from$1));
              }
            },
          );
        },
      );
    },
  );
}

export function get() {
  return do$(
    get_proxy_from(),
    (from) => {
      return do$(
        get_proxy_to(),
        (to) => {
          if (from instanceof Some && to instanceof Some) {
            let from$1 = from[0];
            let to$1 = to[0];
            return $cli.return$(new Some(new Proxy(from$1, to$1)));
          } else if (from instanceof Some && to instanceof None) {
            return $cli.throw$(new IncompleteProxy(toList(["proxy-to"])));
          } else if (from instanceof None && to instanceof Some) {
            return $cli.throw$(new IncompleteProxy(toList(["proxy-from"])));
          } else {
            return $cli.return$(new None());
          }
        },
      );
    },
  );
}
