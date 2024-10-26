import * as $ansi from "../../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $erlang from "../../gleam_erlang/gleam/erlang.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $glint from "../../glint/glint.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import * as $spinner from "../../spinner/spinner.mjs";
import * as $tom from "../../tom/tom.mjs";
import { Ok, Error, toList, CustomType as $CustomType } from "../gleam.mjs";
import * as $error from "../lustre_dev_tools/error.mjs";
import { TemplateMissing } from "../lustre_dev_tools/error.mjs";
import * as $project from "../lustre_dev_tools/project.mjs";

class Cli extends $CustomType {
  constructor(run) {
    super();
    this.run = run;
  }
}

class Env extends $CustomType {
  constructor(muted, spinner, flags, config) {
    super();
    this.muted = muted;
    this.spinner = spinner;
    this.flags = flags;
    this.config = config;
  }
}

class Running extends $CustomType {
  constructor(spinner, message) {
    super();
    this.spinner = spinner;
    this.message = message;
  }
}

class Paused extends $CustomType {}

export function run(step, flags) {
  return $result.try$(
    $project.config(),
    (config) => {
      let env = new Env(false, new Paused(), flags, config);
      let $ = step.run(env);
      let env$1 = $[0];
      let result = $[1];
      let $1 = env$1.spinner;
      if ($1 instanceof Running) {
        let spinner = $1.spinner;
        $spinner.stop(spinner)
      } else {
        undefined
      }
      let $2 = env$1.spinner;
      if (!result.isOk() && $2 instanceof Running) {
        let message = $2.message;
        $io.println("❌ " + $ansi.red(message))
      } else if (!result.isOk()) {
        undefined
      } else {
        undefined
      }
      return result;
    },
  );
}

export function return$(value) {
  return new Cli((env) => { return [env, new Ok(value)]; });
}

export function throw$(error) {
  return new Cli((env) => { return [env, new Error(error)]; });
}

export function from_result(result) {
  return new Cli((env) => { return [env, result]; });
}

export function do$(step, next) {
  return new Cli(
    (env) => {
      let $ = step.run(env);
      let env$1 = $[0];
      let result = $[1];
      if (result.isOk()) {
        let value = result[0];
        return next(value).run(env$1);
      } else {
        let error = result[0];
        let $1 = env$1.spinner;
        if ($1 instanceof Running) {
          let spinner = $1.spinner;
          $spinner.stop(spinner)
        } else {
          undefined
        }
        return [env$1, new Error(error)];
      }
    },
  );
}

export function in$(value) {
  return new Cli((env) => { return [env, new Ok(value())]; });
}

export function map(step, next) {
  return new Cli(
    (env) => {
      let $ = step.run(env);
      let env$1 = $[0];
      let result = $[1];
      let result$1 = $result.map(result, next);
      return [env$1, result$1];
    },
  );
}

export function try$(result, next) {
  return new Cli(
    (env) => {
      if (result.isOk()) {
        let a = result[0];
        return next(a).run(env);
      } else {
        let error = result[0];
        let $ = env.spinner;
        if ($ instanceof Running) {
          let spinner = $.spinner;
          $spinner.stop(spinner)
        } else {
          undefined
        }
        return [env, new Error(error)];
      }
    },
  );
}

export function log(message, next) {
  return new Cli(
    (env) => {
      let env$1 = (() => {
        let $ = env.muted;
        if ($) {
          return env;
        } else {
          return env.withFields({
            spinner: (() => {
              let $1 = env.spinner;
              if ($1 instanceof Paused) {
                return new Running(
                  (() => {
                    let _pipe = $spinner.new$(message);
                    let _pipe$1 = $spinner.with_colour(_pipe, $ansi.magenta);
                    let _pipe$2 = $spinner.with_frames(
                      _pipe$1,
                      $spinner.snake_frames,
                    );
                    return $spinner.start(_pipe$2);
                  })(),
                  message,
                );
              } else {
                let spinner = $1.spinner;
                $spinner.set_text(spinner, message);
                return new Running(spinner, message);
              }
            })()
          });
        }
      })();
      return next().run(env$1);
    },
  );
}

export function success(message, next) {
  return new Cli(
    (env) => {
      let env$1 = env.withFields({
        spinner: (() => {
          let $ = env.spinner;
          if ($ instanceof Paused) {
            return new Paused();
          } else {
            let spinner = $.spinner;
            $spinner.stop(spinner);
            return new Paused();
          }
        })()
      });
      let $ = env$1.muted;
      if ($) {
        undefined
      } else {
        $io.println("✅ " + $ansi.green(message))
      }
      return next().run(env$1);
    },
  );
}

export function notify(message, next) {
  return new Cli(
    (env) => {
      let env$1 = env.withFields({
        spinner: (() => {
          let $ = env.spinner;
          if ($ instanceof Paused) {
            return new Paused();
          } else {
            let spinner = $.spinner;
            $spinner.stop(spinner);
            return new Paused();
          }
        })()
      });
      let $ = env$1.muted;
      if ($) {
        undefined
      } else {
        $io.println($ansi.bright_cyan(message))
      }
      return next().run(env$1);
    },
  );
}

export function mute() {
  return new Cli(
    (env) => { return [env.withFields({ muted: true }), new Ok(undefined)]; },
  );
}

export function unmute() {
  return new Cli(
    (env) => { return [env.withFields({ muted: false }), new Ok(undefined)]; },
  );
}

export function get_config() {
  return new Cli((env) => { return [env, new Ok(env.config)]; });
}

export function get_name() {
  return new Cli((env) => { return [env, new Ok(env.config.name)]; });
}

export function get_flags() {
  return new Cli((env) => { return [env, new Ok(env.flags)]; });
}

export function get_config_value(name, fallback, namespace, toml, flag) {
  return new Cli(
    (env) => {
      let toml_path = $list.concat(
        toList([toList(["lustre-dev"]), namespace, toList([name])]),
      );
      let value = (() => {
        let _pipe = $result.or(
          $result.nil_error(flag(env.flags)),
          $result.nil_error(toml(env.config.toml, toml_path)),
        );
        return $result.unwrap(_pipe, fallback);
      })();
      return [env, new Ok(value)];
    },
  );
}

export function get_int(name, fallback, namespace, flag) {
  return get_config_value(name, fallback, namespace, $tom.get_int, flag);
}

export function get_string(name, fallback, namespace, flag) {
  return get_config_value(name, fallback, namespace, $tom.get_string, flag);
}

export function get_bool(name, fallback, namespace, flag) {
  return get_config_value(name, fallback, namespace, $tom.get_bool, flag);
}
