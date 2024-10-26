import * as $envoy from "../envoy/envoy.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $platform from "../platform/platform.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import { Ok, Error, toList } from "./gleam.mjs";

function check_dirs(paths) {
  let _pipe = paths;
  let _pipe$1 = $list.filter(
    _pipe,
    (a) => {
      return !$string.is_empty(a) && $result.unwrap(
        $simplifile.is_directory(a),
        false,
      );
    },
  );
  return $list.first(_pipe$1);
}

function check_dir_from_env(vars) {
  let _pipe = vars;
  let _pipe$1 = $list.filter_map(
    _pipe,
    (_capture) => { return $envoy.get(_capture); },
  );
  return check_dirs(_pipe$1);
}

function get_env(var$) {
  return $result.unwrap($envoy.get(var$), "");
}

function home_dir_path(path) {
  return get_env("HOME") + path;
}

function other_os_message(other_os) {
  $io.print_error(
    ("[WARN][directories] Operating system '" + other_os) + "' is not supported by this library",
  );
  return new Error(undefined);
}

export function tmp_dir() {
  let $ = check_dir_from_env(toList(["TMPDIR", "TEMP", "TMP"]));
  if ($.isOk()) {
    let path = $[0];
    return new Ok(path);
  } else {
    let $1 = $platform.os();
    if ($1 instanceof $platform.Win32) {
      return check_dirs(toList(["C:\\TEMP", "C:\\TMP", "\\TEMP", "\\TMP"]));
    } else if ($1 instanceof $platform.Darwin) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.Linux) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.FreeBsd) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.OpenBsd) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.SunOs) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else if ($1 instanceof $platform.Aix) {
      return check_dirs(toList(["/tmp", "/var/tmp", "/usr/tmp"]));
    } else {
      let os = $1[0];
      return other_os_message(os);
    }
  }
}

export function home_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["UserProfile", "Profile"]));
  } else if ($ instanceof $platform.Darwin) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.Linux) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.SunOs) {
    return check_dir_from_env(toList(["HOME"]));
  } else if ($ instanceof $platform.Aix) {
    return check_dir_from_env(toList(["HOME"]));
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

export function cache_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["APPDATA"]));
  } else if ($ instanceof $platform.Darwin) {
    return check_dirs(toList([get_env("HOME") + "/Library/Caches"]));
  } else if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else if ($ instanceof $platform.SunOs) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else if ($ instanceof $platform.Aix) {
    return check_dirs(
      toList([get_env("XDG_CACHE_HOME"), home_dir_path("/.cache")]),
    );
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

export function config_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["APPDATA"]));
  } else if ($ instanceof $platform.Darwin) {
    return check_dirs(
      toList([get_env("HOME") + "/Library/Application Support"]),
    );
  } else if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else if ($ instanceof $platform.SunOs) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else if ($ instanceof $platform.Aix) {
    return check_dirs(
      toList([get_env("XDG_CONFIG_HOME"), home_dir_path("/.config")]),
    );
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

export function config_local_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["LOCALAPPDATA"]));
  } else {
    return config_dir();
  }
}

export function data_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([get_env("XDG_DATA_HOME"), home_dir_path("/.local/share")]),
    );
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([get_env("XDG_DATA_HOME"), home_dir_path("/.local/share")]),
    );
  } else {
    return config_dir();
  }
}

export function data_local_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return check_dir_from_env(toList(["LOCALAPPDATA"]));
  } else {
    return data_dir();
  }
}

export function executable_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return new Error(undefined);
  } else if ($ instanceof $platform.Darwin) {
    return new Error(undefined);
  } else if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else if ($ instanceof $platform.SunOs) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else if ($ instanceof $platform.Aix) {
    return check_dirs(
      toList([
        get_env("XDG_BIN_HOME"),
        home_dir_path("/.local/bin"),
        get_env("XDG_DATA_HOME") + "../bin",
      ]),
    );
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

export function preference_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Darwin) {
    return check_dirs(toList([home_dir_path("/Library/Preferences")]));
  } else {
    return config_dir();
  }
}

export function runtime_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return new Error(undefined);
  } else if ($ instanceof $platform.Darwin) {
    return new Error(undefined);
  } else if ($ instanceof $platform.Linux) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else if ($ instanceof $platform.SunOs) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else if ($ instanceof $platform.Aix) {
    return check_dir_from_env(toList(["XDG_RUNTIME_DIR"]));
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

export function state_dir() {
  let $ = $platform.os();
  if ($ instanceof $platform.Win32) {
    return new Error(undefined);
  } else if ($ instanceof $platform.Darwin) {
    return new Error(undefined);
  } else if ($ instanceof $platform.Linux) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else if ($ instanceof $platform.FreeBsd) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else if ($ instanceof $platform.OpenBsd) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else if ($ instanceof $platform.SunOs) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else if ($ instanceof $platform.Aix) {
    return check_dirs(
      toList([get_env("XDG_STATE_HOME"), home_dir_path("/.local/state")]),
    );
  } else {
    let os = $[0];
    return other_os_message(os);
  }
}

export function main() {
  $io.print("Current Platform: ");
  let $ = $io.debug($platform.os());
  
  $io.println("===");
  $io.print("Temp Directory: ");
  let $1 = $io.debug(tmp_dir());
  
  $io.print("Home Directory: ");
  let $2 = $io.debug(home_dir());
  
  $io.print("Cache Directory: ");
  let $3 = $io.debug(cache_dir());
  
  $io.print("Config Directory: ");
  let $4 = $io.debug(config_dir());
  
  $io.print("Config Directory (Local): ");
  let $5 = $io.debug(config_local_dir());
  
  $io.print("Data Directory: ");
  let $6 = $io.debug(data_dir());
  
  $io.print("Data Directory (Local): ");
  let $7 = $io.debug(data_local_dir());
  
  $io.print("Executables Directory: ");
  let $8 = $io.debug(executable_dir());
  
  $io.print("Preferences Directory: ");
  let $9 = $io.debug(preference_dir());
  
  $io.print("Runtime Directory: ");
  let $10 = $io.debug(runtime_dir());
  
  $io.print("State Directory: ");
  let $11 = $io.debug(state_dir());
  
  return $11;
}
