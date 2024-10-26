import * as $filepath from "../filepath/filepath.mjs";
import * as $bit_array from "../gleam_stdlib/gleam/bit_array.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $set from "../gleam_stdlib/gleam/set.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  toBitArray,
} from "./gleam.mjs";
import {
  fileInfo as file_info,
  delete_ as delete$,
  readBits as read_bits,
  writeBits as write_bits,
  appendBits as append_bits,
  isDirectory as is_directory,
  createDirectory as create_directory,
  createSymlink as create_symlink,
  readDirectory as read_directory,
  isFile as is_file,
  isSymlink as is_symlink,
  createDirAll as do_create_dir_all,
  copyFile as do_copy_file,
  renameFile as rename_file,
  setPermissionsOctal as set_permissions_octal,
  currentDirectory as current_directory,
} from "./simplifile_js.mjs";

export {
  append_bits,
  create_directory,
  create_symlink,
  current_directory,
  delete$,
  file_info,
  is_directory,
  is_file,
  is_symlink,
  read_bits,
  read_directory,
  rename_file,
  set_permissions_octal,
  write_bits,
};

export class Eacces extends $CustomType {}

export class Eagain extends $CustomType {}

export class Ebadf extends $CustomType {}

export class Ebadmsg extends $CustomType {}

export class Ebusy extends $CustomType {}

export class Edeadlk extends $CustomType {}

export class Edeadlock extends $CustomType {}

export class Edquot extends $CustomType {}

export class Eexist extends $CustomType {}

export class Efault extends $CustomType {}

export class Efbig extends $CustomType {}

export class Eftype extends $CustomType {}

export class Eintr extends $CustomType {}

export class Einval extends $CustomType {}

export class Eio extends $CustomType {}

export class Eisdir extends $CustomType {}

export class Eloop extends $CustomType {}

export class Emfile extends $CustomType {}

export class Emlink extends $CustomType {}

export class Emultihop extends $CustomType {}

export class Enametoolong extends $CustomType {}

export class Enfile extends $CustomType {}

export class Enobufs extends $CustomType {}

export class Enodev extends $CustomType {}

export class Enolck extends $CustomType {}

export class Enolink extends $CustomType {}

export class Enoent extends $CustomType {}

export class Enomem extends $CustomType {}

export class Enospc extends $CustomType {}

export class Enosr extends $CustomType {}

export class Enostr extends $CustomType {}

export class Enosys extends $CustomType {}

export class Enotblk extends $CustomType {}

export class Enotdir extends $CustomType {}

export class Enotsup extends $CustomType {}

export class Enxio extends $CustomType {}

export class Eopnotsupp extends $CustomType {}

export class Eoverflow extends $CustomType {}

export class Eperm extends $CustomType {}

export class Epipe extends $CustomType {}

export class Erange extends $CustomType {}

export class Erofs extends $CustomType {}

export class Espipe extends $CustomType {}

export class Esrch extends $CustomType {}

export class Estale extends $CustomType {}

export class Etxtbsy extends $CustomType {}

export class Exdev extends $CustomType {}

export class NotUtf8 extends $CustomType {}

export class Unknown extends $CustomType {
  constructor(inner) {
    super();
    this.inner = inner;
  }
}

export class FileInfo extends $CustomType {
  constructor(size, mode, nlinks, inode, user_id, group_id, dev, atime_seconds, mtime_seconds, ctime_seconds) {
    super();
    this.size = size;
    this.mode = mode;
    this.nlinks = nlinks;
    this.inode = inode;
    this.user_id = user_id;
    this.group_id = group_id;
    this.dev = dev;
    this.atime_seconds = atime_seconds;
    this.mtime_seconds = mtime_seconds;
    this.ctime_seconds = ctime_seconds;
  }
}

export class Read extends $CustomType {}

export class Write extends $CustomType {}

export class Execute extends $CustomType {}

export class FilePermissions extends $CustomType {
  constructor(user, group, other) {
    super();
    this.user = user;
    this.group = group;
    this.other = other;
  }
}

export function describe_error(error) {
  if (error instanceof Eperm) {
    return "Operation not permitted";
  } else if (error instanceof Enoent) {
    return "No such file or directory";
  } else if (error instanceof Esrch) {
    return "No such process";
  } else if (error instanceof Eintr) {
    return "Interrupted system call";
  } else if (error instanceof Eio) {
    return "Input/output error";
  } else if (error instanceof Enxio) {
    return "Device not configured";
  } else if (error instanceof Ebadf) {
    return "Bad file descriptor";
  } else if (error instanceof Edeadlk) {
    return "Resource deadlock avoided";
  } else if (error instanceof Edeadlock) {
    return "Resource deadlock avoided";
  } else if (error instanceof Enomem) {
    return "Cannot allocate memory";
  } else if (error instanceof Eacces) {
    return "Permission denied";
  } else if (error instanceof Efault) {
    return "Bad address";
  } else if (error instanceof Enotblk) {
    return "Block device required";
  } else if (error instanceof Ebusy) {
    return "Resource busy";
  } else if (error instanceof Eexist) {
    return "File exists";
  } else if (error instanceof Exdev) {
    return "Cross-device link";
  } else if (error instanceof Enodev) {
    return "Operation not supported by device";
  } else if (error instanceof Enotdir) {
    return "Not a directory";
  } else if (error instanceof Eisdir) {
    return "Is a directory";
  } else if (error instanceof Einval) {
    return "Invalid argument";
  } else if (error instanceof Enfile) {
    return "Too many open files in system";
  } else if (error instanceof Emfile) {
    return "Too many open files";
  } else if (error instanceof Etxtbsy) {
    return "Text file busy";
  } else if (error instanceof Efbig) {
    return "File too large";
  } else if (error instanceof Enospc) {
    return "No space left on device";
  } else if (error instanceof Espipe) {
    return "Illegal seek";
  } else if (error instanceof Erofs) {
    return "Read-only file system";
  } else if (error instanceof Emlink) {
    return "Too many links";
  } else if (error instanceof Epipe) {
    return "Broken pipe";
  } else if (error instanceof Erange) {
    return "Result too large";
  } else if (error instanceof Eagain) {
    return "Resource temporarily unavailable";
  } else if (error instanceof Enotsup) {
    return "Operation not supported";
  } else if (error instanceof Enobufs) {
    return "No buffer space available";
  } else if (error instanceof Eloop) {
    return "Too many levels of symbolic links";
  } else if (error instanceof Enametoolong) {
    return "File name too long";
  } else if (error instanceof Edquot) {
    return "Disc quota exceeded";
  } else if (error instanceof Estale) {
    return "Stale NFS file handle";
  } else if (error instanceof Enolck) {
    return "No locks available";
  } else if (error instanceof Enosys) {
    return "Function not implemented";
  } else if (error instanceof Eftype) {
    return "Inappropriate file type or format";
  } else if (error instanceof Eoverflow) {
    return "Value too large to be stored in data type";
  } else if (error instanceof Ebadmsg) {
    return "Bad message";
  } else if (error instanceof Emultihop) {
    return "Multihop attempted";
  } else if (error instanceof Enolink) {
    return "Link has been severed";
  } else if (error instanceof Enosr) {
    return "No STREAM resources";
  } else if (error instanceof Enostr) {
    return "Not a STREAM";
  } else if (error instanceof Eopnotsupp) {
    return "Operation not supported on socket";
  } else if (error instanceof NotUtf8) {
    return "File not UTF-8 encoded";
  } else {
    let inner = error.inner;
    return "Unknown error: " + inner;
  }
}

export function delete_all(loop$paths) {
  while (true) {
    let paths = loop$paths;
    if (paths.hasLength(0)) {
      return new Ok(undefined);
    } else {
      let path = paths.head;
      let rest = paths.tail;
      let $ = delete$(path);
      if ($.isOk() && !$[0]) {
        loop$paths = rest;
      } else if (!$.isOk() && $[0] instanceof Enoent) {
        loop$paths = rest;
      } else {
        let e = $;
        return e;
      }
    }
  }
}

export function read(filepath) {
  let $ = read_bits(filepath);
  if ($.isOk()) {
    let bits = $[0];
    let $1 = $bit_array.to_string(bits);
    if ($1.isOk()) {
      let str = $1[0];
      return new Ok(str);
    } else {
      return new Error(new NotUtf8());
    }
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function write(filepath, contents) {
  let _pipe = contents;
  let _pipe$1 = $bit_array.from_string(_pipe);
  return write_bits(filepath, _pipe$1);
}

export function append(filepath, contents) {
  let _pipe = contents;
  let _pipe$1 = $bit_array.from_string(_pipe);
  return append_bits(filepath, _pipe$1);
}

export function create_file(filepath) {
  let $ = (() => {
    let _pipe = filepath;
    return is_file(_pipe);
  })();
  let $1 = (() => {
    let _pipe = filepath;
    return is_directory(_pipe);
  })();
  if ($.isOk() && $[0]) {
    return new Error(new Eexist());
  } else if ($1.isOk() && $1[0]) {
    return new Error(new Eexist());
  } else {
    return write_bits(filepath, toBitArray([]));
  }
}

export function create_directory_all(dirpath) {
  let is_abs = $filepath.is_absolute(dirpath);
  let path = (() => {
    let _pipe = dirpath;
    let _pipe$1 = $filepath.split(_pipe);
    return $list.fold(_pipe$1, "", $filepath.join);
  })();
  let path$1 = (() => {
    if (is_abs) {
      return "/" + path;
    } else {
      return path;
    }
  })();
  return do_create_dir_all(path$1 + "/");
}

export function copy_file(src, dest) {
  let _pipe = do_copy_file(src, dest);
  return $result.replace(_pipe, undefined);
}

function do_copy_directory(src, dest) {
  return $result.try$(
    read_directory(src),
    (segments) => {
      let _pipe = segments;
      $list.each(
        _pipe,
        (segment) => {
          let src_path = $filepath.join(src, segment);
          let dest_path = $filepath.join(dest, segment);
          let $ = is_file(src_path);
          let $1 = is_directory(src_path);
          if ($.isOk() && $[0] && $1.isOk() && !$1[0]) {
            return $result.try$(
              read_bits(src_path),
              (content) => {
                let _pipe$1 = content;
                return write_bits(dest_path, _pipe$1);
              },
            );
          } else if ($.isOk() && !$[0] && $1.isOk() && $1[0]) {
            return $result.try$(
              create_directory(dest_path),
              (_) => { return do_copy_directory(src_path, dest_path); },
            );
          } else if (!$.isOk()) {
            let e = $[0];
            return new Error(e);
          } else if (!$1.isOk()) {
            let e = $1[0];
            return new Error(e);
          } else if ($.isOk() && !$[0] && $1.isOk() && !$1[0]) {
            return new Error(new Unknown("Unknown error copying directory"));
          } else {
            return new Error(new Unknown("Unknown error copying directory"));
          }
        },
      )
      return new Ok(undefined);
    },
  );
}

export function copy_directory(src, dest) {
  return $result.try$(
    create_directory_all(dest),
    (_) => { return do_copy_directory(src, dest); },
  );
}

export function rename_directory(src, dest) {
  return $result.try$(
    copy_directory(src, dest),
    (_) => { return delete$(src); },
  );
}

export function clear_directory(path) {
  return $result.try$(
    read_directory(path),
    (paths) => {
      let _pipe = paths;
      let _pipe$1 = $list.map(
        _pipe,
        (_capture) => { return $filepath.join(path, _capture); },
      );
      return delete_all(_pipe$1);
    },
  );
}

export function get_files(directory) {
  return $result.try$(
    read_directory(directory),
    (contents) => {
      return $list.try_fold(
        contents,
        toList([]),
        (acc, content) => {
          let path = $filepath.join(directory, content);
          let $ = is_file(path);
          if (!$.isOk()) {
            let e = $[0];
            return new Error(e);
          } else if ($.isOk() && $[0]) {
            return new Ok(listPrepend(path, acc));
          } else {
            let $1 = is_directory(path);
            if (!$1.isOk()) {
              let e = $1[0];
              return new Error(e);
            } else if ($1.isOk() && !$1[0]) {
              return new Ok(acc);
            } else {
              return $result.try$(
                get_files(path),
                (nested_files) => {
                  return new Ok($list.append(acc, nested_files));
                },
              );
            }
          }
        },
      );
    },
  );
}

function permission_to_integer(permission) {
  if (permission instanceof Read) {
    return 0o4;
  } else if (permission instanceof Write) {
    return 0o2;
  } else {
    return 0o1;
  }
}

export function file_permissions_to_octal(permissions) {
  let make_permission_digit = (permissions) => {
    let _pipe = permissions;
    let _pipe$1 = $set.to_list(_pipe);
    let _pipe$2 = $list.map(_pipe$1, permission_to_integer);
    return $int.sum(_pipe$2);
  };
  return (make_permission_digit(permissions.user) * 64 + make_permission_digit(
    permissions.group,
  ) * 8) + make_permission_digit(permissions.other);
}

export function set_permissions(filepath, permissions) {
  return set_permissions_octal(filepath, file_permissions_to_octal(permissions));
}
