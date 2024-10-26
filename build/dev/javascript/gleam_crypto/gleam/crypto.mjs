import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  CustomType as $CustomType,
  isEqual,
  toBitArray,
  stringBits,
} from "../gleam.mjs";
import { strongRandomBytes as strong_random_bytes, hash, hmac } from "../gleam_crypto_ffi.mjs";

export { hash, hmac, strong_random_bytes };

export class Sha224 extends $CustomType {}

export class Sha256 extends $CustomType {}

export class Sha384 extends $CustomType {}

export class Sha512 extends $CustomType {}

export class Md5 extends $CustomType {}

export class Sha1 extends $CustomType {}

function do_secure_compare(loop$left, loop$right, loop$accumulator) {
  while (true) {
    let left = loop$left;
    let right = loop$right;
    let accumulator = loop$accumulator;
    if (left.length >= 1 && right.length >= 1) {
      let x = left.byteAt(0);
      let left$1 = left.sliceAfter(1);
      let y = right.byteAt(0);
      let right$1 = right.sliceAfter(1);
      let accumulator$1 = $int.bitwise_or(
        accumulator,
        $int.bitwise_exclusive_or(x, y),
      );
      loop$left = left$1;
      loop$right = right$1;
      loop$accumulator = accumulator$1;
    } else {
      return (isEqual(left, right)) && (accumulator === 0);
    }
  }
}

export function secure_compare(left, right) {
  let $ = $bit_array.byte_size(left) === $bit_array.byte_size(right);
  if ($) {
    return do_secure_compare(left, right, 0);
  } else {
    return false;
  }
}

function signing_input(digest_type, message) {
  let protected$ = (() => {
    if (digest_type instanceof Sha224) {
      return "HS224";
    } else if (digest_type instanceof Sha256) {
      return "HS256";
    } else if (digest_type instanceof Sha384) {
      return "HS384";
    } else if (digest_type instanceof Sha512) {
      return "HS512";
    } else if (digest_type instanceof Sha1) {
      return "HS1";
    } else {
      return "HMD5";
    }
  })();
  return $string.concat(
    toList([
      $bit_array.base64_url_encode(toBitArray([stringBits(protected$)]), false),
      ".",
      $bit_array.base64_url_encode(message, false),
    ]),
  );
}

export function sign_message(message, secret, digest_type) {
  let input = signing_input(digest_type, message);
  let signature = hmac(toBitArray([stringBits(input)]), digest_type, secret);
  return $string.concat(
    toList([input, ".", $bit_array.base64_url_encode(signature, false)]),
  );
}

export function verify_signed_message(message, secret) {
  return $result.then$(
    (() => {
      let $ = $string.split(message, ".");
      if ($.hasLength(3)) {
        let a = $.head;
        let b = $.tail.head;
        let c = $.tail.tail.head;
        return new Ok([a, b, c]);
      } else {
        return new Error(undefined);
      }
    })(),
    (_use0) => {
      let protected$ = _use0[0];
      let payload = _use0[1];
      let signature = _use0[2];
      let text = $string.concat(toList([protected$, ".", payload]));
      return $result.then$(
        $bit_array.base64_url_decode(payload),
        (payload) => {
          return $result.then$(
            $bit_array.base64_url_decode(signature),
            (signature) => {
              return $result.then$(
                $bit_array.base64_url_decode(protected$),
                (protected$) => {
                  return $result.then$(
                    (() => {
                      if (protected$.byteAt(0) === 72 &&
                      protected$.byteAt(1) === 83 &&
                      protected$.byteAt(2) === 50 &&
                      protected$.byteAt(3) === 50 &&
                      protected$.byteAt(4) === 52 &&
                      protected$.length == 5) {
                        return new Ok(new Sha224());
                      } else if (protected$.byteAt(0) === 72 &&
                      protected$.byteAt(1) === 83 &&
                      protected$.byteAt(2) === 50 &&
                      protected$.byteAt(3) === 53 &&
                      protected$.byteAt(4) === 54 &&
                      protected$.length == 5) {
                        return new Ok(new Sha256());
                      } else if (protected$.byteAt(0) === 72 &&
                      protected$.byteAt(1) === 83 &&
                      protected$.byteAt(2) === 51 &&
                      protected$.byteAt(3) === 56 &&
                      protected$.byteAt(4) === 52 &&
                      protected$.length == 5) {
                        return new Ok(new Sha384());
                      } else if (protected$.byteAt(0) === 72 &&
                      protected$.byteAt(1) === 83 &&
                      protected$.byteAt(2) === 53 &&
                      protected$.byteAt(3) === 49 &&
                      protected$.byteAt(4) === 50 &&
                      protected$.length == 5) {
                        return new Ok(new Sha512());
                      } else if (protected$.byteAt(0) === 72 &&
                      protected$.byteAt(1) === 83 &&
                      protected$.byteAt(2) === 49 &&
                      protected$.length == 3) {
                        return new Ok(new Sha1());
                      } else if (protected$.byteAt(0) === 72 &&
                      protected$.byteAt(1) === 77 &&
                      protected$.byteAt(2) === 68 &&
                      protected$.byteAt(3) === 53 &&
                      protected$.length == 4) {
                        return new Ok(new Md5());
                      } else {
                        return new Error(undefined);
                      }
                    })(),
                    (digest_type) => {
                      let challenge = hmac(
                        toBitArray([stringBits(text)]),
                        digest_type,
                        secret,
                      );
                      let $ = secure_compare(challenge, signature);
                      if ($) {
                        return new Ok(payload);
                      } else {
                        return new Error(undefined);
                      }
                    },
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}
