import * as $exception from "../../exception/exception.mjs";
import * as $request from "../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../gleam_http/gleam/http/response.mjs";
import * as $bytes_builder from "../../gleam_stdlib/gleam/bytes_builder.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $mist from "../../mist/mist.mjs";
import * as $wisp from "../wisp.mjs";
import * as $internal from "../wisp/internal.mjs";

function wrap_mist_chunk(chunk) {
  let _pipe = chunk;
  let _pipe$1 = $result.nil_error(_pipe);
  return $result.map(
    _pipe$1,
    (chunk) => {
      if (chunk instanceof $mist.Done) {
        return new $internal.ReadingFinished();
      } else {
        let data = chunk.data;
        let consume = chunk.consume;
        return new $internal.Chunk(
          data,
          (size) => { return wrap_mist_chunk(consume(size)); },
        );
      }
    },
  );
}
