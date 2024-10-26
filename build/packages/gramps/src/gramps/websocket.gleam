import gleam/bit_array
import gleam/bool
import gleam/bytes_builder.{type BytesBuilder}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gramps/websocket/compression.{type Context}

pub type DataFrame {
  TextFrame(payload_length: Int, payload: BitArray)
  BinaryFrame(payload_length: Int, payload: BitArray)
}

pub type ControlFrame {
  CloseFrame(payload_length: Int, payload: BitArray)
  PingFrame(payload_length: Int, payload: BitArray)
  PongFrame(payload_length: Int, payload: BitArray)
}

pub type Frame {
  Data(DataFrame)
  Control(ControlFrame)
  Continuation(length: Int, payload: BitArray)
}

@external(erlang, "crypto", "exor")
fn crypto_exor(a a: BitArray, b b: BitArray) -> BitArray

fn mask_data(
  data: BitArray,
  masks: List(BitArray),
  index: Int,
  resp: BitArray,
) -> BitArray {
  case data {
    <<masked:bits-size(8), rest:bits>> -> {
      let assert [one, two, three, four] = masks
      let mask_value = case index % 4 {
        0 -> one
        1 -> two
        2 -> three
        3 -> four
        _ -> panic as "Somehow a value mod 4 is not 0, 1, 2, or 3"
      }
      let unmasked = crypto_exor(mask_value, masked)
      mask_data(rest, masks, index + 1, <<resp:bits, unmasked:bits>>)
    }
    _ -> resp
  }
}

pub type FrameParseError {
  NeedMoreData(BitArray)
  InvalidFrame
}

pub type ParsedFrame {
  Complete(Frame)
  Incomplete(Frame)
}

pub fn frame_from_message(
  message: BitArray,
  context: Option(Context),
) -> Result(#(ParsedFrame, BitArray), FrameParseError) {
  case message {
    <<
      complete:1,
      compressed:1,
      _reserved:2,
      opcode:int-size(4),
      masked:1,
      payload_length:int-size(7),
      rest:bits,
    >> -> {
      let compressed = compressed == 1
      let masked = masked == 1
      use <- bool.guard(
        when: compressed && option.is_none(context),
        return: Error(InvalidFrame),
      )
      let payload_size = case payload_length {
        126 -> 16
        127 -> 64
        _ -> 0
      }
      let maybe_pair = case masked, rest {
        True,
          <<
            length:int-size(payload_size),
            mask1:bytes-size(1),
            mask2:bytes-size(1),
            mask3:bytes-size(1),
            mask4:bytes-size(1),
            rest:bits,
          >>
        -> {
          let payload_byte_size = case length {
            0 -> payload_length
            n -> n
          }
          case rest {
            <<payload:bytes-size(payload_byte_size), rest:bits>> -> {
              let data =
                mask_data(payload, [mask1, mask2, mask3, mask4], 0, <<>>)
              Ok(#(data, rest))
            }
            _ -> {
              Error(NeedMoreData(message))
            }
          }
        }
        False, <<length:int-size(payload_size), rest:bits>> -> {
          let payload_byte_size = case length {
            0 -> payload_length
            n -> n
          }
          case rest {
            <<payload:bytes-size(payload_byte_size), rest:bits>> -> {
              Ok(#(payload, rest))
            }
            _ -> {
              Error(NeedMoreData(message))
            }
          }
        }
        _, _ -> Error(InvalidFrame)
      }

      use #(data, rest) <- result.try(maybe_pair)
      case opcode {
        0 -> {
          data
          |> inflate(compressed, context, _)
          |> result.map(fn(p) { Continuation(p.0, p.1) })
        }
        1 -> {
          data
          |> inflate(compressed, context, _)
          |> result.map(fn(p) { Data(TextFrame(p.0, p.1)) })
        }
        2 -> {
          data
          |> inflate(compressed, context, _)
          |> result.map(fn(p) { Data(BinaryFrame(p.0, p.1)) })
        }
        8 -> Ok(Control(CloseFrame(payload_length, data)))
        9 -> Ok(Control(PingFrame(payload_length, data)))
        10 -> Ok(Control(PongFrame(payload_length, data)))
        _ -> Error(InvalidFrame)
      }
      |> result.then(fn(frame) {
        case complete {
          1 -> Ok(#(Complete(frame), rest))
          0 -> Ok(#(Incomplete(frame), rest))
          _ -> Error(InvalidFrame)
        }
      })
    }
    _ -> Error(InvalidFrame)
  }
}

pub fn frame_to_bytes_builder(
  frame: Frame,
  mask: Option(BitArray),
) -> BytesBuilder {
  case frame {
    Data(TextFrame(payload_length, payload)) ->
      make_frame(1, payload_length, payload, mask)
    Control(CloseFrame(payload_length, payload)) ->
      make_frame(8, payload_length, payload, mask)
    Data(BinaryFrame(payload_length, payload)) ->
      make_frame(2, payload_length, payload, mask)
    Control(PongFrame(payload_length, payload)) ->
      make_frame(10, payload_length, payload, mask)
    Control(PingFrame(payload_length, payload)) ->
      make_frame(9, payload_length, payload, mask)
    Continuation(length, payload) -> make_frame(0, length, payload, mask)
  }
}

pub fn compressed_frame_to_bytes_builder(
  frame: Frame,
  context: Context,
  mask: Option(BitArray),
) -> BytesBuilder {
  case frame {
    Data(TextFrame(_payload_length, payload)) ->
      make_compressed_frame(1, payload, context, mask)
    Data(BinaryFrame(_payload_length, payload)) ->
      make_compressed_frame(2, payload, context, mask)
    Control(CloseFrame(payload_length, payload)) ->
      make_frame(8, payload_length, payload, mask)
    Control(PongFrame(payload_length, payload)) ->
      make_frame(10, payload_length, payload, mask)
    Control(PingFrame(payload_length, payload)) ->
      make_frame(9, payload_length, payload, mask)
    Continuation(length, payload) -> make_frame(0, length, payload, mask)
  }
}

fn make_length(length: Int) -> BitArray {
  case length {
    length if length > 65_535 -> <<127:7, length:int-size(64)>>
    length if length >= 126 -> <<126:7, length:int-size(16)>>
    _length -> <<length:7>>
  }
}

fn make_compressed_frame(
  opcode: Int,
  payload: BitArray,
  context: Context,
  mask: Option(BitArray),
) -> BytesBuilder {
  let data = compression.deflate(context, payload)
  let length = bit_array.byte_size(data)
  let length_section = make_length(length)

  let masked = case option.is_some(mask) {
    True -> 1
    False -> 0
  }

  let mask_key = option.unwrap(mask, <<>>)

  <<
    1:1,
    1:1,
    0:2,
    opcode:4,
    masked:1,
    length_section:bits,
    mask_key:bits,
    data:bits,
  >>
  |> bytes_builder.from_bit_array
}

fn make_frame(
  opcode: Int,
  length: Int,
  payload: BitArray,
  mask: Option(BitArray),
) -> BytesBuilder {
  let length_section = make_length(length)

  let masked = case option.is_some(mask) {
    True -> 1
    False -> 0
  }

  let mask_key = option.unwrap(mask, <<>>)

  <<
    1:1,
    0:3,
    opcode:4,
    masked:1,
    length_section:bits,
    mask_key:bits,
    payload:bits,
  >>
  |> bytes_builder.from_bit_array
}

fn apply_mask(data: BitArray, mask: BitArray) -> BitArray {
  let assert <<
    mask1:bytes-size(1),
    mask2:bytes-size(1),
    mask3:bytes-size(1),
    mask4:bytes-size(1),
  >> = mask
  mask_data(data, [mask1, mask2, mask3, mask4], 0, <<>>)
}

pub fn to_text_frame(
  data: String,
  context: Option(Context),
  mask: Option(BitArray),
) -> BytesBuilder {
  let data = bit_array.from_string(data)
  let data =
    mask
    |> option.map(apply_mask(data, _))
    |> option.unwrap(data)
  let size = bit_array.byte_size(data)
  let frame = Data(TextFrame(size, data))
  case context {
    Some(context) -> compressed_frame_to_bytes_builder(frame, context, mask)
    _ -> frame_to_bytes_builder(frame, mask)
  }
}

pub fn to_binary_frame(
  data: BitArray,
  context: Option(Context),
  mask: Option(BitArray),
) -> BytesBuilder {
  let data =
    mask
    |> option.map(apply_mask(data, _))
    |> option.unwrap(data)
  let size = bit_array.byte_size(data)
  let frame = Data(BinaryFrame(size, data))
  case context {
    Some(context) -> compressed_frame_to_bytes_builder(frame, context, mask)
    _ -> frame_to_bytes_builder(frame, mask)
  }
}

pub fn get_messages(
  data: BitArray,
  frames: List(ParsedFrame),
  context: Option(Context),
) -> #(List(ParsedFrame), BitArray) {
  case frame_from_message(data, context) {
    Ok(#(frame, <<>>)) -> #(list.reverse([frame, ..frames]), <<>>)
    Ok(#(frame, rest)) -> get_messages(rest, [frame, ..frames], context)
    Error(NeedMoreData(rest)) -> #(list.reverse(frames), rest)
    Error(InvalidFrame) -> #(list.reverse(frames), data)
  }
}

fn append_frame(left: Frame, length: Int, data: BitArray) -> Frame {
  case left {
    Data(TextFrame(len, payload)) ->
      Data(TextFrame(len + length, <<payload:bits, data:bits>>))
    Data(BinaryFrame(len, payload)) ->
      Data(BinaryFrame(len + length, <<payload:bits, data:bits>>))
    Control(CloseFrame(len, payload)) ->
      Control(CloseFrame(len + length, <<payload:bits, data:bits>>))
    Control(PingFrame(len, payload)) ->
      Control(PingFrame(len + length, <<payload:bits, data:bits>>))
    Control(PongFrame(len, payload)) ->
      Control(PongFrame(len + length, <<payload:bits, data:bits>>))
    Continuation(..) -> left
  }
}

pub fn aggregate_frames(
  frames: List(ParsedFrame),
  previous: Option(Frame),
  joined: List(Frame),
) -> Result(List(Frame), Nil) {
  case frames, previous {
    [], _ -> Ok(list.reverse(joined))
    [Complete(Continuation(length, data)), ..rest], Some(prev) -> {
      let next = append_frame(prev, length, data)
      aggregate_frames(rest, None, [next, ..joined])
    }
    [Incomplete(Continuation(length, data)), ..rest], Some(prev) -> {
      let next = append_frame(prev, length, data)
      aggregate_frames(rest, Some(next), joined)
    }
    [Incomplete(frame), ..rest], None -> {
      aggregate_frames(rest, Some(frame), joined)
    }
    [Complete(frame), ..rest], None -> {
      aggregate_frames(rest, None, [frame, ..joined])
    }
    _, _ -> Error(Nil)
  }
}

const websocket_key = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

pub const client_key = "dGhlIHNhbXBsZSBub25jZQ=="

type ShaHash {
  Sha
}

pub fn parse_websocket_key(key: String) -> String {
  key
  |> string.append(websocket_key)
  |> crypto_hash(Sha, _)
  |> base64_encode
}

@external(erlang, "crypto", "hash")
fn crypto_hash(hash hash: ShaHash, data data: String) -> String

@external(erlang, "base64", "encode")
fn base64_encode(data data: String) -> String

fn inflate(
  compressed: Bool,
  context: Option(Context),
  data: BitArray,
) -> Result(#(Int, BitArray), FrameParseError) {
  case compressed, context {
    True, Some(context) -> {
      let data = compression.inflate(context, data)
      let length = bit_array.byte_size(data)
      Ok(#(length, data))
    }
    True, None -> Error(InvalidFrame)
    _, _ -> Ok(#(bit_array.byte_size(data), data))
  }
}

pub fn has_deflate(extensions: List(String)) -> Bool {
  list.any(extensions, fn(str) { str == "permessage-deflate" })
}
