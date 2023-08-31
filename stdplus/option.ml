open Stdlib
include Option

module Infix = struct
  let ( >>= ) = Option.bind
end
