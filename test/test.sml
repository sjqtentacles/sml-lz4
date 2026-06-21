structure Tests =
struct
  open Harness

  (* ---- byte-vector helpers ---- *)
  fun bytes (xs : int list) : Word8Vector.vector =
    Word8Vector.fromList (List.map Word8.fromInt xs)

  fun vecToInts (v : Word8Vector.vector) : int list =
    Word8Vector.foldr (fn (b, acc) => Word8.toInt b :: acc) [] v

  fun strBytes (s : string) : Word8Vector.vector =
    Byte.stringToBytes s

  (* readable byte-vector equality check *)
  fun checkBytes name (expected, actual) =
    if expected = actual then
      check name true
    else
      let
        fun show v = "[" ^ String.concatWith ","
                       (List.map Int.toString (vecToInts v)) ^ "]"
      in
        ( print ("    expected " ^ show expected
                 ^ " but got " ^ show actual ^ "\n");
          check name false )
      end

  fun repeat (n, b) = bytes (List.tabulate (n, fn _ => b))

  (* ---------------------------------------------------------------- *)
  fun runAll () =
    let
      (* ===== decode: hand-constructed LZ4 blocks ===== *)
      val () = section "decode (hand-constructed blocks)"

      (* literals-only: token 0x30 (litlen=3, matchlen nibble=0), then "ABC" *)
      val litBlock = bytes [0x30, 0x41, 0x42, 0x43]
      val () = checkBytes "literals-only block -> ABC"
                 (strBytes "ABC", Lz4.decompress litBlock)

      (* empty block: token 0x00, litlen=0, no match *)
      val () = checkBytes "empty block -> empty"
                 (bytes [], Lz4.decompress (bytes [0x00]))

      (* back-reference: token 0x13 (litlen=1, matchlen nibble=3 -> 3+4=7),
         literal 'a'=0x61, offset=1 (0x01 0x00). Output = 8 'a's. *)
      val refBlock = bytes [0x13, 0x61, 0x01, 0x00]
      val () = checkBytes "back-ref block -> 8 'a's"
                 (repeat (8, 0x61), Lz4.decompress refBlock)

      (* literal-length extension: 16 literals.
         litlen=16 -> nibble 15 + ext byte (16-15)=1. token 0xF0, ext 0x01,
         then 16 bytes 0x00..0x0F (here just 16 'b'=0x62 for simplicity). *)
      val sixteenB = List.tabulate (16, fn _ => 0x62)
      val longLitBlock = bytes ([0xF0, 0x01] @ sixteenB)
      val () = checkBytes "long-literal block (litlen ext) -> 16 'b's"
                 (repeat (16, 0x62), Lz4.decompress longLitBlock)

      (* match-length extension: 1 literal 'c', then a match of length 19.
         matchlen=19 -> matchlen-4=15 -> nibble 15 + ext (15-15)=0.
         token: litlen=1 (nibble 1), matchlen nibble 15 -> 0x1F.
         literal 0x63, offset 1 (0x01 0x00), match ext byte 0x00.
         Output = 20 'c's (1 literal + 19 copied). *)
      val longMatchBlock = bytes [0x1F, 0x63, 0x01, 0x00, 0x00]
      val () = checkBytes "long-match block (matchlen ext) -> 20 'c's"
                 (repeat (20, 0x63), Lz4.decompress longMatchBlock)

      (* overlapping copy with offset 2: literals "ab", then match len 4 at
         offset 2 -> copies a,b,a,b. token litlen=2 nibble 2,
         matchlen 4 -> nibble 0 -> 0x20. literals 0x61 0x62, offset 2. *)
      val overlapBlock = bytes [0x20, 0x61, 0x62, 0x02, 0x00]
      val () = checkBytes "overlapping copy offset 2 -> ababab"
                 (strBytes "ababab", Lz4.decompress overlapBlock)

      (* ===== decode: malformed input raises Lz4 ===== *)
      val () = section "decode errors"
      (* truncated: token says 3 literals but only 1 present *)
      val () = checkRaises "truncated literals raises"
                 (fn () => Lz4.decompress (bytes [0x30, 0x41]))
      (* offset of 0 is invalid (would copy from out of range) *)
      val () = checkRaises "missing offset bytes raises"
                 (fn () => Lz4.decompress (bytes [0x13, 0x61, 0x01]))
      (* offset larger than current output -> invalid *)
      val () = checkRaises "offset beyond output raises"
                 (fn () => Lz4.decompress (bytes [0x13, 0x61, 0x05, 0x00]))

      (* ===== compress -> own decode (round trip) ===== *)
      val () = section "round-trip"
      fun roundTrip name v =
        checkBytes name (v, Lz4.decompress (Lz4.compress v))

      val () = roundTrip "empty" (bytes [])
      val () = roundTrip "single byte" (bytes [0x42])
      val () = roundTrip "two bytes" (bytes [0x01, 0x02])
      val () = roundTrip "three bytes (< min match)" (bytes [0x01, 0x02, 0x03])
      val () = roundTrip "hello world" (strBytes "hello world")
      val () = roundTrip "256 a's" (repeat (256, 0x61))
      val () = roundTrip "1000 a's" (repeat (1000, 0x61))
      val () = roundTrip "text with repeats"
                 (strBytes "abcabcabcabcabcabcabcabcabcabc")
      val () = roundTrip "long literal run (no repeats)"
                 (strBytes "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJ")
      val () = roundTrip "mixed repeats and literals"
                 (strBytes "the quick brown fox the quick brown fox jumps")

      (* pseudo-random / incompressible bytes *)
      val rnd =
        let
          val st = ref 0w12345
          fun next () =
            ( st := !st * 0w1103515245 + 0w12345;
              Word.toInt (Word.andb (Word.>> (!st, 0w16), 0wxFF)) )
        in
          bytes (List.tabulate (500, fn _ => next ()))
        end
      val () = roundTrip "pseudo-random 500 bytes" rnd

      (* highly repetitive long match (> 15+ runs to exercise extensions) *)
      val () = roundTrip "5000 same byte" (repeat (5000, 0x7A))

      (* alternating pattern (overlapping matches) *)
      val () = roundTrip "alternating ababab... (2000)"
                 (bytes (List.tabulate (2000,
                    fn i => if i mod 2 = 0 then 0x61 else 0x62)))

      (* longer english-ish text with many repeats *)
      val lorem =
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, " ^
        "sed do eiusmod tempor incididunt ut labore et dolore magna " ^
        "aliqua. Lorem ipsum dolor sit amet, consectetur adipiscing " ^
        "elit, sed do eiusmod tempor incididunt ut labore et dolore."
      val () = roundTrip "lorem ipsum with repeats" (strBytes lorem)

      (* boundary: exactly 4 bytes (min match), 5 bytes *)
      val () = roundTrip "exactly 4 bytes" (bytes [0x10, 0x20, 0x30, 0x40])
      val () = roundTrip "exactly 5 bytes"
                 (bytes [0x10, 0x20, 0x30, 0x40, 0x50])

      (* a repeated 4-byte token, many times *)
      val () = roundTrip "repeated 4-byte token x300"
                 (bytes (List.tabulate (1200, fn i =>
                    List.nth ([0xDE,0xAD,0xBE,0xEF], i mod 4))))
    in () end

  fun run () = (reset (); runAll (); Harness.run ())
end
