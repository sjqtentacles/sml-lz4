(* demo.sml - compress and decompress a byte vector with the LZ4 block
   codec, and show malformed input being rejected. Deterministic: identical
   output on every run and both compilers. *)

structure L = Lz4

val original =
  "the quick brown fox the quick brown fox the quick brown fox"
val originalBytes = Byte.stringToBytes original

val () = print "LZ4 block codec over a repetitive string:\n"
val () = print ("  original length     = "
                ^ Int.toString (Word8Vector.length originalBytes) ^ " bytes\n")

val compressed = L.compress originalBytes
val () = print ("  compressed length   = "
                ^ Int.toString (Word8Vector.length compressed) ^ " bytes\n")

val restored = L.decompress compressed
val restoredStr = Byte.bytesToString restored
val () = print ("  round-trip matches  = "
                ^ (if restoredStr = original then "true" else "false") ^ "\n")

val () = print "\nDecompressing malformed input:\n"
(* token = 0x00 (0 literals, a match follows) but the block is truncated
   before the required 2-byte back-reference offset. *)
val badInput = Word8Vector.fromList [0wx00, 0wx00]
val () =
  (ignore (L.decompress badInput);
   print "  (unexpected) decompressed without error\n")
  handle L.Lz4 => print "  Lz4 raised, as expected, on malformed input\n"

(* A non-repetitive string still round-trips, just with a larger ratio. *)
val sparse = "qjxzv 7 kw 2 pl 9 mtb"
val sparseBytes = Byte.stringToBytes sparse
val sparseCompressed = L.compress sparseBytes
val sparseRestored = Byte.bytesToString (L.decompress sparseCompressed)
val () = print "\nLZ4 block codec over a low-repetition string:\n"
val () = print ("  original length     = "
                ^ Int.toString (Word8Vector.length sparseBytes) ^ " bytes\n")
val () = print ("  compressed length   = "
                ^ Int.toString (Word8Vector.length sparseCompressed) ^ " bytes\n")
val () = print ("  round-trip matches  = "
                ^ (if sparseRestored = sparse then "true" else "false") ^ "\n")
