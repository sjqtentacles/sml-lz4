(* lz4.sig — LZ4 block-format compression / decompression *)
signature LZ4 =
sig
  (* Raised on malformed/truncated LZ4 block input during decompression. *)
  exception Lz4

  (* Compress a byte vector into an LZ4 block.  The result is valid LZ4
     block data that [decompress] round-trips exactly, but is not
     guaranteed to be byte-for-byte identical to the reference encoder. *)
  val compress   : Word8Vector.vector -> Word8Vector.vector

  (* Decompress an LZ4 block back into the original bytes.
     Raises [Lz4] on malformed input. *)
  val decompress : Word8Vector.vector -> Word8Vector.vector
end
