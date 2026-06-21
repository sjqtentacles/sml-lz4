(* lz4.sml — LZ4 block-format compression / decompression in pure SML.

   Block format (no frame header):
     A block is a sequence of "sequences".  Each sequence is:
       - 1 token byte: high nibble = literal length (0..15),
                       low  nibble = match length - 4 (0..15).
       - if literal-length nibble = 15: read extension bytes, each adding
         its value; keep reading while the byte = 255.
       - the literal bytes (literal-length of them), copied verbatim.
       - the offset: 2 bytes, little-endian (distance back into output).
       - if match-length nibble = 15: read extension bytes (as above).
       - copy (match-length nibble + extensions + 4) bytes from `offset`
         bytes back in the output, byte-by-byte (overlap allowed).
     The final sequence ends after its literals: it has no offset/match. *)
structure Lz4 :> LZ4 =
struct
  exception Lz4

  val minMatch = 4

  (* ============================ DECOMPRESS ============================ *)

  fun decompress (input : Word8Vector.vector) : Word8Vector.vector =
    let
      val n = Word8Vector.length input
      val out = ref (Array.array (if n < 16 then 16 else n * 3, 0w0 : Word8.word))
      val outLen = ref 0

      fun ensure extra =
        let
          val need = !outLen + extra
          val cap = Array.length (!out)
        in
          if need <= cap then ()
          else
            let
              val newCap = ref (if cap = 0 then 16 else cap)
              val () = while !newCap < need do newCap := !newCap * 2
              val a = Array.array (!newCap, 0w0 : Word8.word)
            in
              Array.copy { src = !out, dst = a, di = 0 };
              out := a
            end
        end

      fun pushByte b =
        ( ensure 1;
          Array.update (!out, !outLen, b);
          outLen := !outLen + 1 )

      fun getIn i =
        if i < 0 orelse i >= n then raise Lz4
        else Word8Vector.sub (input, i)

      (* read length extension starting at input index i; returns (added, nextI) *)
      fun readExt i acc =
        let
          val b = Word8.toInt (getIn i)
        in
          if b = 255 then readExt (i + 1) (acc + 255)
          else (acc + b, i + 1)
        end

      (* main loop; i = current read index in input *)
      fun loop i =
        if i >= n then ()
        else
          let
            val token = Word8.toInt (getIn i)
            val i = i + 1
            val litNibble = Int.div (token, 16)        (* high nibble *)
            val matchNibble = Int.mod (token, 16)      (* low nibble *)

            (* literal length *)
            val (litLen, i) =
              if litNibble = 15 then readExt i 15
              else (litNibble, i)

            (* copy literals *)
            val () = ensure litLen
            fun copyLits (k, j) =
              if k >= litLen then j
              else ( pushByte (getIn j); copyLits (k + 1, j + 1) )
            val i = copyLits (0, i)
          in
            (* If we've consumed all input exactly after literals, this was
               the final sequence (last-literals rule). *)
            if i >= n then ()
            else
              let
                (* 2-byte little-endian offset *)
                val lo = Word8.toInt (getIn i)
                val hi = Word8.toInt (getIn (i + 1))
                val i = i + 2
                val offset = lo + hi * 256
                val () = if offset = 0 orelse offset > !outLen then raise Lz4 else ()

                (* match length *)
                val (matchLen, i) =
                  if matchNibble = 15 then readExt i (15 + minMatch)
                  else (matchNibble + minMatch, i)

                (* copy match byte-by-byte (overlap aware) *)
                val start = !outLen - offset
                val () = ensure matchLen
                fun copyMatch k =
                  if k >= matchLen then ()
                  else
                    ( pushByte (Array.sub (!out, start + k));
                      copyMatch (k + 1) )
                val () = copyMatch 0
              in
                loop i
              end
          end

      val () = loop 0
    in
      Word8Vector.tabulate (!outLen, fn k => Array.sub (!out, k))
    end

  (* ============================= COMPRESS ============================= *)

  (* Greedy LZ4 compressor using a single-entry hash table of the position
     of the most recent occurrence of each 4-byte sequence.  Produces valid
     LZ4 block data that `decompress` round-trips exactly. *)

  val hashLog = 12
  val hashSize = 4096   (* 1 << hashLog *)

  fun compress (input : Word8Vector.vector) : Word8Vector.vector =
    let
      val n = Word8Vector.length input
      fun sub i = Word8.toInt (Word8Vector.sub (input, i))

      (* output buffer *)
      val out = ref (Array.array (if n < 16 then 16 else n + (Int.div (n, 2)) + 16,
                                  0w0 : Word8.word))
      val outLen = ref 0
      fun ensure extra =
        let val need = !outLen + extra and cap = Array.length (!out)
        in
          if need <= cap then ()
          else
            let
              val newCap = ref cap
              val () = while !newCap < need do newCap := !newCap * 2
              val a = Array.array (!newCap, 0w0 : Word8.word)
            in Array.copy { src = !out, dst = a, di = 0 }; out := a end
        end
      fun emit b =
        ( ensure 1; Array.update (!out, !outLen, Word8.fromInt b);
          outLen := !outLen + 1 )

      (* emit a length value using LZ4 extension encoding (value already has
         the 15 baseline subtracted by caller before calling). *)
      fun emitExt v =
        if v >= 255 then (emit 255; emitExt (v - 255))
        else emit v

      (* hash of the 4 bytes at position p *)
      fun hash4 p =
        let
          val v = Word32.fromInt (sub p)
          val v = Word32.orb (v, Word32.<< (Word32.fromInt (sub (p + 1)), 0w8))
          val v = Word32.orb (v, Word32.<< (Word32.fromInt (sub (p + 2)), 0w16))
          val v = Word32.orb (v, Word32.<< (Word32.fromInt (sub (p + 3)), 0w24))
          val h = Word32.* (v, 0wx9E3779B1)
        in
          Word32.toInt (Word32.>> (h, Word.fromInt (32 - hashLog)))
        end

      val table = Array.array (hashSize, ~1)

      fun match4 (a, b) =
        sub a = sub b andalso sub (a+1) = sub (b+1)
        andalso sub (a+2) = sub (b+2) andalso sub (a+3) = sub (b+3)

      (* emit a sequence: literals from [litStart, litEnd), then either a
         match (matchLen>0 with given offset) or nothing (final). *)
      fun emitSeq (litStart, litEnd, matchLen, offset) =
        let
          val litLen = litEnd - litStart
          val litNib = if litLen >= 15 then 15 else litLen
          val matNib =
            if matchLen = 0 then 0
            else
              let val m = matchLen - minMatch
              in if m >= 15 then 15 else m end
          val token = litNib * 16 + matNib
          val () = emit token
          val () = if litLen >= 15 then emitExt (litLen - 15) else ()
          (* literals *)
          fun putLits k =
            if k >= litEnd then ()
            else (emit (sub k); putLits (k + 1))
          val () = putLits litStart
        in
          if matchLen = 0 then ()
          else
            let
              val () = emit (Int.mod (offset, 256))
              val () = emit (Int.div (offset, 256))
              val m = matchLen - minMatch
              val () = if m >= 15 then emitExt (m - 15) else ()
            in () end
        end

      (* main scan.
         anchor = start of pending literals.
         p = current scan position. *)
      fun scan (p, anchor) =
        if p + minMatch > n then
          (* flush remaining literals as final sequence *)
          emitSeq (anchor, n, 0, 0)
        else
          let
            val h = hash4 p
            val cand = Array.sub (table, h)
            val () = Array.update (table, h, p)
          in
            if cand >= 0 andalso cand < p
               andalso (p - cand) <= 65535
               andalso match4 (cand, p)
            then
              let
                (* extend the match forward *)
                fun extend len =
                  if p + len < n andalso sub (cand + len) = sub (p + len)
                  then extend (len + 1) else len
                val matchLen = extend minMatch
                val offset = p - cand
                val () = emitSeq (anchor, p, matchLen, offset)
                (* insert hashes for the matched region (improves matches),
                   then continue after the match *)
                val q = p + matchLen
                fun insertHashes k =
                  if k >= q orelse k + minMatch > n then ()
                  else (Array.update (table, hash4 k, k); insertHashes (k + 1))
                val () = insertHashes (p + 1)
              in
                scan (q, q)
              end
            else
              scan (p + 1, anchor)
          end

      val () = if n = 0 then emit 0   (* empty input -> single zero token *)
               else scan (0, 0)
    in
      Word8Vector.tabulate (!outLen, fn k => Array.sub (!out, k))
    end
end
