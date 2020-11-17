
structure Timing : TIMING = struct

    type tag = string

    fun hashChar (c : char, h : Word32.word) : Word32.word =
        Word32.<<(h, 0w5) + h + 0w720 + (Word32.fromInt (Char.ord c))

    fun hashString s =
        let fun hash (~1, h) = h
              | hash (i, h) =
                hash (i-1, hashChar (String.sub (s, i), h))
        in
            hash (String.size s - 1, 0w0)
        end

    structure H = HashTableFn (struct
                                type hash_key = string
                                val hashVal = hashString
                                val sameKey = op=
                                end)

    exception InternalError
                              
    val aggregates : Time.time H.hash_table = H.mkTable (200, InternalError)

    fun record tag t =
        case H.find aggregates tag of
            NONE => H.insert aggregates (tag, t)
          | SOME t' => H.insert aggregates (tag, Time.+ (t, t'))
                               
    fun timed tag f =
        let val start = Time.now ()
            val result = f ()
            val finish = Time.now ()
            val elapsed = Time.- (finish, start)
            val () = record tag elapsed
            val () = Log.debug
                         (fn () => ["%: % ms", tag,
                                    Log.R (Time.toReal elapsed * 1000.0)])
        in
            result
        end

    fun summarise () =
        (Log.info (fn () => ["Aggregate times:"]);
         H.appi (fn (tag, elapsed) =>
                    Log.info
                        (fn () => ["%: % ms", tag,
                                   Log.R (Time.toReal elapsed * 1000.0)]))
                aggregates)
            
end
