
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

    type time_rec = {
        total : Time.time,
        min : Time.time,
        max : Time.time,
        count : int
    }
                  
    val aggregates : time_rec H.hash_table = H.mkTable (200, InternalError)

    fun record tag t =
        case H.find aggregates tag of
            NONE =>
            H.insert aggregates
                     (tag, { total = t,
                             min = t,
                             max = t,
                             count = 1 })
          | SOME { total, min, max, count } =>
            H.insert aggregates
                     (tag, { total = Time.+ (total, t),
                             min = if Time.< (t, min) then t else min,
                             max = if Time.> (t, max) then t else max,
                             count = count + 1 })
                               
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
        let open Log
        in
            (info (fn () => ["Aggregate times:"]);
             H.appi (fn (tag, { total, min, max, count }) =>
                        info (fn () => ["%: total % ms, min %, max %, average % (% / sec)",
                                        tag,
                                        R (Time.toReal total * 1000.0),
                                        R (Time.toReal min * 1000.0),
                                        R (Time.toReal max * 1000.0),
                                        R ((Time.toReal total * 1000.0) /
                                           Real.fromInt count),
                                        R (Real.fromInt count /
                                           (*!!! fix: blow up if total = 0 *)
                                           Time.toReal total)]))
                    aggregates)
        end
            
end
