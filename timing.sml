
structure Timing : TIMING = struct

    type tag = string

    fun hashChar (c : char, h : Word.word) : Word.word =
        Word.<<(h, 0w5) + h + 0w720 + (Word.fromInt (Char.ord c))

    fun hashString s =
        let fun hash (~1, h) = h
              | hash (i, h) =
                hash (i-1, hashChar (String.sub (s, i), h))
        in
            hash (String.size s - 1, 0w0)
        end

    structure H = HashTableFn (struct
                                type hash_key = tag
                                val hashVal = hashString
                                val sameKey = op=
                                end)

    exception NotFound

    type time_rec = {
        total : Time.time,
        min : Time.time,
        max : Time.time,
        count : int
    }
                  
    val aggregates : time_rec H.hash_table = H.mkTable (200, NotFound)
    val recordOrder : tag list ref = ref []
                                                       
    fun record tag t =
        case H.find aggregates tag of
            NONE =>
            (recordOrder := tag :: (!recordOrder);
             H.insert aggregates
                      (tag, { total = t,
                              min = t,
                              max = t,
                              count = 1 }))
          | SOME { total, min, max, count } =>
            H.insert aggregates
                     (tag, { total = Time.+ (total, t),
                             min = if Time.< (t, min) then t else min,
                             max = if Time.> (t, max) then t else max,
                             count = count + 1 })

    val mu = implode [chr 0xCE, chr 0xBC]
    fun toUsReal t = Time.toReal t * 1000000.0
                                         
    fun timed tag f =
        let open Log
            val start = Time.now ()
            val result = f ()
            val finish = Time.now ()
            val elapsed = Time.- (finish, start)
            val () = record tag elapsed
            val usElapsed = toUsReal elapsed
            val () = Log.debug
                         (fn () =>
                             ["%: %%s (%/s)",
                              tag,
                              N usElapsed, mu,
                              if usElapsed > 0.0
                              then N (1000000.0 / usElapsed)
                              else "-"
                         ])
        in
            result
        end

    fun summarise level =
        let open Log
            fun summariseOne tag =
                let val { total, min, max, count } = H.lookup aggregates tag
                    val usTotal = toUsReal total
                    val usMax = toUsReal max
                in
                    log level
                        (fn () =>
                            ["%: mean %%s (%/s), worst %%s, total %%s",
                             tag,
                             N (usTotal / Real.fromInt count), mu,
                             if usTotal > 0.0
                             then N (Real.fromInt count * 1000000.0 / usTotal)
                             else "-",
                             N usMax, mu,
                             N usTotal, mu
                        ])
                end
                handle NotFound =>
                       Log.warn (fn () => ["tag % not found in aggregates", tag])
        in
            (log level (fn () => ["Aggregate times in order of appearance:"]);
             List.app summariseOne (rev (!recordOrder)))
        end
            
end
