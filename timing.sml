
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
    fun usPerSecStr u = if u > 0.0 then Log.N (1000000.0 / u) else "-"

    fun summarise level =
        let open Log
            fun summariseOne tag =
                let val { total, min, max, count } = H.lookup aggregates tag
                    val usTotal = toUsReal total
                    val usMax = toUsReal max
                    fun number r =
                        if Real.>= (r, 100.0)
                        then I (Real.round r)
                        else N r
                in
                    log level
                        (fn () =>
                            ["%1: mean %2%3s (%4/s), worst %5%6s, total %7%8s",
                             tag,
                             number (usTotal / Real.fromInt count), mu,
                             if usTotal > 0.0
                             then number (Real.fromInt count * 1000000.0 / usTotal)
                             else "-",
                             number usMax, mu,
                             number usTotal, mu
                        ])
                end
                handle NotFound =>
                       Log.warn (fn () => ["tag %1 not found in aggregates", tag])
        in
            (log level (fn () => ["Aggregate times in order of appearance:"]);
             List.app summariseOne (rev (!recordOrder)))
        end
                                                                       
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
                             ["%1: %2%3s (%4/s)",
                              tag,
                              N usElapsed, mu, usPerSecStr usElapsed
                         ])
        in
            result
        end
                                         
    fun timedToBudget (tag, budget) f =
        let open Log
            val start = Time.now ()
            val result = f ()
            val finish = Time.now ()
            val elapsed = Time.- (finish, start)
            val () = record tag elapsed
            val usElapsed = toUsReal elapsed
            val () = Log.debug
                         (fn () =>
                             ["%1: %2%3s (%4/s)",
                              tag,
                              N usElapsed, mu, usPerSecStr usElapsed
                         ])
            val usBudget = toUsReal budget
            val () = if Time.> (elapsed, budget)
                     then (Log.warn
                               (fn () =>
                                   ["%1: exceeded budget of %2%3s with elapsed time of %4%5s (%6/s)",
                                    tag,
                                    N usBudget, mu,
                                    N usElapsed, mu, usPerSecStr usElapsed
                               ]);
                           summarise Log.WARN)
                      else ();
        in
            result
        end
            
end
