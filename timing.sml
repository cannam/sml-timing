structure TimingFormat : TIMING_FORMAT = struct

    val mu = implode [chr 0xCE, chr 0xBC]
    fun toUsReal t = Time.toReal t * 1000000.0
    fun usPerSecStr u = if u > 0.0 then Log.N (1000000.0 / u) else "-"
    fun spaces n = String.concat (List.tabulate (n, fn _ => " "))
                                                                       
    fun formatElapsedTime t =
        let val us = toUsReal t
            fun str r = if Real.>= (r, 100.0)
                        then Log.I (Real.round r)
                        else Log.N r
            val ustr = str us
        in
            ustr ^ " " ^ mu ^ "s"
        end
                                                                       
    fun formatElapsedTimePadded padTo t =
        let val us = toUsReal t
            fun alignWidth r = if Real.>= (r, 1.0)
                               then #exp (Real.toDecimal r)
                               else 1
            val str = formatElapsedTime t
            val alignAt = alignWidth us
            val padding = if padTo > alignAt
                          then spaces (padTo - alignAt)
                          else ""
        in
            padding ^ str
        end

    fun formatElapsedTimePerSec t =
        let val us = toUsReal t
        in
            usPerSecStr us ^ " /sec"
        end
end
                                             
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

    structure Log = CategoryLogFn (struct
                                    val category = "timing"
                                    end)
                              
    exception NotFound

    type time_rec = {
        total : Time.time,
        min : Time.time,
        max : Time.time,
        last : Time.time,
        updated : Time.time,
        count : int
    }
                  
    val aggregates : time_rec H.hash_table = H.mkTable (200, NotFound)
    val summaryOrder : tag list ref = ref []
                                                       
    fun recordAt tag (t, finishedAt) =
        case H.find aggregates tag of
            NONE =>
            (summaryOrder := ListMergeSort.sort String.>
                             (tag :: (!summaryOrder));
             H.insert aggregates
                      (tag, { total = t,
                              min = t,
                              max = t,
                              last = t,
                              updated = finishedAt,
                              count = 1 }))
          | SOME { total, min, max, last, updated, count } =>
            H.insert aggregates
                     (tag, { total = Time.+ (total, t),
                             min = if Time.< (t, min) then t else min,
                             max = if Time.> (t, max) then t else max,
                             last = t,
                             updated = finishedAt,
                             count = count + 1 })

    fun record tag t =
        recordAt tag (t, Time.now ())

    val formatTime = TimingFormat.formatElapsedTime
    val formatTimePadded = TimingFormat.formatElapsedTimePadded 12
    val formatTimePerSec = TimingFormat.formatElapsedTimePerSec
                 
    fun summarise level =
        let open Log
            fun summariseOne tag =
                let val { total, min, max, count, ... } =
                        H.lookup aggregates tag
                    val mean = Time.fromReal (Time.toReal total / real count)
                in
                    log level (fn () => [tag]);
                    log level (fn () => ["%1 total (%2 %3)",
                                         formatTimePadded total,
                                         I count,
                                         if count = 1 then "call" else "calls"]);
                    if count > 1
                    then (log level (fn () => ["%1 worst-case (%2)",
                                               formatTimePadded max,
                                               formatTimePerSec max]);
                          log level (fn () => ["%1 mean (%2)",
                                               formatTimePadded mean,
                                               formatTimePerSec mean])
                         )
                    else ()
                end
                handle NotFound =>
                       warn (fn () => ["tag %1 not found in aggregates", tag])
        in
            log level (fn () => ["Aggregate times:"]);
            List.app summariseOne (!summaryOrder)
        end
                                                                       
    fun timed tag f =
        let open Log
            val start = Time.now ()
            val result = f ()
            val finish = Time.now ()
            val elapsed = Time.- (finish, start)
            val () = recordAt tag (elapsed, finish)
            val () = Log.debug
                         (fn () =>
                             ["%1: %2 (%3)",
                              tag,
                              formatTime elapsed,
                              formatTimePerSec elapsed
                         ])
        in
            result
        end
                                                                       
    fun summariseWhenBudgetBlown level elapsed =
        let open Log
            val now = Time.now ()
            fun summariseOne tag =
                let val { total, last, updated, count, ... } =
                        H.lookup aggregates tag
                    val mean = Time.fromReal (Time.toReal total / real count)
                    val ago = Time.- (now, updated)
                in
                    if Time.< (ago, elapsed)
                    then (log level (fn () => [tag]);
                          log level (fn () => ["%1 last (completed %2 ago; %3)",
                                               formatTimePadded last,
                                               formatTime ago,
                                               formatTimePerSec last]);
                          log level (fn () => ["%1 mean (%2 %3)",
                                               formatTimePadded mean,
                                               I count,
                                               if count = 1
                                               then "call"
                                               else "calls"]))
                    else ()
                end
                handle NotFound =>
                       warn (fn () => ["tag %1 not found in aggregates", tag])
        in
            log level (fn () => ["Calls completing within last %1, by descending last-call duration:",
                                 formatTime elapsed]);
            List.app summariseOne
                     (ListMergeSort.sort
                          (fn (t1, t2) =>
                              Time.< (#last (H.lookup aggregates t1),
                                      #last (H.lookup aggregates t2)))
                          (! summaryOrder))
        end
        handle NotFound =>
               Log.warn (fn () => ["tag in summaryOrder not found in aggregates"])
                                         
    fun timedToBudget (tag, budget) f =
        let open Log
            val start = Time.now ()
            val result = f ()
            val finish = Time.now ()
            val elapsed = Time.- (finish, start)
            val () = recordAt tag (elapsed, finish)
            val () = Log.debug
                         (fn () =>
                             ["%1: %2 (%3)",
                              tag,
                              formatTime elapsed,
                              formatTimePerSec elapsed
                         ])
            val () = if Time.> (elapsed, budget)
                     then (Log.warn
                               (fn () =>
                                   ["%1: elapsed time of %2 (%3) %4 budget of %5 (%6)",
                                    tag,
                                    formatTime elapsed,
                                    formatTimePerSec elapsed,
                                    if Time.toReal elapsed >
                                       Time.toReal budget * 10.0
                                    then "wildly exceeds"
                                    else "exceeds",
                                    formatTime budget,
                                    formatTimePerSec budget
                               ]);
                           summariseWhenBudgetBlown Log.WARN elapsed)
                      else ();
        in
            result
        end
            
end
