
(** Call a function and log how long it takes. Remembers the timings
    to report on in an aggregated summary, and can optionally also log
    immediately on function return.
*)
signature TIMING = sig

    (** Label for a specific function invocation point.
     *)
    type tag = string

    (** Call the given function, time how long it takes, and record
        the time against the given tag. The time is also logged
        immediately if DEBUG log level is active.
     *)
    val timed : tag -> (unit -> 'a) -> 'a

    (** Call the given function, time how long it takes, and record
        the time against the given tag. The time is also logged
        immediately if DEBUG log level is active. If the time taken
        exceeds the given budget, log immediately with the WARN log
        level.
     *)
    val timedToBudget : tag * Time.time -> (unit -> 'a) -> 'a

    (** Associate the given time with the given tag, as if we had just
        timed an invocation of it.
     *)
    val record : tag -> Time.time -> unit

    (** Output a summary of recorded timings to the given log level.
     *)
    val summarise : Log.level -> unit
                                           
end
