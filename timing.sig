
signature TIMING = sig

    type tag = string
                   
    val timed : tag -> (unit -> 'a) -> 'a

    val record : tag -> Time.time -> unit

    val summarise : unit -> unit
                                           
end
