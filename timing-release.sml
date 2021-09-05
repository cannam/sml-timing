
(* Release builds that don't want to include timing measurements may
   wish to have this included after timing.sml, in order to eliminate
   the timing calls without having to change the rest of the code. *)

structure Timing : TIMING = struct

    type tag = string

    fun record tag t = ()
    fun timed tag f = f ()
    fun timedToBudget (tag, budget) f = f ()
    fun summarise level = ()
            
end
