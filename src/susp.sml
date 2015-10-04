structure Susp :> SUSPENSION =
struct
  type 'a t = unit -> 'a

  fun susp f =
    let
      val cache = ref NONE
    in
      fn () =>
         case !cache of
             NONE => (cache := SOME (f ()); Option.valOf (!cache))
          |  SOME a =>  a
    end

  fun view s = s ()
end
