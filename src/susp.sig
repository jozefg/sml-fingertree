signature SUSPENSION =
sig
  type 'a t

  val susp : (unit -> 'a) -> 'a t
  val view : 'a t -> 'a
end
