signature FINGERTREE =
sig
  type 'a t

  val fromList : 'a list -> 'a t
  val toList : 'a t -> 'a list

  val cons : 'a -> 'a t -> 'a t
  val snoc : 'a t -> 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val head : 'a t -> 'a option
  val last : 'a t -> 'a option
end
