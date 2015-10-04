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
  val tail : 'a t -> 'a t option
  val init : 'a t -> 'a t option
  val null : 'a t -> bool

  datatype 'a list_view = NIL | CONS of 'a * 'a t
  val consView : 'a t -> 'a list_view
  val snocView : 'a t -> 'a list_view

end
