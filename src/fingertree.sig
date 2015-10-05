signature FINGERTREE =
sig
  type 'a t

  val empty    : 'a t
  val tabulate : int -> (int -> 'a) -> 'a t
  val fromList : 'a list -> 'a t
  val toList   : 'a t -> 'a list

  val size : 'a t -> int

  val cons : 'a -> 'a t -> 'a t
  val snoc : 'a t -> 'a -> 'a t

  val map    : ('a -> 'b)   -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t

  val foldr  : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val foldl  : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val reduce : ('a * 'a -> 'a) -> 'a -> 'a t -> 'a

  val head : 'a t -> 'a option
  val last : 'a t -> 'a option
  val tail : 'a t -> 'a t option
  val init : 'a t -> 'a t option
  val null : 'a t -> bool
  val nth  : int -> 'a t -> 'a option
  val rev  : 'a t -> 'a t

  datatype 'a list_view = NIL | CONS of 'a * 'a t
  val consView : 'a t -> 'a list_view
  val snocView : 'a t -> 'a list_view

  datatype 'a tree_view = LEAF | NODE of 'a t * 'a * 'a t
  val splitAt : int -> 'a t -> 'a tree_view option
  val split : 'a t -> 'a tree_view

  val >< : 'a t * 'a t -> 'a t
end
