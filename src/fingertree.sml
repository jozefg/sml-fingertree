structure FingerTree : FINGERTREE =
struct
  datatype 'a node
           = LIFT of 'a
           | NODE2 of 'a node * 'a node
           | NODE3 of 'a node * 'a node * 'a node

  fun nmap f (LIFT a) = LIFT (f a)
    | nmap f (NODE2 (a, b)) = NODE2 (nmap f a, nmap f b)
    | nmap f (NODE3 (a, b, c)) = NODE3 (nmap f a, nmap f b, nmap f c)

  fun ntoList (LIFT a) xs = a :: xs
    | ntoList (NODE2 (a, b)) xs = ntoList a (ntoList b xs)
    | ntoList (NODE3 (a, b, c)) xs = ntoList a (ntoList b (ntoList c xs))

  fun nhead (LIFT a) = a
    | nhead (NODE2 (a, b)) = nhead a
    | nhead (NODE3 (a, b, c)) = nhead a

  fun nlast (LIFT a) = a
    | nlast (NODE2 (a, b)) = nlast b
    | nlast (NODE3 (a, b, c)) = nlast c

  datatype 'a t
    = EMPTY
    | SINGLE of 'a node
    | DEEP of 'a node list * 'a t Susp.t * 'a node list

  fun cons' a EMPTY : 'a t = SINGLE a
    | cons' a (SINGLE b) = DEEP ([a], Susp.susp (fn () => EMPTY), [b])
    | cons' a (DEEP ([b, c, d, e], m, sf)) =
      DEEP ([a, b], Susp.susp (fn () => cons' (NODE3 (c, d, e)) (Susp.view m)), sf)
    | cons' a (DEEP (pr, m, sf)) = DEEP (a :: pr, m, sf)

  fun cons a t = cons' (LIFT a) t

  fun snoc' EMPTY a = SINGLE a
    | snoc' (SINGLE b) a = DEEP ([a], Susp.susp (fn () => EMPTY), [b])
    | snoc' (DEEP (pf, m, [e, d, c, b])) a =
      DEEP (pf, Susp.susp (fn () => snoc' (Susp.view m) (NODE3 (e, d, c))), [b, a])
    | snoc' (DEEP (pr, m, sf)) a = DEEP (pr, m, sf @ [a])

  fun snoc t a = snoc' t (LIFT a)

  fun ncons (LIFT a) t = cons a t
    | ncons (NODE2 (a, b)) t = cons' a (cons' b t)
    | ncons (NODE3 (a, b, c)) t = cons' a (cons' b (cons' c t))

  fun nsnoc (LIFT a) t = snoc t a
    | nsnoc (NODE2 (a, b)) t = snoc' (snoc' t a) b
    | nsnoc (NODE3 (a, b, c)) t = snoc' (snoc' (snoc' t a) b) c

  fun fromList xs = List.foldr (fn (a, b) => cons a b) EMPTY xs
  fun toList' EMPTY xs = xs
    | toList' (SINGLE a) xs = ntoList a xs
    | toList' (DEEP (pf, m, sf)) xs =
      let
        val xs' = List.foldr (fn (n, xs) => ntoList n xs) xs sf
        val xs'' = toList' (Susp.view m) xs'
      in
        List.foldr (fn (n, xs) => ntoList n xs) xs'' pf
      end
  fun toList t = toList' t []

  fun tabulate 0 f = EMPTY
    | tabulate n f = snoc (tabulate (n - 1) f) (f n)

  fun map f EMPTY = EMPTY
    | map f (SINGLE a) = SINGLE (nmap f a)
    | map f (DEEP (pf, m, sf)) =
      DEEP ( List.map (nmap f) pf
           , Susp.susp (fn () => map f (Susp.view m))
           , List.map (nmap f) sf)

  datatype 'a list_view = NIL | CONS of 'a * 'a t
  datatype 'a list_view' = NIL' | CONS' of 'a node * 'a t

  fun consView' EMPTY = NIL'
    | consView' (SINGLE x) = CONS' (x, EMPTY)
    | consView' (DEEP (p :: pr, m, sf)) =
      case pr of
          _ :: _ => CONS' (p, DEEP (pr, m, sf))
        | [] =>
          case consView' (Susp.view m) of
              CONS' (a, t) => CONS' (p, DEEP ([a], Susp.susp (fn () => t), sf))
            | NIL' => CONS' (p, List.foldl (fn (a, b) => nsnoc a b) EMPTY sf)

  fun snocView' EMPTY = NIL'
    | snocView' (SINGLE x) = CONS' (x, EMPTY)
    | snocView' (DEEP (pf, m, sf)) =
      let
        val s = List.last sf
      in
        case List.take (sf, List.length sf - 1) of
            _ :: _ => CONS' (s, DEEP (pf, m, sf))
          | [] =>
            case snocView' (Susp.view m) of
                CONS' (a, t) => CONS' (s, DEEP (pf, Susp.susp (fn () => t), [a]))
              | NIL' => CONS' (s, List.foldl (fn (a, b) => ncons a b) EMPTY pf)
      end

  fun consView t =
      case consView' t of
          CONS' (LIFT a, t) => CONS (a, t)
        | NIL' => NIL

  fun snocView t =
      case snocView' t of
          CONS' (LIFT a, t) => CONS (a, t)
        | NIL' => NIL

  fun head EMPTY = NONE
    | head (SINGLE a) = SOME (nhead a)
    | head (DEEP (pf, _, _)) = SOME (nhead (List.hd pf))

  fun last EMPTY = NONE
    | last (SINGLE a) = SOME (nlast a)
    | last (DEEP (_, _, sf)) = SOME (nlast (List.last sf))

  fun tail t =
      case consView t of
          CONS (_, t') => SOME t'
        | NIL => NONE

  fun init t =
      case snocView t of
          CONS (_, t') => SOME t'
        | NIL => NONE

  fun null t =
      case consView t of
          NIL => true
        | _ => false

  fun nodes [a, b] = [NODE2 (a, b)]
    | nodes [a, b, c] = [NODE3 (a, b, c)]
    | nodes [a, b, c, d] = [NODE2 (a, b), NODE2 (c, d)]
    | nodes (a :: b :: c :: xs) = NODE3 (a, b, c) :: nodes xs

  fun joinWith EMPTY xs t2 = List.foldl (fn (a, b) => ncons a b) t2 xs
    | joinWith t1 xs EMPTY = List.foldl (fn (a, b) => nsnoc a b) t1 xs
    | joinWith (SINGLE a) xs t2 = ncons a (List.foldl (fn (a, b) => ncons a b) t2 xs)
    | joinWith t1 xs (SINGLE a) = nsnoc a (List.foldl (fn (a, b) => nsnoc a b) t1 xs)
    | joinWith (DEEP (pf1, m1, sf1)) xs (DEEP (pf2, m2, sf2)) =
      DEEP (pf1,
            Susp.susp (fn () =>
              joinWith (Susp.view m1) (nodes (sf1 @ xs @ pf2)) (Susp.view m2)),
            sf2)

  fun >< (t1, t2) = joinWith t1 [] t2
end
