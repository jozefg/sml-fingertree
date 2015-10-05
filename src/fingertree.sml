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

  fun nmeasure (LIFT a) = 1
    | nmeasure (NODE2 (a, b)) = nmeasure a + nmeasure b
    | nmeasure (NODE3 (a, b, c)) = nmeasure a + nmeasure b + nmeasure c

  fun flatten (LIFT a) = [LIFT a]
    | flatten (NODE2 (a, b)) = [a, b]
    | flatten (NODE3 (a, b, c)) = [a, b, c]

  datatype 'a t
    = EMPTY
    | SINGLE of 'a node
    | DEEP of int Susp.t * 'a node list * 'a t Susp.t * 'a node list

  val empty = EMPTY

  fun size EMPTY = 0
    | size (SINGLE a) = nmeasure a
    | size (DEEP (v, _, _, _)) = Susp.view v

  fun deep (pf, m, sf) =
      DEEP (Susp.susp (fn () =>
              List.foldl (fn (a, b) => nmeasure a + b) 0 pf +
              List.foldl (fn (a, b) => nmeasure a + b) 0 sf +
              size (Susp.view m)),
            pf, m, sf)

  fun cons' a EMPTY : 'a t = SINGLE a
    | cons' a (SINGLE b) = deep ([a], Susp.susp (fn () => EMPTY), [b])
    | cons' a (DEEP (_, [b, c, d, e], m, sf)) =
      deep ([a, b], Susp.susp (fn () => cons' (NODE3 (c, d, e)) (Susp.view m)), sf)
    | cons' a (DEEP (_, pr, m, sf)) = deep (a :: pr, m, sf)

  fun cons a t = cons' (LIFT a) t

  fun snoc' EMPTY a = SINGLE a
    | snoc' (SINGLE b) a = deep ([b], Susp.susp (fn () => EMPTY), [a])
    | snoc' (DEEP (_, pf, m, [e, d, c, b])) a =
      deep (pf, Susp.susp (fn () => snoc' (Susp.view m) (NODE3 (e, d, c))), [b, a])
    | snoc' (DEEP (_, pr, m, sf)) a = deep (pr, m, sf @ [a])

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
    | toList' (DEEP (_, pf, m, sf)) xs =
      let
        val xs' = List.foldr (fn (n, xs) => ntoList n xs) xs sf
        val xs'' = toList' (Susp.view m) xs'
      in
        List.foldr (fn (n, xs) => ntoList n xs) xs'' pf
      end
  fun toList t = toList' t []

  fun tabulate 0 f = EMPTY
    | tabulate n f = snoc (tabulate (n - 1) f) (f (n - 1))

  fun map f EMPTY = EMPTY
    | map f (SINGLE a) = SINGLE (nmap f a)
    | map f (DEEP (v, pf, m, sf)) =
      DEEP ( v
           , List.map (nmap f) pf
           , Susp.susp (fn () => map f (Susp.view m))
           , List.map (nmap f) sf)

  datatype 'a list_view = NIL | CONS of 'a * 'a t
  datatype 'a list_view' = NIL' | CONS' of 'a node * 'a t

  fun deepl pr m sf =
      case pr of
          _ :: _ => deep (pr, m, sf)
        | [] =>
          case consView' (Susp.view m) of
              CONS' (a, t) => deep (flatten a, Susp.susp (fn () => t), sf)
            | NIL' => List.foldl (fn (a, b) => nsnoc a b) EMPTY sf
  and consView' EMPTY = NIL'
    | consView' (SINGLE x) = CONS' (x, EMPTY)
    | consView' (DEEP (_, p :: pr, m, sf)) = CONS' (p, deepl pr m sf)


  fun deepr pf m sf =
      case sf of
            _ :: _ => deep (pf, m, sf)
          | [] =>
            case snocView' (Susp.view m) of
                CONS' (a, t) => deep (pf, Susp.susp (fn () => t), flatten a)
              | NIL' => List.foldl (fn (a, b) => ncons a b) EMPTY pf
  and snocView' EMPTY = NIL'
    | snocView' (SINGLE x) = CONS' (x, EMPTY)
    | snocView' (DEEP (_, pf, m, sf)) =
      let
        val s = List.last sf
      in
        CONS' (s, deepr pf m (List.take (sf, List.length sf - 1)))
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
    | head (DEEP (_, pf, _, _)) = SOME (nhead (List.hd pf))

  fun last EMPTY = NONE
    | last (SINGLE a) = SOME (nlast a)
    | last (DEEP (_, _, _, sf)) = SOME (nlast (List.last sf))

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
    | joinWith (DEEP (_, pf1, m1, sf1)) xs (DEEP (_, pf2, m2, sf2)) =
      deep (pf1,
            Susp.susp (fn () =>
              joinWith (Susp.view m1) (nodes (sf1 @ xs @ pf2)) (Susp.view m2)),
            sf2)

  infixr ><
  fun t1 >< t2 = joinWith t1 [] t2

  fun splitDigit p i [a] = ([], a, [])
    | splitDigit p i (x :: xs) =
      let
        val i' = i + nmeasure x
      in
        if p i'
        then ([], x, xs)
        else
          let
            val (ls, m, rs) = splitDigit p i' xs
          in
            (x :: ls, m, rs)
          end
      end

  fun split' p i (SINGLE a) = (EMPTY, a, EMPTY)
    | split' p i (DEEP (v, pf, m, sf)) =
      let
        val i' = List.foldl (fn (a, b) => nmeasure a + b) 0 pf + i
      in
        case (p i', p i' orelse p (i' + size (Susp.view m))) of
            (true, _) =>
            let
              val (ls, x, rs) = splitDigit p i pf
            in
              (List.foldr (fn (a, b) => ncons a b) EMPTY ls, x, deepl rs m sf)
            end
          | (false, true) =>
            let
              val (lt, x, rt) = split' p i' (Susp.view m)
              val (l, x, r) = splitDigit p (i' + size lt) (flatten x)
            in
              ( deepr pf (Susp.susp (fn () => lt)) l
              , x
              , deepl r (Susp.susp (fn () => rt)) sf)
            end
          | (false, false) =>
            let
              val (ls, x, rs) = splitDigit p i sf
            in
              (deepr pf m ls, x, List.foldr (fn (a, b) => ncons a b) EMPTY rs)
            end
      end


  datatype 'a tree_view = NODE of 'a t * 'a * 'a t | LEAF

  fun splitAt i EMPTY = SOME LEAF
    | splitAt i t =
      if size t < i
      then NONE
      else
        let
          val (l, LIFT x, r) = split' (fn j => i < j) 0 t
        in
          SOME (NODE (l, x, r))
        end

  fun split t = Option.valOf (splitAt (size t div 2) t)

  fun nth i t =
      case splitAt i t of
          SOME LEAF => NONE
        | SOME (NODE (_, x, _)) => SOME x
        | NONE => NONE

  fun filter f t =
    case split t of
        LEAF => EMPTY
      | NODE (l, a, r) =>
        if f a
        then filter f l >< cons a (filter f r)
        else filter f l >< filter f r
end
