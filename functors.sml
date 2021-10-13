functor MakeSet (eqtype t; val lt: t * t -> bool):
  sig
    type ''a set
    val empty: t set
    val add: t set -> t -> t set
    val delete: t set -> t -> t set
    val contains: t set * t -> bool
  end =
struct
  datatype ''a set = Empty | Node of ''a * ''a set * ''a set
  val empty = Empty;
  fun add Empty x = Node (x, Empty, Empty)
    | add (s as Node(y, left, right)) x =
        if (x = y) then s
        else if lt(x, y) then Node(y, add left x, right)
        else Node(y, left, add right x);
  fun delete s x = raise (Fail "delete not implemented");
  fun contains (Empty, x) = false
    | contains (Node(y, left, right), x) =
        if lt(x, y) then contains(left, x)
        else if lt(y,x) then contains(right, x)
        else true;
end;