local
  datatype 'a set = null | ins of 'a * 'a set
in
  type 'a set = 'a set
  val emptyset = null
  val addset = ins
  fun memberset (x, null) = false
    | memberset (x, ins (v, s)) = x = v orelse memberset (x, s)
end;

abstype 'a set = null | ins of 'a * 'a set
with
  val emptyset = null
  val addset = ins
  fun memberset (x, null) = false
    | memberset (x, ins (v, s)) = x = v orelse memberset (x, s)
  local
    fun subset (null, _) = true
      | subset (ins (x, s1), s2) =
        memberset (x, s1) andalso subset (s1, s2)
  in
    fun equalset (s1, s2) = subset (s1, s2) andalso subset (s2, s1)
  end
end;

fun allmembers ([], _) = true
  | allmembers (h::t, s) = memberset (h, s) andalso allmembers (t, s);
This function has type (''a list * ''a set) -> bool. They may also be returned from functions as results. The following function has type ('a list * 'a set) -> 'a set.

fun addmembers ([], s) = s
  | addmembers (h::t, s) = addset (h, addmembers (t, s));