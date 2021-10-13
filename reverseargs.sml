fun reverse [] = []
  | reverse (h :: t) = reverse t @ [h];

fun concat_space s = s ^ " ";

val _ =
  let
    val args = CommandLine.arguments()
  in
    map (print o concat_space) (reverse args);
    print "\n"
  end;