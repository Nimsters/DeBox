structure Validation :> Validation =
struct

exception ValidationError of string;

open Proof

fun valid proof = true;

end
