structure Validation :> Validation =
struct

exception ValidationError of string;

open Unittest

fun valid proof = proofValidation proof;

end
