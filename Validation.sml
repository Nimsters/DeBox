structure Validation :> Validation =
struct

exception ValidationError of string;

open Unittest

fun valid proof = proofValidation proof;

val dummy = dummyinput ();

fun english proof = proofToString proof;

end
