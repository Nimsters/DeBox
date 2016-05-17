use "testproof.ml";
use "testproof_numbers.ml";

local
val step_test0 = proofstepsToString (steps, [])
val step_test1 = printl step_test0
val proof_test = printl (proofToString proof)
val proof_numbers = printl (proofToString proofN)
in
end;
quit();
