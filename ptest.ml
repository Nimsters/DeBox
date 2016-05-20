use "testproof.ml";
use "testproof_numbers.ml";
use "testproof_invalid.ml";

local
val step_test0          = proofstepsToString (steps, [])
val step_test1          = printl step_test0
val print_proof         = printl (proofToString proof)
val print_num_proof     = printl (proofToString proofN)
val print_inv_proof     = printl (proofToString proofI)
in
val test_validation_valid   = proofValidation(proof)
val test_validation_invalid = proofValidation(proofI)
end;
quit();
