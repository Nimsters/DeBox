use "testproof.ml";

local
val step_test0 = map proofstepToString steps
val step_test1 = map printl step_test0
val proof_test = printl (proofToString proof)
in
end;
quit();
