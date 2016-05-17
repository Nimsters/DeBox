use "testproof.ml";

local
val step_test0 = proofstepsToString (steps, [])
val step_test1 = printl step_test0
val proof_test = printl (proofToString proof)
in
end;
quit();
