signature Validation =
sig

  exception ValidationError of string

  val valid : Proof.proof -> bool
  
  val dummy : Proof.proof

  val english : Proof.proof -> string

end
