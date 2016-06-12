signature Validation =
sig

  exception ValidationError of string

  val valid : Proof.proof -> bool * string
  
  val dummy : Proof.proof

  val english : Proof.proof -> string

end
