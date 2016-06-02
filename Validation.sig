signature Validation =
sig

  val valid : Proof.proof -> bool * string

  exception ValidationError of string

end
