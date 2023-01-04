type opComp =
  Eq | Neq | Lt | Le | Gt | Ge

  

exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string