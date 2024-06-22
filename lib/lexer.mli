type t

val make : string -> t
val from_in_channel : in_channel -> t
val tokenize : t -> Token.t list
