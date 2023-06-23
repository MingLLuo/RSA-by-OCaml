type rsa_config = { p_len : int; q_len : int; e_len : int }
type public_key = { n : Z.t; e : Z.t }
type private_key = { n : Z.t; p : Z.t; q : Z.t; e : Z.t; d : Z.t }

(* types for recover *)
type plaintext = { message : Z.t list; types : string }
type cipertext = { c : Z.t list; types : string }

(* Helper Fuction *)
(* check gcd = 1 *)
val coprime : Z.t -> Z.t -> bool

(* generate Phi of primes list *)
val prime_phi : Z.t list -> Z.t
val mod_exp : Z.t -> Z.t -> Z.t -> Z.t
val mod_minv : Z.t -> Z.t -> Z.t

(* generate Z.t with length n *)
val z_gen : int -> Z.t
val plaintext_input : unit -> plaintext
val plaintext_input_string : string -> plaintext
val cipertext_output : cipertext -> unit
val plaintext_output : plaintext -> unit

(* generate prime with length n,
   use fermat test to check prime: probability *)
val miller_rabin_test : ?trails:int -> Z.t -> bool
val prime_gen : ?trails:int -> int -> Z.t
val e_gen : rsa_config -> Z.t -> Z.t
val d_gen : Z.t -> Z.t -> Z.t

(* Generate Private Key,
   with config of rsa input *)
val private_key_gen : rsa_config -> private_key

(* Generate Public Key,
   with n, e input *)
val public_key_gen : private_key -> public_key
val config_input : unit -> rsa_config

(* Encrypt plaintext,
   seperate by each char *)
val plaintext_encrypt : plaintext -> public_key -> cipertext

(* Decrypt plaintext *)
val cipertext_decrypt : cipertext -> private_key -> plaintext
