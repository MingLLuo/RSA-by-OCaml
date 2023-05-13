type rsa_config = {
    p_len : int;
    q_len : int;
    e_len : int;
}

type public_key = {
    n : Z.t;
    e : Z.t;
}
type private_key = {
    n : Z.t;
    p : Z.t;
    q : Z.t;
    e : Z.t;
    d : Z.t;
}
type plaintext = {
    message : Z.t list;
    types : string;
}
type cipertext = {
    c : Z.t list;
    types : string;
}

(* Helper Fuction *)
(* check gcd = 1 *)
let coprime a b =
  Z.gcd a b = Z.one

let rec prime_phi plist = 
  match plist with
  | [] -> Z.one
  | p::plist' -> Z.mul (Z.sub p Z.one) (prime_phi plist')

let mod_exp a b n = 
  Z.powm a b n
let mod_minv a n = 
  Z.invert a n
(* concat String to int *)
(* Can optimize *)
let z_gen (len:int) = 
    let rec z_gen' len acc = 
        if len = 0 then acc
        else z_gen' (len - 1) (Z.add (Z.mul acc (Z.of_int 10)) (Z.of_int (Random.int 10)))
    in z_gen' len Z.zero

(* for any a^{n-1} \equiv 1 \pmod n
   there will be a list with power [q, 2q, 4q, ..., 2^{k-1}q] 
   and abs(a^{2^kq}) \equiv 1 \pmod n
*)
let miller_rabin_test ?(trails = 50) n = 
  let rec get_factor_q num = 
  if Z.(mod) num (Z.of_int 2) = Z.zero
    then get_factor_q (Z.div num (Z.of_int 2))
  else num in
  if Z.of_int 2 = n 
    then true
  else
    let q = get_factor_q (Z.sub n Z.one) in
    let rec miller_rabin_test' trails = 
      if trails = 0 
        then true
      else 
        (* TODO, MORE RANDOM *)
        let a = Z.of_int64 (Random.int64 (Z.to_int64 (Z.min n (Z.of_int64 (Int64.max_int))))) in
        let rec miller_rabin_test'' expp = 
          if Z.abs (Z.powm a expp n) = Z.one then
            miller_rabin_test' (trails - 1)
          else
            if expp = Z.sub n Z.one 
              then false
            else if Z.(mod) (Z.powm a expp n) n = Z.sub n Z.one
              then miller_rabin_test'' (Z.mul expp (Z.of_int 2))
            else false
        in miller_rabin_test'' q
      in miller_rabin_test' trails

let prime_gen ?(trails = 50) len = 
  let rec prime_gen' len = 
    let p = z_gen len in
    if miller_rabin_test ~trails p 
      then p
      else prime_gen' len
  in prime_gen' len

let e_gen rc phi =
  let rec e_gen' phi = 
    let e = z_gen rc.e_len in
    if coprime e phi 
      then e
      else e_gen' phi
  in e_gen' phi

let d_gen e phi =
  mod_minv e phi

let private_key_gen rc = 
  let p = prime_gen rc.p_len in
  let q = prime_gen rc.q_len in
  let phi = Z.mul (Z.sub p Z.one) (Z.sub q Z.one) in
  let e = e_gen rc phi in
  let d = d_gen e phi in
  {n = Z.mul p q; p = p; q = q; e = e; d = d}


let public_key_gen pk = 
  {n = pk.n; e = pk.e}

let config_input () =
  let _ = print_string "Please input p_len: " in
  let p_len = read_int () in
  let _ = print_string "Please input q_len: " in
  let q_len = read_int () in
  let _ = print_string "Please input e_len: " in
  let e_len = read_int () in
  {p_len = p_len; q_len = q_len; e_len = e_len}

let plaintext_input () =
  let _ = print_string "Please input plaintext: " in
  let m = read_line () in
  let types = "String" in
  {message = List.map (fun x -> Z.of_int (Char.code x)) (String.to_seq m |> List.of_seq); types = types}

let plaintext_input_string s =
  {message = List.map (fun x -> Z.of_int (Char.code x)) (String.to_seq s |> List.of_seq); types = "String"}
  
let plaintext_encrypt pt (pk:public_key) =
  let rec plaintext_encrypt' m acc = 
    match m with
    | [] -> acc
    | h::t -> plaintext_encrypt' t (Z.powm h pk.e pk.n :: acc)
  in {c = plaintext_encrypt' pt.message []; types = pt.types}

let cipertext_decrypt ct sk =
  let rec cipertext_decrypt' c acc = 
    match c with
    | [] -> acc
    | c::c' -> cipertext_decrypt' c' (Z.powm c sk.d sk.n :: acc)
  in {message = cipertext_decrypt' ct.c []; types = ct.types}

let cipertext_output ct = 
  let _ = print_endline "Cipertext: " in
    let rec cipertext_output' c acc = 
      match c with
      | [] -> acc
      | c::c' -> cipertext_output' c' (acc ^ (Z.to_string c))
    in let _ = print_string (cipertext_output' ct.c "")
  in print_endline ""
  
let plaintext_output pt = 
  let _ = print_endline "Plaintext: " in
    let rec plaintext_output' m acc = 
      match m with
      | [] -> acc
      | m::m' -> plaintext_output' m' (acc ^ (Char.escaped (Char.chr (Z.to_int m))))
    in let _ = print_string (plaintext_output' pt.message "")
  in print_endline ""