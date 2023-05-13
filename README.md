- This is a simple RSA implementation by OCaml.
- Encryption and decryption are based on ASCII code, which encrypts and decrypts one character at a time.
- Prime number generation is based on the Miller-Rabin primality test.
- The test here is not complete


You can use the following commands to test the program in terminal(with utop or ...)
```ocaml
let rc = config_input ()
let sk = private_key_gen rc
let pk = public_key_gen sk
let pt = plaintext_input ()
let ct = plaintext_encrypt pt pk
let _ = cipertext_output ct
let pt' = cipertext_decrypt ct sk
let _ = plaintext_output pt'
```

You can also use the following commands to use the program in terminal(with ocaml)
```ocaml
$ dune utop
utop # open Rsa;;
(command begins here)
```
