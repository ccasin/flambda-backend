(* TEST
 readonly_files = "a.ml b.ml c.ml";
 subdirectories = "subdir";
 setup-ocamlc.byte-build-env;
 module = "a.ml";
 ocamlc.byte;
 module = "b.ml";
 ocamlc.byte;
 module = "c.ml";
 ocamlc.byte;
 script = "rm -f a.cmi c.cmi c.cmo";
 module = "c.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Note this tests also checks that serialization of kinds works as expected,
   as we check that things work before deleting a. *)
