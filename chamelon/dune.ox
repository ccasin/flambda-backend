(executable
 (name chamelon)
 (public_name chamelon)
 (modes native)
 (libraries ocamlcommon unix str
   (select compat.ml from ( -> compat.ox.ml))
 )
 (package ocaml)
)

(env
  (dev
    (flags (:standard -no-principal -warn-error -A -w -70)))
  (_
    (flags (:standard -no-principal -w -70))))

(include_subdirs unqualified)
