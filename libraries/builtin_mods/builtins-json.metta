; loaded with:  !(import! &self json)

(@doc get-value
  (@desc "Function takes space and key as input, checks if space contains key-value pairs in form of (key value) and returns value tied to the input key")
  (@params (
    (@param "Space")
    (@param "Key")))
  (@return "Value which tied to input key, empty if no such key in space"))
(: get-value (-> Grounded Atom Atom))
(= (get-value $dictspace $key) (match $dictspace ($key $value) $value))

(@doc get-keys
  (@desc "Function takes space and returns all keys from (<key> <value>) tuples in space")
  (@params (
    (@param "Space")))
  (@return "All keys in the input space"))
(: get-keys (-> Grounded Expression))
(= (get-keys $dictspace)
   (function
     (chain (unify $dictspace ($key $value) $key Empty) $t (return $t)) ))

(@doc dict-space
  (@desc "Function takes key-value pairs in form of expression as input, creates space and adds key-value pairs into it")
  (@params (
    (@param "Expression")))
  (@return "Space"))
(: dict-space (-> Expression Grounded))
(= (dict-space $expr) (chain (eval (new-space)) $dictspace (let $res (add-reducts $dictspace $expr) $dictspace)))

(@doc json-encode
  (@desc "Function takes atom as an input and encodes it to json-string. Atom could be a string, number, expression, space and combination of those")
  (@params (
    (@param "Input atom")))
  (@return "Json string"))

(@doc json-decode
  (@desc "Function takes json string as an input and decodes it to the metta objects (list to expression, dictionary to space which will contain key-value pairs in form of (key value), string to string, number to number)")
  (@params (
    (@param "Json string")))
  (@return "Metta object"))

            
!(assertEqual (json-encode 5) \"5\")
           !(assertEqual (json-encode (5 4 3)) \"[5, 4, 3]\")
            !(assertEqual (let $encoded (json-encode (5 4 3)) (json-decode $encoded)) (5 4 3))
            !(assertEqual (json-decode 5) (Error 5 BadType))
            !(assertEqual (json-decode \"[5, 4, 3]\") (5 4 3))
            !(bind! &dictspace (dict-space ((\"k1\" v1) (\"k2\" v2) (\"k3\" (4 \"a\" 5)))))
            !(assertEqual (let $decoded (let $encoded (json-encode &dictspace) (json-decode $encoded)) (get-keys $decoded)) (superpose (\"k1\" \"k2\" \"k3\")))
            !(assertEqual (json-encode True) \"true\")
            !(assertEqual (let $encoded (json-encode False) (json-decode $encoded)) False)
