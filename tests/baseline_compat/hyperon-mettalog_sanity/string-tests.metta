;; String <=> List of Characters

!(assertEqual (stringToChars "xyzzy") ('x' 'y' 'z' 'z' 'y'))

!(assertEqual (charsToString ('x' 'y' 'z' 'z' 'y')) "xyzzy")

!(assertEqual (stringToChars "") ())

!(assertEqual (charsToString ()) "")

;; this one works differently in metta and mettalog
!(assertEqual (charsToString (stringToChars "xyzzy")) "xyzzy")

!(assertEqual (stringToChars (charsToString ('x' 'y' 'z' 'z' 'y'))) ('x' 'y' 'z' 'z' 'y'))

;; metta gets all these wrong
!(assertEqual (charsToString ('x' 'y' ' ' 'z' 'y')) "xy zy")

!(assertEqual (stringToChars "xy zy") ('x' 'y' ' ' 'z' 'y'))

!(assertEqual (charsToString (' ')) " ")

!(assertEqual (stringToChars " ") (' '))

!(assertEqual (stringToChars (superpose ("ab" "cd" "ef"))) (superpose (('a' 'b') ('c' 'd') ('e' 'f'))))

;; format-args

!(assertEqual (format-args "" (1 2 3)) "")

!(assertEqual (format-args " " (1 2 3)) " ")

!(assertEqual (format-args "{}" (1 2 3)) "1")

!(assertEqual (format-args "{}}" (1 2 3)) "{}")

!(assertEqual (format-args "xyz zy" (1 2 3)) "xyz zy")

!(assertEqual (format-args "Indexed {1} {} {0} {}" (1 2 3)) "Indexed 2 1 1 2")

!(assertEqual (format-args "Different types {} {} {} {} {} {}" (1 "2" 'c' -0.5 atom (1 2 c -0.5 atom))) "Different types 1 2 c -0.5 atom (1 2 c -0.5 atom)")

!(assertEqual (format-args "Two digit index={11}" (0 1 2 3 4 5 6 7 8 9 10 "eleven")) "Two digit index=eleven")

;; malformed examples
!(assertEqual (format-args "Bad args list {1} {} {0} {}" x) (Error x BadType))

!(assertEqual (format-args "Malformed format}{{}{{{}{} {4} { } {-1} {x} {{{{{{}}}}}}{{{{{}}}}}" ("success1" "success2")) "Malformed format}{}{success1success2 {4} { } {-1} {x} {{{}}}{{{}}}")
