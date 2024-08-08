!(assertEqual (stringToChars "xyzzy") ('x' 'y' 'z' 'z' 'y'))

!(assertEqual (charsToString ('x' 'y' 'z' 'z' 'y')) "xyzzy")

!(assertEqual (stringToChars "") ())

!(assertEqual (charsToString ()) "")

;; this one works differently in metta and mettalog
!(assertEqual (charsToString (stringToChars "xyzzy")) "xyzzy")

!(assertEqual (stringToChars (charsToString ('x' 'y' 'z' 'z' 'y'))) ('x' 'y' 'z' 'z' 'y'))
