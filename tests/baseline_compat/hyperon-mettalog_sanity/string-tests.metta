!(stringToChars "xyzzy")

!(charsToString ('x' 'y' 'z' 'z' 'y'))

!(stringToChars "")

!(charsToString ())

; this one works differently in metta and mettalog
!(charsToString (stringToChars "xyzzy"))

!(stringToChars (charsToString ('x' 'y' 'z' 'z' 'y')))
