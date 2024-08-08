!(stringToChars "xyzzy")

!(charsToString ('x' 'y' 'z' 'z' 'y'))

!(stringToChars "")

!(charsToString ())

!(charsToString (stringToChars "xyzzy"))

!(stringToChars (charsToString ('x' 'y' 'z' 'z' 'y')))
