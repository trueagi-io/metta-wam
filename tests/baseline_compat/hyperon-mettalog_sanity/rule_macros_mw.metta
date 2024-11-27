
!(pragma! source-macros True)

(==> a b)
a

!(assertEqual (match &self b b) b)
; !(assertEqualToResult (match &self $x $x) (a b &corelib &stdlib))