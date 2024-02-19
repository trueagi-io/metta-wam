;; Import TruthValue
!(import! &self EvidentialTruthValue.metta)

!(mode (ETV (Singleton 42) (STV 0.1 0.2)))        ; 0.1
!(mean (ETV (Singleton 42) (STV 0.1 0.9)))        ; 0.14
!(confidence (ETV (Singleton 42) (STV 0.1 0.2)))  ; 0.2
!(count (ETV (Singleton 42) (STV 0.1 0.9)))       ; 9.000000000000002
