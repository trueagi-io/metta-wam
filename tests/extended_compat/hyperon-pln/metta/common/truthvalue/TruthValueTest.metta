;; Import TruthValue
!(import! &self TruthValue.metta)

!(assertEqual (count->confidence 1) 0.5)
!(assertEqual (count->confidence 10) 0.9090909090909091)
!(assertEqual (confidence->count 0.9) 9.000000000000002)
!(assertEqual (mode (STV 0.1 0.2)) 0.1)
!(assertEqual (mean (STV 0.1 0.9)) 0.14)
!(assertEqual (confidence (STV 0.1 0.2)) 0.2)
!(assertEqual (count (STV 0.1 0.9)) 9.000000000000002)
!(assertEqual (inc-neg-count (STV 1 0)) (STV 0 0.5))
!(assertEqual (inc-pos-count (STV 1 0)) (STV 1 0.5))
