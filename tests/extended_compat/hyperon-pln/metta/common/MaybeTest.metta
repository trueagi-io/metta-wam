!(import! &self Maybe.metta)

;; Test Maybe
!(Maybe< (Just 10) (Just 20))
!(Maybe< (Just 10) Nothing)
!(Maybe< Nothing (Just 10))
