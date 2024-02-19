;;  Load the VSPACE python space

;; to start in MeTTaLog use    ./MeTTa 8-FlybaseQuestions.metta --repl

!(extend-py! mettalog)

!(fb-info!)

!(pwd!)
;; MeTTaLog only:
!(ensure-loaded! ../../data/whole_flybase_2)

!(fb-info!)



