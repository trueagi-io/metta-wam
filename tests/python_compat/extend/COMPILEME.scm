(import (chicken platform) (chicken foreign) (chicken string))

(define-external (mattamorph_init) void
                 (eval '(import amb amb-extras)))

(define-external (mettamorph (c-string arg1)) c-string
                 (let ((result (eval (list 'amb-collect (read (open-input-string arg1))))))
                      (if (symbol? result)
                          (string-append "'" (->string result))
                          (if (boolean? result)
                              (if result "True" "False")
                              (if (string? result)
                                  (string-append (string-append "\"" result) "\"")
                                  (->string result))))))

(return-to-host)
