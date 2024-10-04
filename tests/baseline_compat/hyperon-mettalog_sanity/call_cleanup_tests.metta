;; setup

!(bind! &var (new-state 0))
(= (increment)
   (sequential ((do (change-state! &var (+ 1 (get-state &var))))
               (get-state &var))))

;; call-cleanup! returns result of Form, calls cleanup

!(assertEqualToResult (sequential ((call-cleanup! (increment) 
                                                  (increment))
                                   (increment))) 
                     (1 3))

;; cleanup

!(change-state! &var 0)

;; call-cleanup! still calls cleanup if evaluation errors

!(assertEqualToResult (catch (call-cleanup! (throw foo) 
                                            (increment)) 
                              foo 
                              (increment)) 
                     (2))

;; setup-call-cleanup! does Setup, then Form, then Cleanup, returning result of Form

!(assertEqualToResult (sequential ((setup-call-cleanup! (change-state! &var 0) 
                                                        (increment) 
                                                        (increment)) 
                                   (increment))) 
                     (1 3))

;; setup-call-cleanup! Setup is protected from interrupts, Cleanup is called if Setup is interrupted (but Form is not)

!(assertEqualToResult (catch (max-time! 1 
                                        (setup-call-cleanup! (sequential ((sleep! 2)
                                                                          (change-state! &var 0)))
                                                             ((increment) (increment))
                                                             (increment)))
                             time_limit_exceeded
                             (time_limit_exceeded (get-state! &var)))
                      ((time_limit_exceeded 1)))

!(assertEqualToResult (not (== (get-state! &var) 0)) (True))

!(assertEqualToResult (catch (max-time! 1 
                                        (setup-call-cleanup! ((sleep! 2)
                                                              (change-state! &var 0))
                                                             ((increment) (increment))
                                                             (increment)))
                             time_limit_exceeded
                             (time_limit_exceeded (get-state! &var)))
                      ((time_limit_exceeded 1)))
