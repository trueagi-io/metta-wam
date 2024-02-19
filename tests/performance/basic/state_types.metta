


(: ABType Type)
(: AType Type)
(: A ABType)
(: A AType)
(: B ABType)


!("Bind A-first with A")
!(bind! &a-first (new-state A))
!("(get-type A-first)=" (get-type (get-state &a-first)))
!("Change A-first into B")
!(change-state! &a-first B)
!("(get-type A-first)=" (get-type (get-state &a-first)))


!("Bind B-first with B")
!(bind! &b-first (new-state B))
!("(get-type B-first)=" (get-type (get-state &b-first)))
!("Change B-first into A")
!(change-state! &b-first A)
!("(get-type B-first)=" (get-type (get-state &b-first)))



!("Bind False-first with False")
!(bind! &false-first (new-state False))
!("(get-type False-first)=" (get-type (get-state &false-first)))
!("Change False-first into Sometimes")
!(change-state! &false-first Sometimes)
!("(get-type False-first)=" (get-type (get-state &false-first)))




; Also works for custom types
(: Either Type)
!(assertEqual
  (get-type Either)
  Type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Arrow types of functions and constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: Left (-> %Undefined% Either))
(: Right (-> %Undefined% Either))

!(assertEqual
  (get-type (Left 5))
  Either)

(: isLeft (-> Either Bool))
(= (isLeft (Left $x)) True)
(= (isLeft (Right $x)) False)


!(assertEqual (get-type (isLeft (Right 5))) Bool)

!(assertEqual
  (get-type (isLeft (Right 5)))
  (get-type (get-state (new-state  (isLeft (Right 5))))))

!(bind! &is_left (isLeft (Right 5)))
!(bind! &right_7 (Right 7))

!("(get-type (isLeft (Right 5)))=" (get-type (isLeft (Right 5))))
!("(get-type (get-state (new-state  (isLeft (Right 5)))))=" (get-type (get-state (new-state  (isLeft (Right 5))))))
!("(get-type (get-state (new-state (Left  6))))=" (get-type (get-state (new-state (Left  6)))) )
!("(get-type (get-state (new-state (Right 7))))=" (get-type (get-state (new-state (Right 7)))) )
!("(get-type &is_left)=" &is_left (get-type &is_left))
!("(get-type &right_7)=" &right_7 (get-type &right_7))

!("bind &right_7-first with &right_7")
!(bind! &right_7-first (new-state &right_7))
!("change &right_7-first into &is_left" (change-state! &right_7-first &is_left))
!("(get-type (get-state &right_7-first))=(" (get-state &right_7-first) ")=" (get-type (get-state &right_7-first)))
!("(get-type &right_7-first)=" (get-type (get-state &right_7-first)))



