; We can insert templates into other templates to
; compose and reuse them
(Script 04_prompt_template.mps)

; Prompt templates are evaluate in the context of the parent agent,
; and can be parameterized by expressions, which should be defined outside
(topic)

; Ordinary Metta expressions will be also evaluated by metta-motto
(if (== (story) None)
    ()
    (assistant (story)))


