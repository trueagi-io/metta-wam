
```
deb12user@GODLIKE:~/metta-wam/tests/zebra$ time mettalog fish_riddle_2_no_states.metta
!(include dv-pl)

Deterministic: ()
!(query &self (because red house keeps dogs the $norwegian is the fish owner))

Deterministic: (Succeed ((quote (because red house keeps dogs the norwegian is the fish owner))))
!(query &self (because blue house keeps dogs the $brit is the fish owner))

Deterministic: (Succeed ((quote (because blue house keeps dogs the brit is the fish owner))))
!(query &self (because red house keeps cats the $norwegian is the fish owner))

Deterministic: (Succeed ((quote (because red house keeps cats the norwegian is the fish owner))))
% 4,329,368 inferences, 0.375 CPU in 0.377 seconds (100% CPU, 11540291 Lips)

[()]

[(Succeed ((quote (because red house keeps dogs the norwegian is the fish owner))))]

[(Succeed ((quote (because blue house keeps dogs the brit is the fish owner))))]

[(Succeed ((quote (because red house keeps cats the norwegian is the fish owner))))]
; DEBUG Exit code of METTA_CMD: 7

real    0m3.445s
user    0m3.356s
sys     0m0.106s
```

```
(venv) deb12user@GODLIKE:~/metta-wam/tests/zebra$ time metta fish_riddle_2_no_states.metta
[(Succeed ((quote (because red house keeps dogs the norwegian is the fish owner)) (quote (because red house keeps dogs the norwegian is the fish owner))))]
[(Succeed ((quote (because blue house keeps dogs the brit is the fish owner)) (quote (because blue house keeps dogs the brit is the fish owner))))]
[(Succeed ((quote (because red house keeps cats the norwegian is the fish owner)) (quote (because red house keeps cats the norwegian is the fish owner))))]

real    2m52.320s
user    2m52.054s
sys     0m0.260s

```


