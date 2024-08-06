
!(import! &self NARS)


;;NAL tests

!(|- ((Garfield --> cat) (1.0 0.9)) ((cat --> animal) (1.0 0.9)))
;expected: [((animal --> Garfield) (1.0 0.44751381215469616)), ((Garfield --> animal) (1.0 0.81))]

!(check-state)

!(|- ((Garfield --> carnivore) (1.0 0.9)) (((Garfield --> cat) ==> (Garfield --> carnivore)) (1.0 0.9)))
;expected: [((Garfield --> cat) (1.0 0.44751381215469616))]

!(|- (((ExtSet a b) --> mwawa) (1.0 0.9)))
;expected: [(((ExtSet a) --> mwawa) (1.0 0.81)), (((ExtSet b) --> mwawa) (1.0 0.81))]

!(|- (((a x b) --> R) (1.0 0.9)))
;expected: [((a --> (R /1 b)) (1.0 0.81)), ((b --> (R /2 a)) (1.0 0.81))]

!(|- ((R --> (a x b)) (1.0 0.9)))
;expected: [(((R \1 b) --> a) (1.0 0.81)), (((R \2 a) --> b) (1.0 0.81))]

!(|- ((A --> cat) (1.0 0.9)) ((B --> cat) (1.0 0.9)))
;expected: [((B --> A) (1.0 0.44751381215469616)), (((A ~ B) --> cat) (0.0 0.81)), (((A | B) --> cat) (1.0 0.81)), ((B <-> A) (1.0 0.44751381215469616)), (((A & B) --> cat) (1.0 0.81))]

!(|- (|- ((Garfield --> cat) (1.0 0.9)) ((cat --> animal) (1.0 0.9))) ((animal --> being) (1.0 0.9)))
;expected: [((animal --> (Garfield | being)) (1.0 0.40276243093922653)), ((animal --> (Garfield & being)) (1.0 0.40276243093922653)), ((animal --> (Garfield - being)) (0.0 0.40276243093922653)), ((being <-> Garfield) (1.0 0.28712091374556914)), ((being --> Garfield) (1.0 0.28712091374556914)), ((being --> Garfield) (1.0 0.42163100057836905)), ((Garfield --> being) (1.0 0.7290000000000001))]


