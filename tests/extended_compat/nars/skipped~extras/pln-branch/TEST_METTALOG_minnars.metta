!(import! &self LIB_MINNARS.metta)
;; How much evidence is there that garfield is an artist given the following input using up to 3 inference steps? 
!(Question ((Sentence ((({ garfield }) --> cat) (1.0 0.9)) (1)) 
            (Sentence (((cat * sky) --> like) (1.0 0.9)) (2)) 
            (Sentence ((sky --> ([ blue ])) (1.0 0.9)) (3))
            (Sentence ((((({ garfield }) * ([ blue ])) --> like) ==> (({ garfield }) --> artist)) (1.0 0.9)) (4)))
           (({ garfield }) --> artist) 3)
