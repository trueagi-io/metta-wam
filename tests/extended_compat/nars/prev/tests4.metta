!(import! &self NARS.metta)

!(AddBeliefEvent ((({ garfield }) --> cat) (1.0 0.9)))
!(AddBeliefEvent (((cat * sky) --> like) (1.0 0.9)))
!(AddBeliefEvent ((sky --> ([ blue ])) (1.0 0.9)))
!(AddBeliefEvent ((((({ garfield }) * ([ blue ])) --> like) ==> (({ garfield }) --> artist)) (1.0 0.9)))

!(assertEqual
  (EternalQuestion (({ garfield }) --> artist))
  (Event ((({ garfield }) --> artist) (1.0 0.27508280575237937)) (eternal (4 3 2 1.0) (0 0.0))))
