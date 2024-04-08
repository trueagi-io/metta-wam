
!(import! &self NARS)

!(AddBeliefEvent (((ExtSet garfield) --> cat) (1.0 0.9)))
!(AddBeliefEvent (((cat x sky) --> like) (1.0 0.9)))
!(AddBeliefEvent ((sky --> (IntSet blue)) (1.0 0.9)))
!(AddBeliefEvent (((((ExtSet garfield) x (IntSet blue)) --> like) ==> ((ExtSet garfield) --> artist)) (1.0 0.9)))
!(EternalQuestion ((ExtSet garfield) --> artist))
;[(Event (((ExtSet garfield) --> artist) (1.0 0.27508280575237937)) (eternal (4 3 2 1.0) (0 0.0)))]
