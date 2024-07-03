
!(import! &self NARS)

!(AddBeliefEvent (((ExtSet garfield) --> cat) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent (((cat x sky) --> like) (1.0 0.9)))
!(profile! (assertEqualToResult
    (check-state)
    ( ( (___ currentTime 3)
        (___ evidentialBase 3)
        (___ concepts 0 ())
        (___ attentional_focus 1
          ( (Concept
              ( (ExtSet garfield) --> cat)
              (Event
                ( ( (ExtSet garfield) --> cat) (1.0 0.4736842105263158))
                (eternal
                  (1)
                  (0 0.0)))
              (Event
                ( ( (ExtSet garfield) --> cat) (1.0 0.9))
                (1
                  (1)
                  (1 1.0)))
              (1 1.0))))
        (___ FIFO 2
          ( (Event
              ( ( (cat x sky) --> like) (1.0 0.9))
              (2
                (2)
                (2 1.0))) (Event (((ExtSet garfield) --> cat) (1.0 0.9)) (1 (1) (1 1.0)))))
        (___ tempbest
          (Event
            ( ( (cat x sky) --> like) (1.0 0.9))
            (2
              (2)
              (2 1.0))))
        (___ belief_events 1
          ( (Event
              ( ( ( (ExtSet garfield) x sky) --> like) (1.0 0.81))
              (2
                (2 1.0)
                (2 0.4525)))))
        (___ goal_events 0 ())))))

!(AddBeliefEvent ((sky --> (IntSet blue)) (1.0 0.9)))
!(assertEqualToResult
    (check-state)
    ( ( (___ currentTime 4)
        (___ evidentialBase 4)
        (___ concepts 0 ())
        (___ attentional_focus 1
          ( (Concept
              ( (ExtSet garfield) --> cat)
              (Event
                ( ( (ExtSet garfield) --> cat) (1.0 0.4736842105263158))
                (eternal
                  (1)
                  (0 0.0)))
              (Event
                ( ( (ExtSet garfield) --> cat) (1.0 0.9))
                (1
                  (1)
                  (1 1.0)))
              (1 1.0))))
        (___ FIFO 3
          ( (Event
              ( (sky -->
                  (IntSet blue)) (1.0 0.9))
              (3
                (3)
                (3 1.0)))
            (Event
              ( ( (cat x sky) --> like) (1.0 0.9))
              (2
                (2)
                (2 1.0)))
            (Event
              ( ( (ExtSet garfield) --> cat) (1.0 0.9))
              (1
                (1)
                (1 1.0)))))
        (___ tempbest
          (Event
            ( (sky -->
                (IntSet blue)) (1.0 0.9))
            (3
              (3)
              (3 1.0))))
        (___ belief_events 1
          ( (Event
              ( ( ( (ExtSet garfield) x sky) --> like) (1.0 0.81))
              (2
                (2 1.0)
                (2 0.4525)))))
        (___ goal_events 0 ()))))

!(AddBeliefEvent (((((ExtSet garfield) x (IntSet blue)) --> like) ==> ((ExtSet garfield) --> artist)) (1.0 0.9)))
!(check-state)
!(assertEqualToResult
     (EternalQuestion ((ExtSet garfield) --> artist))
    ((Event (((ExtSet garfield) --> artist) (1.0 0.27508280575237937)) (eternal (4 3 2 1.0) (0 0.0)))))
!(check-state)

