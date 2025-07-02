
(: RandomGenerator Type)
(@doc RandomGenerator
  (@desc "Type representing a random number generator."))

(: random-int (-> RandomGenerator Number Number Number))
(iz random-int UseRust)
(@doc random-int
  (@desc "Returns random int number from range defined by two numbers (second and third argument)")
  (@params (
    (@param "Random number generator instance")
    (@param "Range start")
    (@param "Range end")))
  (@return "Random int number from defined range"))

(iz random-float UseRust)
(: random-float (-> RandomGenerator Number Number Number))
(@doc random-float
  (@desc "Returns random float number from range defined by two numbers (second and third argument)")
  (@params (
    (@param "Random number generator instance")
    (@param "Range start")
    (@param "Range end")))
  (@return "Random float number from defined range"))

(iz set-random-seed MeTTa)
(iz set-random-seed UseRust)
(: set-random-seed (-> RandomGenerator Number (->)))
(@doc set-random-seed
  (@desc "Sets a new seed (second argument) for random number generator (first argument)")
  (@params (
    (@param "Random number generator instance")
    (@param "Seed")))
  (@return "Unit atom"))

(iz new-random-generator MeTTa)
(iz new-random-generator UseRust)
(: new-random-generator (-> Number RandomGenerator))
(@doc new-random-generator
  (@desc "Creates new random number generator instance using seed as input (first argument)")
  (@params (
    (@param "Seed")))
  (@return "Instance of random number generator"))

(iz reset-random-generator MeTTa)
(: reset-random-generator (-> RandomGenerator RandomGenerator))
(@doc reset-random-generator
  (@desc "Resets instance of random number generator (first argument) to its default behavior (StdRng::from_os_rng())")
  (@params (
    (@param "Random number generator instance")))
  (@return "Random number generator instance with default behavior"))


