!(import! &self motto)
!((open-router-agent ) (user "Who was the 22nd President of France?"))
!((open-router-agent "openai/gpt-3.5-turbo" True) (user "Who was the 22nd President of France?"))

(= (doc get_current_weather)
  (Doc
    (description "Get the current weather in a given location")
    (parameters
      (location "The name of the city, e.g. San Francisco")
    ))
)

(= (get_current_weather ($arg) $msgs)
   (if (contains-str $arg "Tokyo")
       "The temperature in Tokyo is 75 Fahrenheit"
       (if (contains-str $arg "New York")
           "The temperature in New York is 80 Fahrenheit"
           (concat-str (concat-str "The temperature in " $arg)
                       " is 70 Fahrenheit")
    )
  )
)

!((open-router-agent)
   (user "Get the current weather in London")
   (Function get_current_weather) ; The temperature in London is 70 Fahrenheit.
)