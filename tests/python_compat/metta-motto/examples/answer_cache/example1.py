from motto.agents import MettaAgent
from hyperon import G, GroundingSpaceRef


if __name__ == '__main__':
    cache_space = G(GroundingSpaceRef())
    code = '''    
    (= (response)
        (let (user $msg) (messages)
            (case (match &cache ($msg $ans) $ans)
                (
                   (Empty (let*
                     (($response ((chat-gpt-agent) (messages)))
                      (() (add-atom &cache ($msg $response))))
                     $response)
                   )
                   ($ans  (CACHED $ans))
               )
            )
         )
    )
    '''

    agent = MettaAgent(code=code, atoms={'&cache': cache_space})
    print(agent('(user "What is your name?")'))
    print(agent('(user "What is your name?")'))
