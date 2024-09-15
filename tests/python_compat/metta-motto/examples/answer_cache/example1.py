from motto.agents import MettaAgent
from hyperon import G, GroundingSpaceRef


if __name__ == '__main__':
    cache_space = G(GroundingSpaceRef())
    code = '''
    ! (let (user $msg) (messages)
        (case (match &cache ($msg $ans) $ans)
          (($_ (Response (CACHED $ans)))
           (Empty (let*
             (($response ((chat-gpt-agent) (messages)))
              (() (add-atom &cache ($msg $response))))
             (Response $response))
           )
          )
        )
      )
    '''

    agent = MettaScriptAgent(code=code, atoms={'&cache': cache_space})

    print(agent('(user "What is your name?")'))
    print(agent('(user "What is your name?")'))
