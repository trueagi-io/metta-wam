from motto.agents import DialogAgent

def reply_and_print(agent, msg):
    print("USER: ", msg)
    print("AI: ", agent(msg).content)

# Currently, DialogAgent is inherited from MettaScriptAgent class
# (instead of, say, wrapping any other agent). Thus, we need to
# call another agent (or implement Response somehow else) to
# make it work. DialogAgent provides `history` function to get
# the history of the user and the agent messages.
agent = DialogAgent(path="07_dialog.msa")

reply_and_print(agent, '(user "Hello!")')
reply_and_print(agent, '(user "May I sell the ring?")')
reply_and_print(agent, '(user "Sorry, I am confused. What I wanted to sell?")')
