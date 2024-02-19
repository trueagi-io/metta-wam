

!(start-vspace-service &self 3023)

!(start-vspace-service &share1 3025)

!(remote-eval 3023 (add-atom &self A-Self))

!(remote-eval 3025 (add-atom &self A-Share1))

!(assertEqual (match &self A-Self :Pass) :Pass)

!(assertEqual (match &share1 A-Share1 :Pass) :Pass)

!(add-atom &self B-Self)

!(add-atom &share1 B-Share1)

!(assertEqual (remote-eval 3023 (match &self B-Self :Pass)) :Pass)

!(assertEqual (remote-eval 3025 (match &self B-Share1 :Pass)) :Pass)






