
# named MeTTaLogImpl since our older class is named MeTTaLog
from hyperlog import MeTTaLogImpl

def main():
    metta = MeTTaLogImpl()
    print("Evaluating (+ 3 4):", metta.run("!(+ 3 4)"))
    print("Parsing (* 2 5):", metta.parse("(* 2 5)"))

if __name__ == "__main__":
    main()
