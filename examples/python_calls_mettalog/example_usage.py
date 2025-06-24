# examples/example_usage.py

from hyperlog import MeTTaLog

def main():
    mlog = MeTTaLog(debug=True)
    print("Evaluating (+ 3 4):", mlog.run("(+ 3 4)"))
    print("Parsing (* 2 5):", mlog.parse("(* 2 5)"))

if __name__ == "__main__":
    main()
