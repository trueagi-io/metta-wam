import os
import time
import numpy as np

#Tests to run:
tests = [
     "!(factorial 30)",
     "!(range 1 30)",
     "!(TupleCount (1 2 3 4 5 6 7 8 9 10 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))",
   """!(StampDisjoint (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30) 
                      (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))"""
]

#Run and time them:
runs=10
print(f"Starting {len(tests)} tests with {runs} runs each, grab a coffee!")
workdir = os.getcwd()
for test in tests:
    os.chdir(workdir)
    os.system("cp timing.metta RUN.metta")
    with open("RUN.metta", "a") as file:
        file.write(test)
    os.chdir("../")
    os.system("sh run.sh ./timing/RUN.metta cat-only")
    os.system("sh compile_scheme.sh")
    os.system("./RUN > /dev/null") #make sure binary is already in file cache
    print(f"TEST: {test}")
    time_metta = []
    time_scheme = []
    for run in range(runs):
        if run > 1:
            print(".", end="", flush=True)
        t1 = time.time()
        redirectOutput = "> /dev/null" if run != 0 else ""
        os.system("metta RUN.metta" + redirectOutput)
        t2 = time.time()
        os.system("./RUN" + redirectOutput)
        t3 = time.time()
        time_metta.append(t2 - t1)
        time_scheme.append(t3 - t2)
    avg_time_metta = np.average(np.array(time_metta))
    avg_time_scheme = np.average(np.array(time_scheme))
    var_time_metta = np.var(np.array(time_metta))
    var_time_scheme = np.var(np.array(time_scheme))
    speedup = avg_time_metta / avg_time_scheme
    print(f"\nTime MeTTa:\tavg={avg_time_metta} seconds, var={var_time_metta}")
    print(f"Time Scheme:\tavg={avg_time_scheme} seconds, var={var_time_scheme}")
    print(f"Speedup:\t{speedup} times faster")
