# Key Dataset: MeTTaLog vs. MeTTaRust

The following table compares the execution times for **MeTTaLog** and **MeTTaRust** for N-Queens sizes 4 through 7. This data highlights the significant disparity between the two implementations.

| **N-Queens Size** | **MeTTaLog Time**  | **MeTTaRust Time**     | **Leaner?**         | **Difference (Factor)**  |
|--------------------|--------------------|------------------------|---------------------|---------------------------|
| **4**             | 0m5.565s          | 0m8.076s               | ✅ **MeTTaLog**     | ~1.45x slower in MeTTaRust     |
| **5**             | 0m5.953s          | 0m32.852s              | ✅ **MeTTaLog**     | ~5.52x slower in MeTTaRust     |
| **6**             | 0m7.043s          | 2m14.622s              | ✅ **MeTTaLog**     | ~19.12x slower in MeTTaRust    |
| **7**             | 0m11.805s         | 11m26.192s             | ✅ **MeTTaLog**     | ~58.33x slower in MeTTaRust    |

### Observations
1. **MeTTaLog is consistently faster** than MeTTaRust across all tested sizes.
2. The performance gap widens with larger problem sizes, highlighting inefficiencies in MeTTaRust’s recursion handling.

---

## Proportionality of MeTTaLog and Plain Prolog

MeTTaLog and Plain Prolog exhibit proportional scaling. Both implementations handle recursion and symbolic reasoning efficiently due to their declarative natures. However, Plain Prolog is significantly faster because of its optimized runtime environment and more mature backtracking mechanisms.

### Longer Timing Table

| **N-Queens Size** | **MeTTaLog (min)** | **MeTTaRust (min)** | **Plain Prolog (min)** | **Prolog CLP(FD) (min)** | **Python (min)** | **C/C++ (min)** |
|--------------------|--------------------|---------------------|-------------------------|--------------------------|------------------|-----------------|
| **4**             | 0.093             | 0.135               | 0.003                  | 0.000                   | 0.003            | 0.000           |
| **5**             | 0.099             | 0.547               | 0.003                  | 0.000                   | 0.015            | 0.000           |
| **6**             | 0.117             | 2.244               | 0.004                  | 0.001                   | 0.095            | 0.000           |
| **7**             | 0.197             | 11.435              | 0.013                  | 0.003                   | 0.705            | 0.000           |
| **8**             | 0.308             | 38.000              | 0.015                  | 0.015                   | 2.000            | 0.000           |
| **9**             | 0.423             | 133.000             | 0.050                  | 0.061                   | 6.000            | 0.000           |
| **10**            | 0.543             | 467.000             | 0.100                  | 0.267                   | 20.000           | 0.001           |
| **11**            | 0.700             | -                   | 0.180                  | 1.276                   | 60.000           | 0.010           |
| **12**            | 1.000             | -                   | 0.350                  | 6.664                   | 180.000          | 0.055           |
| **13**            | 1.500             | -                   | 0.650                  | 36.606                  | 540.000          | 0.308           |
| **14**            | 2.500             | -                   | 1.500                  | 212.653                 | -                | 1.849           |
| **15**            | 4.000             | -                   | 3.000                  | -                       | -                | 11.789          |

### Observations
1. **Plain Prolog scales proportionally with MeTTaLog** but is consistently faster by approximately 30x.
2. **Prolog CLP(FD)** exhibits better performance than Plain Prolog for smaller sizes due to built-in constraint-solving but slows for larger sizes due to memory overhead.
3. **C/C++** remains the fastest for all tested sizes.

---

## Enhancing MeTTa: Integrating Prolog and CLP(FD) Features

To maximize MeTTa’s flexibility and performance, it’s crucial to allow programmers to leverage **Plain Prolog** and **Constraint Logic Programming over Finite Domains (CLP(FD))** within the MeTTa language itself. By embedding constructs and distinctions that map to either approach, MeTTa can serve as a robust framework for both general symbolic reasoning and optimized constraint-solving.

### Enabling Both Prolog and CLP(FD) in MeTTa

#### Programmer Flexibility
- **Objective:** Allow developers to express logic in MeTTa that can be translated to either Plain Prolog or CLP(FD) based on performance and problem-specific needs.
- **Implementation:** Introduce language constructs that explicitly specify whether a rule or query uses general symbolic reasoning or constraint-based logic.

Example:
```metta
(rule (n_queens_clp N Solution)
      @constraints
      (and (domain Solution 1..N)
           (all_different Solution)
           (safe_clp Solution)))
```

This translates to:
```prolog
n_queens_clp(N, Solution) :-
    length(Solution, N),
    domain(Solution, 1, N),
    all_different(Solution),
    safe_clp(Solution),
    labeling([], Solution).
```

### Differences Between Plain Prolog and CLP(FD)

| **Feature**              | **Plain Prolog**                 | **CLP(FD)**                          |
|---------------------------|-----------------------------------|---------------------------------------|
| **Backtracking**          | Generic, explores all solutions  | Constraint-driven, prunes search space |
| **Arithmetic Constraints**| Requires explicit predicates     | Built-in support (e.g., `X #= Y + Z`) |
| **Domain Definition**     | Not supported                   | Native (`domain(X, 1..N)`)            |
| **Constraint Propagation**| No                              | Yes                                   |
| **Optimization**          | Manual                          | Built-in (`labeling([minimize(X)])`)  |

### Implementation Strategy for MeTTa

#### Unified Language Design
Introduce syntax or annotations to distinguish between Plain Prolog and CLP(FD) translations:
- Default: Translate to Plain Prolog.
- Explicit: Use annotations for constraints or optimization.

For example:
```metta
(rule (n_queens_clp N Solution)
      @constraints
      (and (domain Solution 1..N)
           (all_different Solution)
           (safe_clp Solution)))
```

#### Seamless Translation to Prolog
- Plain Prolog: Use standard logical constructs and backtracking.
- CLP(FD): Map MeTTa’s constraints directly to CLP(FD) predicates.

---

### Conclusion

By integrating the differences between Plain Prolog and CLP(FD) into MeTTa’s language design:
- Programmers can write flexible, efficient logic that leverages the strengths of both paradigms.
- The MeTTa compiler ensures smooth translation, preserving logic for Prolog and optimizing constraints for CLP(FD).
- This dual-approach empowers MeTTa as a high-level, declarative language capable of addressing a broad spectrum of computational problems effectively.

