from hyperon.base import Atom
from hyperon.atoms import OperationAtom, E, GroundedAtom, GroundedObject
from hyperon.ext import register_tokens, register_atoms
from hyperon.atoms import G, AtomType
from hyperon.runner import MeTTa
import hyperonpy as hp

import sys
import readline
import os
import atexit
import time

runner = MeTTa()

# Make this function print the time it took with high precision
def rust_metta_run(obj):
    start_time = time.perf_counter()
    result = runner.run(obj)
    end_time = time.perf_counter()
    elapsed_time = end_time - start_time
    print(f"Execution time: {elapsed_time:.6f} seconds")
    print("Result:")
    print(result)
    return result


src_file_src = """
;; tuples.
(: synthesize (-> $a (-> $kt) (-> $rt) Nat $a))
;; Nullary rule (axiom)
(= (synthesize $query $kb $rb $depth)
   (let $query ($kb) $query))
;; Unary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise $conclusion)) ($rb))
          ((: ($ructor $proof) $conclusion) $query)
          ((: $proof $premise) (synthesize (: $proof $premise) $kb $rb $k)))
     $query))
;; Binary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $conclusion)) ($rb))
          ((: ($ructor $proof1 $proof2) $conclusion) $query)
          ((: $proof1 $premise1) (synthesize (: $proof1 $premise1) $kb $rb $k))
          ((: $proof2 $premise2) (synthesize (: $proof2 $premise2) $kb $rb $k)))
     $query))
;; Trinary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $premise3 $conclusion)) ($rb))
          ((: ($ructor $proof1 $proof2 $proof3) $conclusion) $query)
          ((: $proof1 $premise1) (synthesize (: $proof1 $premise1) $kb $rb $k))
          ((: $proof2 $premise2) (synthesize (: $proof2 $premise2) $kb $rb $k))
          ((: $proof3 $premise3) (synthesize (: $proof3 $premise3) $kb $rb $k)))
     $query))
;; Quaternary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $premise3 $premise4 $conclusion)) ($rb))
          ((: ($ructor $proof1 $proof2 $proof3 $proof4) $conclusion) $query)
          ((: $proof1 $premise1) (synthesize (: $proof1 $premise1) $kb $rb $k))
          ((: $proof2 $premise2) (synthesize (: $proof2 $premise2) $kb $rb $k))
          ((: $proof3 $premise3) (synthesize (: $proof3 $premise3) $kb $rb $k))
          ((: $proof4 $premise4) (synthesize (: $proof4 $premise4) $kb $rb $k)))
     $query))
;; Quintenary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $premise3 $premise4 $premise5 $conclusion)) ($rb))
          ((: ($ructor $proof1 $proof2 $proof3 $proof4 $proof5) $conclusion) $query)
          ((: $proof1 $premise1) (synthesize (: $proof1 $premise1) $kb $rb $k))
          ((: $proof2 $premise2) (synthesize (: $proof2 $premise2) $kb $rb $k))
          ((: $proof3 $premise3) (synthesize (: $proof3 $premise3) $kb $rb $k))
          ((: $proof4 $premise4) (synthesize (: $proof4 $premise4) $kb $rb $k))
          ((: $proof5 $premise5) (synthesize (: $proof5 $premise5) $kb $rb $k)))
     $query))

(: kb (-> Atom))
(= (kb) (: a A))
(= (kb) (: a B))
(= (kb) (: abc (Implication (AndLink A B) C)))
(= (kb) (: cde (Implication (OrLink C D) E)))

(: rb (-> Atom))
(= (rb) (: ModusPonens (-> (ImplicationLink $p $q) $p $q)))
(= (rb) (: Deduction (-> (ImplicationLink $p $q) (ImplicationLink $q $r) (ImplicationLink $p $r))))
(= (rb) (: DisjunctiveSyllogism (-> (OrLink $p $q) (NotLink $p) $q)))
(= (rb) (: DisjunctiveSyllogism (-> (OrLink $p $q) (NotLink $q) $p)))
(= (rb) (: ConjunctionIntroduction (-> $p $q (AndLink $p $q))))
(= (rb) (: ConjunctionEliminationLeft (-> (AndLink $p $q) $p)))
(= (rb) (: ConjunctionEliminationRight (-> (AndLink $p $q) $q)))
(= (rb) (: DisjunctionIntroduction (-> $p $q (OrLink $p $q))))

"""

# Run the code and measure execution time
rust_metta_run(src_file_src)
rust_metta_run("!(collapse (synthesize (: $term $type) kb rb (S Z)))")
rust_metta_run("!(nop (collapse (synthesize (: $term $type) kb rb (S Z))))")
