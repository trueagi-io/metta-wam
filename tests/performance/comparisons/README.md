
| Test                          | Rust MeTTa                  | MeTTaLog               |
|-------------------------------|-----------------------------|------------------------|
| nils_if_control_test_3.metta  | 0m49.559s                   | 0m11.563s              |
| fibo_hang_10.metta            | 0m0.412s                    | 0m3.911s               |
| fibo_hang_20.metta            | 0m1.427s                    | 0m1.564s               |
| fibo_hang_80.metta            | 0m21.265s                   | 0m1.549s               |
| fibo_hang_900.metta           | I terminated it             | 0m1.600s               |
```

# time metta nils_if_control_test_3.metta 2>&1  > /dev/null
real    0m49.559s
user    0m49.468s
sys     0m0.088s


# time MeTTa nils_if_control_test_3.metta 2>&1  > /dev/null
real    0m11.563s
user    0m11.511s
sys     0m0.070s


# time MeTTa --v_old nils_if_control_test_3.metta 2>&1  > /dev/null
real    0m8.552s
user    0m8.609s
sys     0m0.091s

```




```
# time metta  fibo_hang_10.metta
55
[()]

real    0m0.412s
user    0m0.388s
sys     0m0.024s



# time MeTTa fibo_hang_10.metta
[55]

real    0m3.911s
user    0m3.874s
sys     0m0.056s

```

```
# time metta fibo_hang_20.metta
6765
[()]
[()]

real    0m1.427s
user    0m1.411s
sys     0m0.016s


# time MeTTa fibo_hang_20.metta
[]
[6765]

real    0m1.564s
user    0m1.523s
sys     0m0.059s
```


```
# time metta fibo_hang_80.metta
23416728348467685
[()]
[()]

real    0m21.317s
user    0m21.265s
sys     0m0.052s



# time MeTTa fibo_hang_80.metta
[]
[23416728348467685]
real    0m1.549s
user    0m1.500s
sys     0m0.066s
```



```
# time MeTTa fibo_hang_900.metta
[]
[54877108839480000051413673948383714443800519309123592724494953427039811201064341234954387521525390615504949092187441218246679104731442473022013980160407007017175697317900483275246652938800]

real    0m1.600s
user    0m1.582s
sys     0m0.036s


# time metta fibo_hang_900.metta
 Unknowm
```