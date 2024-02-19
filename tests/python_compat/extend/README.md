This is the **Mettamorph extension for the MeTTa interpreter!**

It allows you to mix MeTTa and compiled MeTTa code in a single file so you get the best of both worlds,
namely the speed where you need it, and 100% MeTTa support where you need it.

**Example:**

Running MeTTa code and compiled MeTTa code in the same file:

```metta example2.metta```

Running MeTTa code and a compiled MeTTa file compileme.metta:

```metta example1.metta``` 

**Invocation**

As seen in the examples

```!(extend-py! mettamorph)```

loads the Metta compiler,

```!(compile! "CodeString")```

compiles some MeTTa code in-line and you will be directly able to call the MeTTa functions in the code string.

```!(compile! CodeFile.metta)```

compiles the code file, again allowing you to call the MeTTa functions within.

**Additional comments**

As you see in test5 in compileme.metta, the compiled code can also invoke MeTTa code 
as the MeTTa interpreter takes care of that when the expression reduces to a function call it notices.

Also you can impose type restrictions in your non-compiled code for compiled functions!
This allows you to use the full flexibility of the MeTTa type system even for the compiled functions instead of relying on Mettamorphs more limited type system subset.


