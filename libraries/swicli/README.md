# YAP/SWI-Prolog 2-Way Interface to Common Language Infrastructure (.NET)

## Overview

SwiCLI provides a seamless integration between **SWI-Prolog** and the **Common Language Infrastructure (.NET/Mono)**. It allows Prolog to interact with managed code, including C#, F#, and VB.NET, as well as accessing unmanaged libraries.

### Features:
- Full control of the **Common Language Infrastructure (.NET/Mono)**
- Integration with **C/C++/Objective-C unmanaged libraries**
- Works on **Linux, macOS, and Windows**
- **CLI predicates (`cli_`)** are inspired by the `jpl_` interface of JPL
- Based on **SwiPlCS** by Uwe Lesta
- Available for **SWI-Prolog** and **YAP-Prolog**

---
```
# from https://github.com/dotnet/docs/blob/main/docs/core/install/linux-debian.md
wget https://packages.microsoft.com/config/debian/12/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
sudo apt update -y
sudo apt install -y dotnet-sdk-6.0
```


## Downloads

- **[Download Prebuilt Releases](http://code.google.com/p/opensim4opencog/downloads/list)**
  - Look for **SWICLI-xxx-DIST-xxxx.zip**
- **[Source Code Repository](https://github.com/swi-to-yap/swicli)**
- **[API Documentation](http://swi-to-yap.github.io/swicli/api.html)**
- **[Old Documentation](http://swi-to-yap.github.io/swicli/documentation.html)**

### Example Project Using SwiCLI:
- [Cogbot (OpenSim AI)](https://github.com/Tandysony/opensim4opencog/blob/master/bin/prolog/cogbot.pl)

---

## Installation

### Windows (Requires .NET 4.0 or later)

1. **Download and extract** `SWICLI-xxx-DIST-xxxx.zip`
2. **Copy the following directories** into your **SWI-Prolog installation directory**:
   ```sh
   Copy pl/bin  to C:\Program Files (x86)\swipl\bin
   Copy pl/library  to C:\Program Files (x86)\swipl\library
   ```
3. **Install .NET Framework 4.0 or later** (if not already installed)
   - Download from [Microsoft](https://dotnet.microsoft.com/en-us/download/dotnet-framework)
4. **That's it!** SwiCLI should now be available in SWI-Prolog.

### Linux/macOS (Requires Mono 2.10.8 or later)

1. **Install dependencies** (Ubuntu/Debian-based systems):
   ```sh
   sudo apt update
   sudo apt install mono-devel mono-complete
   # used to be
   # sudo apt install mono-devel libmono-system-data-linq4.0-cil libmono-system-xml-linq4.0-cil libmono-microsoft-visualbasic10.0-cil
   ```
2. **Copy the following directories** into your **SWI-Prolog installation directory**:
   ```sh
   cp -r pl/lib/ /usr/lib/swipl/
   cp -r pl/library/ /usr/lib/swipl/
   ```
3. **Set up Mono environment variables**:
   ```sh
   source mono_sysvars.sh
   ```
4. **SwiCLI is now installed and ready to use!**

---

## Running SwiCLI

### Example 1: Loading the Module

Start **SWI-Prolog** and load SwiCLI:

```prolog
?- use_module(library(swicli)).
SetupProlog
```

Expected output:
```
Cannot install hook ThreadExit to Mono
Swicli.Library.Embedded.install succeeded
true.
```

### Example 2: Calling a Native Function

Invoke a **C function (`printf`)** from **libc**:

```prolog
?- cli_get_dll('libc.so.6', DLL), cli_call(DLL, printf, ["I have been clicked %d times\n", 2], O).
```

Expected output:
```
I have been clicked 2 times
DLL = @'C#666',
O = @void.
```

### Example 3: Interacting with .NET Classes

Create a new **List of Strings** in .NET:

```prolog
?- cli_new('System.Collections.Generic.List'('System.String'), [int], [10], Obj).
Obj = @'C#516939544'.
```

Add elements to the list:

```prolog
?- cli_call($Obj, 'Add'("foo"), _).
?- cli_call($Obj, 'Add'("bar"), _).
```

Retrieve the number of elements:

```prolog
?- cli_get($Obj, 'Count', Count).
Count = 2.
```

Iterate through the list:

```prolog
?- cli_col($Obj, E).
E = "foo";
E = "bar";
false.
```

---

## Handling Events in SwiCLI

### Example: Adding an Event Hook

Find an event to hook into:

```prolog
?- botget(['Self'], AM), cli_memb(AM, e(A, B, C, D, E, F, G)).
```

Expected output (example):
```
A = 6,
B = 'IM',  % Event name
C = 'System.EventHandler'('InstantMessageEventArgs'),
D = ['Object', 'InstantMessageEventArgs'],
F = decl(static(false), 'AgentManager'),
```

Register the event handler:

```prolog
?- botget(['Self'], AM), cli_add_event_handler(AM, 'IM', handle_im(_, _, _)).
```

Define the event handler:

```prolog
handle_im(Origin, Obj, IM) :-
    writeq(handle_im(Origin, Obj, IM)), nl.
```

---

## Release Notes

### TODO:
- Publish autoload examples on the website.

### Version 0.7:
- Added **PL_agc_hook** for garbage collection tracking of foreign objects.
- Implemented dynamic registrations for **exit and abort hooks**.

