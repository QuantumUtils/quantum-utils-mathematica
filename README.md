# QuantumUtils

QuantumUtils is a Mathematica library blah blah blah

## Installation

Once this repository is cloned onto your computer, open the *Install.nb* notebook in Mathematica and follow instructions there (basically just *Evaluation -> Evaluate Notebook*).

The installation places and/or symlinks files in your *`$UserBaseDirectory`/Applications* folder. Packages from the QuantumUtils library can then be loaded by invoking the `Needs` function, for example:

    Needs["QSim`"];
    
## Documentation

Documentation is stored as *.nb* notebooks in the *doc* folder. The documentation index can be opened from within Mathematica at any time by evaluating:

    <<QUDoc`
    
Note the necessary backtick.

Additionally, each function implemented by QuantumUtils should come with a `usage` tag. This tag can be displayed using the `?` symbol as with built-in functions:

    ?EvalPulse
