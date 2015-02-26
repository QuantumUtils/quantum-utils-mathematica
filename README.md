# QuantumUtils

QuantumUtils is a Mathematica library blah blah blah

## Installation

Once this repository is cloned onto your computer, open the *Install.nb* notebook in Mathematica and follow instructions there (basically just *Evaluation -> Evaluate Notebook*).

The installation places and/or symlinks files in your *`$UserBaseDirectory`/Applications* folder. 

## Using QuantumUtils Mathematica

Packages from the QuantumUtils library can then be loaded by invoking the `Needs` function, for example:

    Needs["QSim`"];
    
All packages provided by QuantumUtils can be loaded simultaneously by needsing QuantumUtils`:

    Needs["QuantumUtils`"]
    
## Documentation

Documentation is stored as *.nb* notebooks in the *doc* folder. The documentation index can be opened from within Mathematica at any time by evaluating:

    <<QUDoc`
    
Note the necessary backtick.

Additionally, each function implemented by QuantumUtils should come with a `usage` tag. This tag can be displayed using the `?` symbol as with built-in functions:

    ?EvalPulse


## License

[![license](https://img.shields.io/badge/license-New%20BSD-blue.svg)](http://en.wikipedia.org/wiki/BSD_licenses#3-clause_license_.28.22Revised_BSD_License.22.2C_.22New_BSD_License.22.2C_or_.22Modified_BSD_License.22.29)

You are free to use this software, with or without modification, provided that the conditions listed in the LICENSE.txt file are satisfied.
