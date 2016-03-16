# QuantumUtils for Mathematica


**QuantumUtils for Mathematica is still a pre-alpha version, expect bugs until version 1.**


QuantumUtils for Mathematica is a software library for quantum information scientists. Features include:

 - Multi-partite tensor manipulations
 - Quantum system modelling including qudits, circuits, spins, and cavities
 - Symbolic Lie algebra simplification for spin and cavity systems
 - Quantum channels in multiple representations
 - Numerical and symbolic simulators for open and closed quantum system evolution
 - An implementation of GRadient Ascent Pulse Engineering including distortions
 - Perturbative expansion tools such as the Magnus series, the Zassenhaus formula, and matrix power series
 - Visualization functions for displaying matrices, quantum states, data etc.

## Requirements

QuantumUtils officially requires Mathematica 10.0.0 or newer. Most features should work with Mathematica 9, and full compatibility with Mathematica 9 will be added in the future. Older versions of Mathematica are not supported.

## Installation

Once this repository is cloned onto your computer, open the *Install.nb* notebook in Mathematica and follow instructions there (basically just *Evaluation -> Evaluate Notebook*).

The installation places and/or symlinks files in your *`$UserBaseDirectory`/Applications* folder. 

QuantumUtils includes a suite of unit tests to check if the installed packages are running correctly. After installation these tests can be accesed from the package `QUTesting` by running the commands:

    Needs["QUTesting`"];
    RunAllTests[]
	
## Using QuantumUtils for Mathematica

Packages from the QuantumUtils for Mathematica library can be loaded by invoking the `Needs` function, for example:

    Needs["QuantumChannel`"];
    
All packages provided by QuantumUtils for Mathematica can be loaded simultaneously by needsing QuantumUtils`:

    Needs["QuantumUtils`"]
    
## Documentation

**It is highly recommended that the icon for opening/closing cell groups be enabled. This makes it easy to expand and contract sections in the documentation notebooks. This option can be found in the Interface tab of the Preferences window (Edit>Preferences in Windows/Linux, Mathematica>Preferences in MacOS).**

Documentation is stored as *.nb* notebooks in the *doc* folder. The documentation index can be opened from within Mathematica at any time by evaluating:

    <<QUDoc`

Alternatively, once any package from Quantum Utils for Mathematica has been loaded, the `QUDoc[]` function may be used: 

    QUDoc[]                       (* Opens index *)
    QUDoc["QUPackageName`"]       (* Opens documentation notebook for QUPackageName` *)
    QUDoc[FunctionName]           (* Opens and highlights documentation for FunctionName *)

For inline function descriptions, the usage text can be displayed using the `?` symbol as with built-in functions:

    ?FunctionName


## License

[![license](https://img.shields.io/badge/license-New%20BSD-blue.svg)](http://en.wikipedia.org/wiki/BSD_licenses#3-clause_license_.28.22Revised_BSD_License.22.2C_.22New_BSD_License.22.2C_or_.22Modified_BSD_License.22.29)

You are free to use this software, with or without modification, provided that the conditions listed in the LICENSE.txt file are satisfied.
