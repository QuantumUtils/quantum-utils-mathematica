(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*GRAPE Unit Tests*)


(* ::Subsection::Closed:: *)
(*Copyright and License Information*)


(* ::Text:: *)
(*This package is part of QuantumUtils for Mathematica.*)
(**)
(*Copyright (c) 2015 and later, Christopher J. Wood, Christopher E. Granade, Ian N. Hincks*)
(**)
(*Redistribution and use in source and binary forms, with or withoutmodification, are permitted provided that the following conditions are met:*)
(*1. Redistributions of source code must retain the above copyright notice, this  list of conditions and the following disclaimer.*)
(*2. Redistributions in binary form must reproduce the above copyright notice,  this list of conditions and the following disclaimer in the documentation  and/or other materials provided with the distribution.*)
(*3. Neither the name of quantum-utils-mathematica nor the names of its  contributors may be used to endorse or promote products derived from  this software without specific prior written permission.*)
(**)
(*THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THEIMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE AREDISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLEFOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIALDAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS ORSERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVERCAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USEOF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.*)


(* ::Subsection:: *)
(*Preamble*)


BeginPackage["GRAPETests`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];
Needs["QuantumSystems`"];
Needs["GRAPE`"];


(* ::Section::Closed:: *)
(*Results*)


Begin["`UnitTests`"];


$RegisteredTests={};
$TestResults := RunTest[$RegisteredTests];


End[];


(* ::Section:: *)
(*Unit Tests*)


Begin["`UnitTests`"];


(* ::Subsection:: *)
(*Pulse Object*)


(* ::Subsubsection:: *)
(*Initialization*)


TestCase[$RegisteredTests, "PulseDataStructure:InitializeHControl", 
	Module[{p, HControl},
		HControl = {RandomHermitian[8], RandomHermitian[8]};		
		p = Pulse[
			ControlHamiltonians -> HControl
		];
		ControlHamiltonians[p] == Hinternal;
	]
]
TestCase[$RegisteredTests, "PulseDataStrucutre:InitializeHInternal",
	Module[{p, Hinternal},
	Hinternal = RandomHermitian[8];
	p = Pulse[InternalHamiltonian -> Hinternal];
	InternalHamiltonian[p] == Hinternal;
]];

TestCase[$RegisteredTests, "PulseDataStructure:InitializeUtarget",
	Module[{p, target},
		target = RandomUnitary[8];
		p = Pulse[
			Target -> target
		];
		Target[p] == target;
	]
];


(* ::Subsection:: *)
(*FindPulse*)


(* ::Subsubsection:: *)
(*Simple Pulse*)


TestCase[$RegisteredTests, "FindPulse:SimplePulse",
	Module[{
		pulse, Hint, HControl, controlRange, initialGuess, target, \[Phi]target
	},
		Hint = TP[Z];
		HControl = 2\[Pi] * {TP[X], TP[Y]};
		controlRange = {{-1, 1}, {-1, 1}};
		target = RandomUnitary[2];
		\[Phi]target = 0.99;
		initialGuess = RandomSmoothPulse[1, 10, controlRange];
		pulse = FindPulse[initialGuess, target, \[Phi]target, controlRange, HControl, Hint];
		Re[Tr[pulse[Target] . target / 2]] >= \[Phi]target;
];
]


(* ::Subsubsection:: *)
(*Repetitions*)


TestCase[$RegisteredTests, "FindPulse:Repetitions",
	Module[{
		pulse, repetitions, Hint, HControl, controlRange, initialGuess, target, \[Phi]target
	},
		Hint = TP[Z];
		repetitions = 10;
		HControl = 2\[Pi] * {TP[X], TP[Y]};
		controlRange = {{-1, 1}, {-1, 1}};
		target = RandomUnitary[2];
		\[Phi]target = 0.99;
		initialGuess = RandomSmoothPulse[1, 10, controlRange];
		pulse = FindPulse[initialGuess, target, \[Phi]target, controlRange, HControl, Hint, Repetitions -> repetitions];
		Re[Tr[pulse[Target] . target / 2]] >= \[Phi]target;
];
]


End[];


(* ::Section:: *)
(*End Package*)


EndPackage[];
