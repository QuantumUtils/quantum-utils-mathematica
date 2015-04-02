(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Lindblad Solver Unit Tests*)


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


(* ::Subsection::Closed:: *)
(*Preamble*)


BeginPackage["LindbladSolverTests`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];
Needs["LindbladSolver`"];


(* ::Section::Closed:: *)
(*Results*)


Begin["`UnitTests`"];


$RegisteredTests={};
$TestResults := RunTest[$RegisteredTests];


End[];


(* ::Section::Closed:: *)
(*Unit Tests*)


Begin["`UnitTests`"];


(* ::Subsubsection::Closed:: *)
(*Preformating State and Lindblad*)


TestCase[$RegisteredTests,"LindbladSolver:PreformatGenerator", 
	Block[{t,a},
		And[
		Norm@Simplify[
				LindbladSolver`Private`PreformatGenerator[Sin[#]*Array[a,{2,2}]&,t]-Sin[t]*Array[a,{2,2}]
				]===0,
		Norm@Simplify[
			LindbladSolver`Private`PreformatGenerator[Array[a,{2,2}],t]-Array[a,{2,2}]
			]===0]
	]
];


TestCase[$RegisteredTests,"LindbladSolver:PreformatLindblad",
	Block[{t,
		ham=PauliMatrix[3],
		cOp={{{0,1},{0,0}}},
		L=DiagonalMatrix[{0,2*I,-2*I,0}],
		D={{0,0,0,1},{0,-1/2,0,0},{0,0,-1/2,0},{0,0,0,-1}}},
	And[
		Norm@Simplify[LindbladSolver`Private`PreformatLindblad[ham,cOp][t]-L-D]===0,
		Norm@Simplify[LindbladSolver`Private`PreformatLindblad[0,cOp][t]-D]===0,
		Norm@Simplify[LindbladSolver`Private`PreformatLindblad[ham,{}][t]-L]===0]
	]]


TestCase[$RegisteredTests,"LindbladSolver:PreformatHamiltonian",
	Block[{ham=PauliMatrix[1],t},
	And[
		Norm@Simplify[LindbladSolver`Private`PreformatHamiltonian[ham][t]+I*ham]===0,
		Norm@Simplify[LindbladSolver`Private`PreformatHamiltonian[Cos[#]*ham&][t]+I*Cos[t]*ham]===0
	]
	]]


TestCase[$RegisteredTests,"LindbladSolver:PreformatState", 
	And[
		LindbladSolver`Private`PreformatState[{{"a",0},{0,"b"}}]==={"a",0,0,"b"},
		LindbladSolver`Private`PreformatState[{{"a"},{"b"}}]==={"a","b"},
		LindbladSolver`Private`PreformatState[{"a","b"}]==={"a","b"}]	
]


(* ::Subsubsection::Closed:: *)
(*Setting Up ODE*)


TestCase[$RegisteredTests,"LindbladSolver:ODEInitialConditions",
	ODEInitialConditions[{0,1},{0,"x"}]===And["x"[1][0]==0,"x"[2][0]==1]
]


TestCase[$RegisteredTests,"LindbladSolver:ODEFirstOrderSystem",
	Block[{t},
	ODEFirstOrderSystem[{{0,"a"[1]},{"a"[2],0}},{t,"x"}]===And[
	Derivative[1]["x"[1]][t]=="a"[1] "x"[2][t],Derivative[1]["x"[2]][t]=="a"[2] "x"[1][t]]
	]]


(* ::Subsubsection::Closed:: *)
(*Solving ODE*)


TestCase[$RegisteredTests,"LindbladSolver:ODESolver.Numeric", 
With[{sol=ODESolver[-2*Pi*I*PauliMatrix[1],{{0,1},0,1}]},
	And[MatchQ[sol,{__InterpolatingFunction}],
		TrueQ[Abs[Through[sol[1]].{0,1}]^2-1<=0.0001]]
]]


TestCase[$RegisteredTests,"LindbladSolver:ODESolver.Symbolic", 
Block[{t,sol},
	sol=ODESolver[-2*Pi*I*PauliMatrix[1],{{0,1},0}][t];
	TrueQ[sol=={-I*Sin[2*Pi*t],Cos[2*Pi*t]}]
	]]


TestCase[$RegisteredTests,"LindbladSolver:SchrodingerSolver.Numeric", 
Block[{t},
	MatchQ[
		SchrodingerSolver[Cos[#]PauliMatrix[1]&,{{0,1},0,1}][t],
		{InterpolatingFunction[__][t],InterpolatingFunction[__][t]}
	]]]


TestCase[$RegisteredTests,"LindbladSolver:SchordingerSolver.Symbolic",
	Block[{t,a,b},
		SchrodingerSolver[PauliMatrix[3],{{a,b},0}][t]==={a*Exp[-I*t],b*Exp[I*t]}
	]]


TestCase[$RegisteredTests,"LindbladSolver:LindbladSolver.Numeric", 
	Block[{init={{1,0},{0,0}},sol,t},
		sol=LindbladSolver[{Cos[#]PauliMatrix[1]&,{{{0,1},{0,0}}}},
				{{{1,0},{0,0}},0,1}];
		And[
			Chop@Norm[sol[0]-init]===0,
			MatchQ[sol[t],
				{{InterpolatingFunction[__][t],InterpolatingFunction[__][t]},
				{InterpolatingFunction[__][t],InterpolatingFunction[__][t]}}]
		]
	]]


TestCase[$RegisteredTests,"LindbladSolver:LindbladSolver.Symbolic", 
	Block[{sol,t},
		sol=LindbladSolver[{0,{{{0,0},{1,0}}}},{{{1,0},{0,0}},0}][t];
		Norm@Simplify[sol-{{Exp[-t],0},{0,1-Exp[-t]}}]===0
	]]


TestCase[$RegisteredTests,"LindbladSolver:LindbladSolver.QuantumChannel", 
	Block[{sol,t,chan},
		chan=QuantumChannel`QuantumChannel[
				{{-1,0,0,0},{0,-(1/2),0,0},{0,0,-(1/2),0},{1,0,0,0}},
				{QuantumChannel`ChannelRep->QuantumChannel`Super,
					QuantumChannel`InputDim->2,
					QuantumChannel`OutputDim->2,
					Tensor`Basis->"Col"}];
		sol=LindbladSolver[chan,{{{1,0},{0,0}},0}][t];
		Norm@Simplify[sol-{{Exp[-t],0},{0,1-Exp[-t]}}]===0
	]]


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End Package*)


EndPackage[];
