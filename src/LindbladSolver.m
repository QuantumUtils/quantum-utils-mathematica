(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Lindblad Solver Package*)


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


BeginPackage["LindbladSolver`",{"QUDoc`"}];


Needs["QUDevTools`"];
Needs["Tensor`"]
Needs["QuantumChannel`"]


$LindbladSolverUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "LindbladSolver.nb"}]];


(* ::Section::Closed:: *)
(*Usage Declaration*)


(* ::Subsection::Closed:: *)
(*Main Functions*)


Unprotect[ODESolver,SchrodingerSolver,LindbladSolver];


AssignUsage[{ODESolver,SchrodingerSolver,LindbladSolver},$LindbladSolverUsages];


(* ::Subsection::Closed:: *)
(*Utility Functions*)


Unprotect[ODECoefficients,ODEVariables,ODEInitialConditions,ODEFirstOrderSystem];


AssignUsage[{ODECoefficients,ODEVariables,ODEInitialConditions,ODEFirstOrderSystem},$LindbladSolverUsages];


(* ::Subsection::Closed:: *)
(*Error Messages*)


ODESolver::initcond = "Input state must be a square matrix or vector.";
ODESolver::symsol = "ODESolver failed to find an symbolic solution.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Preformating State and Lindblad*)


(* ::Text:: *)
(*These functions process the generator input for time-dependent or time-independent matrices and quantum channels.*)


PreformatGenerator[generator_,t_]:=
	Which[
		Head[generator]===Function,
			PreformatGenerator[generator[t],t],
		Head[generator]===QuantumChannel,
			PreformatGenerator[First@Super[generator],t],
		True,generator]


PreformatLindblad[ham_,collapseOps_List]:=
 Function[t,Evaluate[
	With[{
		ham1=PreformatGenerator[ham,t],
		cOps1=Map[PreformatGenerator[#,t]&,PreformatGenerator[collapseOps,t]]},
	First[If[ham1===0,0,-I*ComChannel[ham1]]
		+If[cOps1==={},0,LindbladDissipator[cOps1]]
	]]
]]


PreformatHamiltonian[ham_]:=
	Function[t,
		Evaluate[-I*PreformatGenerator[ham,t]]
	]


PreformatState[initState_]:=
	Which[
		SquareMatrixQ[initState], Flatten@Vec[Normal[initState]],
		GeneralVectorQ[initState], Flatten[Normal[initState]],
		True, Message[ODESolver::initcond]
	]


(* ::Subsection::Closed:: *)
(*Setting up the ODE*)


(* ::Subsubsection::Closed:: *)
(*First order system of equations*)


(* ::Text:: *)
(*Variables for a matrix ODE*)


ODECoefficients[{d_,sym_}]:=Array[sym,d]


(* ::Text:: *)
(*Time dependent variables for a matrix ODE*)


ODEVariables[{d_,t_,sym_}]:=Through[ODECoefficients[{d,sym}][t]]


(* ::Text:: *)
(*Setting up conditions for a first order matrix ODE*)


ODEInitialConditions[initState_,{t0_,sym_}]:=
	With[{state=PreformatState[initState]},
		LogicalExpand[ODEVariables[{Length[state],t0,sym}]==state]]


Options[ODEFirstOrderSystem]:={Compile->False};
ODEFirstOrderSystem[generator_,{t_,sym_},opts:OptionsPattern[ODEFirstOrderSystem]]:=
	With[{
	op=PreformatGenerator[generator,t]},
		If[OptionValue[Compile],
			CompileFirstOrderODE[op,{t,sym}],
			LogicalExpand[
				D[ODEVariables[{Length[op],t,sym}],t]
				==op.ODEVariables[{Length[op],t,sym}]]
	]]


(* ::Subsubsection::Closed:: *)
(*Compiling part of ODE*)


(* ::Text:: *)
(*Compiling RHS of ODE (seems to be slower for most things)*)


CompileODETerm[genTerm_,{t_,sym_}]:=
	Block[{inside,$t,$x},
		ReleaseHold[
			Hold[
				Compile[{{$t,_Real},{$x,_Complex,1}},
					inside,
					Parallelization->True,
				   CompilationTarget->"C"]
			]//.{inside->genTerm,t->$t, sym[m_Integer]:>$x[[m]]}
		]
	]


ClearAll[CompileFirstOrderODE];
CompileFirstOrderODE[generator_,{t_,sym_}]:=
CompileFirstOrderODE[generator,{t,sym}]=
	Block[{inside,genTerms,
		d=Length[generator],
		rhsfun=Unique["rhs"],
		cfun=Unique["cfun"]},
		genTerms=generator.ODECoefficients[{d,sym}];
	MapThread[
		(Evaluate[cfun[#1]]=CompileODETerm[#2,{t,sym}])&,
		{Range[d],genTerms}];
	rhsfun[j_,$t_?NumericQ,$x_]:=Evaluate[cfun[j][$t,$x]];
	Inner[Equal,
		D[ODEVariables[{d,t,sym}],t],
		Map[rhsfun[#,t,ODEVariables[{d,t,sym}]]&,Range[d]],
		And]
]


(* ::Subsection::Closed:: *)
(*Numerically Solving ODE*)


(* ::Subsubsection:: *)
(*General ODE Solver*)


(* ::Text:: *)
(*Filter option function for passing options to NDSolveValue and DSolveValue*)


FilterOptions[targetFun_,opts___]:=Sequence@@FilterRules[{opts},Options[targetFun]];


Options[ODESolver]=Join[{Symbol->"x"},Options[ODEFirstOrderSystem],Options[NDSolve]];


ODESolver[generator_,{initState_,t0_?NumericQ,tf_?NumericQ},opts:OptionsPattern[ODESolver]]:=
	With[{sym=OptionValue[Symbol]},
	With[{init=ODEInitialConditions[initState,{t0,sym}]},
		NDSolveValue[
			And[
				ODEFirstOrderSystem[generator,{t,sym}
				,FilterOptions[ODEFirstOrderSystem,opts]],
				init],
			ODECoefficients[{Length[init],sym}],
			{t,0,tf},
			FilterOptions[NDSolve,opts]]
	]]


(* ::Subsubsection:: *)
(*Schrodinger Equation Solver*)


(* ::Text:: *)
(*Returns the result for ODESolver but with -I coefficient added to the hamiltonian operator as per Schrodingers equation. It also returns the result as a function of time rather than the raw interpolation functions.*)


SchrodingerSolver[ham_,{initState_,t0_?NumericQ,tf_?NumericQ},opts:OptionsPattern[ODESolver]]:=
		Function[t,Evaluate[
			Through[
				ODESolver[PreformatHamiltonian[ham],{initState,t0,tf},opts][t]
	]]]


(* ::Subsubsection:: *)
(*Lindblad Equation Solver*)


(* ::Text:: *)
(*If only one argument is input it behaves the same as ODESolver, but with the addition of devectorizing the output to return a density matrix, it also returns the resulting state as a function of time rather than the raw interpolation functions.*)


LindbladSolver[generator_,{initState_,t0_?NumericQ,tf_?NumericQ},opts:OptionsPattern[ODESolver]]:=
	Function[t,Evaluate[
		Devec[Through[
			ODESolver[generator,{initState,t0,tf},opts][t]]
	]]]


LindbladSolver[
	{ham_,collapseOps_List},
	{initState_,t0_?NumericQ,tf_?NumericQ},
	opts:OptionsPattern[ODESolver]]:=
		LindbladSolver[PreformatLindblad[ham,collapseOps],{initState,t0,tf},opts]


(* ::Subsection:: *)
(*Symbolically Solver ODE*)


(* ::Subsubsection:: *)
(*General Solver*)


ODESolver[generator_,{initState_,t0_},opts:OptionsPattern[ODESolver]]:=
	With[{sym=OptionValue[Symbol]},
	Function[t,
		Evaluate[
			Block[{vars,sys,sol,
				init=ODEInitialConditions[initState,{t0,sym}]},
				vars=ODEVariables[{Length[init],t,sym}];
				sys=And[ODEFirstOrderSystem[generator,{t,sym}],init];
				sol=DSolve[sys,vars,t,FilterOptions[DSolve,opts]];
				If[Head[sol]===DSolve,
					Message[ODESolver::symsol],
					vars/.Flatten[sol]
				]
			]
		]
	]]


ODESolver[generator_,opts:OptionsPattern[ODESolver]]:=
	With[{sym=OptionValue[Symbol]},
	Function[t,
		Evaluate[
			Block[{vars,sol,
				sys=ODEFirstOrderSystem[generator,{t,sym}],vars},
				vars=ODEVariables[{Length[sys],t,sym}];
				sol=DSolve[sys,vars,t,FilterOptions[DSolve,opts]];
				If[Head[sol]===DSolve,
					Message[ODESolver::symsol],
					vars/.Flatten[sol]
				]
			]
		]
	]]


(* ::Subsubsection::Closed:: *)
(*Schrodinger Equation Solver*)


SchrodingerSolver[ham_,{initState_,t0_},opts:OptionsPattern[ODESolver]]:=
	ODESolver[PreformatHamiltonian[ham],{initState,t0},opts]

SchrodingerSolver[ham_,opts:OptionsPattern[ODESolver]]:=ODESolver[PreformatHamiltonian[ham],opts]


(* ::Subsubsection::Closed:: *)
(*Lindblad Equation Solver*)


LindbladSolver[generator_,opts:OptionsPattern[ODESolver]]:=
	With[{sol=ODESolver[generator,opts]},
	ReplacePart[sol,2->Evaluate[Devec[Part[sol,2]]]]]


LindbladSolver[generator_,{initState_,t0_},opts:OptionsPattern[ODESolver]]:=
	With[{sol=ODESolver[generator,{initState,t0},opts]},
	ReplacePart[sol,2->Evaluate[Devec[Part[sol,2]]]]]


LindbladSolver[{ham_,collapseOps_List},opts:OptionsPattern[ODESolver]]:=
		LindbladSolver[PreformatLindblad[ham,collapseOps],opts]


LindbladSolver[{ham_,collapseOps_List},{initState_,t0_},opts:OptionsPattern[ODESolver]]:=
		LindbladSolver[PreformatLindblad[ham,collapseOps],{initState,t0},opts]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


Protect[ODESolver,SchrodingerSolver,LindbladSolver];
Protect[ODECoefficients,ODEVariables,ODEInitialConditions,ODEFirstOrderSystem];


EndPackage[];
