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


(* ::Subsection:: *)
(*Preamble*)


BeginPackage["LindbladSolver`"];


Needs["Tensor`"]
Needs["QuantumChannel`"]


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection:: *)
(*Main Functions*)


ODESolver::usage = 
"ODESolver[mat,{init,t0_,tf_},opts] numerically solves the matrix first order ODE x'[t] = mat[t].x[t] from time t0 to tf for initial condition x[t0]=init.
'mat' may be either a matrix, or a pure function of a single variable (t) which evaluates to a matrix.
This is done by calling NDSolveValue, and returns the interpolation function for the coefficients of the vector x.
These may be put into the form of a function of time by calling Through[sol[t]] on the solution sol.

ODESolver[mat,{init,t0},opts] attempts to analytically solve the matrix ODE by calling DSolveValue rather than NDSolveValue for initial condition x[t0]=init.
If successful it returns the solution vector x as a function of time.

ODESolver[mat,opts] attempts to analytically solve the matrix ODE by calling DSolveValue without an initial condition. 
If successful it returns the solution vector x as a function of time in terms of constants C[j] which can later be evaluated for different initial conditions.

NDSolveValue and DSolveValue options will be passed to the respective function.
There is an additional option Symbol->''x'' which allows for specififying a custom symbol to be used for the elements of the vector x in the solution.
This can be used to pass custom options to NDSolveValue such as EvaluationMonitor in terms of these coefficients.";


SchrodingerSolver::usage = 
"SchrodingerSolver[mat,{init,t0_,tf_},opts] numerically solves the Schrodinger equation x'[t] = -I*mat.x[t] from time t0 to tf for initial condition x[t0]=init.
'mat' may be either a matrix, or a pure function of a single variable (t) which evaluates to a matrix.
This is done by calling ODESolver, which then calls NDSolveValue, and returns the interpolation function for the coefficients of the vector x.
These may be put into the form of a function of time by calling Through[sol[t]] on the solution sol.

SchrodingerSolver[mat,{init,t0},opts] attempts to analytically solve the matrix ODE by calling DSolveValue rather than NDSolveValue for initial condition x[t0]=init.
If successful it returns the solution vector x as a function of time.

SchrodingerSolver[mat,opts] attempts to analytically solve the matrix ODE by calling DSolveValue without an initial condition. 
If successful it returns the solution vector x as a function of time in terms of constants C[j] which can later be evaluated for different initial conditions.

NDSolveValue and DSolveValue options will be passed to the respective function.
There is an additional option Symbol->''x'' which allows for specififying a custom symbol to be used for the elements of the vector x in the solution.
This can be used to pass custom options to NDSolveValue such as EvaluationMonitor in terms of these coefficients.";


LindbladSolver::usage = 
"LindbladSolver[mat,{init,t0_,tf_},opts] numerically solves the Schrodinger equation Vec[x]'[t] = -mat.Vec[x][t] from time t0 to tf for initial condition x[t0]=init.
'mat' may be either a matrix or QuantumChannel, or a pure function of a single variable (t) which evaluates to a matrix or QuantumChannel.
This is done by calling ODESolver, which then calls NDSolveValue, and returns the interpolation function for the coefficients of the vector x.
These may be put into the form of a function of time by calling Through[sol[t]] on the solution sol.

LindbladSolver[{ham,cOps},{init,t0_,tf_},opts] calls LindbladSolver for a Lindblad dissipator with Hamiltonian term ham, and collapose operators cOps.
'ham' may be either a matrix, or a pure function of a single variable (t) which evaluates to a matrix.
'cOps' may be a list of matrices, a list of pure functions of a single variable (t) which evaluate to matrices, or a pure function which evalutes to a list of matrices.

LindbladSolver[mat,{init,t0},opts] attempts to analytically solve the matrix ODE by calling DSolveValue rather than NDSolveValue for initial condition x[t0]=init.
If successful it returns the solution vector x as a function of time.

LindbladSolver[{ham,cOps},{init,t0},opts] attempts to analytically solve the matrix ODE by calling DSolveValue rather than NDSolveValue for initial condition x[t0]=init.
If successful it returns the solution vector x as a function of time.

Lindblad[mat,opts] attempts to analytically solve the matrix ODE by calling DSolveValue without an initial condition. 
If successful it returns the solution vector x as a function of time in terms of constants C[j] which can later be evaluated for different initial conditions.

Lindblad[{ham,cOps},opts] attempts to analytically solve the matrix ODE by calling DSolveValue without an initial condition. 
If successful it returns the solution vector x as a function of time in terms of constants C[j] which can later be evaluated for different initial conditions.

NDSolveValue and DSolveValue options will be passed to the respective function.
There is an additional option Symbol->''x'' which allows for specififying a custom symbol to be used for the elements of the vector x in the solution.
This can be used to pass custom options to NDSolveValue such as EvaluationMonitor in terms of these coefficients.";


(* ::Subsection:: *)
(*Utility Functions*)


ODECoefficients::usage = "Write me...";
ODEVariables::usage = "Write me...";
ODEInitialConditions::usage = "Write me...";
ODEFirstOrderSystem::usage = "Write me...";


(* ::Subsection:: *)
(*Error Handling*)


ODESolver::initcond = "Input state must be a square matrix or vector.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


FilterOptions[targetFun_,opts___]:=Sequence@@FilterRules[{opts},Options[targetFun]];


(* ::Subsubsection:: *)
(*Preformating State and Lindblad*)


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


PreformatState[initState_]:=
	Which[
		SquareMatrixQ[initState], Flatten@Vec[initState],
		GeneralVectorQ[initState], Flatten[initState],
		True, Message[ODESolver::initcond]
	]


(* ::Subsubsection:: *)
(*Setting Up ODE*)


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


(* ::Subsubsection:: *)
(*Numerically Solving ODE*)


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
(*Schrodinger Equation Case*)


(* ::Text:: *)
(*Returns the result for ODESolver but with -I coefficient added to the hamiltonian operator as per Schrodingers equation. It also returns the result as a function of time rather than the raw interpolation functions.*)


SchrodingerSolver[ham_,{initState_,t0_?NumericQ,tf_?NumericQ},opts:OptionsPattern[ODESolver]]:=
		Function[t,Evaluate[
			Through[
				ODESolver[-I*ham,{initState,t0,tf},opts][t]
	]]]


(* ::Subsubsection:: *)
(*Lindblad Equation Case*)


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


(* ::Subsubsection:: *)
(*Symbolically Solving ODE*)


ODESolver[generator_,{initState_,t0_},opts:OptionsPattern[ODESolver]]:=
	With[{sym=OptionValue[Symbol]},
	Function[t,
		Evaluate[With[{
			init=ODEInitialConditions[initState,{t0,sym}]},
		DSolveValue[
			And[ODEFirstOrderSystem[generator,{t,sym}],init],
			ODEVariables[{Length[init],t,sym}],
			t,
			FilterOptions[DSolveValue,opts]]]]]
	]


ODESolver[generator_,opts:OptionsPattern[ODESolver]]:=
	With[{sym=OptionValue[Symbol]},
	Function[t,
		Evaluate[
		With[{sys=ODEFirstOrderSystem[generator,{t,sym}]},
		DSolveValue[
			sys,ODEVariables[{Length[sys],t,sym}],t,
			FilterOptions[DSolveValue,opts]]]]
	]]


(* ::Subsubsection:: *)
(*Symbolic Schrodingers*)


SchrodingerSolver[ham_,{initState_,t0_},opts:OptionsPattern[ODESolver]]:=
	ODESolver[-I*ham,{initState,t0},opts]

SchrodingerSolver[ham_,opts:OptionsPattern[ODESolver]]:=ODESolver[-I*ham,opts]


(* ::Subsubsection:: *)
(*Symbolic Lindblad*)


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
(*End*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


EndPackage[];
