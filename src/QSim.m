(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*QSim Package*)


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


BeginPackage["QSim`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["UnitTesting`"];
Needs["QUOptions`"];
Needs["DocTools`"];


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "QSim.nb"}]];


(* ::Section::Closed:: *)
(*Usage Declarations*)


(* ::Subsection::Closed:: *)
(*Predicates*)


LindbladForm::usage = "LindbladForm[H,L1,L2,...] represents the Hamiltonian H with Lindblad dissipators L1, L2, ...";


PulseShapeFileQ::usage = "PulseShapeFileQ[str] returns True iff str is a string pointing to a text file containg a pulse.";
PulseShapeMatrixQ::usage = "PulseShapeMatrixQ[M] returns True iff M is a 2D matrix";
PulseShapeQ::usage = "PulseShapeQ[in] returns True iff one of PulseShapeFileQ or PulseShapeMatrixQ is True";


ShapedPulseQ::usage = "ShapedPulse[p] returns True iff p is of the form {pulse,{Hcontrol1,Hcontrol2,..}} where the Hcontrols are the control Hamiltonians and pulse satisfies PulseShapeQ.";
DriftPulseQ::usage = "DriftPulseQ[p] returns True iff p is a real number indicating the amount of time to evolve under the drift Hamiltonian, or if p is a Symbol.";
UnitaryPulseQ::usage = "UnitaryPulseQ[p] returns True iff p is of the form {U,t} where U is a matrix and t is a number representing how much time you want the unitary pulse to occupy.";
ChannelPulseQ::usage = "ChannelPulseQ[p] returns True iff p is of the form {S,t} where S satisfies ChannelQ (i.e. is a super operator) and t is a number representing how much time you want the channel to occupy.";
PulseQ::usage = "PulseQ[p] returns True iff p satisfies at least one of the QSim predicates ending in PulseQ (ShapedPulseQ,UnitaryPulseQ,etc)";


PulseSequenceQ::usage = "PulseSequenceQ[seq] returns True iff seq is a list where each element satisfies PulseQ.";


DriftHamConstQ::usage = "DriftHamConstQ[H] returns True iff H is a square matrix.";
DriftHamNonConstQ::usage = "DriftHamNonConst[H] returns True iff H is a function which accepts a number and returns a square matrix.";
DriftHamQ::usage = "DriftHamQ[H] returns True iff one of DriftHamConstQ[H] or DriftHamNonConstQ[H] is True.";


LindbladConstQ::usage = "LindbladConstQ[L] returns True iff L has Head LindbladForm, has first argument satisfying DriftHamConstQ, and the rest of the arguments are square matrices. In other words, we are looking for the form LindbladForm[H,L1,L2,...] where H is the Hamiltonian, and the Ls are the lindblad matrices.";
LindbladNonConstQ::usage = "LindbladNonConst[L] returns True iff L has Head LindbladForm, has first argument satisfying DriftHamNonConstQ, and the rest of the arguments are square matrices. In other words, we are looking for the form LindbladForm[H,L1,L2,...] where is a single parameter function returning a Hamiltonian, and the Ls are the lindblad matrices.";
LindbladQ::usage = "LindbladQ[L] returns True iff one of DriftHamConstQ[L] or DriftHamNonConstQ[L] is True.";


DensityMatrixQ::usage = "DensityMatrixQ[\[Rho]] returns True iff \[Rho] is a square matrix.";
ObservableListQ::usage = "ObservableListQ[obs] returns True iff obs is a list of square matrices.";
FunctionListQ::usage = "FunctionListQ[lst] retruns True iff lst is a List.";


DistributionQ::usage = "DistributionQ[dist] returns True iff dist is of the form {{prob1,prob2,...},{{symb1->val11,symb2->val12,...},{symb1->val21,symb2->val22,...},...}}.";


Else=True;


(* ::Subsection::Closed:: *)
(*Options and Helper Functions*)


SimulationOptions::usage = "SimulationOptions is a dummy function which stores the options for various QSim functions. Use the command Options[SimulationOptions] to view these options and their default values.";


StepSize::usage = "StepSize is a simulation option that chooses the time discretization when the internal Hamiltonian is time dependent. Can be set to Automatic.";
PollingInterval::usage = "PollingInterval is a simulation option that specifies the time interval at which results of the simulation should be returned. The default value is Off.";
InitialState::usage = "InitialState is a simulation option. Set this option to the initial density matrix of your system. The default value is None.";
SimulationOutput::usage = "SimulationOutput is a simulation option. Set this option to be one of, or a nonempty subset of, the list {Unitaries,States,Observables,Functions}. These will be the values (and order) of the simulation output. The default value is Automatic.";
SequenceMode::usage = "SequenceMode is a simulation option. If set to True, EvalPulse returns the final state (or None) in addition to the usual output.";
NumericEvaluation::usage = "NumericEvaluation is a simulation option. If set to True, N is called on all times inside MatrixExp, and if set to False, N is not called. The default value is True.";


TimeVector::usage = "TimeVector is a function Head used in a simulation's output. TimeVector[data] can also be used to extract the TimeVector from data.";
Superoperators::usage = "Superoperators is two things: (1) an element of the SimulationOutput list, and (2) a function Superoperators[data] (or Superoperators[data,t]) which exctracts the super operators out of data (or extracts the superoperator which happens at the closest time to t calculated), where data is in the form as outputed by EvalPulse.";
Unitaries::usage = "Unitaries is two things: (1) an element of the SimulationOutput list, and (2) a function Unitaries[data] (or Unitaries[data,t]) which exctracts the unitaries out of data (or extracts the unitary which happens at the closest time to t calculated), where data is in the form as outputed by EvalPulse.";
States::usage = "States is two things: (1) an element of the SimulationOutput list, and (2) a function States[data] (or States[data,t]) which exctracts the states out of data (or extracts the state which happens at the closest time to t calculated), where data is in the form as outputed by EvalPulse.";
Functions::usage = "Functions is three things: (1) simulation option which can be set to a list of functions which take a square matrix as input, (2) an element of the SimulationOutput list, and (3) a function Functions[data] (Functions[data,n]) which extracts all function values (n'th function values) from data, where data is in the format that EvalPulse outputs.";
Observables::usage = "Observables is three things: (1) simulation option which can be set to a list of observables (list of hermitian matrices), (2) an element of the SimulationOutput list, and (3) a function Observables[data] (Observables[data,n]) which extracts all observable values (n'th observable values) from data, where data is in the format that EvalPulse outputs.";


FormatOutputAndReturn::usage="FormatOutputAndReturn[] returns all privately stored simulation data.";


GetPulseShapeMatrix::usage = "GetPulseShapeMatrix[in] returns in if in is a matrix, but if in is a file name, returns the contents of that file as a matrix.";
GetStepSize::usage = "GetStepSize[H,stepsize:Automatic] returns a fifth of the biggest element of H[0] if stepsize is Automatic, and stepsize otherwise.";
GetPollingInterval::usage = "GetPollingInterval[pollingInterval,T] returns T if pollingInterval is Off, and pollingInterval otherwise.";


DivideEvenly::usage = "DivideEvenly[dt,T] returns the largest number no bigger than dt such that T/dt is an integer.";
MakeMultipleOf::usage = "MakeMultipleOf[dt,T] returns the largest integer multiple of dt smaller than T. (And if T<dt, just returns dt.)";


LindbladSuper::usage = "LindbladSuper[L_?LindbladQ] returns I times the super generator of the given LindbladForm in column stacking convention.";


(* ::Subsection::Closed:: *)
(*Single Pulse Evaluator*)


AssignUsage[EvalPulse,$Usages];


(* ::Subsection::Closed:: *)
(*Pulse Sequence Evaluator*)


EvalPulseSequence::usage = "EvalPulseSequence[H,{p1,p2,p3,...}] evaluates the pulse sequence {p1,p2,p3,...} by evaluting each of EvalPulse[H,pi] where everything is properly tied together, ie., the initial state for one pulse is taken to be the final state of the previous pulse, etc. etc.";


(* ::Subsection::Closed:: *)
(*Pulse Evaluator over a Distribution*)


EvalPulseOverDist::usage = "EvalPulseOverDist[H, pulse, distribution, options] evaluates EvalPulse[H, pulse, options] for each member of the distribution and takes the expectation value over results. In the case of Unitaries, the expectation value is taken in superoperator space. The distribution should be formatted as in GDistribution: distribution={{prob1,prob2,...},{{symb1->val11,symb2->val12,...},{symb1->val21,symb2->val22,...},...}}.";
EvalPulseSequenceOverDist::usage = "EvalPulseSequenceOverDist[H, pulse, distribution, options] evaluates EvalPulseSequence[H, pulse, options] for each member of the distribution and takes the expectation value over results. In the case of Unitaries, the expectation value is taken in superoperator space. The distribution should be formatted as in GDistribution: distribution={{prob1,prob2,...},{{symb1->val11,symb2->val12,...},{symb1->val21,symb2->val22,...},...}}.";


(* ::Subsection::Closed:: *)
(*Sequence Drawing*)


DrawSequence::usage = "DrawSequence[seq] outputs a grahpical representation of the pulse sequence seq.";


(* ::Subsection::Closed:: *)
(*Messages*)


(* ::Subsubsection:: *)
(*Single Pulse Evaluator*)


EvalPulse::badControlDim = "The internal Hamiltonian dimension, `1`, is neither equal to nor a multiple of (one of) the control Hamiltonian dimension(s), `2`.";


(* ::Section::Closed:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Predicates*)


PulseShapeFileQ[str_]:=StringQ[str]


PulseShapeMatrixQ[M_]:=MatrixQ[M]


PulseShapeQ[in_]:=PulseShapeFileQ[in]||PulseShapeMatrixQ[in]


ShapedPulseQ[p_]:=ListQ[p]&&(Length[p]==2)&&ListQ[p[[2]]]&&PulseShapeQ[p[[1]]]


DriftPulseQ[p_]:=(NumericQ[p]&&p\[Element]Reals)||(Head[p]===Symbol)


UnitaryPulseQ[p_]:=ListQ[p]&&SquareMatrixQ[p[[1]]]&&(p[[2]]\[Element]Reals)


ChannelPulseQ[p_]:=ListQ[p]&&Superoperator`ChannelQ[p[[1]]]&&(p[[2]]\[Element]Reals)


PulseQ[p_]:=Or@@(Through[{ShapedPulseQ,DriftPulseQ,UnitaryPulseQ,ChannelPulseQ}[p]])


PulseSequenceQ[seq_]:=ListQ[seq]&&(And@@(PulseQ/@seq))


DriftHamConstQ[H_]:=SquareMatrixQ[H]


DriftHamNonConstQ[H_]:=SquareMatrixQ[H[0.0]]


DriftHamQ[H_]:=DriftHamConstQ[H]||DriftHamNonConstQ[H]


LindbladConstQ[L_]:=(Head[L]===LindbladForm)&&DriftHamConstQ[First@L]&&(And@@(SquareMatrixQ/@(Rest@L)))


LindbladNonConstQ[L_]:=(Head[L]===LindbladForm)&&DriftHamNonConstQ[First@L]&&(And@@(SquareMatrixQ/@(Rest@L)))


LindbladQ[L_]:=LindbladConstQ[L]||LindbladNonConstQ[L]


ObservableListQ[obs_]:=ListQ[obs]&&(And@@(SquareMatrixQ/@obs))


DensityMatrixQ[\[Rho]_]:=SquareMatrixQ[\[Rho]]


FunctionListQ[lst_]:=ListQ[lst]


DistributionQ[dist_]:=ListQ[dist]&&(Length[dist]==2)&&(Length[First@dist]==Length[Last@dist])&&(Mean[Length/@(Last@dist)]==Length[First@Last@dist])


(* ::Subsection:: *)
(*Options and Helper Functions*)


(* ::Subsubsection:: *)
(*Options and Input Handling*)


Options[SimulationOptions]={
	StepSize->Automatic,
	PollingInterval->Off,
	InitialState->None,
	Observables->None,
	Functions->None,
	SimulationOutput->Automatic,
	SequenceMode->False,
	NumericEvaluation->True,
	LindbladQ->False
};


GetPulseShapeMatrix[in_?PulseShapeFileQ]:=With[{out=Import[in]//N},Pick[out,Length[#]>1&/@out]]
GetPulseShapeMatrix[in_?PulseShapeMatrixQ]:=in//N


(* ::Text:: *)
(*If the step size is set to Automatic, let it be a tenth of the biggest element of H maximized over time, otherwise, the user has given the stepsize.*)


GetStepSize[H_,stepsize_:Automatic]:=
	If[stepsize===Automatic,
		Module[{t,max},
			max=Sqrt[NMaximize[Max[Abs[H[t]]^2],t][[1]]];
			(* we round down to one significant digit *)
			With[{m=10^Floor[Log10[#]]},Round[#/m]m]&[1/(10*max)]
		],
		stepsize
	]


GetPollingInterval[pollInt_,T_]:=If[pollInt===Off,T,Min[pollInt,T]]
GetPollingInterval[pollInt_,T_,dt_]:=
	If[pollInt===Off,
		T,
		Min[T,MakeMultipleOf[dt,pollInt]]
	]


DivideEvenly[dt_,T_]:=T/Ceiling[T/dt]


MakeMultipleOf[dt_,T_]:=Max[dt,dt*Floor[T/dt]]


(* ::Subsubsection::Closed:: *)
(*Warnings*)


CheckStepSizeVsTotalTime[dt_,T_]:=
	If[dt>T,
		Print["Warning: Your step size dt is bigger than your total evolution time T.\n Setting dt=T..."];
		T,
		dt
	]


(* ::Subsubsection:: *)
(*Superoperator functions*)


(* ::Text:: *)
(*Use column stacking. This means Roth's lemma reads ABC=devec(C\[Transpose]\[CircleTimes]A vec(B))*)


vec[\[Rho]_]:=Flatten[\[Rho]\[Transpose]]
devec[\[Rho]_]:=Partition[\[Rho],Sqrt[Length@\[Rho]]]\[Transpose]


adjointsuper[H_]:=-I(\[DoubleStruckOne]\[CircleTimes]H - H\[Transpose]\[CircleTimes]\[DoubleStruckOne])
conjugation[U_]:=U\[Conjugate]\[CircleTimes]U
lindbladtermsuper[L_]:=L\[Conjugate]\[CircleTimes]L-With[{LL=L\[ConjugateTranspose].L},\[DoubleStruckOne]\[CircleTimes]LL+LL\[Transpose]\[CircleTimes]\[DoubleStruckOne]]/2


LindbladSuper[L_?LindbladConstQ]:=I*(adjointsuper[First@L]+Total[lindbladtermsuper/@(List@@Rest[L])])
LindbladSuper[L_?LindbladNonConstQ]:=Function[t,
	Evaluate[I*(adjointsuper[First[L][t]]+Total[lindbladtermsuper/@(List@@Rest[L])])]
]


(* ::Text:: *)
(*We are nice and allow the control hamiltonians to be input either in regular space, or superoperator space. In either case we need the imaginary fix.*)


MakeSuperPulse[L_?LindbladConstQ,p_?ShapedPulseQ]:=With[{dim=Length@First@L},{
	First@p,
	If[Length[#]===dim, {I*adjointsuper[#]}, {I*#}]&/@(Last@p)
}]
MakeSuperPulse[L_?LindbladNonConstQ,p_?ShapedPulseQ]:=With[{dim=Length[First@L][0]},Print[here];{
	First@p,
	If[Length[#]===dim, {I*adjointsuper[#]}, {I*#}]&/@(Last@p)
}]


(* ::Subsubsection:: *)
(*Private Variables*)


staVar::usage = "A private variable to store the initial state.";
obsVar::usage = "A private variable to store the observables.";
funVar::usage = "A private variable to store the functions.";
funAct::usage = "A private variable which performs the functions in funVar on either the states or unitaries.";


sosVals::usage = "A private variable to store the super operators that will be returned.";
uniVals::usage = "A private variable to store the unitaries that will be returned.";
staVals::usage = "A private variable to store the states that will be returned.";
obsVals::usage = "A private variable to store the observable values that will be returned.";
funVals::usage = "A private variable to store the function values that will be returned.";
timVals::usage = "A private variable to store the polling times.";


returnKey::usage = "A private variable to encode which things our simulation is returning.";
outputList::usage = "A private variable to store which things our simulation is returning.";


NN::usage = "NN is set to N if NumericEvaluation is True, and Identity otherwise.";


AppendReturnables::usage = "A private function for appending new data to the already collected data.";


InitializePrivateVariables[H_,OptionsPattern[SimulationOptions]]:=
	(
		(* initialize output storage containers*)
		uniVals={};
		sosVals={};
		staVals={};
		obsVals={};
		funVals={};
		timVals={};

		(* initialize the initial state, obserables, and monitoring functions *)
		staVar=OptionValue[InitialState];
		obsVar=OptionValue[Observables];
		funVar=OptionValue[Functions];

		(* initialize the outputList *)
		If[OptionValue[SimulationOutput]===Automatic,
			If[DensityMatrixQ[staVar],
					outputList=If[ObservableListQ[obsVar],{Observables},{}];,
					outputList=If[FunctionListQ[funVar],{},{Unitaries}];
			];
			If[FunctionListQ[funVar],AppendTo[outputList,Functions]];
			If[Length[outputList]===0,outputList={States}];,
			outputList=OptionValue[SimulationOutput];
			If[Not[ListQ[outputList]],outputList={outputList}];
			If[Not[DensityMatrixQ[staVar]]&&(MemberQ[outputList,States]||MemberQ[outputList,Observables]||MemberQ[outputList,Functions]),
				Print["Warning: You asked for an output which requires an InitialState, but no InitialState was specified."]
			];
			If[DriftHamQ@H,
				If[MemberQ[outputList,Superoperators],Print["Warning: You asked for an output which requires superoperators, but you are evolving under unitary dynamics."]];,
				If[MemberQ[outputList,Unitaries],Print["Warning: You asked for an output which requires unitaries, but you are evolving under nonunitary dynamics."]];
			];
		];
		AppendTo[outputList,TimeVector];

		(* To avoid doing more logic than the above, we simply replace Unitaries output with Superoperators output in the case where we have dissipation. *)
		If[OptionValue[LindbladQ],
			outputList = outputList /. Unitaries->Superoperators;
		];

		(* The functions can act either on the states or the unitaries/superoperators. The behaviour is to act on states if InitialState exists, and on unitaries/superoperators if not. *)
		If[DensityMatrixQ[staVar],
			If[OptionValue[LindbladQ],
				funAct[U_]:=(#[devec[U.vec[staVar]]])&/@funVar;,
				funAct[U_]:=(#[U.staVar.U\[ConjugateTranspose]])&/@funVar;
			],
			funAct[U_]:=(#[U])&/@funVar;			
		];

		(* decide which function NN is set to *)
		NN=If[OptionValue[NumericEvaluation],N,Identity];

		If[Not@OptionValue[LindbladQ],
			(* initialize the returnKey *)
			returnKey=0;
			If[MemberQ[outputList,Unitaries],returnKey+=2^0];
			If[MemberQ[outputList,States],returnKey+=2^1];
			If[MemberQ[outputList,Observables],returnKey+=2^2];
			If[MemberQ[outputList,Functions],returnKey+=2^3];

			(* for each returnKey we need a different AppendReturnables *)
			Which[
				returnKey==1,AppendReturnables[U_,t_]:=(AppendTo[timVals,t];AppendTo[uniVals,U];),
				returnKey==2,AppendReturnables[U_,t_]:=(AppendTo[timVals,t];AppendTo[staVals,U.staVar.U\[ConjugateTranspose]];),
				returnKey==3,AppendReturnables[U_,t_]:=(AppendTo[timVals,t];AppendTo[uniVals,U];AppendTo[staVals,U.staVar.U\[ConjugateTranspose]];),
				returnKey==4,AppendReturnables[U_,t_]:=(AppendTo[timVals,t];AppendTo[obsVals,Re[Tr[#.U.staVar.U\[ConjugateTranspose]]]&/@obsVar];),
				returnKey==5,AppendReturnables[U_,t_]:=(AppendTo[timVals,t];AppendTo[uniVals,U];AppendTo[obsVals,Re[Tr[#.U.staVar.U\[ConjugateTranspose]]]&/@obsVar];),
				returnKey==6,AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},AppendTo[timVals,t];AppendTo[staVals,\[Rho]];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];],
				returnKey==7,AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},AppendTo[timVals,t];AppendTo[uniVals,U];AppendTo[staVals,\[Rho]];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];],
				returnKey==8,AppendReturnables[U_,t_]:=(AppendTo[timVals,t];AppendTo[funVals,funAct[U]];),
				returnKey==9,AppendReturnables[U_,t_]:=(AppendTo[timVals,t];AppendTo[uniVals,U];AppendTo[funVals,funAct[U]];),
				returnKey==10,AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},AppendTo[timVals,t];AppendTo[staVals,\[Rho]];AppendTo[funVals,funAct[U]]],
				returnKey==11,AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},AppendTo[timVals,t];AppendTo[uniVals,U];AppendTo[staVals,\[Rho]];AppendTo[funVals,funAct[U]]],
				returnKey==12,AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},AppendTo[timVals,t];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];AppendTo[funVals,funAct[U]]],
				returnKey==13,AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},AppendTo[timVals,t];AppendTo[uniVals,U];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];AppendTo[funVals,funAct[U]]],
				returnKey==14,AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},AppendTo[timVals,t];AppendTo[staVals,\[Rho]];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];AppendTo[funVals,funAct[U]]],
				returnKey==15,AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},AppendTo[timVals,t];AppendTo[uniVals,U];AppendTo[staVals,\[Rho]];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];AppendTo[funVals,funAct[U]]],
				Else,AppendReturnables[U_,t_]:=Null
			];,
			(* initialize the returnKey *)
			returnKey=0;
			If[MemberQ[outputList,Superoperators],returnKey+=2^0];
			If[MemberQ[outputList,States],returnKey+=2^1];
			If[MemberQ[outputList,Observables],returnKey+=2^2];
			If[MemberQ[outputList,Functions],returnKey+=2^3];

			(* for each returnKey we need a different AppendReturnables *)
			Which[
				returnKey==1,AppendReturnables[S_,t_]:=(AppendTo[timVals,t];AppendTo[sosVals,S];),
				returnKey==2,AppendReturnables[S_,t_]:=(AppendTo[timVals,t];AppendTo[staVals,devec[S.vec[staVar]]];),
				returnKey==3,AppendReturnables[S_,t_]:=(AppendTo[timVals,t];AppendTo[sosVals,S];AppendTo[staVals,devec[S.vec[staVar]]];),
				returnKey==4,AppendReturnables[S_,t_]:=(AppendTo[timVals,t];AppendTo[obsVals,Re[Tr[#.devec[S.vec[staVar]]]]&/@obsVar];),
				returnKey==5,AppendReturnables[S_,t_]:=(AppendTo[timVals,t];AppendTo[sosVals,S];AppendTo[obsVals,Re[Tr[#.devec[S.vec[staVar]]]]&/@obsVar];),
				returnKey==6,AppendReturnables[S_,t_]:=With[{\[Rho]=devec[S.vec[staVar]]},AppendTo[timVals,t];AppendTo[staVals,\[Rho]];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];],
				returnKey==7,AppendReturnables[S_,t_]:=With[{\[Rho]=devec[S.vec[staVar]]},AppendTo[timVals,t];AppendTo[sosVals,S];AppendTo[staVals,\[Rho]];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];],
				returnKey==8,AppendReturnables[S_,t_]:=(AppendTo[timVals,t];AppendTo[funVals,funAct[S]];),
				returnKey==9,AppendReturnables[S_,t_]:=(AppendTo[timVals,t];AppendTo[sosVals,S];AppendTo[funVals,funAct[S]];),
				returnKey==10,AppendReturnables[S_,t_]:=With[{\[Rho]=devec[S.vec[staVar]]},AppendTo[timVals,t];AppendTo[staVals,\[Rho]];AppendTo[funVals,funAct[S]]],
				returnKey==11,AppendReturnables[S_,t_]:=With[{\[Rho]=devec[S.vec[staVar]]},AppendTo[timVals,t];AppendTo[sosVals,S];AppendTo[staVals,\[Rho]];AppendTo[funVals,funAct[S]]],
				returnKey==12,AppendReturnables[S_,t_]:=With[{\[Rho]=devec[S.vec[staVar]]},AppendTo[timVals,t];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];AppendTo[funVals,funAct[S]]],
				returnKey==13,AppendReturnables[S_,t_]:=With[{\[Rho]=devec[S.vec[staVar]]},AppendTo[timVals,t];AppendTo[sosVals,S];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];AppendTo[funVals,funAct[S]]],
				returnKey==14,AppendReturnables[S_,t_]:=With[{\[Rho]=devec[S.vec[staVar]]},AppendTo[timVals,t];AppendTo[staVals,\[Rho]];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];AppendTo[funVals,funAct[S]]],
				returnKey==15,AppendReturnables[S_,t_]:=With[{\[Rho]=devec[S.vec[staVar]]},AppendTo[timVals,t];AppendTo[sosVals,S];AppendTo[staVals,\[Rho]];AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];AppendTo[funVals,funAct[S]]],
				Else,AppendReturnables[S_,t_]:=Null
			];
		];
	)


(* ::Subsubsection::Closed:: *)
(*Output Formatting*)


FormatOutputAndReturn[]:=
	(
		outputList/.{
			Superoperators->{Superoperators,sosVals},
			Unitaries->{Unitaries,uniVals},
			States->{States,staVals},
			Observables->{Observables,obsVals},
			Functions->{Functions,funVals},
			TimeVector->{TimeVector,timVals}
		}
	)


TimeVector[data_]:=Select[data,(#[[1]]===TimeVector)&,1][[1,2]]


Superoperators[data_]:=Select[data,(#[[1]]===Superoperators)&,1][[1,2]]
Superoperators[data_,t_]:=With[{minpos=Ordering[Abs[t-#]&/@TimeVector[data],1][[1]]},Superoperators[data][[minpos]]]


Unitaries[data_]:=Select[data,(#[[1]]===Unitaries)&,1][[1,2]]
Unitaries[data_,t_]:=With[{minpos=Ordering[Abs[t-#]&/@TimeVector[data],1][[1]]},Unitaries[data][[minpos]]]


States[data_]:=Select[data,(#[[1]]===States)&,1][[1,2]]
States[data_,t_]:=With[{minpos=Ordering[Abs[t-#]&/@TimeVector[data],1][[1]]},States[data][[minpos]]]


Observables[data_,OptionsPattern[{TimeVector->False}]]:=
	With[{obs=Transpose[Select[data,(#[[1]]===Observables)&,1][[1,2]]]},
		If[OptionValue[TimeVector]&&Length[obs]>0,
			With[{tv=TimeVector[data]},{tv,#}\[Transpose]&/@obs],
			obs
		]
	]
Observables[data_,n_,opt:OptionsPattern[{TimeVector->False}]]:=Observables[data,opt][[n]]


Functions[data_,OptionsPattern[{TimeVector->False}]]:=
	With[{obs=Transpose[Select[data,(#[[1]]===Functions)&,1][[1,2]]]},
		If[OptionValue[TimeVector]&&Length[obs]>0,
			With[{tv=TimeVector[data]},{tv,#}\[Transpose]&/@obs],
			obs
		]
	]
Functions[data_,n_,opt:OptionsPattern[{TimeVector->False}]]:=Functions[data,opt][[n]]


(* ::Subsubsection:: *)
(*Epilog*)


(* ::Subsection::Closed:: *)
(*Single Pulse Evaluator*)


(* ::Subsubsection::Closed:: *)
(*Shaped Pulse Evaluators*)


(* ::Text:: *)
(*This helper function finds out whether the dimension of the internal Hamiltonian is a multiple of the dimension of a given control Hamiltonian. If this multiple exists, the control Hamiltonian is tensored with an identity of the correct size.*)


MakeControlHamiltonian[dim_,Hctl_]:=Module[{ctlDim=Length@Hctl},
	If[dim==ctlDim,
		Hctl,
		If[dim/ctlDim==Round[dim/ctlDim],
			KroneckerProduct[Hctl,IdentityMatrix[dim/ctlDim]],
			Message[EvalPulse::badControlDim,dim,ctlDim];
		]
	]
]


(* ::Text:: *)
(*Now begin painful bookkeeping.*)


EvalPulse[H_?DriftHamConstQ,p_?ShapedPulseQ,opts:OptionsPattern[SimulationOptions]]:=
	Module[{dt,ds,pt,t,T,dtAcc,pollTimes,pollQuery,ampQuery,m,U,dim,amps,Hctls},
		InitializePrivateVariables[H,opts];

		With[{pulse=GetPulseShapeMatrix[p[[1]]]},
			amps = NN[If[Length[pulse[[1]]]>2,pulse[[All,2;;-1]],pulse[[All,{2}]]]];
			dt = pulse[[All,1]];
		];

		(* Make a list of the times at which the pulse amplitude changes *)
		dtAcc=Accumulate[dt];
		T=Last@dtAcc;
		
		pt=GetPollingInterval[OptionValue[PollingInterval],T];
		dim=Length[H];

		(* Check size of/resize control Hamiltonians *)
		Hctls = MakeControlHamiltonian[dim,#]&/@p[[2]];

		(* Make a list of the times we need to poll at *)
		pollTimes=Table[s,{s,pt,T,pt}];
		(* For each of the following times, we need to do a MatrixExp *)
		t=Union[pollTimes,dtAcc,SameTest->(#1==#2&)];
		(* For all the times in t, decide which ones are polling times *)
		pollQuery=(#<=Length[pollTimes]&)/@Ordering@DeleteDuplicates[Join[pollTimes,dtAcc],#1==#2&];
		pollQuery[[-1]]=True;
		(* For all the times in t, decide which ones mark a change in pulse amplitudes *)
		ampQuery=(#<=Length[dtAcc]&)/@Ordering@DeleteDuplicates[Join[dtAcc,pollTimes],#1==#2&];

		(* as usual, start with the identity *)
		AppendReturnables[U=IdentityMatrix[dim],0];

		(* m records the current amplitude index *)
		m=1;
		(* loop through all times in t *)
		Table[
			ds=t[[k]]-If[k==1,0,t[[k-1]]];
			U=MatrixExp[-I*NN[ds]*(H+Total[Hctls*amps[[m]]])].U;
			(* only append the current unitary if we are at a polling time *)
			If[pollQuery[[k]],AppendReturnables[U,t[[k]]];];
			(* only increase m if we are at an amplitude change*)
			If[ampQuery[[k]]&&m<Length[dt],m++;];,
			{k,Length[t]}
		];

		If[OptionValue[SequenceMode],
			{If[OptionValue[InitialState]===None,None,U.OptionValue[InitialState].U\[ConjugateTranspose]],FormatOutputAndReturn[]},
			FormatOutputAndReturn[]
		]
	]


EvalPulse[H_?DriftHamNonConstQ,p_?ShapedPulseQ,opts:OptionsPattern[SimulationOptions]]:=
	Module[{dt,ds,pt,t,T,dtAcc,tprev,tcurr,pollTimes,pollQuery,ampQuery,U,dim,m,n,amps,Hctls,nSteps},
		InitializePrivateVariables[H,opts];

		With[{pulse=GetPulseShapeMatrix[p[[1]]]},
			amps = NN[If[Length[pulse[[1]]]>2,pulse[[All,2;;-1]],pulse[[All,{2}]]]];
			dt = pulse[[All,1]];
		];
		nSteps=Length[dt];

		(* Make a list of the times at which the pulse amplitude changes *)
		dtAcc=Accumulate[dt];
		T=Last@dtAcc;

		dim=Length[H[0.0]];
		ds=GetStepSize[H,OptionValue[StepSize]];
		pt=GetPollingInterval[OptionValue[PollingInterval],T,ds];

		(* Check size of/resize control Hamiltonians *)
		Hctls = MakeControlHamiltonian[dim,#]&/@p[[2]];

		(* Make a list of the times we need to poll at *)
		pollTimes=Table[s,{s,pt,T,pt}];
		(* At the following times we either need to switch amplitudes or do a poll *)
		t=Union[pollTimes,dtAcc,SameTest->(#1==#2&)];
		(* For all the times in t, decide which ones are polling times *)
		pollQuery=(#<=Length[pollTimes]&)/@Ordering@DeleteDuplicates[Join[pollTimes,dtAcc],#1==#2&];
		pollQuery[[-1]]=True;
		(* For all the times in t, decide which ones mark a change in pulse amplitudes *)
		ampQuery=(#<=Length[dtAcc]&)/@Ordering@DeleteDuplicates[Join[dtAcc,pollTimes],#1==#2&];

		(* as usual, start with the identity *)
		AppendReturnables[U=IdentityMatrix[dim],0];

		(* m records the current amplitude index *)
		m=1;
		(* loop through all times in t *)
		Table[
			tcurr=t[[k]];
			tprev=If[k==1,0,t[[k-1]]];
			n=Floor[(tcurr-tprev)/ds];
			Table[
				U=MatrixExp[-I*NN[ds]*(H[NN[tprev+(l-0.5)*ds]]+Total[Hctls*amps[[m]]])].U;,
				{l,n}
			];
			(* Change dt to be the leftover bit, and if its greater than 0, evolve for it *)
			dt=tcurr-tprev-n*ds;
			If[dt>0,U=MatrixExp[-I*NN[dt]*(H[NN[tcurr-dt/2]]+Total[Hctls*amps[[m]]])].U;];
			(* only append the current unitary if we are at a polling time *)
			If[pollQuery[[k]],AppendReturnables[U,tcurr];];
			(* only increase m if we are at an amplitude change*)
			If[ampQuery[[k]]&&m<nSteps,m++;];,
			{k,Length[t]}
		];

		If[OptionValue[SequenceMode],
			{If[OptionValue[InitialState]===None,None,U.OptionValue[InitialState].U\[ConjugateTranspose]],FormatOutputAndReturn[]},
			FormatOutputAndReturn[]
		]
	]


(* ::Subsubsection:: *)
(*Instantaneous Pulse Evaluators*)


EvalPulse[H_?DriftHamQ,p_?UnitaryPulseQ,opts:OptionsPattern[SimulationOptions]]:=
	(
		InitializePrivateVariables[H,opts];
		AppendReturnables[IdentityMatrix[Length[p[[1]]]],0];
		AppendReturnables[p[[1]],p[[2]]];
		If[OptionValue[SequenceMode],
			With[{\[Rho]=OptionValue[InitialState]},{If[\[Rho]===None,None,p[[1]].\[Rho].p[[1]]\[ConjugateTranspose]],FormatOutputAndReturn[]}],
			FormatOutputAndReturn[]
		]
	)


EvalPulse[H_?DriftHamQ,p_?ChannelPulseQ,opts:OptionsPattern[SimulationOptions]]:=
	Module[{\[Rho]out,dim},
		InitializePrivateVariables[H,opts];
		
		If[MemberQ[outputList,Unitaries],
			Print["Error: You cannot perform a ChannelPulse and ask for Unitaries as an output. Specify an InitialState and/or remove Unitaries from SimulationOptions."];
			Abort[];
		];
	
		dim=If[DriftHamConstQ[H],Length[H],Length[H[0.0]]];
		
		AppendReturnables[IdentityMatrix[dim],0];
		
		(* We cannot call AppendReturnables here because no unitary actually exists. *)
		(* Since the above If statement ensures Unitaries is not an output, and implicitly that InitialState has been entered, 
		   we append necessary information manually without risk. *)
		(* This is only kind of a hack *)
		
		AppendTo[timVals,p[[2]]];
		\[Rho]out=(p[[1]])[OptionValue[InitialState]];
		If[MemberQ[outputList,States],AppendTo[staVals,\[Rho]out];];
		If[MemberQ[outputList,Observables],AppendTo[obsVals,Re[Tr[#.\[Rho]out]]&/@obsVar];];
		If[MemberQ[outputList,Functions],AppendTo[funVals,(#[\[Rho]out])&/@funVar];];
		

		If[OptionValue[SequenceMode],
			{\[Rho]out,FormatOutputAndReturn[]},
			FormatOutputAndReturn[]
		]
	]


(* ::Subsubsection:: *)
(*Drift Pulse Evaluators*)


EvalPulse[H_?DriftHamConstQ,T_?DriftPulseQ,opts:OptionsPattern[SimulationOptions]]:=
	Module[{dt,U,W},

		InitializePrivateVariables[H,opts];

		dt=GetPollingInterval[OptionValue[PollingInterval],T];
		(* reduce  the size of dt so that it divides T into an integer number of pieces *)
		dt=DivideEvenly[dt,T];

		(* we only need to exponentiate for a time dt, and every other step is a multiple of this *)
		U=MatrixExp[-I*NN[dt]*H];
		AppendReturnables[W=IdentityMatrix[Length[H]],0];
		Table[
			W=W.U;
			AppendReturnables[W,k*dt];,
			{k,Round[T/dt]}
		];

		If[OptionValue[SequenceMode],
			{If[OptionValue[InitialState]===None,None,W.OptionValue[InitialState].W\[ConjugateTranspose]],FormatOutputAndReturn[]},
			FormatOutputAndReturn[]
		]
	]


EvalPulse[H_?DriftHamNonConstQ,T_?DriftPulseQ,opts:OptionsPattern[SimulationOptions]]:=
	Module[{dt,pt,U,dim,n},

		InitializePrivateVariables[H,opts];

		dt=GetStepSize[H,OptionValue[StepSize]];
		dt=CheckStepSizeVsTotalTime[dt,T];
		dim=Length[H[0]];

		If[OptionValue[PollingInterval]===Off,
			AppendReturnables[U=IdentityMatrix[dim],0];
			Table[
				U=MatrixExp[-I*NN[dt]*H[NN[t-dt/2]]].U;,
				{t,dt,T,dt}
			];
			(* tack on the remaining <dt *)
			dt=T-Floor[T/dt]*dt;
			U=MatrixExp[-I*NN[dt]*H[NN[T-dt/2]]].U;
			AppendReturnables[U,T];
			,

			(* pt is forced to be an integer multiple of dt*)
			pt=GetPollingInterval[OptionValue[PollingInterval],T,dt];

			(* now we need to loop through multiples of pt *)
			AppendReturnables[U=IdentityMatrix[dim],0];
			Table[
				(* in the following loop we know dt evenly divides pt *)
				Table[U=MatrixExp[-I*NN[dt]*H[NN[(t-pt)+s-dt/2]]].U;,{s,dt,pt,dt}];
				AppendReturnables[U,t];,
				{t,pt,T,pt}
			];
			(* We cannot expect either dt or pt to divide T evenly, so we need to tack on two more things *)
			(* first tack on all dts which fit in the last <pt*)
			Table[U=MatrixExp[-I*NN[dt]*H[NN[s-dt/2]]].U;,{s,Floor[T/pt]*pt+dt,T,dt}];
			(* then tack on the remaining <dt *)
			dt=T-Floor[T/dt]*dt;
			U=MatrixExp[-I*NN[dt]*H[NN[T-dt/2]]].U;
			AppendReturnables[U,T];
		];

		If[OptionValue[SequenceMode],
			{If[OptionValue[InitialState]===None,None,U.OptionValue[InitialState].U\[ConjugateTranspose]],FormatOutputAndReturn[]},
			FormatOutputAndReturn[]
		]
	]


(* ::Subsubsection:: *)
(*Lindblad Pulse Evaluators*)


(* ::Text:: *)
(*MakeSuperPulse takes a pulse and decides how to turn it into a PulseQ usable by the lindblad evalutator. Most notably, for ShapedPulseQ, we check the dimension of the control Hamiltonians and enhance them to the superoperator space if necessary. See notes below for why we multiply the control supergenerators by "i".*)


MakeSuperPulse[L_?LindbladConstQ,p_?ShapedPulseQ]:=If[
	Length[First@Last@p]===Length[First@L], 
	{First@p,I * adjointsuper/@(Last@p)},
	{First@p,I * Last@p}
]
MakeSuperPulse[L_?LindbladNonConstQ,p_?ShapedPulseQ]:=If[
	Length[First@Last@p]===Length[First[L][0]], 
	{First@p,I * adjointsuper/@(Last@p)},
	{First@p,I * Last@p}
]


(* ::Text:: *)
(*These guys don't need any changes:*)


MakeSuperPulse[L_?LindbladQ,p_?UnitaryPulseQ]:=p
MakeSuperPulse[L_?LindbladQ,p_?ChannelPulseQ]:=p
MakeSuperPulse[L_?LindbladQ,p_?DriftPulseQ]:=p


(* ::Text:: *)
(*We can just call the closed system evaluators, but give it a supergenerator instead of a Hamiltonian. Afterall, the most difficult thing EvalPulse does is work out time slicing.*)


(* ::Text:: *)
(*Since all MatrixExp calls in the DriftHamQ evaluators include the "-i" factor in front of the Hamiltonian, we need to be sure to multiply our super operators by "i" to undo this.*)


EvalPulse[L_?LindbladQ,p_?PulseQ,opts:OptionsPattern[SimulationOptions]]:=EvalPulse[
	LindbladSuper@L,
	MakeSuperPulse[L,p],
	LindbladQ->True,
	opts
]


(* ::Subsection::Closed:: *)
(*Pulse Sequence Evaluator*)


EvalPulseSequence[H_?DriftHamQ,seq_?PulseSequenceQ,options:OptionsPattern[SimulationOptions]]:=
	Module[{JoinTwoFields,JoinTwoEvalPulses,pollingInterval,timeStep,updatingOptions},
		(* Define how to join each kind of output *)
		JoinTwoFields[{TimeVector,f1_},{TimeVector,f2_}]:={TimeVector,Join[f1,Last[f1]+Rest[f2]]};
		JoinTwoFields[{Unitaries,f1_},{Unitaries,f2_}]:={Unitaries,Join[f1,(#.Last[f1])&/@Rest[f2]]};
		JoinTwoFields[{Superoperators,f1_},{Superoperators,f2_}]:={Superoperators,Join[f1,(#.Last[f1])&/@Rest[f2]]};
		JoinTwoFields[{type_,f1_},{type_,f2_}]:={type,Join[f1,Rest[f2]]};

		(* Define how to join two outputs from separate EvalPulse calls*)
		JoinTwoEvalPulses[p1_,p2_]:=MapThread[JoinTwoFields,{p1,p2},1];
		
		(* Allow a different PollingInterval and TimeStep to be specified for each member of the pulse sequence. *)
		If[ListQ[OptionValue[PollingInterval]],
			If[Length@OptionValue@PollingInterval<Length@seq,Print["If Specifying multiple PollingIntervals, exactly one must be given for each item in your sequence."];Abort[];];
			Module[{n=1},pollingInterval:=(OptionValue[PollingInterval][[n++]])];,
			pollingInterval=OptionValue[PollingInterval];
		];
		If[ListQ[OptionValue[StepSize]],
			If[Length@OptionValue@StepSize<Length@seq,Print["If Specifying multiple TimeSteps, exactly one must be given for each item in your sequence."];Abort[];];
			Module[{n=1},timeStep:=(OptionValue[StepSize][[n++]])];,
			timeStep=OptionValue[StepSize];
		];
		updatingOptions:=(Sequence@@Join[{PollingInterval->pollingInterval,StepSize->timeStep,SequenceMode->True},List[options]]);

		(* Now iteravely join each pulse. We need to deal with the slight complication of passing 
		   the previous final state to the new pulse as InitialState. This is only possible with 
			the SequenceMode option. *)
		Fold[
			With[{newEval=EvalPulse[H,#2,InitialState->#1[[1]],updatingOptions]},
				{newEval[[1]],JoinTwoEvalPulses[#1[[2]],newEval[[2]]]}
			]&,
			EvalPulse[H,First[seq],SequenceMode->True,updatingOptions],
			Rest[seq]
		][[2]]
	]


(* ::Subsection::Closed:: *)
(*Pulse Evaluator over a Distribution*)


EvalPulseOverDist[H_?DriftHamQ,pulse_?PulseQ,distribution_?DistributionQ,opt:OptionsPattern[SimulationOptions]]:=Module[
	{allData,probs,reps,heads,AddHead,out={}},

	{probs,reps}=distribution;
	allData=Table[
		With[{Hval=H/.rep,pval=pulse/.rep,optval={opt}/.rep},
			EvalPulse[Hval,pval,Sequence@@optval]
		],
		{rep,reps}
	];

	heads=allData[[1,All,1]];
	AddHead[head_,val_]:=AppendTo[out,{head,val}];

	AddHead[TimeVector,TimeVector[First@allData]];
	If[MemberQ[heads,States],
		AddHead[States,Sum[probs[[n]]States[allData[[n]]],{n,Length@probs}]]
	];
	If[MemberQ[heads,Observables],
		AddHead[Observables,Sum[probs[[n]]Observables[allData[[n]]]\[Transpose],{n,Length@probs}]]
	];
	If[MemberQ[heads,Functions],
		AddHead[Functions,Sum[probs[[n]]Functions[allData[[n]]]\[Transpose],{n,Length@probs}]]
	];
	If[MemberQ[heads,Superoperators],
		AddHead[Superoperators,Sum[probs[[n]]Superoperators[allData[[n]]],{n,Length@probs}]]
	];
	If[MemberQ[heads,Unitaries],
		AddHead[Superoperators,Sum[probs[[n]](conjugation/@Unitaries[allData[[n]]]),{n,Length@probs}]]
	];
	out
]


EvalPulseSequenceOverDist[H_?DriftHamQ,pulse_?PulseSequenceQ,distribution_?DistributionQ,opt:OptionsPattern[SimulationOptions]]:=Module[
	{allData,probs,reps,heads,AddHead,out={}},

	{probs,reps}=distribution;
	allData=Table[
		With[{Hval=H/.rep,pval=pulse/.rep,optval={opt}/.rep},
			EvalPulseSequence[Hval,pval,Sequence@@optval]
		],
		{rep,reps}
	];

	heads=allData[[1,All,1]];
	AddHead[head_,val_]:=AppendTo[out,{head,val}];

	AddHead[TimeVector,TimeVector[First@allData]];
	If[MemberQ[heads,States],
		AddHead[States,Sum[probs[[n]]States[allData[[n]]],{n,Length@probs}]]
	];
	If[MemberQ[heads,Observables],
		AddHead[Observables,Sum[probs[[n]]Observables[allData[[n]]]\[Transpose],{n,Length@probs}]]
	];
	If[MemberQ[heads,Functions],
		AddHead[Functions,Sum[probs[[n]]Functions[allData[[n]]]\[Transpose],{n,Length@probs}]]
	];
	If[MemberQ[heads,Superoperators],
		AddHead[Superoperators,Sum[probs[[n]]Superoperators[allData[[n]]],{n,Length@probs}]]
	];
	If[MemberQ[heads,Unitaries],
		AddHead[Superoperators,Sum[probs[[n]](conjugation/@Unitaries[allData[[n]]]),{n,Length@probs}]]
	];
	out
]


(* ::Subsection::Closed:: *)
(*Sequence Drawing*)


(* ::Text:: *)
(*A helper function to print out pulse times in appropriate units.*)


ShowTime[t_?NumberQ]:=
	With[{O=Function[{x},ToString[Round[x]]]},
		Which[
			10^-3<=t<1,O[t*10^3]<>"ms",
			10^-6<=t<10^-3,O[t*10^6]<>"\[Mu]s",
			10^-9<=t<10^-6,O[t*10^9]<>"ns",
			10^-12<=t<10^-9,O[t*10^12]<>"ps",
			True,ToString[PaddedForm[t,{3,1}]]<>"s"
		]
	]


(* ::Text:: *)
(*For shaped pulses we just pick a nice looking shape instead of using the actual data.*)


DrawPulse[p_?ShapedPulseQ,width_,height_,offset_]:=
	Module[{minx,maxx,n,d,data,totaltime},
		minx = -2.1;
		maxx = 3;
		n = 30;
		d=(maxx-minx)/(n-1);
		data = (Exp[-(#-1)^2]+Exp[-3*(#+1)^2]/2)&/@Range[minx,maxx,d];
		data = height*data/Max[data];
		d = width/n;
		totaltime = Total[GetPulseShapeMatrix[p[[1]]][[All,1]]];
		{
			Table[Line[{{(k-1)*d+offset,data[[k]]},{k*d+offset,data[[k]]}}],{k,n}],
			Table[Line[{{k*d+offset,0},{k*d+offset,Max[data[[k]],data[[k+1]]]}}],{k,1,n-1}],
			Text[p[[1]],{d*n/2+offset,-height/8}],
			Text[ShowTime[totaltime],{d*n/2+offset,-height/4}]
		}
	]


DrawPulse[p_?UnitaryPulseQ,width_,height_,offset_]:=
	{
		Line[{{offset,0},{offset,height},{width+offset,height},{width+offset,0}}],
		Text[If[Length[p[[1]]]<3,p[[1]]//N,"Instant Pulse"],{width/2+offset,-height/6}]
	}


DrawPulse[p_?DriftPulseQ,width_,height_,offset_]:=
	{
		Text["\!\(\*SubscriptBox[\(\[ScriptCapitalH]\), \(drift\)]\)",{width/2+offset,-height/8}],
		Text[ShowTime[p],{width/2+offset,-height/4}]
	}


(* ::Text:: *)
(*We give each kind of pulse its own width weight for aesthetics.*)


DrawSequence[seq_?PulseSequenceQ]:=
	Module[{shapedFrac=0.3,instFrac=0.07,driftFrac=0.63,width=500,height=100,widths},
		widths=shapedFrac*(ShapedPulseQ/@seq)/.{True->1,False->0};
		widths=widths+instFrac*(UnitaryPulseQ/@seq)/.{True->1,False->0};
		widths=widths+driftFrac*(DriftPulseQ/@seq)/.{True->1,False->0};
		widths=width*widths/Total[widths];
		Graphics[{
			Arrowheads[0.02],
			Arrow[{{0,0},{width,0}},-width/20],
			Text["t",{width+width/15,0}],
			Table[DrawPulse[seq[[k]],widths[[k]],height*0.8,Total[Take[widths,k]]-widths[[k]]],{k,Length[seq]}]
		}]
	]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*Unit Testing*)


(* ::Subsection:: *)
(*Helper Functions*)


(* ::Subsection:: *)
(*Single Pulse Evaluator*)


(* ::Subsection:: *)
(*Pulse Sequence Evaluator*)


(* ::Subsection:: *)
(*Pulse Evaluator over a Distribution*)


(* ::Subsection:: *)
(*Sequence Drawing*)


(* ::Section:: *)
(*End Package*)


Protect[PollingInterval,StepSize,IntitialState,NumericEvaluation,Observables,Functions,SimulationOutput,Unitaries,States,TimeVector];


EndPackage[];
