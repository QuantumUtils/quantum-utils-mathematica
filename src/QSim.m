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


(* ::Subsection::Closed:: *)
(*Preamble*)


BeginPackage["QSim`",{"QUDoc`","QuantumChannel`","Predicates`"}];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["LindbladSolver`"];
Needs["Tensor`"];
Needs["QUDevTools`"];


$QSimUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "QSim.nb"}]];


(* ::Section:: *)
(*Usage Declarations*)


(* ::Subsection::Closed:: *)
(*Predicates*)


Unprotect[
	PulseShapeFileQ,PulseShapeMatrixQ,PulseShapeQ,
	ShapedPulseQ,DriftPulseQ,UnitaryPulseQ,ChannelPulseQ,PulseQ,
	PulseSequenceQ,
	DriftHamConstQ,DriftHamTimeDepQ,DriftHamQ,
	LindbladConstQ,LindbladTimeDepQ,LindbladQ,
	GeneratorQ,
	ObservableListQ,FunctionListQ,
	DistributionQ
];


AssignUsage[
	{
		PulseShapeFileQ,PulseShapeMatrixQ,PulseShapeQ,
		ShapedPulseQ,DriftPulseQ,UnitaryPulseQ,ChannelPulseQ,PulseQ,
		PulseSequenceQ,
		DriftHamConstQ,DriftHamTimeDepQ,DriftHamQ,
		LindbladConstQ,LindbladTimeDepQ,LindbladQ,
		GeneratorQ,
		ObservableListQ,FunctionListQ,
		DistributionQ
	},
	$Usages
];


(* ::Subsection::Closed:: *)
(*Options and Types*)


Unprotect[
	StepSize,PollingInterval,InitialState,SimulationOutput,SequenceMode,NumericEvaluation,ForceSuperoperator,
	TimeVector,Superoperators,Unitaries,States,Functions,Observables
];


AssignUsage[
	{
		StepSize,PollingInterval,InitialState,SimulationOutput,SequenceMode,NumericEvaluation,ForceSuperoperator,
		TimeVector,Superoperators,Unitaries,States,Functions,Observables
	},
	$Usages
];


(* ::Subsection::Closed:: *)
(*Auxiliary Functions*)


Unprotect[
	LindbladForm,
	GetPulseShapeMatrix,
	GetStepSize,GetPollingInterval,DivideEvenly,MakeMultipleOf
];


AssignUsage[
	{
		LindbladForm,
		GetPulseShapeMatrix,
		GetStepSize,GetPollingInterval,DivideEvenly,MakeMultipleOf
	},
	$Usages
];


(* ::Subsection::Closed:: *)
(*Simulator*)


Unprotect[PulseSim];
AssignUsage[PulseSim,$Usages];


PulseSim::badControlDim = "The internal Hamiltonian dimension, `1`, is neither equal to nor a multiple of (one of) the control Hamiltonian dimension(s), `2`.";


(* ::Subsection::Closed:: *)
(*Pulse Sequences*)


Unprotect[DrawSequence];
AssignUsage[DrawSequence,$Usages];


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Predicates*)


PulseShapeFileQ[str_]:=StringQ[str]


PulseShapeMatrixQ[M_]:=MatrixQ[M]


PulseShapeQ[in_]:=PulseShapeFileQ[in]||PulseShapeMatrixQ[in]


ShapedPulseQ[p_]:=
	And[
		ListQ[p],
		Length[p]==2,
		ListQ[p[[2]]],
		PulseShapeQ[p[[1]]]
	]


DriftPulseQ[p_]:=(NumericQ[p]&&p\[Element]Reals)||(Head[p]===Symbol)


UnitaryPulseQ[p_]:=And[
	ListQ[p],
	Length[p]==2,
	SquareMatrixQ[p[[1]]],
	DriftPulseQ[p[[2]]]
]


ChannelPulseQ[p_]:=And[
	ListQ[p],
	Length[p]==2,
	QuantumChannel===Head[p[[1]]],
	DriftPulseQ[p[[2]]]
]


PulseQ[p_]:=Or@@(Through[{ShapedPulseQ,DriftPulseQ,UnitaryPulseQ,ChannelPulseQ}[p]])


PulseSequenceQ[seq_]:=ListQ[seq]&&(And@@(PulseQ/@seq))


DriftHamConstQ[H_]:=SquareMatrixQ[H]


DriftHamTimeDepQ[H_]:=SquareMatrixQ[H[0.0]]


DriftHamQ[H_]:=DriftHamConstQ[H]||DriftHamTimeDepQ[H]


LindbladConstQ[L_]:=
	And[
		Length[L]==2,
		Head[L]===LindbladForm,
		DriftHamConstQ[First[L]],
		AllQ[DriftHamConstQ, Last[L]]
	]


LindbladTimeDepQ[L_]:=
	And[
		Length[L]==2,
		Head[L]===LindbladForm,
		AllQ[DriftHamQ, Append[Last[L],First[L]]],
		AnyQ[DriftHamTimeDepQ, Append[Last[L],First[L]]]
	]


LindbladQ[L_]:=LindbladConstQ[L]||LindbladTimeDepQ[L]


GeneratorQ[G_]:=LindbladQ[G]||DriftHamQ[G]


ObservableListQ[obs_]:=ListQ[obs]&&(And@@(SquareMatrixQ/@obs))


FunctionListQ[lst_]:=ListQ[lst]


DistributionQ[dist_]:=ListQ[dist]&&(Length[dist]==2)&&(Length[First@dist]==Length[Last@dist])&&(Mean[Length/@(Last@dist)]==Length[First@Last@dist])


(* ::Subsection::Closed:: *)
(*Options and Helper Functions*)


(* ::Subsubsection::Closed:: *)
(*Options and Input Handling*)


Options[PulseSim]={
	StepSize->Automatic,
	PollingInterval->Off,
	InitialState->None,
	Observables->None,
	Functions->None,
	SimulationOutput->Automatic,
	SequenceMode->False,
	NumericEvaluation->True,
	ForceSuperoperator->False
};


GetPulseShapeMatrix[in_?PulseShapeFileQ]:=With[{out=Import[in]//N},Pick[out,Length[#]>1&/@out]]
GetPulseShapeMatrix[in_?PulseShapeMatrixQ]:=in//N


(* ::Text:: *)
(*If the step size is set to Automatic, let it be a tenth of the biggest element of H maximized over time, otherwise, the user has given the stepsize.*)


GetStepSize[H_,p_,stepsize_:Automatic]:=
	If[stepsize===Automatic,
		Module[{t,max,cons},
			cons=Which[
					ShapedPulseQ[p], 0<=t<=Last[p[[1,All,1]]],
					DriftPulseQ[p], 0<=t<=p,
					True, 0<=t];
			max=Sqrt[NMaximize[{Re@Tr[H[t]\[ConjugateTranspose].H[t]],cons},t][[1]]];
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


(* ::Subsubsection::Closed:: *)
(*Pulse Helpers*)


(* ::Text:: *)
(*This helper function finds out whether the dimension of the internal Hamiltonian is a multiple of the dimension of a given control Hamiltonian. If this multiple exists, the control Hamiltonian is tensored with an identity of the correct size.*)


MakeControlHamiltonian[dim_,Hctl_]:=Module[{ctlDim=Length@Hctl},
	If[dim==ctlDim,
		Hctl,
		If[dim/ctlDim==Round[dim/ctlDim],
			KroneckerProduct[Hctl,IdentityMatrix[dim/ctlDim]],
			Message[PulseSim::badControlDim,dim,ctlDim];
		]
	]
]


(* ::Text:: *)
(*MakeSuperPulse takes a pulse and decides how to turn it into a PulseQ usable by the lindblad evalutator. Most notably, for ShapedPulseQ, we check the dimension of the control Hamiltonians and enhance them to the superoperator space if necessary. See notes below for why we multiply the control supergenerators by "i".*)


MakeSuperPulse[L_?LindbladConstQ,p_?ShapedPulseQ]:=If[
	Length[First@Last@p]===Length[First@L], 
	{First@p, First[ComChannel[#]]&/@(Last@p)},
	{First@p,I * Last@p}
]
MakeSuperPulse[L_?LindbladTimeDepQ,p_?ShapedPulseQ]:=With[{H=If[DriftHamConstQ[First@L],First@L,First[L][0]]},
	If[
		Length[First@Last@p]===Length[H], 
		{First@p, First[ComChannel[#]]&/@(Last@p)},
		{First@p,I * Last@p}
	]
]


(* ::Text:: *)
(*These guys don't need any changes:*)


MakeSuperPulse[L_?LindbladQ,p_?UnitaryPulseQ]:=p
MakeSuperPulse[L_?LindbladQ,p_?ChannelPulseQ]:=p
MakeSuperPulse[L_?LindbladQ,p_?DriftPulseQ]:=p


(* ::Subsubsection::Closed:: *)
(*Output Formatting*)


TimeVector[data_]:=Select[data,(#[[1]]===TimeVector)&,1][[1,2]]


Superoperators[data_]:=Select[data,(#[[1]]===Superoperators)&,1][[1,2]]
Superoperators[data_,t_]:=With[{minpos=Ordering[Abs[t-#]&/@TimeVector[data],1][[1]]},Superoperators[data][[minpos]]]


Unitaries[data_]:=Select[data,(#[[1]]===Unitaries)&,1][[1,2]]
Unitaries[data_,t_]:=With[{minpos=Ordering[Abs[t-#]&/@TimeVector[data],1][[1]]},Unitaries[data][[minpos]]]


States[data_]:=Select[data,(#[[1]]===States)&,1][[1,2]]
States[data_,t_]:=With[{minpos=Ordering[Abs[t-#]&/@TimeVector[data],1][[1]]},States[data][[minpos]]]


Observables[data_,OptionsPattern[{TimeVector->False}]]:=
	With[{obs=Select[data,(#[[1]]===Observables)&,1][[1,2]]},
		If[OptionValue[TimeVector]&&Length[obs]>0,
			With[{tv=TimeVector[data]},{tv,#}\[Transpose]&/@obs],
			obs
		]
	]
Observables[data_,n_,opt:OptionsPattern[{TimeVector->False}]]:=Observables[data,opt][[n]]


Functions[data_,OptionsPattern[{TimeVector->False}]]:=
	With[{obs=Select[data,(#[[1]]===Functions)&,1][[1,2]]},
		If[OptionValue[TimeVector]&&Length[obs]>0,
			With[{tv=TimeVector[data]},{tv,#}\[Transpose]&/@obs],
			obs
		]
	]
Functions[data_,n_,opt:OptionsPattern[{TimeVector->False}]]:=Functions[data,opt][[n]]


(* ::Subsection::Closed:: *)
(*Simulators*)


(* ::Subsubsection::Closed:: *)
(*Shaped Pulse*)


Simulator[H_?DriftHamConstQ,p_?ShapedPulseQ,{NN_,AppendReturnables_},opts:OptionsPattern[PulseSim]]:=
	Module[{dt,ds,pt,t,T,dtAcc,pollTimes,pollQuery,ampQuery,m,U,dim,amps,Hctls,\[Rho]out},

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

		(* return final state *)
		\[Rho]out=OptionValue[InitialState];
		\[Rho]out=If[\[Rho]out===None,None,If[Length[\[Rho]out]===Length[U],U.\[Rho]out.U\[ConjugateTranspose],Devec[U.Vec[\[Rho]out]]]];
		If[OptionValue@SequenceMode, \[Rho]out]
	]


Simulator[H_?DriftHamTimeDepQ,p_?ShapedPulseQ,{NN_,AppendReturnables_},opts:OptionsPattern[PulseSim]]:=
	Module[{dt,ds,pt,t,T,dtAcc,tprev,tcurr,pollTimes,pollQuery,ampQuery,U,dim,m,n,amps,Hctls,nSteps,\[Rho]out},

		With[{pulse=GetPulseShapeMatrix[p[[1]]]},
			amps = NN[If[Length[pulse[[1]]]>2,pulse[[All,2;;-1]],pulse[[All,{2}]]]];
			dt = pulse[[All,1]];
		];
		nSteps=Length[dt];

		(* Make a list of the times at which the pulse amplitude changes *)
		dtAcc=Accumulate[dt];
		T=Last@dtAcc;

		dim=Length[H[0.0]];
		ds=GetStepSize[H,p,OptionValue[StepSize]];
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

		(* return final state *)
		\[Rho]out=OptionValue[InitialState];
		\[Rho]out=If[\[Rho]out===None,None,If[Length[\[Rho]out]===Length[U],U.\[Rho]out.U\[ConjugateTranspose],Devec[U.Vec[\[Rho]out]]]];
		If[OptionValue@SequenceMode, \[Rho]out]
	]


(* ::Subsubsection::Closed:: *)
(*Drift Pulse*)


Simulator[H_?DriftHamConstQ,T_?DriftPulseQ,{NN_,AppendReturnables_},opts:OptionsPattern[PulseSim]]:=
	Module[{dt,U,W,\[Rho]out},

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

		(* return final state *)
		\[Rho]out=OptionValue[InitialState];
		\[Rho]out=If[\[Rho]out===None,None,If[Length[\[Rho]out]===Length[W],W.\[Rho]out.W\[ConjugateTranspose],Devec[W.Vec[\[Rho]out]]]];
		If[OptionValue@SequenceMode, \[Rho]out]
	]


Simulator[H_?DriftHamTimeDepQ,T_?DriftPulseQ,{NN_,AppendReturnables_},opts:OptionsPattern[PulseSim]]:=
	Module[{dt,pt,U,dim,n,\[Rho]out},

		dt=GetStepSize[H,T,OptionValue[StepSize]];
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

		(* return final state *)
		\[Rho]out=OptionValue[InitialState];
		\[Rho]out=If[\[Rho]out===None,None,If[Length[\[Rho]out]===Length[U],U.\[Rho]out.U\[ConjugateTranspose],Devec[U.Vec[\[Rho]out]]]];
		If[OptionValue@SequenceMode, \[Rho]out]
	]


(* ::Subsubsection::Closed:: *)
(*Unitary Pulse*)


Simulator[H_?DriftHamQ,p_?UnitaryPulseQ,{NN_,AppendReturnables_},opts:OptionsPattern[PulseSim]]:=
	(
		AppendReturnables[IdentityMatrix[Length[p[[1]]]],0];
		AppendReturnables[p[[1]],p[[2]]];

		(* return final state *)
		If[OptionValue@SequenceMode, If[OptionValue[InitialState]===None,None,p[[1]].OptionValue[InitialState].p[[1]]\[ConjugateTranspose]]]
	)


(* ::Subsubsection::Closed:: *)
(*Channel Pulse*)


Simulator[H_?DriftHamQ,p_?ChannelPulseQ,{NN_,AppendReturnables_},opts:OptionsPattern[PulseSim]]:=
	Module[{dim,chan},
		chan=First@p;
		dim=InputDim[chan];
		AppendReturnables[IdentityMatrix[dim^2],0];
		AppendReturnables[First@Super[chan],Last@p];
		
		(* return final state *)
		If[OptionValue@SequenceMode, If[OptionValue[InitialState]===None,None,chan[OptionValue[InitialState]]]]
	]


(* ::Subsection::Closed:: *)
(*Single Pulse*)


PulseSim::initstate="You asked for an output which requires an InitialState, but no InitialState was specified.";
PulseSim::notunitary="You asked for an output which requires Unitaries, but you are evolving under nonunitary dynamics. Proceeding using Superoperators.";


PulseSim[G_?GeneratorQ,p_?PulseQ,opts:OptionsPattern[]]:=Module[
	{
		(* Variables to store list of things that will be returned *)
		uniVals={},sosVals={},staVals={},obsVals={},funVals={},timVals={},
		(* Variables to store user-input values *)
		staVar,obsVar,funVar,
		(* Booleans *)
		isLindblad,isChannel,isSuper,hasInputState,requiresInputState,requiresProp,
		(* List of keys to return *)
		outputList,
		(* Function which appends values of interest *)
		AppendReturnables, 
		NN,H,pulse,\[Rho]out
	},

	(* initialize the initial state, observables, and functions *)
	staVar=OptionValue[InitialState];
	obsVar=OptionValue[Observables];
	funVar=OptionValue[Functions];

	(* Determine whether we are doing open quantum systems *)
	isLindblad=LindbladQ[G];
	isChannel=ChannelPulseQ[p];
	isSuper=isLindblad||OptionValue[ForceSuperoperator]||ChannelPulseQ[p]||MemberQ[OptionValue[SimulationOutput],Superoperators];
	hasInputState=SquareMatrixQ[staVar];

	(* decide which function NN is set to *)
	NN=If[OptionValue[NumericEvaluation],N,Identity];

	(* Logic to determine the outputList *)
	If[OptionValue[SimulationOutput]===Automatic,
	(* Automatic Case *)
		outputList = 
			If[hasInputState,
				If[ObservableListQ[obsVar],{Observables},{}],
				If[FunctionListQ[funVar],{},{Unitaries}]
			];
		If[FunctionListQ[funVar],AppendTo[outputList,Functions]];
		If[Length[outputList]===0,outputList={States}];,
	(* Manual Case *)
		outputList=OptionValue[SimulationOutput];
		If[Not[ListQ[outputList]],outputList={outputList}];
		If[isSuper&&MemberQ[outputList,Unitaries],Message[PulseSim::notunitary]];
	];
	AppendTo[outputList,TimeVector];
	requiresInputState=AnyQ[MemberQ[outputList,#]&,{States,Observables}]||(MemberQ[outputList,Functions]&&hasInputState);
	requiresProp=AnyQ[MemberQ[outputList,#]&,{Unitaries,Superoperators}]||(MemberQ[outputList,Functions]&&Not[hasInputState]);
	(* Check for some problems *)
	If[Not[hasInputState]&&requiresInputState,Message[PulseSim::initstate];Abort[];];
	(* We simply replace Unitaries output with Superoperators output in the case where we have dissipation. *)
	If[isSuper,outputList = outputList /. Unitaries->Superoperators;];
	outputList=DeleteDuplicates[outputList];

	(* For each output type, define how new values are appended to the list *)
	(* Here, P is propagator (unitary or super), \[Rho] is density matrix, t is time *)
	AppendReturnables[TimeVector,P_,\[Rho]_,t_]:=AppendTo[timVals,t];
	AppendReturnables[States,P_,\[Rho]_,t_]:=AppendTo[staVals,\[Rho]];
	AppendReturnables[Observables,P_,\[Rho]_,t_]:=AppendTo[obsVals,Re[Tr[#.\[Rho]]]&/@obsVar];
	If[hasInputState,
		AppendReturnables[Functions,P_,\[Rho]_,t_]:=AppendTo[funVals,#[\[Rho]]&/@funVar];,
		(* The following will get called with P as unitary or Super *)
		AppendReturnables[Functions,P_,\[Rho]_,t_]:=AppendTo[funVals,#[P]&/@funVar];
	];
	(* The following will never get called with P as Super *)
	AppendReturnables[Unitaries,P_,\[Rho]_,t_]:=AppendTo[uniVals,P];
	(* The following will never get called with P as unitary *)
	AppendReturnables[Superoperators,P_,\[Rho]_,t_]:=AppendTo[sosVals,P];

	(* Go through logic tree to decide whether we need to actually compute current state, superoperator, etc. *)
	(* basically just want Map[AppendReturnables[#,S,\[Rho]]&,outputList]] where S is super if isSuper, and \[Rho] is computed if requiresInputState *)
	Which[
		requiresInputState&&requiresProp,
			If[isSuper,
				If[isLindblad||isChannel,
					AppendReturnables[S_,t_]:=With[{\[Rho]=Devec[S.Vec[staVar]]},Map[AppendReturnables[#,Super[S],\[Rho],t]&,outputList]];,
					AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},Map[AppendReturnables[#,Super@Unitary[U],\[Rho],t]&,outputList]];
				],
				AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},Map[AppendReturnables[#,U,\[Rho],t]&,outputList]];
			];,
		requiresInputState,
			If[isSuper,
				If[isLindblad||isChannel,
					AppendReturnables[S_,t_]:=With[{\[Rho]=Devec[S.Vec[staVar]]},Map[AppendReturnables[#,None,\[Rho],t]&,outputList]];,
					AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},Map[AppendReturnables[#,None,\[Rho],t]&,outputList]];
				],
				AppendReturnables[U_,t_]:=With[{\[Rho]=U.staVar.U\[ConjugateTranspose]},Map[AppendReturnables[#,None,\[Rho],t]&,outputList]];
			];,
		requiresProp,
			If[isSuper,
				If[isLindblad||isChannel,
					AppendReturnables[S_,t_]:=Map[AppendReturnables[#,Super[S],None,t]&,outputList];,
					AppendReturnables[U_,t_]:=Map[AppendReturnables[#,Super@Unitary[U],None,t]&,outputList];
				],
				AppendReturnables[U_,t_]:=Map[AppendReturnables[#,U,None,t]&,outputList];
			];
	];

	(* To timeslice lindblad master equation, multiply by I and plug it into Hamiltonian time slicer. *)
	If[isLindblad,
		H=If[LindbladTimeDepQ[G],
			Function[t, Evaluate[I*(LindbladSolver`Private`PreformatLindblad@@G)[t]]],
			I*First[Lindblad@@G]
		];
		pulse=MakeSuperPulse[G,p];,
		H=G;
		pulse=p;
	];

	(* Do actual simulation *)
	\[Rho]out=Simulator[H,pulse,{NN,AppendReturnables},opts];

	(* Output results -- be careful not to leak module variables *)
	With[{output=Rule@@@(
		outputList/.{
			Superoperators->{Superoperators,sosVals},
			Unitaries->{Unitaries,uniVals},
			States->{States,staVals},
			Observables->{Observables,If[ArrayDepth[obsVals]>=2,Transpose[obsVals],obsVals]},
			Functions->{Functions,If[ArrayDepth[funVals]>=2,Transpose[funVals],funVals]},
			TimeVector->{TimeVector,timVals}
		}),
		\[Rho]final=\[Rho]out
	},
		ClearAll[uniVals,sosVals,staVals,obsVals,funVals,timVals,
			staVar,obsVar,funVar,isLindblad,isSuper,hasInputState,requiresInputState,requiresProp,
			outputList,AppendReturnables,NN,H,pulse,\[Rho]out
		];
		If[OptionValue[SequenceMode],{\[Rho]final,#},#]&[output]
	]
]


(* ::Subsection::Closed:: *)
(*Pulse Sequence*)


PulseSim[H_?GeneratorQ,seq_?PulseSequenceQ,options:OptionsPattern[]]:=
	Module[{JoinTwoFields,JoinTwoPulseSims,pollingInterval,timeStep,updatingOptions,containsChannel},
		(* Define how to join each kind of output *)
		JoinTwoFields[TimeVector->f1_,TimeVector->f2_]:=TimeVector->Join[f1,Last[f1]+Rest[f2]];
		JoinTwoFields[Unitaries->f1_,Unitaries->f2_]:=Unitaries->Join[f1,(#.Last[f1])&/@Rest[f2]];
		JoinTwoFields[Superoperators->f1_,Superoperators->f2_]:=Superoperators->Join[f1,(#.Last[f1])&/@Rest[f2]];
		JoinTwoFields[States->f1_,States->f2_]:=States->Join[f1,Rest[f2]];
		JoinTwoFields[type_->f1_,type_->f2_]:=type->Join[f1\[Transpose],Rest[f2\[Transpose]]]\[Transpose];

		(* Define how to join two outputs from separate PulseSim calls*)
		JoinTwoPulseSims[p1_,p2_]:=MapThread[JoinTwoFields,{p1,p2},1];
		
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
		containsChannel=AnyQ[ChannelPulseQ,seq];
		updatingOptions:=(Sequence@@Join[{ForceSuperoperator->containsChannel,PollingInterval->pollingInterval,StepSize->timeStep,SequenceMode->True},List[options]]);

		(* Now iteravely join each pulse. We need to deal with the slight complication of passing 
		   the previous final state to the new pulse as InitialState. This is only possible with 
			the SequenceMode option. *)
		Fold[
			With[{newEval=PulseSim[H,#2,InitialState->#1[[1]],updatingOptions]},
				{newEval[[1]],JoinTwoPulseSims[#1[[2]],newEval[[2]]]}
			]&,
			PulseSim[H,First[seq],SequenceMode->True,updatingOptions],
			Rest[seq]
		][[2]]
	]


(* ::Subsection::Closed:: *)
(*Over a Distribution*)


PulseSim[H_?GeneratorQ,pulse_?(PulseQ[#]||PulseSequenceQ[#]&),distribution_?DistributionQ,opt:OptionsPattern[]]:=Module[
	{allData,probs,reps,heads,AddHead,out={}},

	{probs,reps}=distribution;
	allData=Table[
		With[{Hval=H/.rep,pval=pulse/.rep,optval={opt}/.rep},
			PulseSim[Hval,pval,Sequence@@optval]
		],
		{rep,reps}
	];

	heads=allData[[1,All,1]];
	AddHead[head_,val_]:=AppendTo[out,head->val];

	AddHead[TimeVector,TimeVector[First@allData]];
	If[MemberQ[heads,States],
		AddHead[States,Sum[probs[[n]]States[allData[[n]]],{n,Length@probs}]]
	];
	If[MemberQ[heads,Observables],
		AddHead[Observables,Sum[probs[[n]]Observables[allData[[n]]],{n,Length@probs}]]
	];
	If[MemberQ[heads,Functions],
		AddHead[Functions,Sum[probs[[n]]Functions[allData[[n]]],{n,Length@probs}]]
	];
	If[MemberQ[heads,Superoperators],
		AddHead[Superoperators,Sum[probs[[n]]Superoperators[allData[[n]]],{n,Length@probs}]]
	];
	If[MemberQ[heads,Unitaries],
		AddHead[Superoperators,Sum[probs[[n]](Super@Unitary[#]&/@Unitaries[allData[[n]]]),{n,Length@probs}]]
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
		Text["U",{width/2+offset,height/2}],
		Text[If[Length[p[[1]]]<3,p[[1]]//N,"Unitary Pulse"],{width/2+offset,-height/6}]
	}


DrawPulse[p_?ChannelPulseQ,width_,height_,offset_]:=
	{
		Line[{{offset,0},{offset,height},{width+offset,height},{width+offset,0}}],
		Text["S",{width/2+offset,height/2}],
		Text[If[Length[p[[1]]]<3,p[[1]]//N,"Channel Pulse"],{width/2+offset,-height/6}]
	}


DrawPulse[p_?DriftPulseQ,width_,height_,offset_]:=
	{
		Text["\!\(\*SubscriptBox[\(\[ScriptCapitalH]\), \(drift\)]\)",{width/2+offset,-height/8}],
		Text[ShowTime[p],{width/2+offset,-height/4}]
	}


(* ::Text:: *)
(*We give each kind of pulse its own width weight for aesthetics.*)


DrawSequence[seq_?PulseSequenceQ]:=
	Module[{shapedFrac=0.3,uFrac=0.07,sFrac=0.07,driftFrac=0.63,width=500,height=100,widths},
		widths=shapedFrac*(ShapedPulseQ/@seq)/.{True->1,False->0};
		widths=widths+uFrac*(UnitaryPulseQ/@seq)/.{True->1,False->0};
		widths=widths+sFrac*(ChannelPulseQ/@seq)/.{True->1,False->0};
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
(*End Package*)


Protect[
	StepSize,PollingInterval,InitialState,SimulationOutput,SequenceMode,NumericEvaluation,ForceSuperoperator,
	TimeVector,Superoperators,Unitaries,States,Functions,Observables
];


Protect[
	PulseShapeFileQ,PulseShapeMatrixQ,PulseShapeQ,
	ShapedPulseQ,DriftPulseQ,UnitaryPulseQ,ChannelPulseQ,PulseQ,
	PulseSequenceQ,
	DriftHamConstQ,DriftHamTimeDepQ,DriftHamQ,
	LindbladConstQ,LindbladTimeDepQ,LindbladQ,
	GeneratorQ,
	ObservableListQ,FunctionListQ,
	DistributionQ
];


Protect[
	LindbladForm,
	GetPulseShapeMatrix,
	GetStepSize,GetPollingInterval,DivideEvenly,MakeMultipleOf
];


Protect[PulseSim];


Protect[DrawSequence];


EndPackage[];
