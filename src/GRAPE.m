(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*GRAPE Package*)


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


BeginPackage["GRAPE`",{"QUDoc`"}];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];
Needs["Visualization`"];
Needs["QSim`"];
Needs["Tensor`"];


$GRAPEUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "GRAPE.nb"}]];


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection::Closed:: *)
(*Options*)


Unprotect[
	Repetitions,ParameterDistribution,DistortionOperator,ForceDistortionDependence,
	PulsePenalty,DerivativeMask,PulseLegalizer,
	ControlLimitPolicy,MonitorFunction,InitialStepSize,MinimumStepSize,
	LineSearchMethod,MinimumImprovement,MinimumIterations,MaximumIterations,
	SkipChecks,VerboseAscent,
	Ignore,ProjectGradient
];


AssignUsage[
	{
		Repetitions,ParameterDistribution,DistortionOperator,ForceDistortionDependence,
		PulsePenalty,DerivativeMask,PulseLegalizer,
		ControlLimitPolicy,MonitorFunction,InitialStepSize,MinimumStepSize,
		LineSearchMethod,MinimumImprovement,MinimumIterations,MaximumIterations,
		SkipChecks,VerboseAscent,
		Ignore,ProjectGradient
	},
	$GRAPEUsages
];


(* ::Subsection::Closed:: *)
(*Pulses*)


Unprotect[
	Pulse,TimeSteps,UtilityValue,PenaltyValue,Target,ControlHamiltonians,
	InternalHamiltonian,AmplitudeRange,ExitMessage,
	ToPulse,FromPulse,SimForm,AddTimeSteps,SplitPulse,
	PulseRemoveKeys,PulseReplaceKey,PulseHasKey,
	PulsePhaseRotate,PulsePhaseRamp,PulseDivide,PulseModulate,
	RandomPulse,RandomSmoothPulse,GenerateAnnealedPulse,AnnealingGenerator,GaussianTailsPulse,
	LegalizePulse,NormalizePulse
];


AssignUsage[
	{
		Pulse,TimeSteps,UtilityValue,PenaltyValue,Target,ControlHamiltonians,
		InternalHamiltonian,AmplitudeRange,ExitMessage,
		ToPulse,FromPulse,SimForm,AddTimeSteps,SplitPulse,
		PulseRemoveKeys,PulseReplaceKey,PulseHasKey,
		PulsePhaseRotate,PulsePhaseRamp,PulseDivide,PulseModulate,
		RandomPulse,RandomSmoothPulse,GenerateAnnealedPulse,AnnealingGenerator,GaussianTailsPulse,
		LegalizePulse,NormalizePulse
	},
	$GRAPEUsages
];


(* ::Subsection::Closed:: *)
(*Utility Function and Targets*)


Unprotect[
	Utility,UtilityGradient,
	PropagatorFromPulse,PropagatorListFromPulse,
	CoherentSubspaces
];


AssignUsage[
	{
		Utility,UtilityGradient,
		PropagatorFromPulse,PropagatorListFromPulse,
		CoherentSubspaces
	},
	$GRAPEUsages
];


(* ::Subsection::Closed:: *)
(*Distortions*)


Unprotect[
	ApplyInverseDistortion,
	LiftDistortionRank,JoinDistortions,ComposeDistortions,PerturbateDistortion,ScaleDistortion,
	IdentityDistortion,
	TimeScaleDistortion,VariableChangeDistortion,
	ConvolutionDistortion,ExponentialDistortion,
	IQDistortion,
	NonlinearTransferDistortion,
	DEDistortion,DESolver,DESolverArgs,
	LinearDEDistortion,CompensationTimeSteps,DecayRatios,StateMetric,
	FrequencySpaceDistortion,
	CompositePulseDistortion
];


AssignUsage[
	{
		ApplyInverseDistortion,
		LiftDistortionRank,JoinDistortions,ComposeDistortions,PerturbateDistortion,ScaleDistortion,
		IdentityDistortion,
		TimeScaleDistortion,VariableChangeDistortion,
		ConvolutionDistortion,ExponentialDistortion,
		IQDistortion,
		NonlinearTransferDistortion,
		DEDistortion,DESolver,DESolverArgs,
		LinearDEDistortion,CompensationTimeSteps,DecayRatios,StateMetric,
		FrequencySpaceDistortion,
		CompositePulseDistortion
	},
	$GRAPEUsages
];


(* ::Subsection::Closed:: *)
(*Distributions*)


Unprotect[
	IdentityDistribution,
	ParameterDistributionMean,
	ProductParameterDistribution,
	HistogramParameterDistribution,
	RandomSampleParameterDistribution,RandomMultinormalParameterDistribution,RandomUniformParameterDistribution,
	UniformParameterDistribution,
	TargetSelectorDistribution
];


AssignUsage[
	{
		IdentityDistribution,
		ParameterDistributionMean,
		ProductParameterDistribution,
		HistogramParameterDistribution,
		RandomSampleParameterDistribution,RandomMultinormalParameterDistribution,RandomUniformParameterDistribution,
		UniformParameterDistribution,
		TargetSelectorDistribution
	},
	$GRAPEUsages
];


(* ::Subsection::Closed:: *)
(*Pulse Penalties*)


Unprotect[
	ZeroPenalty,
	DemandValuePenalty,
	RingdownPenalty
];


AssignUsage[
	{
		ZeroPenalty,
		DemandValuePenalty,
		RingdownPenalty
	},
	$GRAPEUsages
];


(* ::Subsection::Closed:: *)
(*Plotting Tools*)


Unprotect[
	PulsePlot,PulseFourierPlot,
	ShowDistortedPulse,ChannelMapping,PulseScaling,PulseLayout,PulsePaddingMultiplier,
	DistributeOption,
	RobustnessPlot,LegendIsCell,DistortionOperatorSweep
];


AssignUsage[
	{
		PulsePlot,PulseFourierPlot,
		ShowDistortedPulse,ChannelMapping,PulseScaling,PulseLayout,PulsePaddingMultiplier,
		DistributeOption,
		RobustnessPlot,LegendIsCell,DistortionOperatorSweep
	},
	$GRAPEUsages
];


RobustnessPlot::keys = "RobustnessPlot requires all input pulses to have at least the following keys: TimeSteps, Pulse, InternalHamiltonian, ControlHamiltonians, Target, and DistortionOperator";


(* ::Subsection::Closed:: *)
(*Monitor Functions*)


Unprotect[FidelityMonitor,HistogramMonitor,PulsePlotMonitor];


AssignUsage[{FidelityMonitor,HistogramMonitor,PulsePlotMonitor},$GRAPEUsages];


(* ::Subsection::Closed:: *)
(*Gradient Ascent Tools*)


Unprotect[
	QuadraticFitLineSearch,InterpolatedLineSearch,
	MinStepMul,StepMulStep,MaxStepMul
];


AssignUsage[
	{
		QuadraticFitLineSearch,InterpolatedLineSearch,
		MinStepMul,StepMulStep,MaxStepMul
	},
	$GRAPEUsages
];


(* ::Subsection::Closed:: *)
(*FindPulse*)


Unprotect[
	FindPulse
];


AssignUsage[{FindPulse},$GRAPEUsages];


FindPulse::badguess = "Error: the supplied initialGuess matrix was not understood.";
FindPulse::badrange = "Error: Your controlRange should  be of the form {{\[Epsilon]1min,\[Epsilon]1max},{\[Epsilon]2min,\[Epsilon]2max},...}.";
FindPulse::baddistortion = "Error: Distorted output control count inconsistent with length of Hcontrol, or has the wrong shape.";
FindPulse::badjacobian = "Error: The distortion Jacobian should be a rank 4 tensor. Maybe you need to call LiftDistortionRank on your distortion?";
FindPulse::badjacdim1 = "Error: First dimension of your distortion Jacobian does not match length of your distortedPulse.";
FindPulse::badjacdim2 = "Error: Second dimension of your distortion Jacobian does not match number of Hamiltonians.";
FindPulse::badjacdim3 = "Error: Third dimension of your distortion Jacobian does not match the length of the pulse given by initialGuess.";
FindPulse::badjacdim4 = "Error: Fourth dimension of your distortion Jacobian does not match number of control knobs (taken from length of \[Epsilon]range).";
FindPulse::badpenalty = "Error: The penalty function did not return a real number for a penalty.";
FindPulse::badpenaltygrad = "Error: The penalty function's gradient does not have the required shape.";
FindPulse::baddistsum = "Warning: The probabilities in your ParameterDistribution don't sum to 1. Proceeding anyways...";
FindPulse::baddistreps = "Warning: Not all of your replacement rules assign numbers to all present symbols. Proceeding anyways...";
FindPulse::baddistlength = "Error: The number of probabilities and replacement rules must be the same.";
FindPulse::badderivmask = "Error: your derivative mask seems to have the wrong dimensions."


(* ::Subsection::Closed:: *)
(*Exporters*)


Unprotect[ExportJCAMP,ExportSHP];


AssignUsage[{ExportJCAMP,ExportSHP},$GRAPEUsages]


JCAMPTitle::usage = "JCAMPTitle is an ExportJCAMP option with default value Automatic, which sets the title to the filename.Otherwise it should be a string.";
JCAMPDX::usage = "JCAMPDX is an ExportJCAMP option with default value '5.00 Bruker JCAMP library'.";
JCAMPDataType::usage = "JCAMPDataType is an ExportJCAMP option with default value 'Shape Data'.";
JCAMPOrigin::usage = "JCAMPOrigin is an ExportJCAMP option with default value 'Quantum-Utils Mathematica GRAPE'."; 
JCAMPUser::usage = "JCAMPUser is an ExportJCAMP option with default value 'nmruser'. Should be your username on the spectrometer.";
JCAMPDate::usage = "JCAMPDate is an ExportJCAMP option with default value Automatic, which sets the date to today."; 
JCAMPTime::usage = "JCAMPTime is an ExportJCAMP option with default value Automatic, which uses the current time."; 
JCAMPShapeExMode::usage = "JCAMPShapeExMode is an ExportJCAMP option with default value 'None'."; 
JCAMPShapeToTrot::usage = "JCAMPShapeToTrot is an ExportJCAMP option with default value '9.000000e+01' (string, not number)."; 
JCAMPShapeBWFac::usage = "JCAMPShapeBWFac is an ExportJCAMP option with default value '1.000000e+00' (string, not number)."; 
JCAMPShapeIntegFac::usage = "JCAMPShapeIntegFac is an ExportJCAMP option with default value '1.000000e+00' (string, not number)."; 
JCAMPShapeMode::usage = "JCAMPShapeMode is an ExportJCAMP option with default value '1' (string, not number).";
JCAMPCalibrationFactor::usage = "JCAMPCalibrationFactor is an ExportJCAMP option with default being the integer 1. This is the number the amplitudes are divided by before writing to the JCAMP file.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Pulses*)


(* ::Subsubsection::Closed:: *)
(*Pulse Object*)


Pulse/:Pulse[args___][key_]:=Association[args][key]


Unprotect@Pulse;
Pulse/:Format[Pulse[args__Rule]]:=Module[{modpulse},
	modpulse=Pulse[args];
	If[PulseHasKey[modpulse,Pulse],modpulse=PulseReplaceKey[modpulse,Pulse,MatrixForm[modpulse[Pulse]\[Transpose]]]];
	If[PulseHasKey[modpulse,ControlHamiltonians],modpulse=PulseReplaceKey[modpulse,ControlHamiltonians,MatrixListForm[modpulse[ControlHamiltonians]]]];
	If[PulseHasKey[modpulse,InternalHamiltonian],modpulse=PulseReplaceKey[modpulse,InternalHamiltonian,MatrixForm[modpulse[InternalHamiltonian]]]];
	If[PulseHasKey[modpulse,Target]&&MatrixQ[modpulse[Target]],modpulse=PulseReplaceKey[modpulse,Target,MatrixForm[modpulse[Target]]]];
	Grid[
		Prepend[
			{#,#/.List@@modpulse}&/@
			{UtilityValue,PenaltyValue,ExitMessage,Target,InternalHamiltonian,ControlHamiltonians,AmplitudeRange,TimeSteps,Pulse,ParameterDistribution,DistortionOperator,PulsePenalty},
			{Head,Value}
		],
		Alignment->{Left,Top},
		Dividers->All,
		ItemSize->{{13,32},Automatic},
		Background->{{LightBlue,None},{LightGreen,None}},
		ItemStyle->{Automatic,{"Subsubsubsection"}},
		Spacings->{2,.8}
	]
]


(* ::Subsubsection::Closed:: *)
(*Pulse Conversion and Transformation*)


Options[ToPulse]={
	UtilityValue->None,
	PenaltyValue->0,
	Target->IdentityMatrix[2],
	ControlHamiltonians->2\[Pi]{TP[X]/2,TP[Y]/2},
	InternalHamiltonian->0*TP[Z],
	DistortionOperator->IdentityDistortion[],
	PulsePenalty->ZeroPenalty[],
	ParameterDistribution->None,
	AmplitudeRange->{{-1,1},{-1,1}},
	ExitMessage->"Generated by ToPulse."
};


ToPulse[pulsemat_,opt:OptionsPattern[]]:=Pulse[
	TimeSteps -> pulsemat[[All,1]],
	Pulse -> pulsemat[[All,2;;]],
	Sequence@@DeleteDuplicates[Join[{opt},Options[ToPulse]],First[#1]===First[#2]&]	
]


FromPulse[pulse_Pulse]:=AddTimeSteps[pulse[TimeSteps],pulse[Pulse]];


SimForm[pulse_Pulse, distort_:True]:=
	Module[{p, Hc=pulse[ControlHamiltonians]},
		p=pulse[Pulse];
		If[Length@Dimensions@p==1,p=List/@p];
		p=AddTimeSteps[pulse[TimeSteps], p];
		If[distort, p = pulse[DistortionOperator][p, False]];
		{p, Hc}
	]


AddTimeSteps[dts_,pulse_]:=If[ListQ@dts,Prepend[pulse\[Transpose],dts]\[Transpose],Prepend[pulse\[Transpose],ConstantArray[dts,Length@pulse]]\[Transpose]]


SplitPulse[pulse_]:={pulse[[All,1]],pulse[[All,2;;-1]]}


(* ::Subsubsection::Closed:: *)
(*Pulse Key Modification*)


PulseRemoveKeys[pulse_Pulse,headers__]:=Select[pulse,Not[MemberQ[{headers},#[[1]]]]&]


PulseReplaceKey[pulse_Pulse,header_,newval_]:=Append[PulseRemoveKeys[pulse,header],header->newval]


PulseHasKey[pulse_Pulse,key_]:=MemberQ[pulse[[All,1]],key]


(* ::Subsubsection::Closed:: *)
(*Pulse Manipulation*)


PulseDivide[pulse_Pulse,n_]:=
	Module[{p=pulse[Pulse],dt=pulse[TimeSteps]},
		dt=Flatten[ConstantArray[dt/n,n]\[Transpose]];
		p=p[[Flatten@Table[ConstantArray[k,n],{k,Length@p}]]];
		PulseReplaceKey[PulseReplaceKey[pulse,Pulse,p],TimeSteps,dt]
	]


PulsePhaseRotate[pulse_,\[Phi]_]:=
	Module[{xy=pulse[Pulse],a\[Theta]},
		a\[Theta]={Norm/@xy,\[Phi]+(ArcTan[First@#,Last@#]&/@xy)}\[Transpose];
		xy={First[#]Cos[Last@#],First[#]Sin[Last@#]}&/@a\[Theta];
		PulseReplaceKey[pulse,Pulse,xy]
	]


PulsePhaseRamp[pulse_,f_]:=
	Module[{dt,xy=pulse[Pulse],a\[Theta]},
		dt=pulse[TimeSteps];
		a\[Theta]={Norm/@xy,2\[Pi]*f*(Accumulate[dt]-dt/2)+(If[#=={0,0},0,ArcTan@@#]&/@xy)}\[Transpose];
		xy={First[#]Cos[Last@#],First[#]Sin[Last@#]}&/@a\[Theta];
		PulseReplaceKey[pulse,Pulse,xy]
	]


PulseModulate[pulse_,f_,\[Phi]_:0]:=
	Module[{dt,xy=pulse[Pulse],a\[Theta]p,a\[Theta]m,a\[Theta]},
		dt=pulse[TimeSteps];
		a\[Theta]p={Norm/@xy,2\[Pi]*f*(Accumulate[dt]-dt/2)+(If[#=={0,0},0,ArcTan@@#]&/@xy)}\[Transpose];
		a\[Theta]m={Norm/@xy,-2\[Pi]*f*(Accumulate[dt]-dt/2)+(If[#=={0,0},0,ArcTan@@#]&/@xy)}\[Transpose];
		a\[Theta]p=#1*Exp[I*#2]&@@@a\[Theta]p;
		a\[Theta]m=#1*Exp[I*#2]&@@@a\[Theta]m;
		a\[Theta]=(Exp[-I*\[Phi]]a\[Theta]p+Exp[I*\[Phi]]a\[Theta]m)/2;
		xy={Re[#],Im[\[Phi]]}&/@a\[Theta];
		PulseReplaceKey[pulse,Pulse,xy]
	]


(* ::Subsubsection::Closed:: *)
(*Pulse Matrix Generation*)


RandomPulse[dts_,controlRange_] := AddTimeSteps[dts, (RandomReal[#,Length@dts]&/@controlRange)\[Transpose]]
RandomPulse[dt_?NumericQ,n_Integer,controlRange_] := RandomPulse[ConstantArray[dt,n],controlRange]


Options[RandomSmoothPulse]={InterpolationPoints->10};
RandomSmoothPulse[dts_,controlRange_,OptionsPattern[]]:=
	Module[
		{randomEntries, interpFunctions, numRandomPoints},
		numRandomPoints = Max[Floor[Length@dts/OptionValue[InterpolationPoints]],5];
		randomEntries = RandomReal[#,numRandomPoints]&/@controlRange;
		interpFunctions = Interpolation[#,Method->"Spline"]&/@randomEntries;
		AddTimeSteps[dts, ((#/@Range[1, numRandomPoints, (numRandomPoints-1)/(Length@dts-1)])&/@interpFunctions)\[Transpose]]
	];
RandomSmoothPulse[dt_?NumericQ,n_Integer,controlRange_,opt:OptionsPattern[]] := RandomSmoothPulse[ConstantArray[dt,n],controlRange,opt]


Options[GenerateAnnealedPulse] = {AnnealingGenerator -> RandomPulse};
GenerateAnnealedPulse[original_, controlRange_, OptionsPattern[]] := original + OptionValue[AnnealingGenerator][ConstantArray[0,Length@original], controlRange]


GaussianTailsPulse[dt_,T_,riseTime_,Area->area_]:=
	Module[{\[Sigma],NI,fun,pulse},
		\[Sigma]=riseTime/3;
		fun[t_]:=Piecewise[{{1,riseTime<t<T-riseTime},{Exp[-(t-riseTime)^2/(2\[Sigma]^2)],t<=riseTime},{Exp[-(t-T+riseTime)^2/(2\[Sigma]^2)],t>=T-riseTime}}];
		NI=NIntegrate[fun[t],{t,0,T}];
		pulse=Table[{area*fun[t-dt/2]/NI,0},{t,0,T,dt}];
		(*Correct the area now that we have a descent shape*)
		pulse=area*pulse/(dt*Total@Flatten@pulse);
		AddTimeSteps[dt, pulse]
	];
GaussianTailsPulse[dt_,T_,riseTime_,Max->max_]:=Module[
	{apulse=GaussianTailsPulse[dt,T,riseTime,Area->1]},
	AddTimeSteps[apulse[[All,1]],max*apulse[[All,2;;]]/Max[Flatten[apulse[[All,2;;]]]]]
]


(* ::Subsubsection::Closed:: *)
(*Legalization and Normalization*)


LegalizePulse[pulse_,controlRange_]:={#1, Sequence@@MapThread[Clip, {{##2},controlRange}, 1]}& @@@ pulse


LegalizePulse[profile_][pulse_,controlRange_]:=Table[
	{
		pulse[[n,1]],
		Sequence@@MapThread[Clip, {pulse[[n,2;;]],profile[[n]]*controlRange}, 1]
	},
	{n,Length@pulse}
]


NormalizePulse[pulse_, controlRange_] := 
With[{tp = Transpose[pulse[[All, -2;;]]]},
	AddTimeSteps[pulse[[All,1]],Transpose[(tp - controlRange[[All, 1]]) / (controlRange[[All, 2]] - controlRange[[All, 1]])]]
];


(* ::Subsection::Closed:: *)
(*Utility Function and Targets*)


(* ::Subsubsection::Closed:: *)
(*Unitary Propagators*)


(* ::Text:: *)
(*Takes a pulse and finds the overall unitary.*)


PropagatorFromPulse[pulse_,Hint_,Hcontrol_]:=
	Module[{step=1,dt,dts,amps},
		{dts,amps} = SplitPulse[pulse];
		(* Notice the delay equal on dt! Not a bug. *)
		dt := dts[[step++]];
		Fold[
			MatrixExp[-I(Hint+#2.Hcontrol) dt].#1&,
			IdentityMatrix[Length[Hint]],
			amps
		]
	]


PropagatorListFromPulse[pulse_,Hint_,Hcontrol_]:=
	Module[{dts,dt,step=1,amps},
		{dts,amps} = SplitPulse[pulse];
		dt := dts[[step++]];
		Map[
			MatrixExp[-I(Hint+#.Hcontrol)dt]&,
			amps
		]
	]


(* ::Subsubsection::Closed:: *)
(*Target Unitary*)


(* ::Text:: *)
(*We divide by the dimension to make the maximum value of the objective function equal to 1.*)


Utility[Ucalc_,Utarget_List]:=Abs[Tr[Ucalc\[ConjugateTranspose].Utarget]/Length[Ucalc]]^2;


UtilityGradient[pulse_,Hint_,Hcontrol_,Utarget_List]:=
	Module[
		{
			dim=Length[Hint],
			dts,amps,Uforw,Uback,gradient,utility,unitaries
		},
		unitaries=PropagatorListFromPulse[pulse,Hint,Hcontrol];

		{dts, amps} = SplitPulse[pulse];

		Uforw=Rest[FoldList[#2.#1&,IdentityMatrix[dim],unitaries]];
		Uback=Reverse[FoldList[#2\[ConjugateTranspose].#1&,Utarget,Reverse[Rest[unitaries]]]];
		gradient=Table[
			-2 Re[Tr[Uback[[i]]\[ConjugateTranspose].(I dts[[i]] Hcontrol[[j]].Uforw[[i]])]*Tr[Uforw[[i]]\[ConjugateTranspose].Uback[[i]]]],
			{i,Length[unitaries]},{j,Length[Hcontrol]}
		]/dim^2;
		utility=Utility[Last[Uforw],Utarget];
		{utility,gradient}
	];


(* ::Subsubsection::Closed:: *)
(*Coherent Subspaces*)


Utility[Ucalc_,target_CoherentSubspaces]:=
	With[
		{Xs=target[[1]],Ys=target[[2]]},
		Sum[
			Abs[Tr[Ucalc\[ConjugateTranspose].(Ys[[i]].Xs[[i]]\[ConjugateTranspose])]/Length[Xs[[i,1]]]]^2,
			{i,Length[Xs]}
		]/Length[Xs]
	]


UtilityGradient[pulse_,Hint_,Hcontrol_,target_CoherentSubspaces]:=
	Module[
		{
			dim=Length[Hint],
			Uforw,Uback,derivs,performIndex,unitaries,
			Xs=target[[1]],
			Ys=target[[2]],
			numSubspaces,
			dts,amps
		},
		unitaries=PropagatorListFromPulse[pulse,Hint,Hcontrol];

		{dts, amps} = SplitPulse[pulse];

		numSubspaces=Length[Xs];

		Uforw=Rest[FoldList[#2.#1&,IdentityMatrix[dim],unitaries]];
		Uback=Reverse[FoldList[#2\[ConjugateTranspose].#1&,IdentityMatrix[dim],Reverse[Rest[unitaries]]]];

		derivs=Table[
			Sum[
				With[{Ubackj=Uback[[i]].Ys[[ss]].Xs[[ss]]\[ConjugateTranspose], ssDim2=Length[Xs[[ss,1]]]^2},
					-2 Re[Tr[Ubackj\[ConjugateTranspose].(I dts[[i]] Hcontrol[[j]].Uforw[[i]])]*Tr[Uforw[[i]]\[ConjugateTranspose].Ubackj]]/ssDim2
				],
				{ss,numSubspaces}
			],
			{i,Length[unitaries]},{j,Length[Hcontrol]}
		]/numSubspaces;
		performIndex=Utility[Last[Uforw],target];
		{performIndex,derivs}
	];


(* ::Subsection::Closed:: *)
(*Distortions*)


(* ::Subsubsection::Closed:: *)
(*Distortion Operator*)


DistortionOperator[function_,___][args__]:=function[args]
DistortionOperator/:Format[DistortionOperator[_,format_]]:=format


(* ::Subsubsection::Closed:: *)
(*Apply Inverse Distortion*)


ApplyInverseDistortion[distortion_,exampleInput_,pulseMat_]:=Module[{p,jac,dM,dL,dN,dK},
	p=pulseMat[[All,2;;-1]];
	jac=Last[distortion[exampleInput,True]];
	{dM,dL,dN,dK}=Dimensions[jac];
	(* The idea here is to cast the contraction MLNK.NK over two indeces into regular
		matrix multiplication so that we can use LinearSolve to do all of the work *)
	AddTimeSteps[
		exampleInput[[All,1]],
		ArrayReshape[LinearSolve[ArrayReshape[jac,{dM*dL,dN*dK}],ArrayReshape[p,{dM*dL,1}]],{dN,dK}]
	]
]


(* ::Subsubsection::Closed:: *)
(*Distortion Operator Tools*)


LiftDistortionRank[distortion_DistortionOperator]:=DistortionOperator[
	Function[{pulse,computeJac},
		Which[computeJac===True,
			Module[{d,jac,all,controlRank},
				controlRank=Last@Dimensions@pulse-1;
				(* Compute the Jacobian and distortion for each control channel*)
				all = Table[distortion[pulse[[All,{1,n}]], True], {n,2,Last@Dimensions@pulse}];
				(* Same as above *)
				d = AddTimeSteps[all[[1,1,All,1]], all[[All,1,All,2]]\[Transpose]];
				jac = all[[All,2]];
				If[Length@Dimensions@jac===3,
					(* The user supplied Jacobian is a matrix for each channel; lift to rank 4. *)
					jac = Transpose[Table[KroneckerDelta[in,out]*jac[[in,All,All]],{in,controlRank},{out,controlRank}], {4,2,1,3}];
				];
				If[Length@Dimensions@jac===5,
					(* The user supplied Jacobian is rank 4 for each channel; remove singleton dimensions and repeat above procedure. *)
					jac = Transpose[Table[KroneckerDelta[in,out]*jac[[in,All,1,All,1]],{in,controlRank},{out,controlRank}], {4,2,1,3}];
				];
			{d, jac}
			],
			computeJac===False,
			Module[{d},
				(* Compute the distortion for each control channel *)
				d = Table[distortion[pulse[[All,{1,n}]], False], {n,2,Last@Dimensions@pulse}];
				AddTimeSteps[d[[1,All,1]], d[[All,All,2]]\[Transpose]]
			],
			computeJac===All,
			Module[{d},
				(* Compute the distortion for each control channel *)
				d = Table[distortion[pulse[[All,{1,n}]], All], {n,2,Last@Dimensions@pulse}];
				{
					AddTimeSteps[d[[1,1,All,1]], d[[All,1,All,2]]\[Transpose]],
					AddTimeSteps[d[[1,2,All,1]], d[[All,2,All,2]]\[Transpose]],
					AddTimeSteps[d[[1,3,All,1]], d[[All,3,All,2]]\[Transpose]]
				}
			]
		]
	],
	Format[HoldForm[Superscript[Format[distortion],"\[CircleTimes]numInputChannels"]]]
];


JoinDistortions[Distortions__DistortionOperator]:=DistortionOperator[
	Function[{pulse,computeJac},
		Which[computeJac===True,
			Module[{d,jac,all,controlRank},
				controlRank=Length@{Distortions};
				(* Compute the Jacobian and distortion for each control channel*)
				all = MapIndexed[#1[pulse[[All,{1, First@#2+1}]], True]&, {Distortions}];
				(* Same as above *)
				d = AddTimeSteps[all[[1,1,All,1]], all[[All,1,All,2]]\[Transpose]];
				jac = all[[All,2]];
				(* The user supplied Jacobian is rank 4 for each channel *)
				jac = Transpose[ConstantArray[jac[[All,All,1,All,1]], controlRank], {2,4,1,3}];
				{d, jac}
			],
			computeJac===False,
			Module[{d},
				(* Compute the distortion for each control channel *)
				d = MapIndexed[#1[pulse[[All,{1, First@#2+1}]], False]&, {Distortions}];
				AddTimeSteps[d[[1,All,1]], d[[All,All,2]]\[Transpose]]
			],
			computeJac===All,
			Module[{d},
				(* Compute the distortion for each control channel *)
				d = MapIndexed[#1[pulse[[All,{1, First@#2+1}]], All]&, {Distortions}];
				{
					AddTimeSteps[d[[1,1,All,1]], d[[All,1,All,2]]\[Transpose]],
					AddTimeSteps[d[[1,2,All,1]], d[[All,2,All,2]]\[Transpose]],
					AddTimeSteps[d[[1,3,All,1]], d[[All,3,All,2]]\[Transpose]]
				}
			]
		]
	],
	With[{format=MatrixForm[List/@Format/@{Distortions}]},Format[HoldForm[format]]]
];


ComposeDistortions[Distortions__DistortionOperator]:=DistortionOperator[
	Function[{pulse,computeJac},
		Which[computeJac===True,
			Fold[
				With[{result=#2[First@#1,True]},{
					First@result,
					If[Last@#1===None,
						Last@result,
						TensorPairContract[Last@result,Last@#1,{{3,1},{4,2}}]
					]
				}]&,
				{pulse,None},
				{Distortions}
			],
			computeJac===False,
			Fold[#2[#1,False]&,pulse,{Distortions}],
			computeJac===All,
			Fold[{#1[[1]],Sequence@@((#2[#1[[3]],All])[[{2,3}]])}&,{pulse,pulse,pulse},{Distortions}]
		]
	],
	With[{format=Row[Riffle[Format/@{Distortions},"\[EmptyCircle]"]]},Format[HoldForm[format]]]
]



DistortionOperator /: Composition[distortions__DistortionOperator] := ComposeDistortions[distortions];


PerturbateDistortion[Distortion_DistortionOperator, h_:10^-8]:=Module[{jacobian},
	jacobian[dts_,numControlKnobs_]:=jacobian[dts,numControlKnobs]=Transpose[
		Table[
			Distortion[AddTimeSteps[dts, SparseArray[{{n,k}->h},{Length@dts,numControlKnobs}]], False][[All,2;;-1]]/h,
			{n,Length[dts]}, {k,numControlKnobs}
		],
		{3,4,1,2}
	];
	DistortionOperator[
		Function[{pulse,computeJac},
			If[computeJac===True,
				{Distortion[pulse,False], jacobian[pulse[[All,1]],Last@Dimensions@pulse-1]},
				Distortion[pulse,computeJac]
			]
		],
		Format@HoldForm[PerturbateDistortion[Format[Distortion]]]
	]
]


ScaleDistortion[distortion_DistortionOperator,timeScale_,amplitudeScale_?NumericQ]:=DistortionOperator[
	Function[{pulse,computeJac},
		Which[
			computeJac===False,
				Transpose[Prepend[ConstantArray[amplitudeScale,Length[#]-1],timeScale]*#]&[Transpose@distortion[pulse,False]],
			computeJac===True,
				Module[{outpulse,jac,scale},
					{outpulse,jac}=distortion[pulse,True];
					{
						Transpose[Prepend[ConstantArray[amplitudeScale,Length[#]-1],timeScale]*#]&[Transpose@outpulse],
						amplitudeScale*jac
					}
				],
			computeJac===All,
				{#1,#2,Transpose[Prepend[ConstantArray[amplitudeScale,Length[First@#3]-1],timeScale]*Transpose[#3]]}&@@distortion[pulse,All]
		]
	],
	HoldForm@Format[ScaleDistortion[distortion,Row[{"t\[Rule]",timeScale,"t"}],Row[{"amps\[Rule]",amplitudeScale,"amps"}]]]
];
ScaleDistortion[distortion_DistortionOperator,timeScale_,amplitudeScale_List]:=DistortionOperator[
	Function[{pulse,computeJac},
		Which[
			computeJac===False,
				Transpose[Prepend[amplitudeScale,timeScale]*Transpose[distortion[pulse,False]]],
			computeJac===True,
				Module[{outpulse,jac,scale},
					{outpulse,jac}=distortion[pulse,True];
					{
						Transpose[Prepend[amplitudeScale,timeScale]*Transpose[outpulse]],
						Transpose[amplitudeScale*Transpose[jac,{2,1,3,4}],{2,1,3,4}]
					}
				],
			computeJac===All,
				{#1,#2,Transpose[Prepend[amplitudeScale,timeScale]*Transpose[#3]]}&@@@distortion[pulse,All]
		]
	],
	With[{amps=Row[Flatten@Riffle[Table[{"amp",n,"\[Rule]",amplitudeScale[[n]],"amp",n},{n,Length@amplitudeScale}],", "]]},
		HoldForm@Format[ScaleDistortion[distortion,Row[{"t\[Rule]",timeScale,"t"}],amps]]
	]
]


(* ::Subsubsection::Closed:: *)
(*Identity Distortion*)


IdentityDistortion[]:=Module[{jac},
	jac[{n_,k_}]:=(jac[{n,k}]=Transpose[Table[KroneckerDelta[kk,ll]*IdentityMatrix[n],{kk,k},{ll,k}], {2,4,1,3}]);
	DistortionOperator[
		Function[{pulse,computeJac},
			Which[
				computeJac===False,
				pulse,
				computeJac===True,
				{pulse,jac[Dimensions@pulse-{0,1}]},
				computeJac===All,
				{pulse,pulse,pulse}
			]
		],
		Format@HoldForm[IdentityDistortion[]]
	]
]


(* ::Subsubsection::Closed:: *)
(*Variable Change Distortions*)


Options[VariableChangeDistortion]={
	Simplify->True
};


VariableChangeDistortion[changeFn_, variableSymbol_Symbol, opts:OptionsPattern[]] := VariableChangeDistortion[{changeFn}, {variableSymbol}, opts]
VariableChangeDistortion[changeFn_List,{variableSymbols__Symbol},OptionsPattern[]] := Module[
	{subJacobian},

	(* Calculate the per-step Jacobian now, and we won't have to again *)
	subJacobian = Outer[D, changeFn, {variableSymbols}];

	If[OptionValue[Simplify],
		subJacobian = Simplify@subJacobian;
	];

	With[{subjacVal=subJacobian},
		DistortionOperator[
			Function[{pulse,computeJac},
				Which[computeJac===False,
					AddTimeSteps[pulse[[All,1]],(changeFn/.Thread[{variableSymbols}->#])& /@ pulse[[All, 2;;-1]]],
					computeJac===All,
					{pulse,pulse,AddTimeSteps[pulse[[All,1]],(changeFn/.Thread[{variableSymbols}->#])& /@ pulse[[All, 2;;-1]]]},
					computeJac===True,
					{
						AddTimeSteps[pulse[[All,1]],(changeFn/.Thread[{variableSymbols}->#])& /@ pulse[[All, 2;;-1]]],
						Transpose[#,{1,3,2,4}]&@Table[
							(* Whenever m!=n, we get no derivative, ie, different time steps do not affect each other. *)
							If[m==n,
								subjacVal /. Thread[{variableSymbols}->pulse[[n, 2;;-1]]],
								ConstantArray[0,{Length@changeFn, Last@Dimensions@pulse-1}]
							],
							{m, Length@pulse}, {n, Length@pulse}
						]
					}
				]
			],
			Format@HoldForm[VariableChangeDistortion[{variableSymbols}->changeFn]]
		]
	]
]


TimeScaleDistortion[multiplier_]:=DistortionOperator[
	Function[{pulse,computeJac},
		Which[computeJac===True,
			{
				AddTimeSteps[multiplier * pulse[[All,1]], pulse[[All,2;;-1]]],
				(* The jacobian is the same as IdentityDistortion... *)
				Transpose[ConstantArray[IdentityMatrix[Length@pulse],{Last@Dimensions@pulse-1, Last@Dimensions@pulse-1}], {2,4,1,3}]
			},
			computeJac===False,
			AddTimeSteps[multiplier * pulse[[All,1]], pulse[[All,2;;-1]]],
			computeJac===All,
			{pulse,pulse,AddTimeSteps[multiplier * pulse[[All,1]], pulse[[All,2;;-1]]]}
		]
	],
	Format@HoldForm[TimeScaleDistortion[multiplier]]
]


(* ::Subsubsection::Closed:: *)
(*Linear Distortions*)


ConvolutionDistortion[integral_,numInput_,numOutput_,dtOutput_]:=
	Module[{discreteKernel},
		discreteKernel = SparseArray@Table[
			With[{int=integral[m,n]},If[MatrixQ[int],int,{{int}}]],
			{m, numOutput}, {n, numInput}
		];
		(*Change from dimensions {M,N,L,K} to {M,L,N,K}*)
		discreteKernel=Transpose[discreteKernel,{1,3,2,4}];

		With[{jacval=discreteKernel},
			DistortionOperator[
				Function[{pulse,computeJac},
					Module[{jac=jacval,outputPulse},
						outputPulse=AddTimeSteps[dtOutput, Normal@TensorPairContract[jac,pulse[[All,2;;]],{{3,1},{4,2}}]];
						Which[
							computeJac===True,
							{outputPulse,jac},
							computeJac===False,
							outputPulse,
							computeJac===All,
							{pulse,pulse,outputPulse}
						]
					]
				],
				Format@HoldForm[ConvolutionDistortion[Integral[integral]]]
			]
		]
	];
ConvolutionDistortion[kernel_,numInput_,numOutput_,dtInput_,dtOutput_]:=
	Module[{integral, m, n},
		(* If Mathematica can't do the integral, the integral formula will just have to be 
		evaluated in each entry of discreteKernel... *)
		(* If Mathematica can't do the integral, the integral formula will just have to be 
		evaluated in each entry of discreteKernel... *)
		integral[m_,n_] = Integrate[kernel[(m-1/2)*dtOutput - \[Tau]], {\[Tau],(n-1)*dtInput,n*dtInput}, Assumptions->(n\[Element]Reals&&m\[Element]Reals&&n>0&&m>0)];
		DistortionOperator[First@ConvolutionDistortion[integral,numInput,numOutput,dtOutput],Format@HoldForm[ConvolutionDistortion[kernel]]]
	];


ExponentialDistortion[\[Tau]c_,numInput_,numOutput_,dtInput_,dtOutput_]:=DistortionOperator[
	First@ConvolutionDistortion[
		Function[{m,n},
			\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{
SuperscriptBox["E", 
FractionBox[
RowBox[{"dtOutput", "-", 
RowBox[{"2", " ", "dtOutput", " ", "m"}], "+", 
RowBox[{"2", " ", "dtInput", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "n"}], ")"}]}]}], 
RowBox[{"2", " ", "\[Tau]c"}]]], " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
SuperscriptBox["E", 
RowBox[{"dtInput", "/", "\[Tau]c"}]]}], ")"}]}], 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"dtOutput", ">", "0"}], "||", 
RowBox[{
RowBox[{"2", " ", "dtInput"}], "<", "dtOutput"}]}], ")"}], "&&", 
RowBox[{"0", "<", "n", "<", 
FractionBox[
RowBox[{"dtOutput", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"2", " ", "m"}]}], ")"}]}], 
RowBox[{"2", " ", "dtInput"}]]}], "&&", 
RowBox[{"dtInput", ">", "0"}], "&&", 
RowBox[{
RowBox[{"2", " ", "m"}], ">", "1"}]}]},
{
RowBox[{"1", "-", 
SuperscriptBox["E", 
FractionBox[
RowBox[{"dtOutput", "-", 
RowBox[{"2", " ", "dtOutput", " ", "m"}], "+", 
RowBox[{"2", " ", "dtInput", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "n"}], ")"}]}]}], 
RowBox[{"2", " ", "\[Tau]c"}]]]}], 
RowBox[{
RowBox[{"dtInput", ">", "0"}], "&&", 
RowBox[{
RowBox[{"dtInput", " ", 
RowBox[{"(", 
RowBox[{"dtOutput", "-", 
RowBox[{"2", " ", "dtOutput", " ", "m"}], "+", 
RowBox[{"2", " ", "dtInput", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "n"}], ")"}]}]}], ")"}]}], "<", "0"}], "&&", 
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{
RowBox[{"2", " ", "m"}], ">", "1"}], "&&", 
RowBox[{
FractionBox[
RowBox[{"dtOutput", "-", 
RowBox[{"2", " ", "dtOutput", " ", "m"}], "+", 
RowBox[{"2", " ", "dtInput", " ", "n"}]}], "dtInput"], ">=", "0"}], "&&", 
RowBox[{"(", 
RowBox[{
RowBox[{
RowBox[{"2", " ", "dtInput"}], "<", "dtOutput"}], "||", 
RowBox[{"dtOutput", ">", "0"}]}], ")"}]}], ")"}], "||", 
RowBox[{"(", 
RowBox[{
RowBox[{"n", ">", "0"}], "&&", 
RowBox[{
RowBox[{"2", " ", "m"}], "<=", "1"}], "&&", 
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"dtOutput", ">", "0"}], "&&", 
RowBox[{"m", ">", "0"}], "&&", 
RowBox[{
RowBox[{"2", " ", "dtInput"}], ">=", "dtOutput"}]}], ")"}], "||", 
RowBox[{"(", 
RowBox[{
RowBox[{
RowBox[{
FractionBox["dtInput", "dtOutput"], "+", "m"}], ">", 
FractionBox["1", "2"]}], "&&", 
RowBox[{
RowBox[{"2", " ", "dtInput"}], "<", "dtOutput"}]}], ")"}]}], ")"}]}], ")"}]}], ")"}]}]},
{"0", 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False]\)
		],
		numInput,numOutput,dtOutput
	],
	Format@HoldForm[ExponentialDistortion[\[Tau]c]]
];


(* ::Subsubsection::Closed:: *)
(*IQ Distortion*)


(* Use Global just so  that the Format display is nicer. *)
IQDistortion[Igain_,Ioffset_,Qgain_,Qoffset_,Q\[Theta]_]:=Module[{Global`Ichan,Global`Qchan},
	VariableChangeDistortion[
		{(Igain*Global`Ichan+Ioffset)+Cos[Q\[Theta]](Qgain*Global`Qchan+Qoffset),Sin[Q\[Theta]](Qgain*Global`Qchan+Qoffset)},
		{Global`Ichan,Global`Qchan}
	]
]


(* ::Subsubsection::Closed:: *)
(*NonlinearTransferDistortion*)


(*Use PerturbateDistortion because we are too lazy to compute the jacobian*)
NonlinearTransferDistortion[gainFcn_]:=DistortionOperator[
	First@PerturbateDistortion@DistortionOperator@Function[{pulse,computeJac},
		Module[{L,L2,dt,Fs,freqs,timeDom,freqDom,distorted},
			(* Create fourier domain frequency vector *)
			L=Length@pulse;
			dt=pulse[[1,1]];
			Fs=1/dt;
			If[EvenQ[L],
				L2=L/2;
				freqs=N@Range[-Fs/2+(Fs/2)/L2,Fs/2,(Fs/2)/L2];
				,
				L2=(L-1)/2;
				freqs=Range[-Fs/2,Fs/2,(Fs/2)/L2];
			];
			freqs=RotateLeft[freqs,L2];

			(* fourier, distort, inverse fourier*)
			timeDom=(#1+I*#2)&@@@pulse[[All,{2,3}]];
			freqDom={freqs,Fourier[timeDom]/L}\[Transpose];
			distorted=L*InverseFourier[freqDom[[All,2]]*(gainFcn@@@freqDom)];
			If[computeJac===False,
				{pulse[[All,1]],Re@distorted,Im@distorted}\[Transpose],
				{pulse,pulse,{pulse[[All,1]],Re@distorted,Im@distorted}\[Transpose]}
			]
		]
	],
	Format@HoldForm[NonlinearTransferDistortion[gainFcn]]
]


(* ::Subsubsection::Closed:: *)
(*Differential Equation Distortions*)


Options[DEDistortion]:=Join[
	Options[NDSolve],
	{
		DESolver -> NDSolve,
		DESolverArgs -> {}
	}
];


DEDistortion[deFn_, outputForm_,solnSymbol_Symbol, t_Symbol, min_, max_, \[Delta]t_, M_, opt:OptionsPattern[]] := DEDistortion[deFn, outputForm, {solnSymbol}, t, min, max, \[Delta]t, M, opt];
DEDistortion[deFn_, outputForm_,{solnSymbols__Symbol}, t_Symbol, min_, max_, \[Delta]t_, M_, opt:OptionsPattern[]] := DistortionOperator[
	First@PerturbateDistortion@DistortionOperator@Function[{pulse,computeJac},
		Module[{solution,DE},
			DE=deFn[pulse];

			(* Use DE solver to get an interpolated solution to the DE. *)
			solution = With[{extraSolverArgs = Sequence @@ OptionValue[DESolverArgs]},
				First @ OptionValue[DESolver][
					DE,
					{solnSymbols},
					{t, min, max},
					(* Inject any additional arguments here. *)
					extraSolverArgs,
					FilterOptions[NDSolve,opt]
				]
			];
		
			(* Make a distorted pulse by applying the solution function to the given timesteps. *)
			If[computeJac===False,
				AddTimeSteps[\[Delta]t,Table[outputForm /. Prepend[solution,t->\[Tau]],{\[Tau], \[Delta]t, M \[Delta]t, \[Delta]t}]],
				{pulse,pulse,AddTimeSteps[\[Delta]t,Table[outputForm /. Prepend[solution,t->\[Tau]],{\[Tau], \[Delta]t, M \[Delta]t, \[Delta]t}]]}
			]
		]
	],
	Format[HoldForm[DEDistortion[outputForm]]]
]


Options[LinearDEDistortion]={
	CompensationTimeSteps->None,
	DecayRatios->Automatic,
	StateMetric->Automatic
};


LinearDEDistortion[A_,b_,outputComponent_,numInput_?IntegerQ,numOutput_?IntegerQ,dtInput_?NumericQ,dtOutput_?NumericQ,opt:OptionsPattern[]]:=LinearDEDistortion[A,b,outputComponent,ConstantArray[dtInput,numInput],ConstantArray[dtOutput,numOutput],opt]


LinearDEDistortion[A_,b_,outputComponent_,dtsInput_List,dtsOutput_List,OptionsPattern[]]:=Module[
	{phi,nc,normA,d,U,c,v,w,ts,taus,numInput,numOutput,dtsComp,P,compensationTimeStep,ratios},
	nc=Length[A];

	If[OptionValue[CompensationTimeSteps]=!=None,
		(*For the sake of efficiency, we branch to a another function if we have to do compensation*)
		dtsComp=OptionValue[CompensationTimeSteps];
		P=OptionValue[StateMetric];
		If[P===Automatic,P=DiagonalMatrix[UnitVector[nc,outputComponent]];];
		ratios=OptionValue[DecayRatios];
		If[ratios===Automatic,ratios=ConstantArray[0,Length@dtsComp]];
		LinearDEDistortion[A,b,outputComponent,dtsInput,dtsOutput,dtsComp,ratios,P],

		(*With no compensation, we do the following*)
		numInput=Length@dtsInput;
		numOutput=Length@dtsOutput;
	
		ts=Prepend[Accumulate[dtsInput],0];
		taus=Accumulate[dtsOutput];
		taus=(Most@Prepend[taus,0]+taus)/2;

		(*Medium-efficiency implementation of discrete convolution tensor (phi) of the DE*)
		(*We need to be careful not to do MatrixExp[-t*A] for positive t; this could lead
		to numerical instability. In general, we want to group all of the matrix exponentials 
		together *)
		normA=Norm[A];
		{d,U}=Eigensystem[A/normA];
		d=normA*d; U=U\[Transpose];

		w=Inverse[U].b/normA;
		v=-UnitVector[nc,outputComponent].Inverse[A/normA];
		c=v.b/normA;
		v=v.U;

		phi=Table[
			Which[
				taus[[m]] < ts[[n-1+1]],
					{{0,0},{0,0}},
				taus[[m]] >= ts[[n+1]],
					{{Re[#],-Im[#]},{Im[#],Re[#]}}&[v.((Exp[(taus[[m]]-ts[[n+1]])*d]-Exp[(taus[[m]]-ts[[n-1+1]])*d])*w)],
				True,
					{{Re[#],-Im[#]},{Im[#],Re[#]}}&[c-v.(Exp[(taus[[m]]-ts[[n-1+1]])*d]*w)]
			], 
			{m,numOutput},{n,numInput}
		]; (* index order (M,N,L,K) *)
		phi=Transpose[phi,{1,3,2,4}]; (*index order (M,L,N,K)*)

		(*The rest is no different than, eg, ConvolutionDistortion *)
		With[{phimat=phi},
			DistortionOperator[
				Function[{pulse,computeJac},
						Module[{jac=phimat,outputPulse},
							outputPulse=AddTimeSteps[dtsOutput, Normal@TensorPairContract[jac,pulse[[All,2;;]],{{3,1},{4,2}}]];
							Which[
								computeJac===True,
								(*The compensation timesteps should not appear in in the jacobian*)
								{outputPulse,jac[[All,All,1;;numInput,All]]},
								computeJac===False,
								outputPulse,
								computeJac===All,
								{pulse,pulse,outputPulse}
							]
						]
					],
				Format[HoldForm[LinearDEDistortion[MatrixForm[A],MatrixForm[b]]]]
			]
		]
	]
]


(*This pattern is not documented; it should be accessed through the options*)
LinearDEDistortion[A_,b_,outputComponent_,dtsInput_,dtsOutput_,dtsComp_List,ratios_List,P_?MatrixQ]:=Module[
	{phi,deConv,nc,normA,Ainv,d,U,c,v,w,ts,taus,numInput,numOutput,numModInput,compensationTimeStep,AUs},

	nc=Length[A];
	numInput=Length@dtsInput;
	numOutput=Length@dtsOutput;
	numModInput=Length@dtsComp;
	
	ts=Prepend[Accumulate[Join[dtsInput,dtsComp]],0];
	taus=Accumulate[dtsOutput];
	taus=(Most@Prepend[taus,0]+taus)/2;

	(*Medium-efficiency implementation of discrete convolution tensor (phi) of the DE*)
	(*We need to be careful not to do MatrixExp[-t*A] for positive t; this could lead
	to numerical instability. In general, we want to group all of the matrix exponentials 
	together *)
	normA=Norm[A];
	{d,U}=Eigensystem[A/normA];
	d=normA*d; U=U\[Transpose];
	Ainv=Inverse[A/normA];

	w=Inverse[U].b/normA;
	c=-Ainv.b/normA;
	v=-Ainv.U;

	(*This array will solve for the tensor which solves the entire DE*)
	deConv=Table[
		Which[
			taus[[m]] < ts[[n-1+1]],
				ConstantArray[0,nc],
			taus[[m]] >= ts[[n+1]],
				v.((Exp[(taus[[m]]-ts[[n+1]])*d]-Exp[(taus[[m]]-ts[[n-1+1]])*d])*w),
			True,
				c-v.(Exp[(taus[[m]]-ts[[n-1+1]])*d]*w)
		], 
		{m,numOutput},{n,numInput+numModInput}
	]; (* index order (M,N,nc) *)

	(*We can pick out the component of interest to get the jacobian*)
	phi=Map[{{Re[#],-Im[#]},{Im[#],Re[#]}}&,deConv[[All,All,outputComponent]],{2}]; (* index order (M,N,L,K) *)
	phi=Transpose[phi,{1,3,2,4}]; (*index order (M,L,N,K)*)

	(*We will only care about the last state, so throw out the rest of the tensor*)
	deConv=deConv[[Position[taus-Total[dtsInput], _?Positive, 1, 1][[1, 1]], 1;;numInput,All]]\[Transpose]; (*index order (M,nc)*)

	(*Good idea to precompute these*)
	AUs=MatrixExp[#*A]&/@dtsComp;

	(*The rest is no different than, eg, ConvolutionDistortion *)
	With[{phimat=phi,deConvMat=deConv,AUsMat=AUs},
		DistortionOperator[
			Function[{pulse,computeJac},
					Module[{jac=phimat,AUsM=AUsMat,outputPulse,lastState,modInput,vv,ww,amp},
						modInput=pulse;
						(*Compute the full state of the DE at the end of numInput steps*)
						lastState=deConvMat.(pulse[[All,2]]+I*pulse[[All,3]]);
						(*Now loop through the compensation time steps and find the amplitude we need*)
						Table[
							With[{tmp1=Ainv.(IdentityMatrix[nc]-AUsM[[step]]).b/normA,tmp2=AUsM[[step]].lastState},
								(*This projection finds the optimal compensation amplitude*)
								vv=P.tmp1;
								ww=P.(tmp2-ratios[[step]]*lastState);
								amp=(vv.ww)/(vv.vv);
								AppendTo[modInput,{dtsComp[[step]],Re[amp],Im[amp]}];
								(*This computes the state at the end of the current compensation*)
								lastState=tmp2-amp*tmp1;
							],
							{step,Length@dtsComp}
						];

						(*We have the modified input pulse, and so can compute the output pulse.*)
						outputPulse=AddTimeSteps[dtsOutput, Normal@TensorPairContract[jac,modInput[[All,2;;]],{{3,1},{4,2}}]];
						Which[
							computeJac===True,
							(*The compensation timesteps should not appear in in the jacobian*)
							{outputPulse,jac[[All,All,1;;numInput,All]]},
							computeJac===False,
							outputPulse,
							computeJac===All,
							{pulse,modInput,outputPulse}
						]
					]
				],
			Format[HoldForm[LinearDEDistortion[MatrixForm[A],MatrixForm[b]]]]
		]
	]
]


(* ::Subsubsection::Closed:: *)
(*FrequencySpaceDistortion*)


FrequencySpaceDistortion[freqs_List,dt_,M_] := Module[{c,s,numN,distortionFn},
	numN = Length[freqs];
	c = Table[Cos[(m-1)*dt*2*\[Pi]*N@freqs[[n]]], {m,M}, {n,numN}];
	s = Table[Sin[(m-1)*dt*2*\[Pi]*N@freqs[[n]]], {m,M}, {n,numN}];

	(* TODO be less lazy and actually compute the jacobian*)
	With[{cval=c,sval=s},
		DistortionOperator[
			First@PerturbateDistortion@DistortionOperator@Function[{pulse,computeJac},
				With[{p={ConstantArray[dt,M],cval.pulse[[All,2]]+sval.pulse[[All,3]],cval.pulse[[All,3]]-sval.pulse[[All,2]]}\[Transpose]},
					If[computeJac===False,
						p,
						{pulse,pulse,p}
					]
				]
			],
			Format@HoldForm[FrequencySpaceDistortion[freqs]]
		]
	]
]



(* ::Subsubsection::Closed:: *)
(*CompositePulseDistortion*)


CompositePulseDistortion[divisions_,sequence_]:=Module[{symbols,indeces,seq,doreverse},
	symbols=First/@divisions;
	doreverse[symb_]:=Assuming[And@@(#>0&/@symbols),Simplify[symb<0]];
	seq = If[Head[#]===List,#,{#,0}]&/@sequence;
	seq = {If[doreverse@#1,-#1,#1],#2,doreverse@#1}&@@@seq;
	With[{seqval=seq},
		DistortionOperator[
			First@PerturbateDistortion@DistortionOperator@Function[{pulse,computeJac},
				With[{p=
					Flatten[
						Table[
							(If[Last@s,Reverse,Identity])@If[Abs[s[[2]]]>0,
								pulse[[First@s/.divisions]].RotationMatrix[-s[[2]],{1,0,0}],
								pulse[[(First@s)/.divisions]]
							],
							{s,seqval}
						],
					1]
				},
					If[computeJac===False,
						p,
						{pulse,pulse,p}
					]
				]
			],
			Format@HoldForm[CompositePulseDistortion[sequence]]
		]
	]
]


(* ::Subsection::Closed:: *)
(*Distributions*)


IdentityDistribution[]:={{1}, {{}}};


ProductParameterDistribution[dist1_,dist2_]:=Module[{ps,ps1,ps2,reps,reps1,reps2},
	{ps1,reps1}=dist1;
	{ps2,reps2}=dist2;
	ps=Flatten[Outer[Times,ps1,ps2,1]];
	reps=Flatten[Outer[Join,reps1,reps2,1],1];
	{ps,reps}
]
ProductParameterDistribution[dist1_,dist2_,moreDists__]:=ProductParameterDistribution[ProductParameterDistribution[dist1,dist2],moreDists]


ParameterDistributionMean[gdist_]:=Module[{ps,reps,symbs,mean},
	{ps,reps}=gdist;
	symbs=reps[[1,All,1]];
	mean=Sum[Abs[ps[[n]]]*reps[[n,All,2]],{n,Length@ps}]/Total[Abs@ps];
	Thread[symbs->mean]
]


RandomSampleParameterDistribution[probDist_,symbols_,n_]:=Module[{Distribution,symb},
	symb=If[Head@symbols===List,symbols,{symbols}];
	{ConstantArray[1./n,n], Thread[symbols->#]&/@RandomVariate[probDist, n]}
]


RandomMultinormalParameterDistribution[\[Mu]_,\[CapitalSigma]_,symbols_,n_]:=RandomSampleParameterDistribution[MultinormalDistribution[\[Mu],If[MatrixQ[\[CapitalSigma]],\[CapitalSigma],DiagonalMatrix[\[CapitalSigma]^2]]],symbols,n]
RandomUniformParameterDistribution[minsAndMaxes_,symbols_,n_]:=RandomSampleParameterDistribution[UniformDistribution[Evaluate@minsAndMaxes],symbols,n]


HistogramParameterDistribution[parameter_,distribution_,range_,numSamples_]:=Module[
	{variates,probs,binLimits},
		variates=RandomVariate[distribution,numSamples];
		{binLimits,probs}=HistogramList[variates,range,"Probability"];
		{
			probs,
			{parameter->#}&/@N[Mean/@Partition[binLimits,2,1]]
		}
	]


UniformParameterDistribution[rules__Rule]:=Module[{symbols,means,widths,nums,values},
	symbols={rules}[[All,1]];
	means={rules}[[All,2,1]];
	widths={rules}[[All,2,2]];
	nums={rules}[[All,2,3]];
	values = MapThread[
		Function[{mean,width,num,symbol},
			If[num>1,
				(symbol->#&)/@Range[mean-width,mean+width,2*width/(num-1)],
				{symbol->mean}
			]
		],
		{means,widths,nums,symbols},
		1
	];
	values = Flatten[Outer[List,Sequence@@values,1], Length[values]-1];
	With[{valueval=values,n=Length@values},
		 {ConstantArray[1/n,n], valueval}
	]
]


TargetSelectorDistribution[targetDistributions__Rule]:=Module[{n,targetSymbols,dists},
	targetSymbols={targetDistributions}[[All,1]];
	dists={targetDistributions}[[All,2]];
	n=Length[targetSymbols];
	{
		Flatten[Table[First@dist/n,{dist,dists}]],
		Flatten[Table[
			Map[Flatten@Join[{targetSymbols[[d]]->1},(#->0&)/@Complement[targetSymbols,targetSymbols[[{d}]]],{#}]&,Last[dists[[d]]]],
			{d,n}
		],1]
	}
]


(* ::Subsection::Closed:: *)
(*Pulse Penalties*)


(* ::Subsubsection::Closed:: *)
(*PulsePenalty*)


PulsePenalty[function_,___][args__]:=function[args]
PulsePenalty/:Format[PulsePenalty[_,format_]]:=format


(* ::Subsubsection::Closed:: *)
(*Zero Penalty*)


ZeroPenalty[]:=Module[
	{Penalty},
	Penalty[pulse_, False]:=0;
	Penalty[pulse_, True]:={0, ConstantArray[0, Dimensions[pulse[[All,2;;-1]]]]};
	PulsePenalty[
		Function[{pulse,doGrad},
			If[doGrad,{0, ConstantArray[0, Dimensions[pulse[[All,2;;-1]]]]},0]			
		],
		Format@HoldForm[ZeroPenalty[]]
	]
]


(* ::Subsubsection::Closed:: *)
(*DemandValuePenalty*)


Unprotect@DemandValuePenalty;
DemandValuePenalty[\[Epsilon]_,m_,r_,qmax_]:=With[{M=Total[Flatten[m]]},
	PulsePenalty[
		Function[{pulse,doGrad},
			Module[{x,\[Kappa],\[Alpha],vec,sum,\[Sigma]sum,penalty},
				x=m*(pulse[[All,2;;]]-r)/qmax;

				\[Alpha]=1000;

				vec=Exp[\[Alpha]*Abs[x]];
				sum=Total[Flatten[vec]];
				\[Sigma]sum=Total[Flatten[Abs[x]*vec]];
				\[Kappa]=\[Sigma]sum/sum;

				penalty=\[Epsilon]*\[Kappa];

				If[doGrad,
					{
						penalty,
						(\[Epsilon]*m/qmax)*Sign[x]*(((vec+\[Alpha]*Abs[x]*vec)*sum-\[Alpha]*vec*\[Sigma]sum)/sum^2)
					},
					penalty
				]
			]
		],
		Format@HoldForm[DemandValuePenalty[MatrixForm[r]]]
	]
]


(* ::Subsubsection::Closed:: *)
(*RingdownPenalty*)


RingdownPenalty[\[Epsilon]_,numRingdownSteps_,qmax_]:=Module[{penaltyFn},
	(*Memoize a DemandValuePenalty based on the dimensions of the input pulse. The avoids the user 
	having to tell us what the shape of the pulse will be when defining RingdownPenalty*)
	penaltyFn[{M_,L_}]:=penaltyFn[{M,L}]=DemandValuePenalty[\[Epsilon],Array[ConstantArray[1,L-1]If[#<M-numRingdownSteps,0,1]&,M],0,qmax];
	PulsePenalty[
		Function[{pulse,doGrad},penaltyFn[Dimensions@pulse][pulse,doGrad]],
		Format@HoldForm[RingdownPenalty[numRingdownSteps]]
	]
]


(* ::Subsection::Closed:: *)
(*Plotting Tools*)


(* ::Subsubsection::Closed:: *)
(*PulsePlot*)


InheritOptions[PulsePlot,{ListPlot},
{
	PulseLayout->Row,
	PulseScaling->None,
	ShowDistortedPulse->False,
	ChannelMapping->Automatic,
	PulsePaddingMultiplier->1.06
}
];


(*Divides list as evenly as possible into num sections*)
Chunks[list_List,num_Integer]:=Module[{ds,len,spans},
	len=Length@list;
	ds=Array[Floor[len/num]+If[#<=Mod[len,num],1,0]&,num];
	spans=MapThread[Span,{Accumulate[Prepend[Most@ds,1]],Accumulate[ds]}];
	Part[list,#]&/@spans
]


(*This private function is used by both PulsePlot and FourierPulsePlot (not yet implemented)*)
MasterPulsePlot[Plotter_,input_,modinput_,output_,opt:OptionsPattern[PulsePlot]]:=
	Module[{plotlist,rows,cols,nInput,nOutput,chanMap,inputMax,outputMax,scaleList,scales,inputMat,modinputMat,outputMat,mult},

		nInput=Length@input;
		If[output=!=None,nOutput=Length@output;,nOutput=nInput;];
		inputMat=input;
		modinputMat=modinput;
		outputMat=output;

		(*Figure out pulse scaling*)
		scaleList[{xscale_,yscale_},num_]:=If[ListQ[yscale],Table[{xscale,ys},{ys,yscale}],ConstantArray[{xscale,yscale},num]];
		scales=OptionValue[PulseScaling];
		If[scales=!=None,
			inputMat=scaleList[scales[[{1,2}]],nInput]*inputMat;
			If[modinputMat=!=None,modinputMat=scaleList[scales[[{1,2}]],nInput]*modinputMat;];
			If[outputMat=!=None,outputMat=scaleList[scales[[{1,3}]],nOutput]*outputMat;];,
			scales={1,1,1};
		];

		(*Figure out max values for plots*)
		If[modinputMat===None,
			If[ListQ[scales[[2]]],
				inputMax=Table[Max[Abs[inputMat[[n,2]]]],{n,nInput}];,
				inputMax=ConstantArray[Max[Abs[Flatten[inputMat[[All,2]]]]],nInput];
			];,
			If[ListQ[scales[[2]]],
				(*Here we take the max over both input and modinput amplitudes*)
				inputMax=Table[Max[Abs[Join[inputMat[[n,2]],modinputMat[[n,2]]]]],{n,nInput}];,
				inputMax=ConstantArray[Max[Abs[Flatten[{inputMat[[All,2]],modinputMat[[All,2]]}]]],nInput];
			];
		];

		If[output=!=None,If[ListQ[scales[[3]]],
			outputMax=Table[Max[Abs[outputMat[[n,2]]]],{n,nOutput}];,
			outputMax=ConstantArray[Max[Abs[Flatten[outputMat[[All,2]]]]],nOutput];
		],outputMax=ConstantArray[1,nOutput];];

		mult=OptionValue[PulsePaddingMultiplier];
		If[NumericQ[mult],
			inputMax=mult*inputMax;
			outputMax=mult*outputMax;,
			inputMax=mult[[1]]*inputMax;
			outputMax=mult[[2]]*outputMax;
		];

		(*Fork between plotter that does input or input/output*)
		If[output===None,
			plotlist=Table[
				Plotter[inputMat[[n]],inputMax[[n]],FilterOptions[{DistributeOption,n},ListPlot,opt]],
				{n,nInput}
			];,
			If[OptionValue[ChannelMapping]===Automatic,
				chanMap=Chunks[Range[nOutput],nInput];,
				chanMap=OptionValue[ChannelMapping];
			];
			plotlist=Table[
				Plotter[
					If[modinput===None,{inputMat[[n]]},{inputMat[[n]],modinputMat[[n]]}], inputMax[[n]],
					outputMat[[chanMap[[n]]]], Max[outputMax[[chanMap[[n]]]]],
					FilterOptions[{DistributeOption,n},ListPlot,opt]
				],
				{n,nInput}
			];
		];

		(*Figure out the return structure*)
		Which[
			OptionValue[PulseLayout]===Row,
				Row[plotlist],
			OptionValue[PulseLayout]===Column,
				Column[plotlist],
			ListQ[OptionValue[PulseLayout]],
				{rows,cols}=OptionValue[PulseLayout];
				plotlist=Partition[plotlist,cols,cols,{1,Mod[Length[plotlist]-1,cols]+1},""];
				If[Length[plotlist]>rows,plotlist=plotlist[[;;rows]]];
				Grid[plotlist],
			OptionValue[PulseLayout]===List,
				plotlist
		]
	];


SingleChannelPulsePlot[input_,inputmax_,opt:OptionsPattern[ListPlot]]:=
	Module[{xaxis,yaxis,range},
		xaxis=Prepend[Accumulate[input[[1]]],0];
		yaxis=Append[input[[2]],0];
		range={{Min@xaxis,Max@xaxis},inputmax*{-1,1}};
		ListPlot[
			{xaxis,yaxis}\[Transpose],
			Evaluate@FilterOptions[ListPlot,opt], 
			PlotRange->range,
			InterpolationOrder->0,
			Joined->True,
			ImageSize->400,
			Filling->Axis
		]

	]


SingleChannelPulsePlot[input_,inputmax_,output_,outputmax_,opt:OptionsPattern[ListPlot]]:=
	Module[{xy1,range1,n1,n2,xy2,range2,minx,maxx,pallette},

		xy1={Prepend[Accumulate[#[[1]]],0],Append[#[[2]], 0]}\[Transpose]&/@input;
		xy2={Prepend[Accumulate[#[[1]]],0],Append[#[[2]], 0]}\[Transpose]&/@output;
		n1=Length@input;
		n2=Length@output;

		{minx,maxx}=#[Flatten[{Table[xy[[All,1]],{xy,xy1}],Table[xy[[All,1]],{xy,xy2}]}]]&/@{Min,Max};
		range1={{minx,maxx},inputmax*{-1,1}};
		range2={{minx,maxx},outputmax*{-1,1}};

		pallette=ColorData[97,"ColorList"];
		pallette=Join[pallette[[{1,4,3}]],pallette[[5;;]]];

		Overlay[{
			ListPlot[
				xy1,
				PlotLegends->None,
				Evaluate@FilterOptions[ListPlot,opt], 
				PlotRange->range1,
				InterpolationOrder->0,
				Joined->True,
				ImageSize->400,
				Filling->{1->Axis,2->None},
				Frame->{True,True,True,False},
				PlotStyle->pallette[[;;n1]],
				FrameStyle->{Automatic,First@pallette,Automatic,Automatic},
				ImagePadding->{{40,40},{40,10}}
			],
			ListPlot[
				xy2,
				Evaluate@FilterOptions[ListPlot,opt], 
				PlotRange->range2,
				InterpolationOrder->0,
				Joined->True,
				ImageSize->400,
				PlotStyle->If[n2==1,pallette[[n1+1]],pallette[[n1+1;;]]],
				Frame->{False,False,False,True},
				FrameStyle->{Automatic,Automatic,Automatic,pallette[[n1+1]]},
				FrameTicks->{None,None,None,All},
				ImagePadding->{{40,40},{40,10}}
			]
		}]

	]


(*This function turns a pulse matrix of the form {{dt1,a11,a12,..},{dt2,a21,a22,..},...} into the
form {{{dt1,dt2,..},{a11,a12,..}},{{dt1,dt2,..},{a21,a22,..}},...} --- there is now redundant time information,
but this structure is more natural for plotting, and is required to make ShowDistortedPulse\[Rule]All work.*)
SeparatePulseMatrix[pulseMat_]:=Table[{pulseMat[[All,1]],pulseMat[[All,n]]},{n,2,Last@Dimensions@pulseMat}];


PulsePlot[pulse_Pulse,opt:OptionsPattern[]]:=
	Module[{plotter,input,modinput,output},
		plotter=SingleChannelPulsePlot;
		input=FromPulse[pulse];
		modinput=None;
		Which[
			OptionValue[ShowDistortedPulse]===True,
				output=SeparatePulseMatrix[pulse[DistortionOperator][input,False]];,
			OptionValue[ShowDistortedPulse]===False,
				output=None;,
			OptionValue[ShowDistortedPulse]==="Only",
				(*Use the input plotting mechanism if only the distorted pulse is to be plotted*)
				output=None;
				input=pulse[DistortionOperator][input,False];,
			OptionValue[ShowDistortedPulse]===All,
				(*Make the modifiedInputPulse a part of the output pulse list*)
				output=pulse[DistortionOperator][input,All];
				{modinput,output}=SeparatePulseMatrix/@output[[{2,3}]];
		];
		MasterPulsePlot[plotter,SeparatePulseMatrix[input],modinput,output,opt]		
	]


(* ::Subsubsection::Closed:: *)
(*RobustnessPlot*)


(* ::Text:: *)
(*Mathematica doesn't have a convenient way of making nice looking ticks and consistent ticks on log plots. The following code block is a nice solution to this from http://mathematica.stackexchange.com/questions/5369/about-the-number-format-in-ticks.*)


SetAttributes[dtZahl, Listable]
dtZahl[x_] := Block[{n}, If[IntegerQ[n = Rationalize[x]], n, x]]

exponentForm[x_?NumberQ] := 
  Module[{me = MantissaExponent[x], num, exp}, 
   If[MemberQ[{0, 0., 1, 1., -1, -1.}, x], Return[IntegerPart[x]]];
   exp = Superscript["\[CenterDot]10", me[[2]] - 1];
   num = NumberForm[N[me[[1]]]*10 // dtZahl, 3];
   If[me[[1]] == 0.1,(*no mantissa*)num = "";
    exp = Superscript[10, me[[2]] - 1], 
    If[me[[2]] == 1,(*range 0..10*)exp = ""]];
   Row[{num, exp}]];
exponentForm[x_] := x

LogTicks[von_Integer, bis_Integer, werte_List, subwerte_List] :=
 Module[{mt, st, ticks, res, tf},
  tf = 1;
  mt = {#, exponentForm[N[#]], {0.01, 0}*tf} & /@ 
    Flatten@Table[10^i*werte, {i, von, bis}];
  st = {#, Null, {0.005, 0}*tf} & /@ 
    Flatten@Table[10^i*subwerte, {i, von, bis}];
  Join[mt, st]]


Options[burnTooltips]={ImageSize->360,"LabelFunction"->(With[{h=1,w=2,r=0.5},
Graphics[{White,Opacity[0.5],FilledCurve[{
Line[{{0,0},{0,h-r}}],
BezierCurve[{{0,h-r},{0,h},{r,h}}],
Line[{{r,h},{w-r,h}}],
BezierCurve[{{w-r,h},{w,h},{w,h-r}}],
Line[{{w,h-r},{w,r}}],
BezierCurve[{{w,r},{w,0},{w-r,0}}]
}],
Black,
Opacity[1],
Text[Style[Superscript[10,#],FontSize->Scaled[h/3]],{w/2,h/2-h/10}]
}]]&)};

burnTooltips[plot_,opt:OptionsPattern[]]:=DynamicModule[{
ins={},
wrapper=OptionValue["LabelFunction"],toolRule=Function[{arg},Tooltip[t__]:>Button[Tooltip[t],AppendTo[arg,Inset[
wrapper[Last[{t}]],
MousePosition["Graphics"],
{0,0},
1.5
]]],HoldAll]
},
EventHandler[
Dynamic@Show[plot/.toolRule[ins],Graphics@ins,ImageSize->OptionValue[ImageSize]],
{"MouseUp",2}:>(toolRule={}&)
]
]


InheritOptions[RobustnessPlot,{Options[ListPlot],Options[ListLogPlot],Options[ListContourPlot]},
	{
		LogPlot->True,
		Function->Automatic,
		Grid->Automatic,
		LegendIsCell->True,
		Alignment->Center,
		DistortionOperatorSweep->False
	}
];


RobustnessPlot[{pulses__Pulse}, sweepParams_Rule, constantParams_List, opt:OptionsPattern[]]:=Module[
	{data, Hint, target, simpulse, xRange, xSymbol,pltpt,Fcn},

		If[Not[And@@Flatten[
			Table[
				PulseHasKey[pulse,#]&/@{TimeSteps,Pulse,InternalHamiltonian,ControlHamiltonians,Target,DistortionOperator},
				{pulse,{pulses}}
			]]],
			Message[RobustnessPlot::keys];Abort[];
		];

		Fcn = OptionValue[Function];
		If[Fcn===Automatic,
			Fcn=If[OptionValue[LogPlot], (Max[1-#,$MachineEpsilon])&, #&];
		];

		data=Table[
			Hint = pulse@InternalHamiltonian;
			If[Not[OptionValue[DistortionOperatorSweep]],
				simpulse = SimForm[PulseReplaceKey[pulse,DistortionOperator,pulse@DistortionOperator/.constantParams], True];
			];
			target = pulse@Target;

			xSymbol = First@sweepParams;
			xRange = Range@@Last@sweepParams;

			data = Table[
				With[{reps=Prepend[constantParams, xSymbol->x]},
					If[OptionValue[DistortionOperatorSweep],
						simpulse = SimForm[PulseReplaceKey[pulse,DistortionOperator,pulse@DistortionOperator/.reps], True];
					];
					{x, Fcn@Utility[
						Last@Unitaries@PulseSim[
							Hint/.reps,
							simpulse/.reps
						],
						target/.reps
					]}
				],
				{x, xRange}
			],
			{pulse,{pulses}}
		];

		If[OptionValue[LogPlot],
			ListLogPlot[data, FilterOptions[ListLogPlot,opt]],
			ListPlot[data, FilterOptions[ListPlot,opt]]
		]
	]
RobustnessPlot[pulse_Pulse,sp_Rule,cp_List,opt:OptionsPattern[]]:=RobustnessPlot[{pulse},sp,cp,opt]


RobustnessPlot[pulseList_List, sweepParamsX_Rule, sweepParamsY_Rule, constantParams_List, opt:OptionsPattern[]]:=Module[
	{pulse, data, Hint, simpulse, xSymbol, ySymbol, xRange, yRange, target, extraopt, Fcn, minx,maxx,miny,maxy,minz,maxz,legend,labels,plots,grid},

		If[Not[And@@Flatten[
			Table[
				PulseHasKey[pulse,#]&/@{TimeSteps,Pulse,InternalHamiltonian,ControlHamiltonians,Target,DistortionOperator},
				{pulse,pulseList}
			]]],
			Message[RobustnessPlot::keys];Abort[];
		];

		(* Choose the function we will be applying to the objective function *)
		Fcn = OptionValue[Function];
		If[Fcn===Automatic,
			Fcn=If[OptionValue[LogPlot], Log10[Max[(1-#),$MachineEpsilon]]&, #&];
		];

		(* Extract sweep limits *)
		xSymbol = First@sweepParamsX;
		ySymbol = First@sweepParamsY;
		xRange = Range@@Last@sweepParamsX;
		yRange = Range@@Last@sweepParamsY;

		(* Generate all data in a 3D array (3rd dim is for length of pulseList) *)
		data=Table[
			pulse=pulseList[[d]];
			Hint = pulse@InternalHamiltonian;
			If[Not[OptionValue[DistortionOperatorSweep]],
				simpulse = SimForm[PulseReplaceKey[pulse,DistortionOperator,pulse@DistortionOperator/.constantParams], True];
			];
			target = pulse@Target;

			Table[
				With[{reps=Join[{xSymbol->x, ySymbol->y}, constantParams]},
					If[OptionValue[DistortionOperatorSweep],
						simpulse = SimForm[PulseReplaceKey[pulse,DistortionOperator,pulse@DistortionOperator/.reps], True];
					];
					Fcn@Utility[
						Last@Unitaries@PulseSim[
							Hint/.reps,
							simpulse/.reps
						],
						target/.reps
					]
				],
				{x, xRange},
				{y, yRange}
			]\[Transpose],
			{d,Length@pulseList}
		];

		(* If a z-plotrange is explictly given in a LogPlot, clip the data at 
           the minimum exponent so that it won't appear as a white "out of range"
		   section on the plot. *)
		If[OptionValue[LogPlot]&&MatchQ[OptionValue[PlotRange],{_?NumericQ,_?NumericQ}],
			data=Clip[data,{Min@OptionValue@PlotRange,\[Infinity]}];
		];

		(* Decide plot limits*)
		minx=Min@xRange;
		maxx=Max@xRange;
		miny=Min@yRange;
		maxy=Max@yRange;
		minz=Min@Flatten@data;
		maxz=Max@Flatten@data;

		(* Make a nice colour scheme which will be the same for all plots. *)
		(* Use no color function scaling to make generating the legend easier. *)
		extraopt = {};
		AppendTo[extraopt,ColorFunction->(ColorData["Warm"][(#-minz)/(maxz-minz)]&)];
		AppendTo[extraopt,ColorFunctionScaling->False];

		(* Make some fancy legends *)
		If[OptionValue@LogPlot,
			legend=SwatchLegend[
					ColorData["Warm"][(#-minz)/(maxz-minz)]&/@Range[Floor@minz,Floor@maxz],
					Style[Superscript[">1-10",#+1]]&/@Range[Floor@minz,Floor@maxz],
					LegendMarkerSize->{15,20},
					LegendLabel->Style["Avg. Fidelity",Bold],
					LegendFunction->"Frame"
				];,
			(* Todo fancy non logplot legend *)
			legend=Automatic;
		];

		(* Figure out what the grid size is *)
		grid=OptionValue@Grid;
		If[grid===Automatic, grid={Ceiling[Length@pulseList/4],4}];

		(* Make the individual figures*)
		plots=Table[
			(* Only put our fancy legend on the last plot. *)
			If[(d==Length@pulseList)&&(Not[OptionValue@LegendIsCell]),AppendTo[extraopt,PlotLegends->legend]];
			(* We ensure user specified options override our options by putting ours after pltopt *)
			(* The execption is PlotLabel, which gets special treatment. *)
			If[OptionValue[LogPlot],
				ListContourPlot[data[[d]], 
					Evaluate@FilterOptions[{DistributeOption,d},ListContourPlot,opt],
					extraopt,
					ContourStyle->Thickness[0.003],
					Contours->Range[Floor@minz,Ceiling@maxz],
					DataRange->{{minx,maxx},{miny,maxy}},
					FrameLabel->(ToString/@{xSymbol,ySymbol})
				],
				ListContourPlot[data[[d]], PlotLabel->labels[[d]], 
					Evaluate@FilterOptions[{DistributeOption,d},ListContourPlot,opt],
					extraopt,
					DataRange->{{minx,maxx},{miny,maxy}}, 
					FrameLabel->(ToString/@{xSymbol,ySymbol})
				]
			],
			{d,Length@pulseList}
		];
		If[OptionValue[LegendIsCell]&&legend=!=Automatic,AppendTo[plots,legend]];

		(* Reshape the figure list into the right grid, some might be empty. *)
		plots=With[{extra=Mod[Length@plots,Last@grid]},
			If[extra>0,
				Append[Partition[plots,Last@grid], Join[plots[[-extra;;-1]],ConstantArray[Null,Last@grid-extra]]],
				Partition[plots,Last@grid]
			]
		];
		While[Length@plots<First@grid, AppendTo[plots,ConstantArray[Null,Last@grid]]];
		plots=plots[[1;;(First@grid)]];

		(* Return the grid *)
		Grid[plots,Alignment->OptionValue[Alignment]]
	];
RobustnessPlot[pulse_Pulse,spx_Rule,spy_Rule,cp_List,opt:OptionsPattern[]]:=RobustnessPlot[{pulse},spx,spy,cp,opt]


(* ::Subsection::Closed:: *)
(*Monitor Functions*)


(* ::Text:: *)
(*Hacky check to see if some penalty function was made using ZeroPenalty[]*)


IsZeroPenalty[penaltyfn_]:=And[Head@penaltyfn===PulsePenalty,Last@penaltyfn===Format[HoldForm[ZeroPenalty[]]]]


Unprotect@FidelityMonitor;
SetAttributes[FidelityMonitor,HoldAll];
FidelityMonitor[GRAPE_,currentPulse_,bestPulse_,utilityList_,abortButton_]:=Monitor[
	GRAPE,
	Grid[{
		{
			Button["Good Enough",abortButton=True],
			Button["Next Guess",abortButton=Next],
			"Utility Value: ",
			ProgressIndicator[currentPulse@UtilityValue], 
			ToString[100 currentPulse@UtilityValue]<>"%"
		},
		If[Not@IsZeroPenalty[currentPulse@PulsePenalty],
			{"","","Penalty Value: ",ProgressIndicator[currentPulse@PenaltyValue],ToString[100 currentPulse@PenaltyValue]<>"%"},
			Sequence@@{}
		],
		If[bestPulse=!=None,
			{"","","Best Pulse: ","Utility "<>ToString[100 bestPulse@UtilityValue]<>"%", "Penalty "<>ToString[100 bestPulse@PenaltyValue]<>"%"},
			Sequence@@{}
		]
	},Alignment->Right]
]


SetAttributes[HistogramMonitor,HoldAll];
HistogramMonitor[GRAPE_,currentPulse_,bestPulse_,utilityList_,abortButton_]:=
	Monitor[
		GRAPE,
		Column[{
			Grid[{
				{
					Button["Good Enough",abortButton=True],
					Button["Next Guess",abortButton=Next],
					"Utility Value: ",
					ProgressIndicator[currentPulse@UtilityValue], 
					ToString[100 currentPulse@UtilityValue]<>"%"
				},
				If[Not@IsZeroPenalty[currentPulse@PulsePenalty],
					{"","","Penalty Value: ",ProgressIndicator[currentPulse@PenaltyValue],ToString[100 currentPulse@PenaltyValue]<>"%"},
					Sequence@@{}
				],
				If[bestPulse=!=None,
					{"","","Best Pulse: ","Utility "<>ToString[100 bestPulse@UtilityValue]<>"%", "Penalty "<>ToString[100 bestPulse@PenaltyValue]<>"%"},
					Sequence@@{}
				]
			},Alignment->Right],
			Histogram[utilityList,{0,1,0.01},"Count",AxesOrigin->{0,0},PlotLabel->"Utility Function Histogram",ImageSize->400,PlotTheme->"Detailed"]
		}]
	]


PulsePlotMonitor[opt:OptionsPattern[PulsePlot]]:=Module[{MonitorFn},
	MonitorFn[GRAPE_,currentPulse_,bestPulse_,utilityList_,abortButton_]:=Monitor[
		GRAPE,
		Column[{
			Grid[{
				{
					Button["Good Enough",abortButton=True],
					Button["Next Guess",abortButton=Next],
					"Utility Value: ",
					ProgressIndicator[currentPulse@UtilityValue], 
					ToString[100 currentPulse@UtilityValue]<>"%"
				},
				If[Not@IsZeroPenalty[currentPulse@PulsePenalty],
					{"","","Penalty Value: ",ProgressIndicator[currentPulse@PenaltyValue],ToString[100 currentPulse@PenaltyValue]<>"%"},
					Sequence@@{}
				],
				If[bestPulse=!=None,
					{"","","Best Pulse: ","Utility "<>ToString[100 bestPulse@UtilityValue]<>"%", "Penalty "<>ToString[100 bestPulse@PenaltyValue]<>"%"},
					Sequence@@{}
				]
			},Alignment->Right],
			PulsePlot[currentPulse,opt,PlotTheme->"Detailed",ImageSize->300],
			Sequence@@If[bestPulse=!=None,{PulsePlot[bestPulse,opt,PlotTheme->"Detailed",ImageSize->300]},{}]
		},Dividers->All]
	];
	SetAttributes[MonitorFn,HoldAll];
	MonitorFn
]


(* ::Subsection::Closed:: *)
(*Gradient Ascent Tools*)


(* ::Subsubsection::Closed:: *)
(*Line Search Methods*)


Options[QuadraticFitLineSearch] = {
	MinStepMul -> 0.1
}


Options[InterpolatedLineSearch] = {
	StepMulStep -> 1,
	MinStepMul -> 0.1,
	MaxStepMul -> 4.1
}


QuadraticFitLineSearch[opts : OptionsPattern[]][initialUtility_, testFn_] := Module[{mults, utilityProfile, fitCoeffs, bestMult},
	
	mults={1,2};
	
	utilityProfile=Join[{initialUtility},
		Map[testFn, mults]
	];

	(* We have three points and we fit a quadratic to it, where the coefficients of the quadratic are stored in fitCoeffs *)
	fitCoeffs={{0.5,-1.,0.5},{-1.5,2.,-0.5},{1., 0., 0.}}.utilityProfile;

	If[fitCoeffs[[1]]>0.,
		(* If the quadratic is positive this method didn't work, so just pick the better of the two multipliers *)
		bestMult=mults[[Last@Ordering[Rest@utilityProfile]]];,
		(* If the quadratic is negative we simply go for the maximum value of the quadratic *)
		bestMult=-fitCoeffs[[2]]/(2*fitCoeffs[[1]]);
	];

	(* If the max looks like it's beyond 2x the stepSize check what's up at 4x *)
	If[bestMult>1.99,
		mults={2,4};
		utilityProfile[[2]]  =utilityProfile[[3]];
		utilityProfile[[3]] = testFn[Last @ mults];
			
		(* Do the fitting again with our three new points *)
		fitCoeffs={{0.125,-0.25,0.125},{-0.75,1.,-0.25},{1., 0., 0.}}.utilityProfile;
		If[fitCoeffs[[1]]>0.,
			bestMult=mults[[Last@Ordering[Rest@utilityProfile]]];,
			bestMult=-fitCoeffs[[2]]/(2*fitCoeffs[[1]]);
		];
	];

	(* In case things go bad and we end up with an imaginary bestMult *)
	bestMult = Clip[Re[bestMult], {OptionValue[MinStepMul], 4}];

	bestMult
];


InterpolatedLineSearch[opts : OptionsPattern[]] := Module[{minStepMul = OptionValue[MinStepMul], maxStepMul = OptionValue[MaxStepMul], stepStep = OptionValue[StepMulStep]},

	Function[{initialUtility, testFn}, Module[{interpolatedLineSearch},
		interpolatedLineSearch = Interpolation [{{0, initialUtility}} ~ Join ~ Table[
			{mul, testFn[mul]},
			{mul, Range[minStepMul, maxStepMul, stepStep]}
		]];

		Clip[\[FormalS] /. FindMaximum[{interpolatedLineSearch[\[FormalS]], minStepMul <= \[FormalS] <= maxStepMul}, {\[FormalS], 1}][[2]],minStepMul,maxStepMul]
	]]

];


(* ::Subsection::Closed:: *)
(*FindPulse*)


Options[FindPulse]={
	InitialStepSize->1.0*10^-3,
	MinimumStepSize->1.0*10^-8,
	MinimumImprovement->1.0*10^-10,
	MonitorFunction->FidelityMonitor,
	Repetitions->1,
	SkipChecks->False,
	VerboseAscent->False,
	DistortionOperator->None,
	PulsePenalty->None,
	ParameterDistribution->None,
	ForceDistortionDependence -> Automatic,
	LineSearchMethod -> QuadraticFitLineSearch[],
	ControlLimitPolicy -> Ignore,
	MinimumIterations -> 0,
	MaximumIterations -> \[Infinity],
	DerivativeMask -> None,
	PulseLegalizer -> LegalizePulse
};


SetAttributes[FindPulse,HoldFirst];


Unprotect[FindPulse];
FindPulse[initialGuess_,target_,\[Phi]target_,controlRange_,Hcontrol_,Hint_,OptionsPattern[]]:=Module[
	{
		minStepSize,improveChk,
		numControlKnobs,\[Epsilon]max,numControlHams,
		repeatCounter,utilityList,currentUtility,
		bestPulseObj,overallBestPulseObj,
		derivMask,
		MonitorFun,
		verboseWidths,verboseFields,
		GRAPE,GRAPEWrapper,abortButton=False,
		(*
			The distortion function is a function DistortionFn[pulse, calcJacobians] that returns
			distortedPulse if calcJacobians is False and {distortedPulse, distortionJacobian} if
			calcJacobians is True.
		*)
		DistortionFn,
		PulsePenaltyFn,
		ParamDistribution,
		distortionDependsOnDist,
		tracedDistortion,

		lineSearchMeth = OptionValue[LineSearchMethod],

		badControlLimitGradientMask, badControlPolicy = OptionValue[ControlLimitPolicy],

		minIters = OptionValue[MinimumIterations],
		maxIters = OptionValue[MaximumIterations]
	},
	
	(* initialize the options that are static *)
	minStepSize=OptionValue[MinimumStepSize];
	improveChk=OptionValue[MinimumImprovement];
	\[Epsilon]max=Abs[Subtract@@@controlRange];

	(* Populate the Default values *)
	numControlKnobs=Length[controlRange]; 
	numControlHams=Length[Hcontrol];
	
	(* Figure out the distortion, penalty, and distribution objects *)
	If[OptionValue[DistortionOperator]=!=None,
		DistortionFn = OptionValue[DistortionOperator];,
		DistortionFn = IdentityDistortion[];
	];
	If[OptionValue[PulsePenalty]=!=None,
		PulsePenaltyFn = OptionValue[PulsePenalty];,
		PulsePenaltyFn = ZeroPenalty[];
	];
	If[OptionValue[ParameterDistribution]=!=None,
		ParamDistribution = OptionValue[ParameterDistribution];,
		(* With probability 1 make no replacements *)
		ParamDistribution = IdentityDistribution[];
	];
	If[OptionValue[DerivativeMask]=!=None,
		derivMask=OptionValue[DerivativeMask];,
		derivMask=1;
	];

	(* Optionally check if the distribution affects the distortions. *)
	If[OptionValue[ForceDistortionDependence] === Automatic,
		distortionDependsOnDist = \[Not]And@@(FreeQ[DistortionFn, #]&/@ParamDistribution[[2,1,All,1]]),
		(* If we are given an explicit value instead of Automatic, use that. *)
		distortionDependsOnDist = OptionValue[ForceDistortionDependence]
	];

	(* Do some dimension checking to avoid a few headaches *)
	If[Not[OptionValue[SkipChecks]],
		(* Check various dimensions for consistency. *)
		If[Length[Dimensions[controlRange]]=!=2,          Message[FindPulse::badrange];Return[$Failed];];

		(* ParameterDistribution consistency checks *)
		Module[{distPs, distReps, distNum},
			{distPs, distReps} = ParamDistribution;
			If[Length@distPs =!= Length@distReps, Message[FindPulse::baddistlength];Return[$Failed]];
			If[Abs[Total[distPs]-1] > 10*$MachineEpsilon, Message[FindPulse::baddistsum];];
			(* The following is a mouthful...basically union all present symbols, replace this list with all 
			elements of the distribution, and check to see if every element turns out to be a number.*)
			If[Not[And@@(NumericQ/@Flatten[((Union@@distReps[[All,All,1]])/.#)&/@distReps[[All,All]]])],
				Message[FindPulse::baddistreps];
			];
		];

		(* Distortion related checks. Can be time consuming if distortion is intensive. *)
		(* Start by distorting the zero pulse so that we know what shape the output is. *)
		Module[{testpulse=initialGuess,distortedPulse,distortionJac,dLen,penaltyGrad,penalty},

			If[Not[MatrixQ[testpulse]],Message[FindPulse::badguess];Return[$Failed]];

			(* get the testpulse to have 0 amplitues to make running the distortion more efficient in many cases *)
			testpulse = With[{r = Length@First@testpulse}, (UnitVector[r,1]*#)&/@testpulse];

			If[ListQ[derivMask],
				If[((Length[derivMask]=!=Length[testpulse]) || (Length[First@derivMask]!=Length[First@testpulse]-1)),
					Message[FindPulse::badderivmask];Return[$Failed];
				];
			];

			(* This line can cause errors sometimes, so let's Check it and make
			more informative errors, at least. *)
			Check[
				{distortedPulse,distortionJac} = DistortionFn[testpulse, True];,
				FindPulse::baddistortion
				Set::shape
			];
			dLen = Length@distortedPulse;

			(* Distortion dimension tests *)
			If[(Last@Dimensions@distortedPulse-1) =!= numControlHams,       Message[FindPulse::baddistortion];Return[$Failed];];
			If[Length@Dimensions@distortionJac =!= 4,                       Message[FindPulse::badjacobian];Return[$Failed];];

			(* Jacobian dimension tests *)
			If[Dimensions[distortionJac][[1]] =!= dLen,                      Message[FindPulse::badjacdim1];Return[$Failed];];
			If[Dimensions[distortionJac][[2]] =!= numControlHams,            Message[FindPulse::badjacdim2];Return[$Failed];];
			If[Dimensions[distortionJac][[3]] =!= Length@testpulse,          Message[FindPulse::badjacdim3];Return[$Failed];];
			If[Dimensions[distortionJac][[4]] =!= numControlKnobs,           Message[FindPulse::badjacdim4];Return[$Failed];];

			(* Does the penalty play nice with the distorted pulse? *)
			{penalty, penaltyGrad} = PulsePenaltyFn[distortedPulse, True];
			If[Not[penalty\[Element]Reals],                                           Message[FindPulse::badpenalty];Return[$Failed];];
			If[Dimensions[penaltyGrad]=!={dLen,numControlHams},              Message[FindPulse::badpenaltygrad];Return[$Failed];];
		];
	];


	(* Sometimes we don't want the monitor function at all, like when we are running FindPulse inside of a Parallel function *)
	MonitorFun=OptionValue[MonitorFunction];
	If[MonitorFun===Off,
		MonitorFun=Function[{expr,somethingelse},expr];
	];

	(* This is the actual GRAPE algorithm, we will run it later, just defining it for now. *)
	GRAPE[initialGuessMat_] := Module[
		{
			pulse,oldPulse,
			stepSize=OptionValue[InitialStepSize],
			utility=0,oldUtility=0,rawUtility=0,
			penalty=0,
			improveFlag=True,improveAvg=0,
			stepCounter=0,
			oldDirec=ConstantArray[0.,{Length@initialGuessMat,numControlKnobs}],
			beta=0,betaResetCt=0,
			optFlag=True,

			betaResetFlag,oldGradient,gradient,diffGradient,improvement,goodDirec,bestMult,utilityProfile,improveSum=0.,
			distPs, distReps, distNum, exitMessage, tic, toc,

			UpdateBestPulse
		},

		(* Make sure that the pulse does not exceed the limits *)
		pulse=OptionValue[PulseLegalizer][initialGuessMat,controlRange];
		oldPulse=pulse;

		(*This keeps track of the best pulse found from this initial guess*)
		bestPulseObj=ToPulse[
			pulse,
			UtilityValue->utility,
			PenaltyValue->penalty,
			Target->target,
			ControlHamiltonians->Hcontrol,
			InternalHamiltonian->Hint,
			DistortionOperator->DistortionFn,
			PulsePenalty->PulsePenaltyFn,
			ParameterDistribution->ParamDistribution,
			AmplitudeRange->controlRange,
			ExitMessage->"No exit message set."
		];
		UpdateBestPulse:=(
			bestPulseObj=PulseReplaceKey[bestPulseObj,ExitMessage,exitMessage];
			bestPulseObj=PulseReplaceKey[bestPulseObj,TimeSteps,pulse[[All,1]]];
			bestPulseObj=PulseReplaceKey[bestPulseObj,Pulse,pulse[[All,2;;]]];
			bestPulseObj=PulseReplaceKey[bestPulseObj,UtilityValue,rawUtility];
			bestPulseObj=PulseReplaceKey[bestPulseObj,PenaltyValue,penalty];
		);

		While[optFlag,
			tic=AbsoluteTime[];

				(* The distribution sampling to be used this iteration *)
				{distPs, distReps} = ParamDistribution;
				distNum = Length@distPs;

				stepCounter++;
				betaResetFlag=False;
				badControlLimitGradientMask = ConstantArray[1, Dimensions[SplitPulse[pulse][[2]]]];

				(********************** CHOOSE A DIRECTION ***********************)

				Module[{distortedPulse, distortionJacobian},

					(* Distort the current pulse and calculate the Jacobian with the control knobs. *)
					(* The output may have distribution symbols included *)
					If[\[Not]distortionDependsOnDist,
						{distortedPulse, distortionJacobian} = DistortionFn[pulse, True]
					];

					(* Now we do a big weighted sum over all elements of the distribution *)
					{rawUtility, utility, gradient} = Sum[
						Module[
							{reps, prob, objFunVal, objFunGrad, penaltyGrad, totalGrad},
							
							(* The replacements and probability of the current distribution element *)
							reps=distReps[[d]];
							prob=distPs[[d]];

							(* If the distortion depends on the distribution, we evaluate it here, replacing what we can.
							Anything else will get replaced in the call to UtilityGradient below. *)
							If[distortionDependsOnDist,
								tracedDistortion = Trace[DistortionFn[pulse, True], TraceDepth -> 1][[-1]];
			
								{distortedPulse, distortionJacobian} = ReleaseHold[DistortionFn/.reps][pulse, True];
							];

							(* Generate the unitaries and evaluate derivatives at (current distorted) pulse *)
							{objFunVal,objFunGrad}=UtilityGradient[distortedPulse/.reps,Hint/.reps,Hcontrol/.reps,target/.reps];

							(* Calculate the pulse penalty and penalty gradient *)
							{penalty, penaltyGrad} = PulsePenaltyFn[distortedPulse/.reps, True];

							(* Update the gradient with the distortion's jacobian *)
							totalGrad = TensorPairContract[distortionJacobian/.reps, objFunGrad-penaltyGrad, {{1,1},{2,2}}];

							(* Update the utility with the penalty *)
							prob*{objFunVal, objFunVal - penalty, totalGrad}
						],
						{d,distNum}
					];

					(* Apply the mask to the gradient, so we can avoid bad controls. *)
					gradient = derivMask * badControlLimitGradientMask * gradient;
				];

				(* Update improvement variables *)
				improvement=utility-oldUtility;
				improveSum+=Abs[improvement];

				(* After every 5 iterations calcualte the avg improvement *)
				If[Mod[stepCounter,5]==0,
					improveAvg=improveSum/5;
					improveSum=0.;
					improveFlag=improveAvg>improveChk
				];

				(* Apply the bad control policy, if any, before backtracking to the previous pulse. *)
				If[badControlPolicy =!= Ignore,
					Module[{badControlIdxs, normp = NormalizePulse[pulse, controlRange]},
						badControlIdxs = Position[normp, p_ /; \[Not](0 < p < 1)];
						If[OptionValue[VerboseAscent] && Length[badControlIdxs] > 0,
							Print["Bad controls at:\t", badControlIdxs]
						];
						If[badControlPolicy === ProjectGradient,
							badControlLimitGradientMask = ReplacePart[badControlLimitGradientMask, badControlIdxs -> 0];
						]
					]
				];

				(* If the last iteration did not yield an improvement take a step back *)
				(* Be lenient on the first round; if initial utility is negative, we don't
				   want to get stuck in a loop where we keep resetting the gradient to zilch. *)
				If[utility<=oldUtility && stepCounter>1,
					pulse=oldPulse;
					gradient=oldGradient;
					utility=oldUtility;
					beta=0.;
					betaResetFlag=True;
					betaResetCt++
				];

				(* Calculate the conjugate gradient direction *)
				If[(stepCounter!=1)&&(Not[betaResetFlag]),
					diffGradient=gradient-oldGradient;
					beta= Total[gradient diffGradient,2]/Total[oldGradient^2,2];
				];

				(* If things have gone really bad then beta<0, then we reset and start with the steepeste descent *)
				beta=Max[beta,0.];

				(* Define the good direction as the linear combination *)
				goodDirec=gradient+beta*oldDirec;


				(********************** EXIT CRITERIA ***********************)
				Which[
					utility>=\[Phi]target && stepCounter > minIters,
						optFlag=False;
						exitMessage="Pulse of desired fidelity was found!";,
					stepCounter >= maxIters,
						optFlag=False;
						exitMessage="Maximum iterations reached; exiting.";,
					stepSize<minStepSize,
						optFlag=False;
						exitMessage="The step size dropped below the minimum allowable step size specified by MinimumStepSize.";,
					Not[improveFlag],
						optFlag=False;
						exitMessage="The average improvement over 5 iterations was less than MinimumImprovement.";,
					betaResetCt>9,
						optFlag=False;
						exitMessage="Reset beta (conjugate direction) too many times. The pulse is probably hitting the power limits excessively.";,
					utility==0,
						optFlag=False;
						exitMessage="Bad guess causing divide by 0.";,
					abortButton=!=False,
						optFlag=False;
						exitMessage="User pressed the abort/next button.";
				];
				(* Save the pulse in case an exit criteria is met *)
				If[Not[optFlag],UpdateBestPulse];

				(************** CALCULATE STEP SIZE MULTIPLIER **************)
				(* Evaluate the objective function at two points along the gradient direction *)
				Module[{testFn},
					testFn = With[{distortedPulse=DistortionFn[pulse+AddTimeSteps[0,#*stepSize*(\[Epsilon]max^2*#&/@goodDirec)], False]},
							Sum[
								With[{reps=distReps[[d]], prob=distPs[[d]]},
									prob*(Utility[PropagatorFromPulse[distortedPulse/.reps,Hint/.reps,Hcontrol/.reps], target/.reps] - 
										PulsePenaltyFn[distortedPulse/.reps, False])
								],
								{d, distNum}
							]
						]&;
					bestMult = lineSearchMeth[utility, testFn];
				];
				
				(*********************** UPDATE PULSE ************************)

				(* Update pulse *)
				oldPulse = pulse;
				pulse=pulse+AddTimeSteps[0,(bestMult*stepSize*\[Epsilon]max^2*#&)/@goodDirec];

				(* Make sure that the pulse does not exceed the limits *)
				pulse=OptionValue[PulseLegalizer][pulse,controlRange];

				(* Pulse has changed; update best pulse for monitor *)
				UpdateBestPulse;

				(* Change the stepSize *)
				stepSize=stepSize*Sqrt[bestMult];

				(* Update interation variables *)
				oldUtility=utility;
				oldGradient=gradient;
				oldDirec=goodDirec;

				(* Print gradient ascent information if VerboseAscent is on *)
				If[OptionValue[VerboseAscent],
					toc=AbsoluteTime[]-tic;
					verboseFields={stepCounter,rawUtility,penalty,improvement,stepSize,bestMult,betaResetCt,1000*toc};
					Print[Row[Table[Row[{verboseFields[[n]]},ImageSize->verboseWidths[[n]]],{n,Length[verboseFields]}]]];
				];

			]; (* End of While Loop *)
		]; (* End of GRAPE Module *)


	(* With the Repetitions, we want to return best pulse found, even if it didn't meet the desired fidelity *)
	overallBestPulseObj = None;
	utilityList={};
	currentUtility:=bestPulseObj[UtilityValue]-bestPulseObj[PenaltyValue];

	(* Now define the GRAPE wrapper algorithm, which deals with the repeat number*)
	repeatCounter=1;
	GRAPEWrapper := 
		While[repeatCounter==1||(repeatCounter<=OptionValue[Repetitions]&&(currentUtility<\[Phi]target)&&Not[abortButton===True]),
			repeatCounter++;

			(* Print the column headers if we are in verbose mode. *)
			If[OptionValue[VerboseAscent],
				verboseWidths={40,100,100,100,100,100,100,100};
				verboseFields={"#","rawUtility","penalty","improvement","stepSize","bestMult","betaResetCt","toc (ms)"};
				Print[Row[Table[Row[{verboseFields[[n]]},ImageSize->verboseWidths[[n]]],{n,Length[verboseFields]}]]];
			];	

			(* Run GRAPE with a new initial guess *)
			GRAPE[initialGuess];

			(* Reset the abort button to False if the user has said to move to the next initial guess. *)
			If[abortButton===Next,
				abortButton=False;
			];

			(* Keep track of all utility values found *)
			AppendTo[utilityList,currentUtility];

			(* Check to see if the pulse we just found beats the rest *)
			If[currentUtility >= Max[0,utilityList],
				overallBestPulseObj=bestPulseObj;
			];

	];

	(* Now actually run GRAPE *)
	MonitorFun[GRAPEWrapper, bestPulseObj, overallBestPulseObj, utilityList, abortButton];

	(* Return the best pulse we found along with other information to the desired output format function *)
	overallBestPulseObj
]


(* ::Subsection::Closed:: *)
(*Exporters*)


(* ::Subsubsection::Closed:: *)
(*JCAMP*)


(* ::Text:: *)
(*Need to write this dumb function because Mathematica has no good builtin way to do it...*)


expFormat[x_,digits_:6]:=Module[{exp,num},
	If[Abs[x]<10^-10,
		StringJoin["0.",Sequence@@ConstantArray["0",digits],"e+00"],
		exp=Floor[Log10[Abs[x]]];
		num=N[Abs[x*10^(-exp)]];
		StringJoin@@({
			If[x>=0," ","-"],
			ToString@Floor[num],
			".",
			With[{str=ToString@Round[(10^digits)*(num-Floor[num])]},Nest["0"<>#&,str,Max[0,digits-StringLength[str]]]],
			"e",
			Sign[exp]/.{-1->"-",1->"+",0->"+"},
			If[Abs[exp]<10,"0"<>ToString[Abs[exp]],ToString[Abs[exp]]]
		})
	]
]


Options[ExportJCAMP]={
	JCAMPTitle->Automatic,
	JCAMPDX->"5.00 Bruker JCAMP library",
	JCAMPDataType->"Shape Data",
	JCAMPOrigin->"Quantum-Utils Mathematica GRAPE",
	JCAMPUser->"nmruser",
	JCAMPDate->Automatic,
	JCAMPTime->Automatic,
	JCAMPShapeExMode->"None",
	JCAMPShapeToTrot->"9.000000e+01",
	JCAMPShapeBWFac->"1.000000e+00",
	JCAMPShapeIntegFac->"1.000000e+00",
	JCAMPShapeMode->"1",
	JCAMPCalibrationFactor->1	
};


ExportJCAMP[filename_,pulse_,OptionsPattern[]]:=Module[
	{s=OpenWrite[filename],define,writepoint,title,date,time,ampphase,calib},
	define[var_,val_,space_:True]:=WriteString[s, "##"<>var<>"="<>If[space," ",""]<>val<>"\n"];
	writepoint[x_,y_]:=WriteString[s, " "<>expFormat[x]<>", "<>expFormat[y]<>"\n"];

	calib=OptionValue[JCAMPCalibrationFactor];
	ampphase={Abs[#1+I*#2]/calib, Mod[180.*Arg[#1+I*#2]/\[Pi],360]}&@@@(pulse@Pulse);

	title=OptionValue[JCAMPTitle];
	If[title===Automatic, title=filename];
	date=OptionValue[JCAMPDate];
	If[date===Automatic, date=DateString[{"Day","-","MonthNameShort","-","Year"}]];
	time=OptionValue[JCAMPTime];
	If[time===Automatic, time=DateString[{"Hour24",":","Minute"}]];

	define["TITLE", title];
	define["JCAMP-DX", OptionValue[JCAMPDX]];
	define["DATA TYPE", OptionValue[JCAMPDataType]];
	define["ORIGIN", OptionValue[JCAMPOrigin]];
	define["OWNER", OptionValue[JCAMPUser]];
	define["DATE", date];
	define["TIME", time];
	define["MINX", expFormat@Min[ampphase[[All,1]]], False];
	define["MAXX", expFormat@Max[ampphase[[All,1]]], False];
	define["MINY", expFormat@Min[ampphase[[All,2]]], False];
	define["MAXY", expFormat@Max[ampphase[[All,2]]], False];
	define["$SHAPE_EXMODE", OptionValue[JCAMPShapeExMode]];
	define["$SHAPE_TOTROT", OptionValue[JCAMPShapeToTrot]];
	define["$SHAPE_BWFAC", OptionValue[JCAMPShapeBWFac]];
	define["$SHAPE_INTEGFAC", OptionValue[JCAMPShapeIntegFac]];
	define["$SHAPE_MODE", OptionValue[JCAMPShapeMode]];
	define["NPOINTS", ToString@Length@ampphase];
	define["XYPOINTS", "(XY..XY)"];

	writepoint@@@ampphase;

	define["END", "", False];
	
	Close[s];

	Print["Pulse exported in Bruker JCAMP format to "<>filename];
]


(* ::Subsubsection::Closed:: *)
(*SHP*)


ExportSHP[filename_, pulse_Pulse, scalePower_:Automatic, digits_:6] := Module[
	{max, amps, phases, f},
	max = Norm[Last/@(pulse@AmplitudeRange)];

	amps = Norm/@(pulse@Pulse);
	phases = If[Norm[{##}]>0,ArcTan[##],0]&@@@(pulse@Pulse);

	If[scalePower===Automatic,
		amps = amps / max;,
		amps = amps / scalePower;
	];
	phases = Mod[phases, 2\[Pi]]/(2\[Pi]);

	f=OpenWrite[filename];
	WriteString[f, expFormat[#1, digits]<>", "<>expFormat[#2, digits]<>"\n"] &@@@ ({amps,phases}\[Transpose]);
	Close[f];
]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


Protect[
	Repetitions,ParameterDistribution,DistortionOperator,ForceDistortionDependence,
	PulsePenalty,DerivativeMask,PulseLegalizer,
	ControlLimitPolicy,MonitorFunction,InitialStepSize,MinimumStepSize,
	LineSearchMethod,MinimumImprovement,MinimumIterations,MaximumIterations,
	SkipChecks,VerboseAscent,
	Ignore,ProjectGradient
];


Protect[
	Pulse,TimeSteps,UtilityValue,PenaltyValue,Target,ControlHamiltonians,
	InternalHamiltonian,AmplitudeRange,ExitMessage,
	ToPulse,FromPulse,SimForm,AddTimeSteps,SplitPulse,
	PulseRemoveKeys,PulseReplaceKey,PulseHasKey,
	PulsePhaseRotate,PulsePhaseRamp,PulseDivide,PulseModulate,
	RandomPulse,RandomSmoothPulse,GenerateAnnealedPulse,AnnealingGenerator,GaussianTailsPulse,
	LegalizePulse,NormalizePulse
];


Protect[
	Utility,UtilityGradient,
	PropagatorFromPulse,PropagatorListFromPulse,
	CoherentSubspaces
];


Protect[
	ApplyInverseDistortion,
	LiftDistortionRank,JoinDistortions,ComposeDistortions,PerturbateDistortion,ScaleDistortion,
	IdentityDistortion,
	TimeScaleDistortion,VariableChangeDistortion,
	ConvolutionDistortion,ExponentialDistortion,
	IQDistortion,
	NonlinearTransferDistortion,
	DEDistortion,DESolver,DESolverArgs,
	LinearDEDistortion,CompensationTimeSteps,DecayRatios,StateMetric,
	FrequencySpaceDistortion,
	CompositePulseDistortion
];


Protect[
	IdentityDistribution,
	ParameterDistributionMean,
	ProductParameterDistribution,
	HistogramParameterDistribution,
	RandomSampleParameterDistribution,RandomMultinormalParameterDistribution,RandomUniformParameterDistribution,
	UniformParameterDistribution,
	TargetSelectorDistribution
];


Protect[
	ZeroPenalty,
	DemandValuePenalty,
	RingdownPenalty
];


Protect[
	PulsePlot,PulseFourierPlot,
	ShowDistortedPulse,ChannelMapping,PulseScaling,PulseLayout,PulsePaddingMultiplier,
	DistributeOption,
	RobustnessPlot,LegendIsCell,DistortionOperatorSweep
];


Protect[FidelityMonitor,HistogramMonitor,PulsePlotMonitor];


Protect[
	QuadraticFitLineSearch,InterpolatedLineSearch,
	MinStepMul,StepMulStep,MaxStepMul
];


Protect[FindPulse];


Protect[ExportJCAMP,ExportSHP];


EndPackage[];
