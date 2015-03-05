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


(* ::Subsection:: *)
(*Preamble*)


BeginPackage["GRAPE`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["UnitTesting`"];
Needs["QUOptions`"];
Needs["DocTools`"];
Needs["QSim`"];


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "GRAPE.nb"}]];


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection::Closed:: *)
(*Options*)


Unprotect[
	Repetitions,ParameterDistribution,DistortionOperator,ForceDistortionDependence,
	PulsePenalty,DerivativeMask,PostIterationFunction,PulseLegalizer,
	ControlLimitPolicy,MonitorFunction,InitialStepSize,MinimumStepSize,
	LineSearchMethod,MinimumImprovement,MinimumIterations,MaximumIterations,
	SkipChecks,VerboseAscent,
	Ignore,ProjectGradient
];


AssignUsage[
	{
		Repetitions,ParameterDistribution,DistortionOperator,ForceDistortionDependence,
		PulsePenalty,DerivativeMask,PostIterationFunction,PulseLegalizer,
		ControlLimitPolicy,MonitorFunction,InitialStepSize,MinimumStepSize,
		LineSearchMethod,MinimumImprovement,MinimumIterations,MaximumIterations,
		SkipChecks,VerboseAscent,
		Ignore,ProjectGradient
	},
	$Usages
];


(* ::Subsection::Closed:: *)
(*Pulse Object*)


Unprotect[
	Pulse,TimeSteps,UtilityValue,PenaltyValue,Target,ControlHamiltonians,
	InternalHamiltonian,AmplitudeRange,ExitMessage,
	ToPulse,SimForm,
	PulseRemoveKeys,PulseReplaceKey,
	PulsePhaseRotate,PulsePhaseRamp
];


AssignUsage[
	{
		Pulse,TimeSteps,UtilityValue,PenaltyValue,Target,ControlHamiltonians,
		InternalHamiltonian,AmplitudeRange,ExitMessage,
		ToPulse,SimForm,
		PulseRemoveKeys,PulseReplaceKey,
		PulsePhaseRotate,PulsePhaseRamp
	},
	$Usages
];


(* ::Subsection:: *)
(*UtilityFunction and Targets*)


Unprotect[
	UtilityFunction,UtilityGradient,
	PropagatorFromPulse,PropagatorListFromPulse,
	CoherentSubspaces
];


AssignUsage[
	{
		UtilityFunction,UtilityGradient,
		PropagatorFromPulse,PropagatorListFromPulse,
		CoherentSubspaces
	},
	$Usages
];


(* ::Subsection::Closed:: *)
(*Helper Functions*)


GenerateRandomPulse::usage = "GenerateRandomPulse[dts,\[Epsilon]Range]";
GenerateSmoothRandomPulse::usage = "GenerateSmoothRandomPulse[dts_,\[Epsilon]Range_,interpolationDistance_:10] picks a random amplitude every interpolationDistance steps, and interpolates these amplitudes to generate a relatively smooth pulse.";
GenerateAnnealedPulse::usage = "GenerateAnnealedPulse[originalPulse_, \[Epsilon]Range_, options] returns a pulse that perturbs originalPulse by a new random pulse with \[Epsilon]Range range.";
GAnnealingPulse::usage = "Specifies the pulse to mix with the original pulse when using GenerateAnnealedPulse. Defaults to GenerateRandomPulse.";


GaussianPulse::usage = "GaussianPulse[dt,T,A,riseTime] creates a Gaussian pulse with stepsize dt, total time T (rounded to the nearest dt), total area A, and rise and fall time of riseTime. This does not return a Pulse[..] object, just returns a matrix with two control amplitudes.";


LegalisePulse::usage = "LegalisePulse[pulse,\[Epsilon]Range] limits the power of the pulse amplitudes to the values given in \[Epsilon]Range.";
NormalizePulse::usage = "NormalizePulse[pulse,\[Epsilon]Range] scales and translates the pulse amplitues to the interval [0,1].";


FidelityProgressBar::usage = "FidelityProgressBar[GRAPE,bestPulse,overallBestPulse,overallBestCost,cost,\[Epsilon]Range,costList,abortButton] displays a graphical progress bar showing the current fidelity. The default value of MonitorFunction.";
HistogramProgressBar::usage = "HistogramProgressBar[GRAPE,GRAPE,bestPulse,overallBestPulse,overallBestCost,cost,\[Epsilon]Range,costList,abortButton]";
PulsePlotMonitorFunction::usage = "PulsePlotMonitorFunction[GRAPE,GRAPE,bestPulse,overallBestPulse,overallBestCost,cost,\[Epsilon]Range,costList,abortButton] draws a quick plot of the control amplitudes.";


AddTimeSteps::usage = "AddTimeSteps[dts,pulse] appends the time of the pulse step to each pulse amplitude list. dts can be a scalar or a vector.";
SplitPulse::usage = "SplitPulse[pulse] performs the inverse of AddTimeSteps. I.e. returns {pulse[[All,1]],pulse[[All,2;;-1]]}.";


(* ::Subsubsection:: *)
(*Options*)


Options[GenerateAnnealedPulse] = {
	GAnnealingPulse -> GenerateRandomPulse
};


Options[GenerateRingdownCompensatedPulse] = {
	GControlTransformationMatrix -> Automatic,
	DistortionOperator -> IdentityDistortion[]
};


Options[PlotInitialPulse] = {
	DistortionOperator->Automatic
};


(* ::Subsection::Closed:: *)
(*Line Search Methods*)


QuadraticFitLineSearch::usage = "TODO (Always searches to a max of 4.)";
InterpolatedLineSearch::usage = "TODO";


(* ::Subsubsection:: *)
(*Options*)


Options[QuadraticFitLineSearch] = {
	GMinStepMul -> 0.1
}


Options[InterpolatedLineSearch] = {
	GStepMulStep -> 1,
	GMinStepMul -> 0.1,
	GMaxStepMul -> 4
}


(* ::Subsection::Closed:: *)
(*Distortions*)


LiftDistortionRank::usage = "LiftDistortionRank[distortion] returns a new distortion function where dimensions corresponding to the number of control knobs have been added both the distortion and distortion Jacobian output. This function is useful if all of your channels feel the same distortion, or if you have a single channel and can't be bothered to construct a rank four tensor for the Jacobian.";
JoinDistortions::usage = "JoinDistortions[Distortion1, Distortion2, ...] joins K single-control distortions functions into a new distortion function with K controls.";
ComposeDistortions::usage = "ComposeDistortions[Distortion1, Distortion2, ...] returns a new distortion function which is the composition of the input distortion functions. Distortion1 is applied first, Distortion2 second, and so on.";
ParametricDistortionQ::usage = "Returns True if and only if the distortion requires additional arguments before evaluating.";


IdentityDistortion::usage = "IdentityDistortion[] returns a distortion function that does nothing. Used as the default value in FindPulse.";


TimeScaleDistortion::usage = "TimeScaleDistortion[multiplier] returns a distortion function which does nothing to pulses but multiplies all times by the multiplier.";
VariableChangeDistortion::usage = "VariableChangeDistortion[changeFn, variableSymbol, doSimplify:True] (or VariableChangeDistortion[{changeFn1, changeFn2,...}, {variableSymbol1, variableSymbol2,...}, doSimplify:True] for multiple controls) performs a change of variable distortion on a per pulse basis. This is the most general distortion in the case where different steps don't affect each other. It is assumed that the functions are analytically differentiable. For example, VariableChangeDistortion[{a*Sin[\[Phi]],a*Cos[\[Phi]]},{a,\[Phi]}] would change from polar coordinates to cartesian coordinates. If doSimplify is True, Simplify is called on the analytic Jacobian after it is calculated.";


IQDistortion::usage = "IQDistortion[Igain,Ioffset,Qgain,Qoffset,Q\[Theta]] performs the distortion x=(Igain*I+Ioffset)+Cos[Q\[Theta]](Qgain*Q+Qoffset), y=Sin[Q\[Theta]](Qgain*Q+Qoffset) on a two channel system, where Q\[Theta] is the angle from I to Q."
NonlinearTransferDistortion::usage = "NonlinearTransferDistortion[gainFunction] performs a distortion by taking the discrete fourier transform of the pulse, adjusting every fourier component by the frequency and amplitute dependent function gainFunction[freq,amp], and then returning it to time domain. In the event that gainFunction is only a function of frequency, this should be the same as a convolution. It is assumed that the pulse has uniform time spacing, and that there is only one channel; use LiftDistortionRank or JoinDistortions to overcome this.";


ConvolutionDistortion::usage = "ConvolutionDistortion[kernel,numInput,numOutput,dtInput,dtOutput] creates a distortion function acting on a single control from the input convolution kernel.";
ExponentialDistortion::usage = "ExponentialDistortion[\[Tau]c,numInput,numOutput,dtInput,dtOutput] simply calls ConvolutionDistortion[Piecewise[{{0,#<0}},Exp[-#/\[Tau]c]/\[Tau]c]&,numInput,numOutput,dtInput,dtOutput].";
DEDistortion::usage = "DEDistortion[deFn, solnSymbol, t, min, max, \[Delta]t, M, OptionsPattern[NDSolve]] (or DEDistortion[deFn, {solnSymbol1, solnSymbol2, ...}, ...] for multiple control Hamiltonians) returns a new distortion function that distorts pulses by a system of differential equations and that finds gradients by pertubation. The argument deFn is a function from pulses to systems of differential equations in a format suitable for NDSolve, where solnSymbol is the variable that represents the distorted pulse. All options of NDSolve passed to this function will be relayed to NDSolve.";
PerturbateDistortion::usage = "PertabateDistortion[distortionFn] adds the definition distortionFn[pulse, True] to distortionFn, where distortionFn[pulse, False] is assumed to be already already defined. This is done under the approximation of linearity.";


FrequencySpaceDistortion::usage = "FrequencySpaceDistortion[freqs,dt,M] makes the domain of your distortion frequency amplitudes of the specified frequencies. M is the number of points in the output pulse. Here, K=L=2.";


CompositePulseDistortion::usage = "CompositePulseDistortion[divisions, sequence] expects an input like CompositePulseDistortion[{a->1;;30,b->31;;-1},{a,{b,\[Pi]/3},{a,2\[Pi]/3},b}]. Here, the output distortion function will replicate the specified input pulse sections (named 'a' and 'b') and concatinate them in the specified order in the output pulse, with optional phase rotation angles."


(* ::Subsubsection:: *)
(*Options*)


AuxiliarySymbols::usage = "An option for DEDistortion that accepts a list of extra symbols in your system of DEs that have nothing to do with the output distortion. They are all set to 0 at min.";
GSolver::usage = "An option for DEDistortion that changes the solver function from NDSolve. Normally, this will be used to call ParametricNDSolve instead of the default NDSolve.";
Replacements::usage = "An option for VariableChangeDistortion which specifies a list of replacement rules to substitute the step-wise derivatives with. This is handy if, for example, your Variable change uses a function like Abs, whose derivative, Sign, Mathematica does not know.";


Options[DEDistortion]:=Join[
	Options[NDSolve],
	{
		AuxiliarySymbols->{},
		GSolver -> NDSolve,
		GSolverExtraArgs -> {}
	}
];


Options[VariableChangeDistortion]={
	Simplify->True,
	Replacements->{}
};


(* ::Subsection::Closed:: *)
(*Distributions*)


ParameterDistributionMean::usage = "ParameterDistributionMean[ParameterDistribution] returns a list of replacement rules at the mean of the distrubution (at objval=1).";


RandomSampleParameterDistribution::usage = "RandomSampleParameterDistribution[probDist_, symbols_, n_] returns a ParameterDistribution based on the ProbabilityDistribution object probDist to be used with FindPulse. n is the number of random variates drawn each iteration of GRAPE. symbols can be a list of symbols or a single symbol.";
MultiNormalParameterDistribution::usage = "MultiNormalParameterDistribution[\[Mu]_, \[CapitalSigma]_, symbols_, n_] passes a multinormal distribution to RandomSampleDistribution; see its documentation. \[CapitalSigma] can be a list of standard deviations, or the covariance matrix.";
UniformParameterDistribution::usage = "UniformParameterDistribution[{{symb1min,symb1max},{symb2min,symb2max},...}, symbols_, n_] passes a uniform distribution to RandomSampleParameterDistribution; see its documentation. First argement can be a list of pairs, or just a pair in the case of a single symbol.";
StaticParameterDistribution::usage = "StaticParameterDistribution[{p1,...,pn},{{symb1->val11,...,symbn->val1n},{symb1->val21,...,symbn->val2n},...}]";
UniformStaticParameterDistribution::usage = "UniformStaticParameterDistribution[symb1->{mean1,width1,num1},symb2->{mean2,width2,num2}...] creates a GDistribiton over the specified hyperrectangle."


(* ::Subsection::Closed:: *)
(*Pulse Penalties*)


ZeroPenalty::usage = "ZeroPenalty[] returns a pulse penalty that does nothing.";


DemandValuePenalty::usage = "DemandValuePenalty[\[Epsilon],m,r,qmax] returns a penalty function that is zero whenever (pulse-r)*m is 0. qmax is the maximum possible amplitude in pulse, and \[Epsilon] is an overall scaling factor.";
RingdownPenalty::usage = "RingdownPenalty[\[Epsilon],startIndex,qmax] returns a penalty function that is zero whenever the pulse is 0 starting at startIndex. This is a special case of DemandValuePenalty.";


(* ::Subsection::Closed:: *)
(*Unitary Evaluators*)


PropagatorFromPulse::usage = "PropagatorFromPulse[pulse,Hint,Hcontrol,Hint]";
PropagatorListFromPulse::usage = "PropagatorListFromPulse[pulse,Hint,Hcontrol]";


(* ::Subsection::Closed:: *)
(*Plotting*)


GShowFidelity::usage = "GShowFidelity is an option for pulse plotting functions that controls whether or not fidelity information is printed.";


DividePulse::usage = "SplitPulse[Pulse[...],n] takes a Pulse object, divides the time step by n, and duplicates each pulse step n times. This means the pulse sequence remains unchanged under simulation.";


PulsePlot::usage = "PulsePlot[Pulse[...], OptionsPattern[]] takes the output from FindPulse and plots the control amplitudes. The options are DistortionOperator (True or False, use the DistortionOperator or don't use it. Default is False.), Normalize (False or a real number by which to normalize. Default is False.), and PlotLabel (A list of subtitles. Default is {\"X\",\"Y\"}).";
PulseFourierPlot::usage = "PulseFourierPlot[Pulse[...], controlNames:{\"X\",\"Y\"}, normalization:2\[Pi]] takes the output from FindPulse and plots the control amplitudes in Fourier domain. The controlNames appear on the subtitle. ";


(* ::Subsection::Closed:: *)
(*Grape*)


FindPulse::usage = "FindPulse[initialGuess_,Utarget_,\[Phi]target_,\[Epsilon]Range_,Hcontrol_,Hint_,[Options]]";


(* ::Subsection::Closed:: *)
(*Meta GRAPE*)


LogTicks::usage = "LogTicks[lowest,highest,labeledTicksPerDecade,unlabeledTicksPerDecade] for example, LogTicks[-4, 1, {1}, {2, 3, 5, 7}]. Function taken from StackExchange; see source code for reference.";


RobustnessPlot::usage = "RobustnessPlot[pulse_Pulse, sweepParams, constantParams, [options]] for 1D or RobustnessPlot[pulse_Pulse, sweepParamsX, sweepParamsY, constantParams, [options]] for 2D. SweepParams have the form symbol->{min,max,step} and constantParams is a list of replacement rules for values that don't change. Any plotting options you specify will be passed to the plotting function. You can also set LogPlot to True or False and Function to any function that will be applied to the objective function. The default value of Function depends on the value of LogPlot because (1-fideltiy) is plotted in the log case..";


LegendIsCell::usage = "LegendIsCell is an option for 2D RobustnessPlots which when True, makes the legend its own figure which is put into the grid just like another plot.";
DistortionOperatorSweep::usage = "DistortionOperatorSweep is an option for RobustnessPlots which when True indicates that the distortion contains a robustness parameter which is being swept and therefore needs to be calculated each time on the inner loop. Default is False.";


(* ::Subsection:: *)
(*Exporters*)


Unprotect[ExportJCAMP,ExportSHP];


AssignUsage[{ExportJCAMP,ExportSHP},$Usages]


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


(* ::Subsection::Closed:: *)
(*Error Messages*)


FindPulse::badrange = "Error: Your \[Epsilon]Range should  be of the form {{\[Epsilon]1min,\[Epsilon]1max},{\[Epsilon]2min,\[Epsilon]2max},...}.";
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


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Utility Functions*)


(* ::Subsubsection::Closed:: *)
(*Initial guess functions*)


GenerateRandomPulse[dts_,\[Epsilon]Range_] := AddTimeSteps[dts, (RandomReal[#,Length@dts]&/@\[Epsilon]Range)\[Transpose]]


GenerateSmoothRandomPulse[dts_,\[Epsilon]Range_,interpolationDistance_:10]:=
	Module[
		{randomEntries, interpFunctions, numRandomPoints},
		numRandomPoints = Max[Floor[Length@dts/interpolationDistance],5];
		randomEntries = RandomReal[#,numRandomPoints]&/@\[Epsilon]Range;
		interpFunctions = Interpolation[#,Method->"Spline"]&/@randomEntries;
		AddTimeSteps[dts, ((#/@Range[1, numRandomPoints, (numRandomPoints-1)/(Length@dts-1)])&/@interpFunctions)\[Transpose]]
	]


GenerateAnnealedPulse[original_, \[Epsilon]Range_, OptionsPattern[]] := original + OptionValue[GAnnealingPulse][ConstantArray[0,Length@original], \[Epsilon]Range]


GaussianPulse[dt_,T_,area_,riseTime_]:=
	Module[{\[Sigma],NI,fun,pulse},
		\[Sigma]=riseTime/3;
		fun[t_]:=Piecewise[{{1,riseTime<t<T-riseTime},{Exp[-(t-riseTime)^2/(2\[Sigma]^2)],t<=riseTime},{Exp[-(t-T+riseTime)^2/(2\[Sigma]^2)],t>=T-riseTime}}];
		NI=NIntegrate[fun[t],{t,0,T}];
		pulse=Table[{area*fun[t-dt/2]/NI,0},{t,0,T,dt}];
		(*Correct the area now that we have a descent shape*)
		pulse=area*pulse/(dt*Total@Flatten@pulse);
		AddTimeSteps[dt, pulse]
	];


(* ::Subsubsection::Closed:: *)
(*Legalise and Normalize Pulse*)


(* ::Text:: *)
(*Hooray for one-liners with nested pure functions.*)


LegalisePulse[pulse_,\[Epsilon]Range_]:=
	If[ListQ[\[Epsilon]Range],
		{#1, Sequence@@MapThread[Max[Min[#1,Last@#2],First@#2]&, {{##2},\[Epsilon]Range}]}& @@@ pulse,
		{#1, Sequence@@( {##2} * Min[1, \[Epsilon]Range/Norm[{##2}]] )}& @@@ pulse
	]


NormalizePulse[pulse_, \[Epsilon]Range_] := 
	Transpose @ With[{tp = Transpose[pulse[[All, -Dimensions[\[Epsilon]Range][[1]];;]]]},
	(tp - \[Epsilon]Range[[All, 1]]) / (\[Epsilon]Range[[All, 2]] - \[Epsilon]Range[[All, 1]])
];


(* ::Subsubsection::Closed:: *)
(*Monitor Functions*)


SetAttributes[FidelityProgressBar,HoldAll];
FidelityProgressBar[GRAPE_,bestPulse_,overallBestPulse_,overallBestCost_, {rawUtility_, cost_}, \[Epsilon]Range_,costList_,abortButton_]:=
	Monitor[GRAPE,Row[{Button["Good Enough",abortButton=True],Button["Next Guess",abortButton=Next],ProgressIndicator[rawUtility], ToString[100 rawUtility]<>"% (Penalty: " <> ToString[(rawUtility - cost) 100] <> "%)"},"  "]]


SetAttributes[HistogramProgressBar,HoldAll];
HistogramProgressBar[GRAPE_,bestPulse_,overallBestPulse_,overallBestCost_,{rawUtility_, cost_},\[Epsilon]Range_,costList_,abortButton_]:=
	Monitor[
		GRAPE,
		Column[{
			Row[{Button["Good Enough",abortButton=True],Button["Next Guess",abortButton=Next],ProgressIndicator[cost],ToString[100 cost]<>"%"},"  "],
			Histogram[costList,{0,1,0.01},"Count",AxesOrigin->{0,0},PlotLabel->"Objective Function Histogram",ImageSize->400]
		}]
	]


SetAttributes[PulsePlotMonitorFunction,HoldAll];
PulsePlotMonitorFunction[GRAPE_,bestPulse_,overallBestPulse_,overallBestCost_,{rawUtility_, cost_},\[Epsilon]Range_,costList_,abortButton_]:=
	Monitor[
		GRAPE,
		Column[{
			Row[{Button["Good Enough",abortButton=True],Button["Next Guess",abortButton=Next],ProgressIndicator[cost],ToString[100 cost]<>"%"},"  "],
			If[ListQ@bestPulse,GraphicsRow[Table[
				ListPlot[bestPulse\[Transpose][[k]],PlotLabel->"Control component "<>ToString[k-1],PlotRange->1.05*\[Epsilon]Range[[k-1]],InterpolationOrder->0,Joined->True,ImageSize->400,Filling->Axis,AxesOrigin->{0,0}],
				{k,2,Length[bestPulse\[Transpose]]}
			]]]
		}]
	]


(* ::Subsubsection::Closed:: *)
(*Time Step Stuff*)


AddTimeSteps[dts_,pulse_]:=If[ListQ@dts,Prepend[pulse\[Transpose],dts]\[Transpose],Prepend[pulse\[Transpose],ConstantArray[dts,Length@pulse]]\[Transpose]]


SplitPulse[pulse_]:={pulse[[All,1]],pulse[[All,2;;-1]]}


(* ::Subsection::Closed:: *)
(*Line Search Methods*)


QuadraticFitLineSearch[opts : OptionsPattern[]][initialCost_, testFn_] := Module[{mults, costProfile, fitCoeffs, bestMult},
	
	mults={1,2};
	
	costProfile=Join[{initialCost},
		Map[testFn, mults]
	];

	(* We have three points and we fit a quadratic to it, where the coefficients of the quadratic are stored in fitCoeffs *)
	fitCoeffs={{0.5,-1.,0.5},{-1.5,2.,-0.5},{1., 0., 0.}}.costProfile;

	If[fitCoeffs[[1]]>0.,
		(* If the quadratic is positive this method didn't work, so just pick the better of the two multipliers *)
		bestMult=mults[[Last@Ordering[Rest@costProfile]]];,
		(* If the quadratic is negative we simply go for the maximum value of the quadratic *)
		bestMult=-fitCoeffs[[2]]/(2*fitCoeffs[[1]]);
	];

	(* If the max looks like it's beyond 2x the stepSize check what's up at 4x *)
	If[bestMult>1.99,
		mults={2,4};
		costProfile[[2]]  =costProfile[[3]];
		costProfile[[3]] = testFn[Last @ mults];
			
		(* Do the fitting again with our three new points *)
		fitCoeffs={{0.125,-0.25,0.125},{-0.75,1.,-0.25},{1., 0., 0.}}.costProfile;
		If[fitCoeffs[[1]]>0.,
			bestMult=mults[[Last@Ordering[Rest@costProfile]]];,
			bestMult=-fitCoeffs[[2]]/(2*fitCoeffs[[1]]);
		];
	];

	(* In case things go bad and we end up with an imaginary bestMult *)
	bestMult = Clip[Re[bestMult], {OptionValue[GMinStepMul], 4}];

	bestMult
];


InterpolatedLineSearch[opts : OptionsPattern[]] := Module[{minStepMul = OptionValue[GMinStepMul], maxStepMul = OptionValue[GMaxStepMul], stepStep = OptionValue[GStepMulStep]},

	Function[{initialCost, testFn}, Module[{interpolatedLineSearch},
		interpolatedLineSearch = Interpolation [{{0, initialCost}} ~ Join ~ Table[
			{mul, testFn[mul]},
			{mul, Append[Range[minStepMul, maxStepMul, stepStep], maxStepMul]}
		]];

		\[FormalS] /. FindMaximum[{interpolatedLineSearch[\[FormalS]], minStepMul <= \[FormalS] <= maxStepMul}, {\[FormalS], 1}][[2]]
	]]

];


(* ::Subsection::Closed:: *)
(*Options*)


Options[FindPulse]={
	InitialStepSize->10^-3,
	MinimumStepSize->10^-8,
	MinimumImprovement->10^-10,
	MonitorFunction->FidelityProgressBar,
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
	PostIterationFunction -> Identity,
	DerivativeMask -> None,
	PulseLegalizer -> LegalisePulse
};


(* ::Subsection::Closed:: *)
(*Distortions*)


LiftDistortionRank[distortion_]:=
	Module[{newDistortion},
		newDistortion[pulse_, False_]:=Module[{d},
				(* Compute the distortion for each control channel *)
				d = Table[distortion[pulse[[All,{1,n}]], False], {n,2,Last@Dimensions@pulse}];
				AddTimeSteps[d[[1,All,1]], d[[All,All,2]]\[Transpose]]
			];
		newDistortion[pulse_, True]:=Module[
			{d,jac,all,controlRank},
			controlRank=Last@Dimensions@pulse-1;
			(* Compute the Jacobian and distortion for each control channel*)
			all = Table[distortion[pulse[[All,{1,n}]], True], {n,2,Last@Dimensions@pulse}];
			(* Same as above *)
			d = AddTimeSteps[all[[1,1,All,1]], all[[All,1,All,2]]\[Transpose]];
			jac = all[[All,2]];
			If[Length@Dimensions@jac===3,
				(* The user supplied Jacobian is a matrix for each channel; lift to rank 4. *)
				jac = Transpose[ConstantArray[jac, controlRank], {2,4,1,3}];
			];
			If[Length@Dimensions@jac===5,
				(* The user supplied Jacobian is rank 4 for each channel; remove singleton dimensions and repeat above procedure. *)
				jac = Transpose[ConstantArray[jac[[All,All,1,All,1]], controlRank], {2,4,1,3}];
			];
			{d, jac}
		];
		newDistortion
	];


ParametricDistortionQ[distortionFn_] = False; (* We'll override it on a case-by-case basis. *)


ForceParametric[d_?ParametricDistortionQ] = d;
ForceParametric[d_/;\[Not]ParametricDistortionQ[d]][args__][dummyArgs__] = d[args];
ForceParametric /: Format[ForceParametric[d_/;\[Not]ParametricDistortionQ[d]]] := With[{fd = Format[d]}, Format[HoldForm[Prefix @ ForceParametric[fd]]]];


ComposeDistortions[Distortion1_,Distortion2_,MoreDistortions___]:=
	Module[{DistortionFn},
		ParametricDistortionQ[DistortionFn] = ParametricDistortionQ[Distortion1] \[Or] ParametricDistortionQ[Distortion2];
		Format[DistortionFn] := With[{d1=Distortion1, d2=Distortion2}, Format@HoldForm[ComposeDistortions][d1,d2]];

		If[ParametricDistortionQ[DistortionFn],
			With[{d1 = ForceParametric@Distortion1, d2 = ForceParametric@Distortion1},
				DistortionFn[pulse_, False] := (
					d2[d1[pulse, False][##], False][##]
				)&;
			],
			DistortionFn[pulse_, False] := Distortion2[Distortion1[pulse, False], False];
		];

		DistortionFn[pulse_, True]:=Module[{d1,jac1,d2,jac2},
			{d1, jac1} = Distortion1[pulse, True];
			{d2, jac2} = Distortion2[d1, True];
			{d2, TensorContractQU[jac2,jac1,{{3,1},{4,2}}]}
		];

		If[Length[List[MoreDistortions]] > 0,
			ComposeDistortions[DistortionFn, MoreDistortions],
			DistortionFn
		]
	]


JoinDistortions[Distortions__]:=
	Module[{newDistortion},
		newDistortion[pulse_, False_]:=Module[{d},
				(* Compute the distortion for each control channel *)
				d = MapIndexed[#1[pulse[[All,{1, First@#2+1}]], False]&, {Distortions}];
				AddTimeSteps[d[[1,All,1]], d[[All,All,2]]\[Transpose]]
			];
		newDistortion[pulse_, True]:=Module[
			{d,jac,all,controlRank},
			controlRank=Length@{Distortions};
			(* Compute the Jacobian and distortion for each control channel*)
			all = MapIndexed[#1[pulse[[All,{1, First@#2+1}]], True]&, {Distortions}];
			(* Same as above *)
			d = AddTimeSteps[all[[1,1,All,1]], all[[All,1,All,2]]\[Transpose]];
			jac = all[[All,2]];
			(* The user supplied Jacobian is rank 4 for each channel *)
			jac = Transpose[ConstantArray[jac[[All,All,1,All,1]], controlRank], {2,4,1,3}];
			{d, jac}
		];
		newDistortion
	];


IdentityDistortion[]:=Module[
		{DistortionFn,jac},
			Format[DistortionFn] := Format[HoldForm[IdentityDistortion[]]];

			jac[{n_,k_}]:=(jac[{n,k}]=Transpose[Table[KroneckerDelta[kk,ll]*IdentityMatrix[n],{kk,k},{ll,k}], {2,4,1,3}]);
			DistortionFn[pulse_, False] := pulse;
			DistortionFn[pulse_, True] := {
				pulse, 
				jac[Dimensions@pulse-{0,1}]
			};
			DistortionFn
	];


TimeScaleDistortion[multiplier_]:=Module[
		{DistortionFn},
			Format[DistortionFn] := Format[HoldForm[TimeScaleDistortion[multiplier]]];

			DistortionFn[pulse_, False] := AddTimeSteps[multiplier * pulse[[All,1]], pulse[[All,2;;-1]]];
			DistortionFn[pulse_, True] := {
				AddTimeSteps[multiplier * pulse[[All,1]], pulse[[All,2;;-1]]],
				(* The jacobian is the same as IdentityDistortion... *)
				Transpose[ConstantArray[IdentityMatrix[Length@pulse],{Last@Dimensions@pulse-1, Last@Dimensions@pulse-1}], {2,4,1,3}]
			};
			DistortionFn
	];


ConvolutionDistortion[kernel_,numInput_,numOutput_,dtInput_,dtOutput_]:=
	Module[{integral, m, n, discreteKernel, DistortionFn},
		(* If Mathematica can't do the integral, the integral formula will just have to be 
		evaluated in each entry of discreteKernel... *)
		integral[m_,n_] = Integrate[kernel[(m-1/2)*dtOutput - \[Tau]], {\[Tau],(n-1)*dtInput,n*dtInput}, Assumptions->(n\[Element]Reals&&m\[Element]Reals&&n>0&&m>0)];
		discreteKernel = Table[
			integral[m,n],
			{m, numOutput}, {n, numInput}
		];
		DistortionFn[pulse_, False] := AddTimeSteps[dtOutput, discreteKernel.pulse[[All,{2}]]];
		DistortionFn[pulse_, True] := {DistortionFn[pulse,False], Transpose[{{discreteKernel}},{2,4,1,3}]};
		DistortionFn
	]
ConvolutionDistortion[integral_,numInput_,numOutput_,dtOutput_]:=
	Module[{m, n, discreteKernel, DistortionFn},
		(* If Mathematica can't do the integral, the integral formula will just have to be 
		evaluated in each entry of discreteKernel... *)
		discreteKernel = Table[
			integral[m,n],
			{m, numOutput}, {n, numInput}
		];
		DistortionFn[pulse_, False] := AddTimeSteps[dtOutput, discreteKernel.pulse[[All,{2}]]];
		DistortionFn[pulse_, True] := {DistortionFn[pulse,False], Transpose[{{discreteKernel}},{2,4,1,3}]};
		DistortionFn
	]


(* ::Text:: *)
(*The following is a bit slow to call...*)


(*ExponentialDistortion[\[Tau]c_,numInput_,numOutput_,dtInput_,dtOutput_]:=ConvolutionDistortion[Piecewise[{{0,#<0}},Exp[-#/\[Tau]c]/\[Tau]c]&,numInput,numOutput,dtInput,dtOutput]*)


(* ::Text:: *)
(*This speeds things up (just do the integral manually and analytically)*)


ExponentialDistortion[\[Tau]c_,numInput_,numOutput_,dtInput_,dtOutput_]:=ConvolutionDistortion[
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
]


PerturbateDistortion[distortionFn_Symbol, h_:10^-8]:=
	Module[{jacobian},
		jacobian[dts_,numControlKnobs_]:=jacobian[dts]=Transpose[
			Table[
				Module[{Enk=ConstantArray[0,{Length[dts], numControlKnobs}]},
					Enk[[n,k]] = h;
					distortionFn[AddTimeSteps[dts, Enk], False][[All,2;;-1]]/h
				],
				{n,Length[dts]}, {k,numControlKnobs}
			],
			{3,4,1,2}
		];
		distortionFn[pulse_,True]:=With[
			{distortedPulse=distortionFn[pulse,False]},
			{distortedPulse, jacobian[pulse[[All,1]],Last@Dimensions@pulse-1]}
		];
	];


VariableChangeDistortion[changeFn_, variableSymbol_Symbol, opts:OptionsPattern[]] := VariableChangeDistortion[{changeFn}, {variableSymbol}, opts]
VariableChangeDistortion[changeFn_List,{variableSymbols__Symbol},OptionsPattern[]] := Module[
	{distortionFn, subJacobian},

	distortionFn[pulse_, False] := AddTimeSteps[
		pulse[[All,1]], 
		(changeFn/.Thread[{variableSymbols}->#])& /@ pulse[[All, 2;;-1]]
	];

	(* Calculate the per-step Jacobian now, and we won't have to again *)
	subJacobian = Outer[D, changeFn, {variableSymbols}] /. OptionValue[Replacements];

	If[OptionValue[Simplify],
		subJacobian = Simplify@subJacobian;
	];

	distortionFn[pulse_, True] := {
		distortionFn[pulse, False], 
		Transpose[#,{1,3,2,4}]&@Table[
			(* Whenever m!=n, we get no derivative, ie, different time steps do not affect each other. *)
			If[m==n,
				 subJacobian /. Thread[{variableSymbols}->pulse[[n, 2;;-1]]],
				ConstantArray[0,{Length@changeFn, Last@Dimensions@pulse-1}]
			],
			{m, Length@pulse}, {n, Length@pulse}
		]
	};
	distortionFn
]


IQDistortion[Igain_,Ioffset_,Qgain_,Qoffset_,Q\[Theta]_]:=Module[{Ichan,Qchan},
	VariableChangeDistortion[
		{(Igain*Ichan+Ioffset)+Cos[Q\[Theta]](Qgain*Qchan+Qoffset),Sin[Q\[Theta]](Qgain*Qchan+Qoffset)},
		{Ichan,Qchan},
		True
	]
]


NonlinearTransferDistortion[gainFcn_]:=Module[{distortionFn},
	distortionFn[pulse_,False] := Module[{L,L2,dt,Fs,freqs,timeDom,freqDom,distorted},
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
		distorted=L*InverseFourier[gainFcn@@@freqDom];
		{pulse[[All,1]],Re@distorted,Im@distorted}\[Transpose]
	];
	(* Probably a better way than perturbate... *)
	PerturbateDistortion[distortionFn];
	
	distortionFn
]


DEDistortion[deFn_, outputForm_,solnSymbol_Symbol, t_Symbol, min_, max_, \[Delta]t_, M_, opt:OptionsPattern[]] := DEDistortion[deFn, outputForm, {solnSymbol}, t, min, max, \[Delta]t, M, opt];
DEDistortion[deFn_, outputForm_,{solnSymbols__Symbol}, t_Symbol, min_, max_, \[Delta]t_, M_, opt:OptionsPattern[]] := Module[
	{distortionFn,auxSymbols,ndrules, isPara = OptionValue[GSolver] === ParametricNDSolve},
	auxSymbols=OptionValue[AuxiliarySymbols];
	ndrules=Sequence@@FilterRules[{opt},Options[NDSolve]];

	ParametricDistortionQ[distortionFn] = isPara;
	Format[distortionFn] := Format[HoldForm[DEDistortion[deFn, outputForm,{solnSymbols}, t, min, max, \[Delta]t, M, opt]]];

	(* Make a new distortion function, add a new definition to it and return the function. *)

	distortionFn[pulse_, False] := Module[
		{solution,DE},
		DE=deFn[pulse];

		(* We need a With to inject Sequences into ParametricNDSolve. *)
		solution = With[{extraSolverArgs = Sequence @@ OptionValue[GSolverExtraArgs]},
			First @ OptionValue[GSolver][
				DE,
				{solnSymbols},
				{t, min, max},
				(* Inject any additional arguments here. *)
				extraSolverArgs,
				(* We don't want the solver to miss dynamics in the forcing term, so put a relavent limit on MaxStepSize *)
				ndrules
			]
		];
		
		(* If we are using ParametricNDSolve, return a parametric distortion. Otherwise, we're good to just use the solution. *)
		If[isPara,
			(* Make a distorted pulse by applying the solution function to the given timesteps. *)
			(AddTimeSteps[\[Delta]t, Table[
				outputForm /. Prepend[solution[##], t->\[Tau]],
				{\[Tau], \[Delta]t, M \[Delta]t, \[Delta]t}
			]])&, (* <- FIXME: Ugly, ugly copypasta. *)

			(* Make a distorted pulse by applying the solution function to the given timesteps. *)
			AddTimeSteps[\[Delta]t, Table[
				outputForm /. Prepend[solution,t->\[Tau]],
				{\[Tau], \[Delta]t, M \[Delta]t, \[Delta]t}
			]]
		]
	];
	PerturbateDistortion[distortionFn];
	distortionFn
]



FrequencySpaceDistortion[freqs_List,dt_,M_] := Module[
{c,s,numN,distortionFn},
	numN = Length[freqs];
	c = Table[Cos[(m-1)*dt*2*\[Pi]*N@freqs[[n]]], {m,M}, {n,numN}];
	s = Table[Sin[(m-1)*dt*2*\[Pi]*N@freqs[[n]]], {m,M}, {n,numN}];

	distortionFn[pulse_, False] := {ConstantArray[dt,M],c.pulse[[All,2]]+s.pulse[[All,3]],c.pulse[[All,3]]-s.pulse[[All,2]]}\[Transpose];
	distortionFn[pulse_, True] := {
		distortionFn[pulse, False],
		TODO
	};
	PerturbateDistortion[distortionFn];
	distortionFn
]



CompositePulseDistortion[divisions_,sequence_]:=Module[{Distortion,symbols,indeces,rotate,seq,doreverse},
	symbols=First/@divisions;
	rotate[section_,angle_]:=section.{{1,0,0},{0,Cos[angle],Sin[angle]},{0,-Sin[angle],Cos[angle]}};
	doreverse[symb_]:=Assuming[And@@(#>0&/@symbols),Simplify[symb<0]];
	seq = If[Head[#]===List,#,{#,0}]&/@sequence;
	seq = {If[doreverse@#1,-#1,#1],#2,doreverse@#1}&@@@seq;
	Distortion[pulse_,False]:=Flatten[
		Table[
			(If[Last@s,Reverse,Identity])@If[Abs[s[[2]]]>0,
				rotate[pulse[[First@s/.divisions]],s[[2]]],
				pulse[[(First@s)/.divisions]]
			],
			{s,seq}
		],
	1];
	(* TODO: write a real jacobian. not hard, but tedious. *)
	PerturbateDistortion[Distortion];
	Distortion
]


(* ::Subsection::Closed:: *)
(*Distributions*)


ParameterDistributionMean[gdist_]:=Module[{ps,reps,symbs,mean},
	{ps,reps}=gdist[1.0];
	symbs=reps[[1,All,1]];
	mean=Sum[Abs[ps[[n]]]*reps[[n,All,2]],{n,Length@ps}]/Total[Abs@ps];
	Thread[symbs->mean]
]


RandomSampleParameterDistribution[probDist_,symbols_,n_]:=Module[{DistributionFunction,symb},
	symb=If[Head@symbols===List,symbols,{symbols}];
	DistributionFunction[cost_]:={ConstantArray[1./n,n], Thread[symbols->#]&/@RandomVariate[probDist, n]};
	DistributionFunction
]


MultiNormalParameterDistribution[\[Mu]_,\[CapitalSigma]_,symbols_,n_]:=RandomSampleParameterDistribution[MultinormalDistribution[\[Mu],If[MatrixQ[\[CapitalSigma]],\[CapitalSigma],DiagonalMatrix[\[CapitalSigma]^2]]],symbols,n]
UniformParameterDistribution[minsAndMaxes_,symbols_,n_]:=RandomSampleParameterDistribution[UniformDistribution[Evaluate@minsAndMaxes],symbols,n]


StaticParameterDistribution[probs_,reps_]:=Module[{DistributionFunction},
	DistributionFunction[cost_]={probs,reps};
	DistributionFunction
]


UniformStaticParameterDistribution[rules__Rule]:=Module[{symbols,means,widths,nums,values},
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
	Evaluate[StaticParameterDistribution[
		ConstantArray[1/Length[values],Length[values]],
		values
	][1]]&
]


(* ::Subsection::Closed:: *)
(*Pulse Penalties*)


ZeroPenalty[]:=Module[
	{Penalty},
	Penalty[pulse_, False]:=0;
	Penalty[pulse_, True]:={0, ConstantArray[0, Dimensions[pulse[[All,2;;-1]]]]};
	Penalty
]


DemandValuePenalty[\[Epsilon]_,m_,r_,qmax_]:=Module[
	{Penalty,norm},
	norm[pulse_] := 1/\[Epsilon](*qmax * Total[Flatten@m] / \[Epsilon]*);
	Penalty[pulse_, False] := Total[
		(Flatten[(m pulse[[All, 2;;-1]]-r)^2 / norm[pulse]^2])
	];
	Penalty[pulse_, True] := {Penalty[pulse, False], (
		2 * m (pulse[[All, 2;;-1]]-r) / norm[pulse]^2
	)};
	Penalty
]


RingdownPenalty[\[Epsilon]_,startIndex_,qmax_]:=Module[
	{Penalty,norm,mask},
	norm[pulseLen_] := norm[pulseLen] = Abs[qmax * (pulseLen-startIndex+1)];
	mask[pulseDim_] := mask[pulseDim] = Join[ConstantArray[0,{startIndex-1, Last@pulseDim-1}], ConstantArray[1,{First@pulseDim-startIndex+1, Last@pulseDim-1}]];
	Penalty[pulse_, False] := \[Epsilon]*Total[Abs[Flatten[pulse[[startIndex;;-1, 2;;-1]]] / norm[Length@pulse]]^2];
	Penalty[pulse_, True] := {Penalty[pulse, False], \[Epsilon]*2*pulse[[All, 2;;-1]]*mask[Dimensions@pulse] / (norm[Length@pulse]^2)};
	Penalty
]


RingdownPenalty[\[Epsilon]_,startIndex_,qmax_]:=Module[
	{Penalty, prob, probSum},
	probSum[pulseLen_] := probSum[pulseLen]=Sum[(1./(1+(3*(m-startIndex)/(pulseLen-startIndex))^2)),{m,startIndex,pulseLen}];
	prob[m_, pulseLen_] := prob[m,pulseLen]=(1/(1+(3*(m-startIndex)/(pulseLen-startIndex))^2))/probSum[pulseLen];
	Penalty[pulse_, False] := With[
		{pulseLength=Length@pulse, L=Last@Dimensions@pulse-1},
		\[Epsilon]*Sum[
			prob[m,pulseLength]*(1-Exp[(-1/(2*qmax^2))*Total[Abs[pulse[[m,2;;-1]]]^2]]), 
			{m,startIndex,pulseLength}
		]
	];
	Penalty[pulse_, True] := Module[
		{penalties, pulseLength=Length@pulse, L=Last@Dimensions@pulse-1},
		penalties = \[Epsilon]*Table[
			1-Exp[(-1/(2*qmax^2))*Total[Abs[pulse[[m,2;;-1]]]^2]], 
			{m,startIndex,pulseLength}
		];
		{
			Sum[prob[m,pulseLength]*penalties[[m-startIndex+1]], {m,startIndex,pulseLength}],
			Join[
				ConstantArray[0,{startIndex-1, L}],
				Table[prob[m,pulseLength]*(pulse[[m,2;;-1]]/qmax^2)*(1-penalties[[m-startIndex+1]]), {m,startIndex,pulseLength}]
			]
		}
	];
	Penalty
]


(* ::Subsection::Closed:: *)
(*Unitary Evaluators*)


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


(* ::Subsection::Closed:: *)
(*Objective and Derivative Functions*)


(* ::Subsubsection:: *)
(*Target Unitary*)


(* ::Text:: *)
(*We divide by the dimension to make the maximum value of the objective function equal to 1.*)


UtilityFunction[Ucalc_,Utarget_List]:=Abs[Tr[Ucalc\[ConjugateTranspose].Utarget]/Length[Ucalc]]^2;


UtilityGradient[pulse_,Hint_,Hcontrol_,Utarget_List]:=
	Module[
		{
			dim=Length[Hint],
			dts,amps,Uforw,Uback,gradient,cost,unitaries
		},
		unitaries=PropagatorListFromPulse[pulse,Hint,Hcontrol];

		{dts, amps} = SplitPulse[pulse];

		Uforw=Rest[FoldList[#2.#1&,IdentityMatrix[dim],unitaries]];
		Uback=Reverse[FoldList[#2\[ConjugateTranspose].#1&,Utarget,Reverse[Rest[unitaries]]]];
		gradient=Table[
			-2 Re[Tr[Uback[[i]]\[ConjugateTranspose].(I dts[[i]] Hcontrol[[j]].Uforw[[i]])]*Tr[Uforw[[i]]\[ConjugateTranspose].Uback[[i]]]],
			{i,Length[unitaries]},{j,Length[Hcontrol]}
		]/dim^2;
		cost=UtilityFunction[Last[Uforw],Utarget];
		{cost,gradient}
	];


(* ::Subsubsection:: *)
(*Coherent Subspaces*)


UtilityFunction[Ucalc_,target_CoherentSubspaces]:=
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
		performIndex=UtilityFunction[Last[Uforw],target];
		{performIndex,derivs}
	];


(* ::Subsection::Closed:: *)
(*Pulse and Output Formatting*)


(* ::Subsubsection:: *)
(*Extraction methods*)


Pulse/:Pulse[args___][key_]:=Association[args][key]


(* ::Subsubsection:: *)
(*Converting to special formats*)


ToPulse[pulsemat_]:=Pulse[
	TimeSteps -> pulsemat[[All,1]],
	Pulse -> pulsemat[[All,2;;]],
	UtilityValue -> None,
	ParameterDistribution -> ({{1}, {{}}}&),
	DistortionOperator -> IdentityDistortion[]
]


SimForm[pulse_Pulse, distort_:True]:=
	Module[{p, Hc=pulse[ControlHamiltonians]},
		p=pulse[Pulse];
		If[Length@Dimensions@p==1,p=List/@p];
		p=AddTimeSteps[pulse[TimeSteps], p];
		If[distort, p = pulse[DistortionOperator][p, False]];
		{p, Hc}
	]


(* ::Subsubsection:: *)
(*Pulse utility functions*)


RemovePulseHeader[pulse_Pulse,headers__]:=Select[pulse,Not[MemberQ[{headers},#[[1]]]]&]


ReplacePulseHeader[pulse_Pulse,header_,newval_]:=Append[RemovePulseHeader[pulse,header],header->newval]


DividePulse[pulse_Pulse,n_]:=
	Module[{p=pulse[Pulse],dt=pulse[TimeSteps]},
		dt=Flatten[ConstantArray[dt/n,n]\[Transpose]];
		p=p[[Flatten@Table[ConstantArray[k,n],{k,Length@p}]]];
		ReplacePulseHeader[ReplacePulseHeader[pulse,Pulse,p],TimeSteps,dt]
	]


PulsePhaseRotate[pulse_,\[Phi]_]:=
	Module[
		{xy=pulse[Pulse],a\[Theta]},
		a\[Theta]={Norm/@xy,\[Phi]+(ArcTan[First@#,Last@#]&/@xy)}\[Transpose];
		xy={First[#]Cos[Last@#],First[#]Sin[Last@#]}&/@a\[Theta];
		ReplacePulseHeader[pulse,Pulse,xy]
	]


PulsePhaseRamp[pulse_,\[Omega]_]:=
	Module[
		{dt,xy=pulse[Pulse],a\[Theta]},
		dt=pulse[TimeSteps];
		a\[Theta]={Norm/@xy,2\[Pi]*\[Omega]*(Accumulate[dt]-dt/2)+(If[First@#==0&&Last@#==0,0,ArcTan[First@#,Last@#]]&/@xy)}\[Transpose];
		xy={First[#]Cos[Last@#],First[#]Sin[Last@#]}&/@a\[Theta];
		ReplacePulseHeader[pulse,Pulse,xy]
	]


(* ::Subsubsection:: *)
(*Plotting pulses*)


InheritOptions[PulsePlot,{ListPlot},
{
	PlotLabel->{"X","Y"},
	Normalize->False,
	DistortionOperator->False,
	GShowFidelity -> True
}
];


PulsePlotLabel[pulse_, opt : OptionsPattern[PulsePlot]] := With[
	{
		fid = pulse @ UtilityValue,
		controlNames = OptionValue[PlotLabel],
		pen = pulse @ PenaltyValue
	}, With[{
		showFidelity = OptionValue[GShowFidelity] \[And] \[Not](fid === None)
	},
	
	If[showFidelity, "Fidelity: " <> ToString[fid] <> ReleaseHold@If[
		\[Not](pen === None) && Chop@pen > 0,
		Hold[" (penalty: " <> ToString@pen <> ")"],
		""
	] <> "\n", ""] <>
	controlNames[[Mod[k-1,Length[controlNames]]+1]]<>" component"
	]];


PulsePlot[pulse_, opt : OptionsPattern[]]:=If[Not@OptionValue@DistortionOperator,
	Module[{data=pulse[Pulse],dt=pulse[TimeSteps],t,normalization,range},
		normalization=If[OptionValue[Normalize]===False,1,OptionValue[Normalize]];
		t=Prepend[Accumulate[dt],0];
		AppendTo[data, ConstantArray[0, Last@Dimensions@data]];
		range={{Min@t,Max@t},1.05*{Min@#,Max@#}&@Flatten[data]};
		GraphicsRow[Table[
			ListPlot[{t,data\[Transpose][[k]]}\[Transpose]/normalization,PlotLabel -> PulsePlotLabel[pulse, opt], Sequence@@FilterRules[{opt},Options[ListPlot]], PlotRange->range,InterpolationOrder->0,Joined->True,ImageSize->500,Filling->Axis],
			{k,Length[data\[Transpose]]}
		]]
	],
	Module[{reps,ppulse,data,dt,ddt,ddata,t,subt,normalization,range,drange},
		reps=ParameterDistributionMean[pulse@ParameterDistribution];
		ppulse=pulse/.reps;
		
		data=ppulse@Pulse;
		dt=ppulse@TimeSteps;
		normalization=If[OptionValue[Normalize]===False,1,OptionValue[Normalize]];
		{ddt,ddata} = SplitPulse[ppulse[DistortionOperator][AddTimeSteps[dt, data], False]];
		AppendTo[data, ConstantArray[0, Last@Dimensions@data]];
		AppendTo[ddata, ConstantArray[0, Last@Dimensions@data]];
		t=Prepend[Accumulate[dt],0];
		subt=Prepend[Accumulate[ddt],0];
		range={{Min@subt,Max@@subt},1.05*{-Max@Abs@#,Max@Abs@#}&@Flatten[data]};
		drange={{Min@subt,Max@subt},1.05*{-Max@Abs@#,Max@Abs@#}&@Flatten[ddata]};
		Row[Table[
			Overlay[{
				ListPlot[{subt,ddata\[Transpose][[k]]}\[Transpose]/normalization,
					PlotLabel -> PulsePlotLabel[pulse, opt],
					Sequence@@FilterRules[{opt},Options[ListPlot]],
					ImagePadding->30,
					PlotRange->drange,
					InterpolationOrder->0,
					Joined->True,
					ImageSize->500,
					Filling->Axis,
					PlotStyle->Blue,
					FrameStyle->{Automatic,Blue,Automatic,Automatic}
				],
				ListPlot[{t,data\[Transpose][[k]]}\[Transpose]/normalization,
					PlotLabel -> PulsePlotLabel[pulse, opt],
					Sequence@@FilterRules[{opt},Options[ListPlot]],
					ImagePadding->30,
					ImageSize->500,
					PlotRange->range,
					InterpolationOrder->0,
					Frame->{False,False,False,True},
					FrameTicks->{None,True,None,All},
					Joined->True,
					PlotStyle->Red,
					FrameStyle->{Automatic,Automatic,Automatic,Red}
				]
			}],
			{k,Length[data\[Transpose]]}
		]]
	]
]


PulseFourierPlot[pulse_,controlNames_:{"X","Y"},normalization_:(2*\[Pi]),freqSpace_:False]:=
	Module[{fid=UtilityFunction@pulse,data=Pulse@pulse,dt=First@TimeSteps@pulse,T,len},
		If[freqSpace,
			data=DistortionOperator[pulse][AddTimeSteps[TimeSteps@pulse,data],False];
			len=Length[data];
			T=dt*len;
			DiscreteFourierPlot[data[[All,2]]-I*data[[All,3]],{0,T},{Re,Im},Joined->True,PlotRange->All,ImageSize->500]
			,
			If[Mean[TimeSteps@pulse]=!=dt,Print["Warning: pulse sequence is unequally spaced. Assuming a spacing of the first time step for all steps."]];
			len=Length[data];
			T=dt*len;
			GraphicsRow[Table[
				DiscreteFourierPlot[data[[All,k]],{0,T},Abs,Joined->True,PlotRange->All,ImageSize->500,PlotLabel->{"Fourier of "<>controlNames[[Max[k,Length[controlNames]]]]<>" component"},AxesLabel->{"MHz",""}],
				{k,Length[data\[Transpose]]}
			]]
		]
	]


PulseFourierPlot[pulse_,\[Omega]LO_,\[Omega]lowpass_]:=Module[
{x,y,data,dt,len,T,\[Omega]max,d\[Omega],fun,t,box,Fs,samples,xfun,yfun,s,ts},
data=DistortionOperator[pulse][AddTimeSteps[TimeSteps@pulse,Pulse@pulse],False];
dt=data[[1,1]];x=data[[All,2]];y=data[[All,3]];
len=Length@data;T=len*dt;
Fs=20*\[Omega]LO;
\[Omega]max=1/dt;
d\[Omega]=1/(len*dt);
box[t_,m_]:=Piecewise[{{1,(m-1)*dt<t<=m*dt}},0];
xfun[t_]:=If[t<0||t>T-dt,0,x[[Ceiling[(t+dt/5)/dt]]]];
yfun[t_]:=If[t<0||t>T-dt,0,y[[Ceiling[(t+dt/5)/dt]]]];
x=LowpassFilter[xfun/@Range[-T/2,3T/2,1/Fs],\[Omega]lowpass];
y=LowpassFilter[yfun/@Range[-T/2,3T/2,1/Fs],\[Omega]lowpass];
ts=Range[-T/2,3T/2,1/Fs];
s=Cos[2\[Pi] ts \[Omega]LO]*x+Sin[2 \[Pi] ts \[Omega]LO]*y;
(*fun[t_]=Cos[2*\[Pi]*\[Omega]LO*t]*Sum[x[[m]]*box[t,m],{m,len}]+Sin[2*\[Pi]*\[Omega]LO*t]*Sum[y[[m]]*box[t,m],{m,len}];
samples=fun/@Range[-T/2,3T/2,1/Fs];
ListPlot[Evaluate[{Range[0,Fs,Fs/(Length@samples-1)],Abs@Fourier@samples}\[Transpose]],Joined->True,PlotRange->{{\[Omega]LO-\[Omega]max,\[Omega]LO+\[Omega]max},All}]*)
DiscreteFourierPlot[s,{-T/2,3T/2},Abs,Joined->True,PlotRange->{{\[Omega]LO-\[Omega]max,\[Omega]LO+\[Omega]max},All},PlotLabel->"Pulse Power Spectrum",AxesLabel->{"(MHz)",""}]
]


(* ::Subsection::Closed:: *)
(*Implementations*)


(* ::Text:: *)
(*Actual GRAPE. This code has a long history...as far as I know, Colm Ryan wrote it's first version in MATLAB, probably around 2007, which was subsequently improved and modified by Troy Borneman. This was then ported to Mathematica by Holger Haas, and then packaged and improved by Ian Hincks.*)


SetAttributes[FindPulse,HoldFirst];


FindPulse[initialGuess_,target_,\[Phi]target_,\[Epsilon]Range_,Hcontrol_,Hint_,OptionsPattern[]]:=
Module[
	{
		stepSize,minStepSize,improveChk,
		numControlKnobs,\[Epsilon]max,numControlHams,
		repeatCounter,iterCt,
		overallBestCost,overallBestPulse, penaltyOfBest, costList, penalty,
		pulse,bestPulse,oldPulse,
		oldCost,cost=0,
		oldGradient,oldDirec,
		derivMask,
		beta,betaResetCt,
		optFlag,improveAvg,improveFlag,
		MonitorFun,
		tic,toc=0,verboseWidths,verboseFields,
		exitMessage,
		k,
		GRAPE,GRAPEWrapper,abortButton=False,
		(*
			The distortion function is a function DistortionFn[pulse, calcJacobians] that returns
			distortedPulse if calcJacobians is False and {distortedPulse, distortionJacobian} if
			calcJacobians is True.
		*)
		DistortionFn,
		PulsePenaltyFn,
		DistributionFunction,

		(* Keep metadata on whether the distortion depends on any distributions. *)
		distortionDependsOnDist,

		(* We want to evaluate distortions one step before substituting in replacement rules, so declare something to do so. *)
		tracedDistortion,

		(* Separate out the raw utility for richer monitoring. *)
		rawUtility,

		lineSearchMeth = OptionValue[LineSearchMethod],

		gradientMask, badControlPolicy = OptionValue[ControlLimitPolicy],

		minIters = OptionValue[MinimumIterations],
		maxIters = OptionValue[MaximumIterations],

		postIterFcn = OptionValue[PostIterationFunction]
	},
	
	(* initialize the options that are static *)
	minStepSize=OptionValue[MinimumStepSize];
	improveChk=OptionValue[MinimumImprovement];
	\[Epsilon]max=Abs[Subtract@@@\[Epsilon]Range];

	(* Populate the Default values *)
	numControlKnobs=Length[\[Epsilon]Range]; 
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
		DistributionFunction = OptionValue[ParameterDistribution];,
		(* With probability 1 make no replacements *)
		DistributionFunction = {{1}, {{}}}&;
	];
	If[OptionValue[DerivativeMask]=!=None,
		derivMask=OptionValue[DerivativeMask];,
		derivMask=1;
	];

	(* Optionally check if the distribution affects the distortions. *)
	If[OptionValue[ForceDistortionDependence] === Automatic,
		distortionDependsOnDist = \[Not]And@@(FreeQ[DistortionFn, #]&/@DistributionFunction[1][[2,1,All,1]]),
		(* If we are given an explicit value instead of Automatic, use that. *)
		distortionDependsOnDist = OptionValue[ForceDistortionDependence]
	];

	(* Do some dimension checking to avoid a few headaches *)
	If[Not[OptionValue[SkipChecks]],
		(* Check various dimensions for consistency. *)
		If[Length[Dimensions[\[Epsilon]Range]]=!=2,          Message[FindPulse::badrange];Return[$Failed];];

		(* ParameterDistribution consistency checks *)
		Module[{distPs, distReps, distNum},
			{distPs, distReps} = DistributionFunction[0];
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
		Module[{testpulse=initialGuess,distortedPulse,distortionJac,dLen,penaltyGrad},

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
	GRAPE := (

		While[optFlag,
			tic=AbsoluteTime[];
			Module[
				{
					betaResetFlag,gradient,diffGradient,improvement,goodDirec,bestMult,costProfile,improveSum=0.,
					distPs, distReps, distNum
				},

				(* The distribution sampling to be used this iteration *)
				{distPs, distReps} = DistributionFunction[cost];
				distNum = Length@distPs;

				iterCt++;
				betaResetFlag=False;
				gradientMask = ConstantArray[1, Dimensions[SplitPulse[pulse][[2]]]];

				(* Make sure that the pulse does not exceed the limits *)
				pulse=OptionValue[PulseLegalizer][pulse,\[Epsilon]Range];

				(********************** CHOOSE A DIRECTION ***********************)

				Module[{distortedPulse, distortionJacobian},

					(* Distort the current pulse and calculate the Jacobian with the control knobs. *)
					(* The output may have distribution symbols included *)
					If[\[Not]distortionDependsOnDist,
						{distortedPulse, distortionJacobian} = DistortionFn[pulse, True]
					];

					(* Now we do a big weighted sum over all elements of the distribution *)
					{rawUtility, cost, gradient} = Sum[
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
							totalGrad = TensorContractQU[distortionJacobian/.reps, objFunGrad-penaltyGrad, {{1,1},{2,2}}];

							(* Update the cost with the penalty *)
							prob*{objFunVal, objFunVal - penalty, totalGrad}
						],
						{d,distNum}
					];

					(* Apply the mask to the gradient, so we can avoid bad controls. *)
					gradient = derivMask * gradientMask * gradient;
				];


				(* Update improvement variables *)
				improvement=cost-oldCost;
				improveSum+=Abs[improvement];

				(* After every 5 iterations calcualte the avg improvement *)
				If[Mod[iterCt,5]==0,
					improveAvg=improveSum/5;
					improveSum=0.;
					improveFlag=improveAvg>improveChk
				];

				(* Apply the bad control policy, if any, before backtracking to the previous pulse. *)
				If[badControlPolicy =!= Ignore,
					Module[{badControlIdxs, normp = NormalizePulse[pulse, \[Epsilon]Range]},
						badControlIdxs = Position[normp, p_ /; \[Not](0 < p < 1)];
						If[OptionValue[VerboseAscent] && Length[badControlIdxs] > 0,
							Print["Bad controls at:\t", badControlIdxs]
						];
						If[badControlPolicy === ProjectGradient,
							gradientMask = ReplacePart[gradientMask, badControlIdxs -> 0];
						]
					]
				];

				(* If the last iteration did not yield an improvement take a step back *)
				(* Be lenient on the first round; if initial cost is negative, we don't
				   want to get stuck in a loop where we keep resetting the gradient to zilch. *)
				If[cost<=oldCost && iterCt>1,
					pulse=oldPulse;
					gradient=oldGradient;
					cost=oldCost;
					beta=0.;
					betaResetFlag=True;
					betaResetCt++
				];

				(* Calculate the conjugate gradient direction *)
				If[(iterCt!=1)&&(\[Not]betaResetFlag),
					diffGradient=gradient-oldGradient;
					beta= Total[gradient diffGradient,2]/Total[oldGradient^2,2];
				];

				(* If things have gone really bad then beta<0, then we reset and start with the steepeste descent *)
				beta=Max[beta,0.];

				(* Define the good direction as the linear combination *)
				goodDirec=gradient+beta*oldDirec;


				(********************** EXIT CRITERIA ***********************)
				Which[
					cost>=\[Phi]target && iterCt > minIters,
						optFlag=False;
						exitMessage="Pulse of desired fidelity was found!";,
					iterCt >= maxIters,
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
					cost==0,
						optFlag=False;
						exitMessage="Bad guess causing divide by 0.";,
					abortButton=!=False,
						optFlag=False;
						exitMessage="User pressed the abort/next button.";
				];
				If[\[Not]optFlag,bestPulse=pulse,0];(* Save the pulse in case the exit criteria are met *)

				(************** CALCULATE STEP SIZE MULTIPLIER **************)
				(* Evaluate the objective function at two points along the gradient direction *)
				Module[{testFn},
					testFn = With[{distortedPulse=DistortionFn[pulse+AddTimeSteps[0,#*stepSize*(\[Epsilon]max^2*#&/@goodDirec)], False]},
							Sum[
								With[{reps=distReps[[d]], prob=distPs[[d]]},
									prob*(UtilityFunction[PropagatorFromPulse[distortedPulse/.reps,Hint/.reps,Hcontrol/.reps], target/.reps] - 
										PulsePenaltyFn[distortedPulse/.reps, False])
								],
								{d, distNum}
							]
						]&;
					bestMult = lineSearchMeth[cost, testFn];
				];
				
				(*********************** UPDATE PULSE ************************)

				(* Update pulse *)
				oldPulse = pulse;
				pulse=pulse+AddTimeSteps[0,(bestMult*stepSize*\[Epsilon]max^2*#&)/@goodDirec];

				(* User supplied modification function *)
				pulse = postIterFcn[pulse];

				(* Change the stepSize *)
				stepSize=stepSize*Sqrt[bestMult];

				(* Update interation variables *)
				oldCost=cost;
				oldGradient=gradient;
				oldDirec=goodDirec;

				(* Print gradient ascent information if VerboseAscent is on *)
				If[OptionValue[VerboseAscent],
					toc=AbsoluteTime[]-tic;
					verboseFields={iterCt,rawUtility,penalty,improvement,stepSize,bestMult,betaResetCt,1000*toc};
					Print[Row[Table[Row[{verboseFields[[n]]},ImageSize->verboseWidths[[n]]],{n,Length[verboseFields]}]]];
				];
			];
		]
	);
	(* End of GRAPE algorithm *)

	(* With the Repetitions, we want to return best pulse found, even if it didn't meet the desired fidelity *)
	overallBestCost = 0;
	penaltyOfBest = 0;
	overallBestPulse = ConstantArray[0,{10,numControlKnobs}];
	costList={};

	(* Now define the GRAPE wrapper algorithm, which deals with the repeat number*)
	repeatCounter=1;
	GRAPEWrapper := (
		While[repeatCounter<=OptionValue[Repetitions]&&(cost<\[Phi]target)&&(Not[abortButton===True]),
			repeatCounter++;

			(* Pluck an initial guess. *)
			pulse=initialGuess;

			(* initialize non-static variables *)
			oldPulse=pulse;
			bestPulse=pulse;
			stepSize=OptionValue[InitialStepSize];
			oldCost=0.;
			improveAvg=0.;
			improveFlag=True;
			iterCt=0;
			cost=0;
			oldDirec=ConstantArray[0.,{Length@pulse,numControlKnobs}];
			beta=0.;
			betaResetCt=0;
			optFlag=True;

		(* Print the column headers if we are in verbose mode. *)
		If[OptionValue[VerboseAscent],
			verboseWidths={40,100,100,100,100,100,100,100};
			verboseFields={"#","rawUtility","penalty","improvement","stepSize","bestMult","betaResetCt","toc (ms)"};
			Print[Row[Table[Row[{verboseFields[[n]]},ImageSize->verboseWidths[[n]]],{n,Length[verboseFields]}]]];
		];	

		(* Run GRAPE with this initial guess *)
		GRAPE

		(* Reset the abort button to False if the user has said to move to the next initial guess. *)
		If[abortButton===Next,
			abortButton=False;
		];

		(* Keep track of all costs found *)
		AppendTo[costList,cost];

		(* Check to see if the pulse we just found beats the rest *)
		If[cost > overallBestCost,
			overallBestCost=cost;
			overallBestPulse=bestPulse;
			penaltyOfBest = penalty;
		];

	]
	);

	(* Now actually run GRAPE *)
	MonitorFun[GRAPEWrapper, pulse, overallBestPulse, overallBestCost, {rawUtility, cost}, \[Epsilon]Range, costList, abortButton];

	(* Print the error message if the algorithm failed *)
	If[OptionValue[VerboseAscent](*||cost<\[Phi]target*),Print[exitMessage]];

	(* Return the best pulse we found along with other information to the desired output format function *)
	Pulse[
		UtilityValue -> overallBestCost,
		PenaltyValue -> penaltyOfBest,
		TimeSteps -> First@SplitPulse@overallBestPulse,
		Pulse -> Last@SplitPulse@overallBestPulse,
		Target -> target,
		ControlHamiltonians -> Hcontrol,
		InternalHamiltonian -> Hint,
		DistortionOperator -> DistortionFn,
		PulsePenalty -> PulsePenaltyFn,
		ParameterDistribution -> DistributionFunction,
		AmplitudeRange -> \[Epsilon]Range,
		ExitMessage -> exitMessage
	]
]


(* ::Subsection::Closed:: *)
(*Meta GRAPE*)


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


Options[RobustnessPlot]=DeleteDuplicates@Join[
	Options[ListPlot],
	Options[ListLogPlot],
	Options[ListContourPlot],
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
	{data, Hint, target, simpulse, xRange, xSymbol,pltopt,Fcn},

		Fcn = OptionValue[Function];
		If[Fcn===Automatic,
			Fcn=If[OptionValue[LogPlot], (Max[1-#,$MachineEpsilon])&, #&];
		];

		data=Table[
			Hint = pulse@InternalHamiltonian;
			If[Not[OptionValue[DistortionOperatorSweep]],
				simpulse = SimForm[ReplacePulseHeader[pulse,DistortionOperator,pulse@DistortionOperator/.constantParams], True];
			];
			target = pulse@Target;

			xSymbol = First@sweepParams;
			xRange = Range@@Last@sweepParams;

			data = Table[
				With[{reps=Prepend[constantParams, xSymbol->x]},
					If[OptionValue[DistortionOperatorSweep],
						simpulse = SimForm[ReplacePulseHeader[pulse,DistortionOperator,pulse@DistortionOperator/.reps], True];
					];
					{x, Fcn@UtilityFunction[
						Last@Unitaries@EvalPulse[
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
			pltopt = Sequence@@FilterRules[List@opt,Options@ListLogPlot];
			ListLogPlot[data, pltopt],
			pltopt = Sequence@@FilterRules[List@opt,Options@ListPlot];
			ListPlot[data, pltopt]
		]
	]
RobustnessPlot[pulse_Pulse,sp_Rule,cp_List,opt:OptionsPattern[]]:=RobustnessPlot[{pulse},sp,cp,opt]


RobustnessPlot[pulseList_List, sweepParamsX_Rule, sweepParamsY_Rule, constantParams_List, opt:OptionsPattern[]]:=Module[
	{pulse, data, Hint, simpulse, xSymbol, ySymbol, xRange, yRange, target, pltopt, Fcn, minx,maxx,miny,maxy,minz,maxz,legend,labels,plots,grid},

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
				simpulse = SimForm[ReplacePulseHeader[pulse,DistortionOperator,pulse@DistortionOperator/.constantParams], True];
			];
			target = pulse@Target;

			Table[
				With[{reps=Join[{xSymbol->x, ySymbol->y}, constantParams]},
					If[OptionValue[DistortionOperatorSweep],
						simpulse = SimForm[ReplacePulseHeader[pulse,DistortionOperator,pulse@DistortionOperator/.reps], True];
					];
					Fcn@UtilityFunction[
						Last@Unitaries@EvalPulse[
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

		(* Decide plot limits*)
		minx=Min@xRange;
		maxx=Max@xRange;
		miny=Min@yRange;
		maxy=Max@yRange;
		minz=Min@Flatten@data;
		maxz=Max@Flatten@data;

		(* Make a nice colour scheme which will be the same for all plots *)
		pltopt = List@opt;
		If[Head@OptionValue@PlotRange===List,
			AppendTo[pltopt,ColorFunction->"Warm"],
			AppendTo[pltopt,ColorFunction->(ColorData["Warm"][(#-minz)/(maxz-minz)]&)];
			AppendTo[pltopt,ColorFunctionScaling->False];
		];

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

		(* Allow user to input PlotLabel as a list, one for each plot. *)
		If[Head@OptionValue@PlotLabel===List,
			labels=OptionValue@PlotLabel;
			(* repeats labels cyclicly if necessary*)
			While[Length@labels<Length@pulseList,labels=Join[labels,labels]];,
			labels=ConstantArray[OptionValue@PlotLabel,Length@pulseList];
		];

		(* Figure out what the grid size is *)
		grid=OptionValue@Grid;
		If[grid===Automatic, grid={Ceiling[Length@pulseList/4],4}];

		(* Separate plotting options from RobustnessPlot options *)
		pltopt = Sequence@@FilterRules[pltopt,Options@ListContourPlot];

		(* Make the individual figures*)
		plots=Table[
			(* Only put our fancy legend on the last plot. *)
			If[(d==Length@pulseList)&&(Not[OptionValue@LegendIsCell]),pltopt=Sequence@@Append[{pltopt},PlotLegends->legend]];
			(* We ensure user specified options override our options by putting ours after pltopt *)
			(* The execption is PlotLabel, which gets special treatment. *)
			If[OptionValue[LogPlot],
				ListContourPlot[data[[d]], PlotLabel->labels[[d]], pltopt,
					ContourStyle->Thickness[0.003],
					Contours->Range[Floor@minz,Ceiling@maxz],
					DataRange->{{minx,maxx},{miny,maxy}},
					FrameLabel->(ToString/@{xSymbol,ySymbol})
				],
				ListContourPlot[data[[d]], PlotLabel->labels[[d]], pltopt, 
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
(*Exporters*)


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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


(* ::Section:: *)
(*End Package*)


Protect[
	Repetitions,ParameterDistribution,DistortionOperator,ForceDistortionDependence,
	PulsePenalty,DerivativeMask,PostIterationFunction,PulseLegalizer,
	ControlLimitPolicy,MonitorFunction,InitialStepSize,MinimumStepSize,
	LineSearchMethod,MinimumImprovement,MinimumIterations,MaximumIterations,
	SkipChecks,VerboseAscent,
	Ignore,ProjectGradient
];


Protect[
	Pulse,TimeSteps,UtilityValue,PenaltyValue,Target,ControlHamiltonians,
	InternalHamiltonian,AmplitudeRange,ExitMessage,
	ToPulse,SimForm,
	PulseRemoveKeys,PulseReplaceKey,
	PulsePhaseRotate,PulsePhaseRamp
];


Protect[
	UtilityFunction,UtilityGradient,
	PropagatorFromPulse,PropagatorListFromPulse,
	CoherentSubspaces
];


EndPackage[];
