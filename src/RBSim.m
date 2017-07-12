(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica RBSim Package*)


(* ::Subsection::Closed:: *)
(*Copyright and License Information*)


(* ::Text:: *)
(*This package is part of QuantumUtils for Mathematica.*)
(**)
(*Copyright (c) 2015 and later, Christopher J. Wood, Christopher E. Granade, Ian N. Hincks*)
(**)
(*Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:*)
(*1. Redistributions of source code must retain the above copyright notice, this  list of conditions and the following disclaimer.*)
(*2. Redistributions in binary form must reproduce the above copyright notice,  this list of conditions and the following disclaimer in the documentation  and/or other materials provided with the distribution.*)
(*3. Neither the name of quantum-utils-mathematica nor the names of its  contributors may be used to endorse or promote products derived from  this software without specific prior written permission.*)
(**)
(*THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THEIMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE AREDISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLEFOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIALDAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS ORSERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVERCAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USEOF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.*)


(* ::Subsection::Closed:: *)
(*Preamble*)


BeginPackage["RBSim`",{"QUDoc`","QSim`","Tensor`","GRAPE`","QuantumChannel`"}];


Needs["QUDevTools`"];


$RBSimUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "RBSim.nb"}]];


(* ::Section:: *)
(*Usage Declarations*)


(* ::Subsection::Closed:: *)
(*Gate Sets*)


Unprotect[
	GateSet, GateNoiseGateSet,
	GateSetName, Size, Dimension,
	GateProduct, GateInverse,
	GateUnitary, GatePulse, GateChannel,
	FramePotential, TestGateSetPulses, TestGateSetInverses, TestGateSetProducts,
	GateIndices, TakeOverlap
];


AssignUsage[
	{
		GateSet, GateNoiseGateSet,
		GateSetName, Size, Dimension,
		GateProduct, GateInverse,
		GateUnitary, GatePulse, GateChannel,
		FramePotential, TestGateSetPulses, TestGateSetInverses, TestGateSetProducts,
		GateIndices, TakeOverlap
	},
	$RBSimUsages
]


(* ::Subsection::Closed:: *)
(*Noise Models*)


Unprotect[
	NoiseModel,
	GateNoise, DistortionMultiplier, StochasticNoise, StaticNoise, Generator
];


AssignUsage[
	{
		NoiseModel,
		GateNoise, DistortionMultiplier, StochasticNoise, StaticNoise, Generator
	},
	$RBSimUsages
]


(* ::Subsection::Closed:: *)
(*Compiler*)


Unprotect[
	CompileSequence,
	CompiledSequence, PulseSequence, UndistortedLongPulse, GateSequence,
	CompileGateNoise, GateNoiseProtocol, AllowFinalRingdown
];


AssignUsage[
	{
		CompileSequence,
		CompiledSequence, PulseSequence, UndistortedLongPulse, GateSequence,
		CompileGateNoise, GateNoiseProtocol, AllowFinalRingdown
	},
	$RBSimUsages
];


(* ::Subsection::Closed:: *)
(*Simulator*)


Unprotect[
	SimulateSequence, SimulateProtocol, PulseSubset, 
	ExportSimulation, SimulationExportName
];


AssignUsage[
	{
		SimulateSequence, SimulateProtocol, PulseSubset, 
		ExportSimulation, SimulationExportName
	},
	$RBSimUsages
]


(* ::Subsection::Closed:: *)
(*Visualization*)


Unprotect[PlotSequence];
AssignUsage[PlotSequence, $RBSimUsages];


(* ::Subsection::Closed:: *)
(*Protocols*)


Unprotect[
	Protocol, ProtocolName, SimulationOptions, SimulationParser, GateSimulator,
	SequenceLengths, ExperimentTypes, NumSequenceDraws, NumRepetitions, 
	SequenceGenerator, ParallelOptions, TotalGates,
	RBDraw, RBProtocol
];


AssignUsage[
	{
		Protocol, ProtocolName, SimulationOptions, SimulationParser, GateSimulator,
		SequenceLengths, ExperimentTypes, NumSequenceDraws, NumRepetitions, 
		SequenceGenerator, ParallelOptions, TotalGates,
		RBDraw, RBProtocol
	},
	$RBSimUsages
];


GateSimulator::NotImplemented = "GateSimulator is not implemented for this protocol.";


(* ::Subsection::Closed:: *)
(*Example Gate Sets*)


Unprotect[$gaussianQubitGateSet];
AssignUsage[{$gaussianQubitGateSet}, $RBSimUsages];


(* ::Subsection::Closed:: *)
(*Example Gate Noise*)


IndependentNoise::usage = "IndependentNoise[channel]";


(* ::Subsection::Closed:: *)
(*Example Protocols*)


Unprotect[
	RBDraw, RBProtocol
];


AssignUsage[
	{
		RBDraw, RBProtocol
	},
	$RBSimUsages
];


(* ::Section:: *)
(*Implementations*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Private Helper Functions*)


getValue[container_,key_] := With[{assoc=Association@@container}, assoc[key]]
getValue[container_,key_,default_] := If[MemberQ[container[[All,1]], key],
	getValue[container, key],
	default
]
setValue[container_, key_, value_] := container /. {(key->_):>(key->value)}


makeContainer[head_,{validProperties__}] := (
	UpValues[head] = Join[
		Table[
			If[
				Head[property]===Rule,
				With[{h=head,p=property[[1]],v=property[[2]]}, 
					HoldPattern[p[h[args___]]]:>getValue[h[args], p, v]
				],
				With[{h=head,p=property}, 
					HoldPattern[p[h[args___]]]:>getValue[h[args], p]
				]
			], 
			{property,{validProperties}}
		],
		Table[
			If[
				Head[property]===Rule,
				With[{h=head,p=property[[1]]}, 
					HoldPattern[Set[p[h[args___]],v_]]:>setValue[h[args], p, v]
				],
				With[{h=head,p=property}, 
					HoldPattern[Set[p[h[args___]],v_]]:>setValue[h[args], p, v]
				]
			], 
			{property,{validProperties}}
		]
	];
)


(* ::Subsection::Closed:: *)
(*Gate Sets*)


makeContainer[GateSet, {
	GateSetName -> "Unnamed gate set",
	Size,
	Dimension,
	GateProduct,
	GateInverse,
	GateUnitary,
	GatePulse
}];
GateSet/:Format[gs:GateSet[args__]]:="GateSet"[GateSetName->"\""<>ToString@GateSetName[gs]<>"\"",Dimension->Dimension[gs],Size->Size[gs],"..."]


makeContainer[GateNoiseGateSet, {
	GateSetName -> "Unnamed gate set with gate noise",
	Size,
	Dimension,
	GateProduct,
	GateInverse,
	GateUnitary,
	GateNoise,
	GateChannel
}];
GateNoiseGateSet/:Format[gs:GateNoiseGateSet[args__]]:="GateNoiseGateSet"[GateSetName->"\""<>ToString@GateSetName[gs]<>"\"",Dimension->Dimension[gs],Size->Size[gs],"..."]


FramePotential[gs_GateSet, t_]:=Sum[Abs[Tr[GateUnitary[gs][k]]]^(2 t),{k,Size[gs]}]/Size[gs]


Options[TestGateSetPulses] = {
	GateIndices->All,
	TakeOverlap->True
};
TestGateSetPulses[gs_, OptionsPattern[]] := Module[{Uideal, Upulse, idxs, f, i},
	Upulse[i_]:=With[{pulse=GatePulse[gs][i]},Last@Unitaries@PulseSim[0*pulse[[2,1]],pulse]];
	Uideal[i_]:=GateUnitary[gs][i];
	If[OptionValue@TakeOverlap,
		f[{W_,V_}]:=Abs[Tr[W\[ConjugateTranspose].V]/Length[W]]^2,
		f=Identity;
	];
	idxs = If[OptionValue[GateIndices]===All, Range[Size[gs]], OptionValue[GateIndices]];
	Table[
		f[{Uideal[i], Upulse[i]}],
		{i,idxs}
	]
]


Options[TestGateSetInverses] = {
	GateIndices->All,
	TakeOverlap->True
};
TestGateSetInverses[gs_, OptionsPattern[]] := Module[{U, idxs, f},
	U[i_]:=GateUnitary[gs][i];
	If[OptionValue@TakeOverlap,
		f[W_]:=Abs[Tr[W]/Length[W]]^2,
		f=Identity;
	];
	idxs = If[OptionValue[GateIndices]===All, Range[Size[gs]], OptionValue[GateIndices]];
	Table[
		f[U[i].U[GateInverse[gs][i]]],
		{i,idxs}
	]
]


Options[TestGateSetProducts] = {
	GateIndices->All,
	TakeOverlap->True
};
TestGateSetProducts[gs_, OptionsPattern[]] := Module[{U, idxs, f},
	U[i_]:=GateUnitary[gs][i];
	If[OptionValue@TakeOverlap,
		f[{W_,V_}]:=Abs[Tr[W\[ConjugateTranspose].V]/Length[W]]^2,
		f=Identity;
	];
	idxs = If[OptionValue[GateIndices]===All, 
		Outer[List, Range[Size[gs]], Range[Size[gs]]], 
		OptionValue[GateIndices]
	];
	Apply[
		f[{GateUnitary[gs][GateProduct[gs][##]], U[#2].U[#1]}]&,
		idxs,
		{-2}
	]
]


(* ::Subsection::Closed:: *)
(*Noise Model*)


makeContainer[NoiseModel, {
	GateNoise->None, 
	DistortionMultiplier->Automatic, 
	DistortionOperator->None, 
	StochasticNoise->None, 
	StaticNoise->None, 
	Generator->Automatic, 
	StepSize->Automatic
}];


(* ::Subsection::Closed:: *)
(*Compiler*)


(* ::Subsubsection::Closed:: *)
(*CompileSequence*)


makeContainer[CompiledSequence, {
	Generator,
	PulseSequence,
	StepSize,
	GateSequence,
	UndistortedLongPulse,
	GateSet,
	NoiseModel
}];


Options[CompileSequence] = {AllowFinalRingdown -> True};


CompileSequence[gs_GateSet,sequence_,nm_NoiseModel,OptionsPattern[]]:=Module[
	{
		nGates, totalDim, controlDims,
		pulses, pulseLengths, longPulse, undistortedLongPulse,
		distMult, 
		Hcontrol, 
		hasGateNoise, gateNoiseChannels,
		staticSymbols, stochSymbols,
		staticNoiseInstance, stochNoiseInstance, 
		stepSize, totalTime, countDiscreteGates,
		generator, makeTimeDep
	},
	
	(* Get the pulses for the given sequence *)
	nGates = Length@sequence;
	pulses = GatePulse[gs] /@ sequence;
	Hcontrol = pulses[[1,2]];
	controlDims = Length/@Hcontrol;
	pulses = pulses[[All,1]];
	pulseLengths = Length /@ pulses;
	
	(* Concatenate the pulses, run them through the distortion as one long pulse *)
	longPulse = Catenate[pulses];
	undistortedLongPulse = longPulse;
	distMult = 1;
	If[DistortionOperator[nm]=!=None, 
		longPulse = DistortionOperator[nm][longPulse,False];
		If[DistortionMultiplier[nm]===Automatic,
			distMult = Floor[Length[longPulse]/Total[pulseLengths]];,
			distMult = DistortionMultiplier[nm];
		];
	];
	(* Now break them up again *)
	pulses = FoldPairList[TakeDrop, longPulse, pulseLengths * distMult];
	(* And if there is some ringdown at the end, tack in on *)
	If[OptionValue[AllowFinalRingdown] && (Length@longPulse > Total[pulseLengths] * distMult),
		AppendTo[pulses, longPulse[[Total[pulseLengths] * distMult+1;;]]];
	];
	
	(* Gate noise *)
	hasGateNoise = GateNoise[nm] =!= None;
	gateNoiseChannels = If[hasGateNoise,
		Table[{GateNoise[nm]@@(sequence[[;;n]]), stepSize}, {n, nGates}],
		None
	];
	
	(* Timing *)
	stepSize = If[StepSize[nm]===Automatic, longPulse[[1,1]], StepSize[nm]];
	totalTime = Total[longPulse[[All,1]]];
	totalTime += If[hasGateNoise, stepSize * nGates, 0]; (* specifies that each noise op takes one stepSize *)
	With[{accTime = Prepend[Accumulate[stepSize * (pulseLengths * distMult + 1)],0]},
		If[hasGateNoise,
			countDiscreteGates[t_] := Last@Flatten@Position[accTime, _?(#<=t&)]-1;,
			countDiscreteGates[_] := 0;
		]
	];
	
	(* Generate values for all symbols *)
	staticSymbols={};
	stochSymbols={};
	staticNoiseInstance={};
	stochNoiseInstance={};
	If[ListQ@StochasticNoise[nm],
		stochSymbols = StochasticNoise[nm][[All,1]];
		Table[
			Module[{sn = StochasticNoise[nm][[n,2]], symbol=stochSymbols[[n]], variate},
				variate = RandomFunction[sn, {0,totalTime + stepSize*10,stepSize}];
				AppendTo[stochNoiseInstance, symbol->variate];
			],
			{n, Length@stochSymbols}
		];
	];
	If[ListQ@StaticNoise[nm],
		Table[
			Module[{sn = StaticNoise[nm][[n,2]], symbol=StaticNoise[nm][[n,1]], variate},
				variate = RandomVariate[sn];
				If[ListQ@symbol,
					staticSymbols = Join[staticSymbols, symbol];
					staticNoiseInstance = Join[staticNoiseInstance, Thread[symbol->variate]];,
					AppendTo[staticSymbols, symbol];
					AppendTo[staticNoiseInstance, symbol->variate];
				];
				variate
			],
			{n, Length@StaticNoise[nm]}
		];
		staticSymbols = Flatten[staticSymbols];
	];
	
	(* Construct the generator *)
	makeTimeDep[mat_]:=Module[{presentStochSymbols},
		presentStochSymbols = Select[stochSymbols,Not[FreeQ[mat,#]]&];
		If[Length@presentStochSymbols==0,
			mat/.staticNoiseInstance,
			With[{M=mat/.staticNoiseInstance},
				Function[{t}, M /. Cases[stochNoiseInstance, Rule[x_,y_] :> Rule[x , Mean[y[t + stepSize*countDiscreteGates[t]]]]]]
			]
			(* The following injection almost works for compilation of the above--I'm not even sure if it would be faster*)
			(*With[{vars = presentStochSymbols, M=mat/.staticNoiseInstance, lookup=Transpose@Values@stochNoiseInstance[[Key/@presentStochSymbols]]},
				Compile[{{t,_Real}},
					Module[vars, Module[{idx,data},
						data = lookup[[idx]];
						vars = data;
						M
					]]
				]
			]*)
		]
	];
	generator = If[Generator[nm]===Automatic, 
		ConstantArray[0, {First@controlDims, First@controlDims}], 
		Generator[nm]
	];
	If[LindbladQ[generator],
		totalDim = Length@First@generator;
		generator = makeTimeDep/@generator,
		totalDim = Length@generator;
		generator = makeTimeDep[generator];
	];
	
	(* Finish up pulses *)
	Hcontrol = Hcontrol /. staticNoiseInstance;
	Hcontrol = (# \[CircleTimes] IdentityMatrix[totalDim / Length@#])& /@ Hcontrol;
	pulses = {#, Hcontrol}& /@ pulses;
	
	(* Output *)
	CompiledSequence[
		Generator->generator, 
		PulseSequence->If[hasGateNoise, Riffle[pulses, gateNoiseChannels], pulses], 
		StepSize->stepSize,
		GateSequence->sequence,
		UndistortedLongPulse->undistortedLongPulse,
		GateSet->gs,
		NoiseModel->nm
	]
]


(* ::Subsubsection::Closed:: *)
(*CompileGateNoise*)


GateNoiseProtocol[depth_,gateIndices_,repetitions_,hasGateNoise_] := Protocol[
	ProtocolName -> "Protocol for finding gate noise description",
	SequenceLengths -> Range[depth],
	ExperimentTypes -> Function[M, Tuples[gateIndices, M]], (* here, M is the depth *)
	NumRepetitions -> (repetitions&),
	SequenceGenerator ->  (#2[[2]]&), (* this just returns the experiment type *)
	SimulationOptions -> {SimulationOutput->Superoperators,PulseSubset->Span[If[hasGateNoise,-2,-1],All]}, (*only simulate the last gate *)
	SimulationParser -> (Last@Superoperators[#]&),
	ParallelOptions -> {Method->"CoarsestGrained"},
	TotalGates -> Sum[d * (Length[gateIndices]^d), {d,depth}]
]


Options[CompileGateNoise]={
	NumRepetitions->Automatic,
	GateIndices->All
};


CompileGateNoise[gs_GateSet,nm_NoiseModel,depth_,OptionsPattern[]]:=Module[{gateChannel,gateNoise,protocol,gateIdxs,numIdxs,results,numReps},
	gateIdxs = If[OptionValue[GateIndices]===All, Range[Size[gs]], OptionValue[GateIndices]];
	numIdxs = Length[gateIdxs];
	
	numReps = If[OptionValue[NumRepetitions] === Automatic,
		If[FreeQ[nm,StochasticNoise] && FreeQ[nm,StaticNoise], 1, 100],
		OptionValue[NumRepetitions]
	];
	
	(*Run the protocol which will do all of the heavy lifting*)
	protocol = GateNoiseProtocol[depth, gateIdxs, numReps, GateNoise[nm]=!=None];
	results = SimulateProtocol[gs, nm, protocol, AllowFinalRingdown->False];
	
	(* now go through the results and average over repetitions *)
	results = Table[
		Map[Super, Total[results[[d,All,1]],{2}] / numReps],
		{d, depth}
	];
	gateChannel[idxs___]:=Module[{i={idxs}, d},
		d=Length[i];
		If[d>depth, 
			gateChannel[Sequence@@(i[[-depth;;]])], 
			i = Total[(i - 1) * Table[numIdxs^n, {n,d-1,0,-1}]] + 1;
			results[[d,i]]
		]
	];
	gateNoise[idxs___]:= gateChannel[idxs] . Unitary[GateUnitary[gs][Last[{idxs}]]\[ConjugateTranspose]];
	Join[
		DeleteCases[GateNoiseGateSet@@gs, (GatePulse->_)],
		GateNoiseGateSet[
			GateChannel -> gateChannel,
			GateNoise -> gateNoise
		]
	]
]


(* ::Subsection::Closed:: *)
(*Simulator *)


InheritOptions[SimulateSequence, {PulseSim}, {PulseSubset->All}];
SimulateSequence[seq_CompiledSequence, opt:OptionsPattern[]]:=Module[{},
	PulseSim[
		Generator[seq],
		PulseSequence[seq][[OptionValue[PulseSubset]]],
		FilterOptions[PulseSim, opt],
		StepSize->StepSize[seq]
	]
]


ExportSimulation[fileName_,gs_,protocol_,data_] := (
	Export[
		fileName<>".h5",
		{
			"Datasets"-> {
			"SurvivalData"->data, 
			"ProtocolName"->ProtocolName[protocol],
			"SequenceLengths"-> SequenceLengths[protocol], 
			"ExperimentTypes"->(ExperimentTypes[protocol]/@SequenceLengths[protocol]),
			"Size"-> {Size[gs]},
			 "Dimension"->{ Dimension[gs]},
			"GateSetName"->GateSetName[gs]
		},
		"DataFormat"->Automatic
		},
		"Rules"
	];
	Print["Saved "<>fileName<>".h5"];
)


InheritOptions[SimulateProtocol, {CompileSequence}, {SimulationExportName->None}];


(* ::Subsubsection::Closed:: *)
(*General Simulator*)


SimulateProtocol[gs_GateSet, nm_NoiseModel, protocol_Protocol, opt:OptionsPattern[]] := Module[
	{
		simSeq, getSeq, 
		j=0.0, doParallel,
		seqLengths, experiments, totalGates,
		data
	},

	doParallel = $KernelCount > 0;
	seqLengths = SequenceLengths[protocol];
	experiments = Map[
		Function[M, 
			Map[{M, #, NumSequenceDraws[protocol][#]}&, ExperimentTypes[protocol][M]]
		], 
		seqLengths
	];
	
	If[TotalGates[protocol]===Automatic,
		totalGates = Total@Flatten@Apply[(#1 * NumRepetitions[protocol][##])&, experiments, {-2}];,
		totalGates = TotalGates[protocol];
	];
	
	getSeq[seqLength_,e_,i_] := SequenceGenerator[protocol][gs, {seqLength,e,i}];
	simSeq[seq_] := SimulationParser[protocol][
		SimulateSequence[
			CompileSequence[gs, seq, nm, opt], 
			SimulationOptions[protocol]
		]
	];
	If[doParallel,
		DistributeDefinitions[simSeq, getSeq];
		SetSharedVariable[j];
		data=Monitor[
			Apply[
				ParallelTable[
					With[{seq = getSeq[#1,#2,i]}, 
						j += Length[seq]; 
						Table[simSeq[seq], {n, NumRepetitions[protocol][#1,#2,i]}]
					],
					{i,#3},
					ParallelOptions[protocol]
				]&, 
				experiments,
				{2}
			],
			With[{x=j / totalGates},Row[{ProgressIndicator[x], x}]]
		];,
		data=Monitor[
			Apply[
				Table[
					With[{seq = getSeq[#1,#2,i]}, 
						j += Length[seq]; 
						Table[simSeq[seq], {n, NumRepetitions[protocol][#1,#2,i]}]
					],
					{i,#3}
				]&, 
				experiments,
				{2}
			];,
			With[{x=j / totalGates},Row[{ProgressIndicator[x], x}]]
		]
	];
	If[OptionValue[SimulationExportName]=!=None,
		ExportSimulation[OptionValue[SimulationExportName],gs,protocol,data]
	];
	data
]


(* ::Subsubsection::Closed:: *)
(*GateNoiseGateSet Simulator*)


SimulateProtocol[gs_GateNoiseGateSet, protocol_Protocol,OptionsPattern[]] := Module[
	{
		getSeq, 
		j=0.0, doParallel,
		seqLengths, experiments, totalGates,
		data
	},

	doParallel = $KernelCount > 0;
	seqLengths = SequenceLengths[protocol];
	experiments = Map[
		Function[M, 
			Map[{M, #, NumSequenceDraws[protocol][#]}&, ExperimentTypes[protocol][M]]
		], 
		seqLengths
	];
	
	If[TotalGates[protocol]===Automatic,
		totalGates = Total@Flatten@Apply[(#1 * NumRepetitions[protocol][##])&, experiments, {-2}];,
		totalGates = TotalGates[protocol];
	];
	
	getSeq[seqLength_,e_,i_] := SequenceGenerator[protocol][gs, {seqLength,e,i}];
	If[doParallel,
		DistributeDefinitions[getSeq];
		SetSharedVariable[j];
		data=Monitor[
			Apply[
				ParallelTable[
					With[{seq = getSeq[#1,#2,i]}, 
						j += Length[seq]; 
						Table[GateSimulator[protocol][gs,seq], {n, NumRepetitions[protocol][#1,#2,i]}]
					],
					{i,#3},
					ParallelOptions[protocol]
				]&, 
				experiments,
				{2}
			],
			With[{x=j / totalGates},Row[{ProgressIndicator[x], x}]]
		];,
		data=Monitor[
			Apply[
				Table[
					With[{seq = getSeq[#1,#2,i]}, 
						j += Length[seq]; 
						Table[GateSimulator[protocol][gs,seq], {n, NumRepetitions[protocol][#1,#2,i]}]
					],
					{i,#3}
				]&, 
				experiments,
				{2}
			],
			With[{x=j / totalGates},Row[{ProgressIndicator[x], x}]]
		];
	];
	If[OptionValue[SimulationExportName]=!=None,
		ExportSimulation[OptionValue[SimulationExportName],gs,protocol,data]
	];
	data
]


(* ::Subsection::Closed:: *)
(*Visualization*)


PlotSequence[seq_CompiledSequence, opt:OptionsPattern[PulsePlot]]:=Module[{pulses, longPulse, distortion, showDistorted},
	distortion = DistortionOperator@NoiseModel[seq];
	showDistorted = distortion=!=None;
	longPulse = UndistortedLongPulse[seq];
	PulsePlot[
		ToPulse[longPulse, DistortionOperator->distortion],
		opt,
		ShowDistortedPulse -> showDistorted
	]
]


(* ::Subsection::Closed:: *)
(*Protocols*)


makeContainer[Protocol, {
	ProtocolName->"Unnamed protocol",
	SequenceLengths,
	ExperimentTypes,
	NumSequenceDraws->(1&),
	NumRepetitions->(1&),
	SequenceGenerator,
	SimulationOptions,
	SimulationParser,
	GateSimulator->Function[{gs,idxs},Message[GateSimulator::NotImplemented];$Failed],
	ParallelOptions -> {},
	TotalGates -> Automatic
}];
Protocol/:Format[Protocol[args___]] := "Protocol"[ProtocolName[Protocol[args]]]


(* ::Subsection::Closed:: *)
(*Example Protocols*)


RBDraw[gs:(_GateSet|_GateNoiseGateSet),seqLength_]:=Module[{seq},
	seq = RandomInteger[{1, Size[gs]}, seqLength];
	Append[seq, GateInverse[gs][Fold[GateProduct[gs], seq]]]
]


RBProtocol[seqLengths_,numSeqs_,shotsPerSeq_,\[Rho]_,M_]:=Module[{seqGen,gateSim},
	(* Use two closures to make sure every shot of the same (seqLength,seqNum) is identical *)
	seqGen[gs_,{seqLength_,0,seqNum_}] := RBDraw[gs, seqLength];
	gateSim[gs_GateNoiseGateSet,{idxs__}] := Re@Tr[
		M.Last[Fold[With[{l=Append[First@#1,#2]}, {l, (GateChannel[gs]@@l)[Last@#1]}]&, {{},\[Rho]}, {idxs}]]
	];
	Protocol[
		ProtocolName -> "Randomized Benchmarking",
		SequenceLengths -> seqLengths,
		ExperimentTypes -> ({0}&),
		NumSequenceDraws -> (numSeqs&),
		NumRepetitions -> (shotsPerSeq&),
		SequenceGenerator -> seqGen,
		SimulationOptions -> {InitialState->\[Rho], Observables->{M}},
		SimulationParser -> (Last@First@Observables[#]&),
		GateSimulator -> gateSim,
		ParallelOptions -> {Method->"CoarsestGrained"},
		TotalGates -> (shotsPerSeq * numSeqs * Total[seqLengths+1])
	]
]


(* ::Subsection::Closed:: *)
(*Example Gate Noise*)


IndependentNoise[channel_QuantumChannel][___]:=channel


(* ::Subsection::Closed:: *)
(*Example Gate Sets*)


(*Thus function does xyx euler decompostion to make any unitary out of three gates on the xy plane. *)
xyxQubitPulse[a_,{x_,y_,z_}]:=Module[{pulses,eulerAngles},
	eulerAngles = Reverse@EulerAngles[RotationMatrix[a,{x,y,z}],{1,2,1}];
	pulses=GaussianTailsPulse[0.002,0.019,0.004,Area->#/(2\[Pi])] &/@ eulerAngles;
	{Join[pulses[[1]],{#1,#3,#2}&@@@(pulses[[2]]), pulses[[3]]], 2\[Pi] {TP[X],TP[Y]}/2}
];


$gqgProductTable = Transpose@{{1,2,3,4,5,6,7,8,9,10,11,12},{2,4,5,1,7,8,3,11,12,9,6,10},{3,6,1,9,10,2,8,7,4,5,12,11},{4,1,7,2,3,11,5,6,10,12,8,9},{5,8,2,12,9,4,11,3,1,7,10,6},{6,9,10,3,8,7,1,12,11,4,2,5},{7,11,4,10,12,1,6,5,2,3,9,8},{8,12,9,5,11,3,2,10,6,1,4,7},{9,3,8,6,1,12,10,2,5,11,7,4},{10,7,6,11,4,9,12,1,3,8,5,2},{11,10,12,7,6,5,4,9,8,2,1,3},{12,5,11,8,2,10,9,4,7,6,3,1}};
$gqgInverseTable = {1,4,3,2,9,7,6,10,5,8,11,12};
$gqgDecompositions = {
	{0,{1,0,0}},
	{(2 \[Pi])/3,{-(1/Sqrt[3]),1/Sqrt[3],-(1/Sqrt[3])}},
	{\[Pi],{0,0,1}},
	{(2 \[Pi])/3,{1/Sqrt[3],-(1/Sqrt[3]),1/Sqrt[3]}},
	{(2 \[Pi])/3,{1/Sqrt[3],1/Sqrt[3],1/Sqrt[3]}},
	{(2 \[Pi])/3,{-(1/Sqrt[3]),-(1/Sqrt[3]),1/Sqrt[3]}},
	{(2 \[Pi])/3,{1/Sqrt[3],1/Sqrt[3],-(1/Sqrt[3])}},
	{(2 \[Pi])/3,{-(1/Sqrt[3]),1/Sqrt[3],1/Sqrt[3]}},
	{(2 \[Pi])/3,{-(1/Sqrt[3]),-(1/Sqrt[3]),-(1/Sqrt[3])}},
	{(2 \[Pi])/3,{1/Sqrt[3],-(1/Sqrt[3]),-(1/Sqrt[3])}},
	{\[Pi],{0,1,0}},
	{\[Pi],{-1,0,0}}};
$gqgUnitaries = MatrixExp[-I (#1/2)(#2[[1]] TP[X]+#2[[2]] TP[Y]+#2[[3]] TP[Z])]& @@@ $gqgDecompositions;
$gqgPulses = xyxQubitPulse @@@ $gqgDecompositions;
With[{
	productTable = $gqgProductTable,
	inverseTable = $gqgInverseTable,
	unitaries = $gqgUnitaries,
	pulses = $gqgPulses
},
	$gaussianQubitGateSet = GateSet[
		GateSetName->"Gaussian 2-design on Qubits",
		Dimension->2,
		Size->12,
		GateProduct->(productTable[[#1,#2]]&),
		GateInverse->(inverseTable[[#]]&),
		GateUnitary->(unitaries[[#]]&),
		GatePulse->(pulses[[#]]&)
	];
]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


Protect[
	GateSet, GateNoiseGateSet, 
	GateSetName, Size, Dimension,
	GateProduct, GateInverse,
	GateUnitary, GatePulse, GateChannel,
	FramePotential, TestGateSetPulses, TestGateSetInverses, TestGateSetProducts,
	GateIndices, TakeOverlap
];
Protect[$gaussianQubitGateSet];


Protect[
	NoiseModel,
	GateNoise, DistortionMultiplier, StochasticNoise, StaticNoise, Generator
];


Protect[
	CompileSequence,
	CompiledSequence, PulseSequence, UndistortedLongPulse, GateSequence,
	CompileGateNoise, GateNoiseProtocol, AllowFinalRingdown
];


Protect[
	SimulateSequence, SimulateProtocol, PulseSubset, 
	ExportSimulation, SimulationExportName
];


Protect[
	Protocol, ProtocolName, SimulationOptions, SimulationParser, GateSimulator,
	SequenceLengths, ExperimentTypes, NumSequenceDraws, NumRepetitions, 
	SequenceGenerator,  ParallelOptions, TotalGates,
	RBDraw, RBProtocol
];
Protect[
	RBDraw, RBProtocol
];


Unprotect[PlotSequence];


EndPackage[];
