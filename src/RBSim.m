(* ::Package:: *)

(* ::Chapter:: *)
(*RBSim*)


(* ::Subsection:: *)
(*Preamble*)


BeginPackage["RBSim`",{"QSim`","Tensor`","GRAPE`","QuantumChannel`"}];


(* ::Section:: *)
(*Usage Declarations*)


(* ::Subsection:: *)
(*Gate Sets*)


GateSet::usage = "GateSet[name, size, dimension, gateProductFunction, gateInverseFunction, idealUnitaryFunction, gatePulseFunction] stores the necessary info of a gateset. The arguments to the functions should be indeces between 1 and Size[gateset]. gateProductFunction obviously needs two indeces.";
GateSetName::usage = "GaseSetName[gs_GateSet] returns a string with some unimportant human readable string describing the gate set gs";
Size::usage = "Size[gs_GateSet] returns the number of gates in the gateset";
Dimension::usage = "Dimension[gs_GateSet] returns the Hilbert space dimension of the gates";
GateProduct::usage = "GateProduct[gs_GateSet] returns the function f[i,j] that multiplies i and j together, returning the resulting index.";
GateInverse::usage = "GateInverse[gs_GateSet] returns the function f[i] that returns the index of the inverse of the i'th gate";
GateUnitary::usage = "GateUnitary[gs_GateSet] returns the function f[i] that returns the ideal unitary matrix corresponding to the i'th gate of the gateset.";
GatePulse::usage = "GatePulse[gs_GateSet] returns the function f[i] that returns the pulse corresponding to the given gate in a format accepted by PulseSim.";


FramePotential::usage = "FramePotential[gs_GateSet, t] returns the t-frame potential of the given gateset.";


TestGateSetPulses::usage = "TestGateSetPulses[gateSet] simulates the pulses (with no drift hamiltonian) and compares the result with the ideal unitary. By default, a list of overlaps is returned.";
TestGateSetInverses::usage = "TestGateSetInverses[gateSet] uses the gateSet inverse function to multiply gates with their supposed inverses. By default, a list of overlaps with the identity matrix is returned.";
TestGateSetProducts::usage = "TestGateSetProducts[gateSet] uses the gateSet product function to multply  gates together. By default, a list of overlaps with product as calculatedi with matrix multiplication is returned.";
GateIndeces::usage = "GateIndeces is an option for TestGateSetPulses, TestGateSetInverses, and TestGateSetProducts specifying which indeces or pairs of indeces to test. Default is All.";
TakeOverlap::usage = "GateIndeces is a boolean option for TestGateSetPulses, TestGateSetInverses, and TestGateSetProducts specifying whether to return the matrices themsevlves, or just the relevant overlap.";


(* ::Subsection:: *)
(*Noise Models*)


NoiseModel::usage = "NoiseModel is a container for information detailing the noise model of a given quantum system.";


GateNoise::usage = "GateNoise is a NoiseModel Property which specifies a function f[i1,i2,...,in] that returns a ChannelPulseQ given the gate index history i1,i2,...,in where in is the most recent";
DistortionMultiplier::usage = "DistortionMultiplier is a NoiseModel Property which is a number specifying how many output pulses there are per input pulse. In particular, the end of the i'th pulse is calculation as this number times the total number of input time steps until this point.";
StochasticNoise::usage = "StochasticNoise is a NoiseModel Property which is a list of the form, for example, {\[Gamma]\[Distributed]WeinerProcess[0,0.1],\[Delta]\[Distributed]OrnsteinUhlenbeckProcess[1,2,3]}.";
StaticNoise::usage = "StaticNoise is a NoiseModel Property which is a list of the form, for example, {{x,y}\[Distributed]MultinormalDistribution[{0,1},{{1,0},{0.5,1}}], \[Xi]\[Distributed]BetaDistribution[1,2]}.";
Generator::usage = "Generator is a NoiseModel Property which specifies a hamiltonian matrix or LindbladForm of the system.";


(* ::Subsection::Closed:: *)
(*Compiler*)


CompileSequence::usage = "CompileSequence[gateSet, sequence, gateNoise, generator, stochasticNoise, distortion] outputs valid arguments to PulseSim given the GateSet, list of indeces sequence, and noise models.";
PulseSequence::usage = "PulseSequence refers to the compiled pulses sequence and gates produced by CompileSequence.";
UndistortedLongPulse::usage = "UndistortedLongPulse refers to the catenated pulse sequence  before the DistortionOperator (if any) is applied.";
GateSequence::usage = "GateSequence refers to a list of integers corresponding to gates from a given GateSet.";
CompiledSequence::usage = "CompiledSequence is a storage container for the output of CompileSequence";


(* ::Subsection:: *)
(*Simulator*)


SimulateSequence::usage = "SimulateSequence[seq_CompiledSequence, \[Rho], OptionsPattern[PulseSim]] simulates the given the seq generated by CompileSequence, the initial state \[Rho], and the PulseSim simulation options";
SimulateProtocol::usage = "SimulateProtocol[protocol_Protocol]";


(* ::Subsection:: *)
(*Visualization*)


PlotSequence::usage = "PlotSequence[seq_CompiledSequence] plots the pulse sequence inside some output of CompileSequence."


(* ::Subsection:: *)
(*Protocols*)


Protocol::usage = "";
ProtocolName::usage = "ProtocolName is a Protocol property specifying a human readable name for the protocol.";
SimulationOptions::usage = "SimulationOptions is a Protocol property specifying a list of options to pass to the simulator, such as InitialState.";
DataDimensions::usage = "DataDimensions is a Protocol property specifying the output shape of the array containing data from the protocol.";
SequenceGenerator::usage = "SequenceGenerator is a Protocol property specifying a function seqGen[gs,{idx1,idx2,..}] that takes a GateSet and indeces within DataDimensions[protocol] and returns the corresponding gate sequence indeces according to the protocol.";
SimulationParser::usage = "SimulationParser is a Protocol property specifying a function which acts on the output of PulseSim to return the quantity of interest.";
SequenceLengths::usage = "SequenceLengths is a Protocol property for some protocols specifying which sequence lengths to use.";


RBDraw::usage = "RBDraw[gateSet, seqLength] returns a list of gate indeces of length seqLength+1, as specified by the Randomized Benchmarking protocol.";
RBProtocol::usage = "";


(* ::Subsection:: *)
(*Example Gate Sets*)


$gaussianQubitGateset::usage = "Example gateset for qubits with gaussian pulse shapes.";


(* ::Subsection:: *)
(*Example Gate Noise*)


IndependentNoise::usage = "IndependentNoise[channel]";


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


makeContainer[head_,{validProperties__}] := (
	UpValues[head] = Table[
		If[
		Head[property]===Rule,
			With[{h=head,p=property[[1]],v=property[[2]]}, p[h[args___]]:>getValue[h[args], p, v]],
			With[{h=head,p=property}, p[h[args___]]:>getValue[h[args], p]]
		], 
		{property,{validProperties}}
	]
)
(*Table[
	head/:property[head[args__]] := getValue[head[args], property];,
	{property, {validProperties}}
]*)


(* ::Subsection:: *)
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


FramePotential[gs_GateSet, t_]:=Sum[Abs[Tr[GateUnitary[gs][k]]]^(2 t),{k,Size[gs]}]/Size[gs]


Options[TestGateSetPulses] = {
	GateIndeces->All,
	TakeOverlap->True
};
TestGateSetPulses[gs_, OptionsPattern[]] := Module[{Uideal, Upulse, idxs, f, i},
	Upulse[i_]:=With[{pulse=GatePulse[gs][i]},Last@Unitaries@PulseSim[0*pulse[[2,1]],pulse]];
	Uideal[i_]:=GateUnitary[gs][i];
	If[OptionValue@TakeOverlap,
		f[{W_,V_}]:=Abs[Tr[W\[ConjugateTranspose].V]/Length[W]]^2,
		f=Identity;
	];
	idxs = If[OptionValue[GateIndeces]===All, Range[Size[gs]], OptionValue[GateIndeces]];
	Table[
		f[{Uideal[i], Upulse[i]}],
		{i,idxs}
	]
]


Options[TestGateSetInverses] = {
	GateIndeces->All,
	TakeOverlap->True
};
TestGateSetInverses[gs_, OptionsPattern[]] := Module[{U, idxs, f},
	U[i_]:=GateUnitary[gs][i];
	If[OptionValue@TakeOverlap,
		f[W_]:=Abs[Tr[W]/Length[W]]^2,
		f=Identity;
	];
	idxs = If[OptionValue[GateIndeces]===All, Range[Size[gs]], OptionValue[GateIndeces]];
	Table[
		f[U[i].U[GateInverse[gs][i]]],
		{i,idxs}
	]
]


Options[TestGateSetProducts] = {
	GateIndeces->All,
	TakeOverlap->True
};
TestGateSetProducts[gs_, OptionsPattern[]] := Module[{U, idxs, f},
	U[i_]:=GateUnitary[gs][i];
	If[OptionValue@TakeOverlap,
		f[{W_,V_}]:=Abs[Tr[W\[ConjugateTranspose].V]/Length[W]]^2,
		f=Identity;
	];
	idxs = If[OptionValue[GateIndeces]===All, 
		Outer[List, Range[Size[gs]], Range[Size[gs]]], 
		OptionValue[GateIndeces]
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


(* ::Subsection:: *)
(*Compiler*)


CompileSequence[gs_GateSet,sequence_,nm_NoiseModel]:=Module[
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
	If[Length@longPulse > Total[pulseLengths] * distMult,
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


makeContainer[CompiledSequence, {
	Generator,
	PulseSequence,
	StepSize,
	GateSequence,
	UndistortedLongPulse,
	GateSet,
	NoiseModel
}];


(* ::Subsection:: *)
(*Simulator*)


SimulateSequence[seq_CompiledSequence, opt:OptionsPattern[PulseSim]]:=Module[{},
	PulseSim[
		Generator[seq],
		PulseSequence[seq],
		opt,
		StepSize->StepSize[seq]
	]
]


SimulateProtocol[gs_GateSet, protocol_Protocol, nm_NoiseModel] := Module[{simSeq, j=0},
	simSeq[idxs__] := SimulationParser[protocol][
		SimulateSequence[
			CompileSequence[gs, SequenceGenerator[protocol][gs, {idxs}], nm], 
			SimulationOptions[protocol]
		]
	];
	If[$KernelCount > 0,
		DistributeDefinitions[simSeq];
		SetSharedVariable[j];
		Monitor[
			ParallelArray[(j++;simSeq[##])&, DataDimensions[protocol]],
			ProgressIndicator[j / Times@@DataDimensions[protocol]]
		],
		Monitor[
			Array[(j++;simSeq[##])&, DataDimensions[protocol]],
			ProgressIndicator[j / Times@@DataDimensions[protocol]]
		]
	]
]


(* ::Subsection:: *)
(*Visualization*)


PlotSequence[seq_CompiledSequence, opt:OptionsPattern[PulsePlot]]:=Module[{pulses, longPulse, distortion, showDistorted},
	distortion = DistortionOperator@NoiseModel[seq];
	showDistorted = distortion=!=None;
	longPulse = UndistortedLongPulse[seq];
	PulsePlot[
		ToPulse[longPulse, DistortionOperator->distortion],
		ShowDistortedPulse -> showDistorted
	]
]


(* ::Subsection:: *)
(*Protocols*)


makeContainer[Protocol, {
	ProtocolName->"Unnamed protocol",
	SequenceLengths,
	DataDimensions,
	SequenceGenerator,
	SimulationOptions,
	SimulationParser
}];
Protocol/:Format[Protocol[args___]] := "Protocol"[ProtocolName[Protocol[args]]]


RBDraw[gs_GateSet,seqLength_]:=Module[{seq},
	seq = RandomInteger[{1, Size[gs]}, seqLength];
	Append[seq, GateInverse[gs][Fold[GateProduct[gs], seq]]]
]


RBProtocol[seqLengths_,numSeqs_,shotsPerSeq_,\[Rho]_,M_]:=Module[{seqGen},
	(* Use two closures to make sure every shot of the same (seqLength,seqNum) is identical *)
	seqGen[gs_,{idxSeqLength_,seqNum_}] := seqGen[gs,{idxSeqLength,seqNum}] = RBDraw[gs, seqLengths[[idxSeqLength]]];
	seqGen[gs_,{idxSeqLength_,seqNum_,_}] := seqGen[gs,{idxSeqLength,seqNum}];
	Protocol[
		ProtocolName -> "Randomized Benchmarking",
		SequenceLengths -> seqLengths,
		DataDimensions -> {Length@seqLengths, numSeqs, shotsPerSeq},
		SequenceGenerator -> seqGen,
		SimulationOptions -> {InitialState->\[Rho], Observables->{M}},
		SimulationParser -> (Last@First@Observables[#]&)
	]
]


(* ::Subsection:: *)
(*Example Gate Noise*)


IndependentNoise[channel_QuantumChannel][___]:=channel


(* ::Subsection:: *)
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
	$gaussianQubitGateset = GateSet[
		GateSetName->"Gaussian 2-design on Qubits",
		Dimension->2,
		Size->12,
		GateProduct->(productTable[[#1,#2]]&),
		GateInverse->(inverseTable[[#]]&),
		GateUnitary->(unitaries[[#]]&),
		GatePulse->(pulses[[#]]&)
	];
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End Package*)


EndPackage[];
