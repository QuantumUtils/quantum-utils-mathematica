(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Quantum Channel*)


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


BeginPackage["QuantumChannel`",{"QUDoc`","Tensor`","Predicates`"}];


Needs["QUDevTools`"]
Needs["QuantumSystems`"]


$QuantumChannelUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "QuantumChannel.nb"}]];


(* ::Section:: *)
(*Usage Declarations*)


(* ::Subsection::Closed:: *)
(*Quantum Channels*)


Unprotect[QuantumChannel,ChannelRep,InputDim,OutputDim,Basis];
Unprotect[Choi,Super,Chi,Kraus,Stinespring,Unitary,SysEnv];


AssignUsage[QuantumChannel,$QuantumChannelUsages];
AssignUsage[Unitary,$QuantumChannelUsages];
AssignUsage[Super,$QuantumChannelUsages];
AssignUsage[Choi,$QuantumChannelUsages];
AssignUsage[Chi,$QuantumChannelUsages];
AssignUsage[Kraus,$QuantumChannelUsages];
AssignUsage[Stinespring,$QuantumChannelUsages];
AssignUsage[SysEnv,$QuantumChannelUsages];


AssignUsage[InputDim,$QuantumChannelUsages];
AssignUsage[OutputDim,$QuantumChannelUsages];
AssignUsage[Basis,$QuantumChannelUsages];
AssignUsage[ChannelRep,$QuantumChannelUsages];
AssignUsage[ChannelParameters,$QuantumChannelUsages];


(* ::Subsection::Closed:: *)
(*Channel Functions and Metrics*)


Unprotect[GateFidelity,AverageGateFidelity,EntanglementFidelity,ChannelVolume,Unitarity,DiamondNormDistance];


AssignUsage[ProcessFidelity,$QuantumChannelUsages];
AssignUsage[GateFidelity,$QuantumChannelUsages];
AssignUsage[AverageGateFidelity,$QuantumChannelUsages];
AssignUsage[EntanglementFidelity,$QuantumChannelUsages];
AssignUsage[DiamondNormDistance,$QuantumChannelUsages];
AssignUsage[ChannelVolume,$QuantumChannelUsages];
AssignUsage[Unitarity,$QuantumChannelUsages];


(* ::Subsection::Closed:: *)
(*Predicates*)


Unprotect[CompletelyPositiveQ,TracePreservingQ,HermitianPreservingQ,UnitaryQ,UnitalQ,PauliChannelQ];


AssignUsage[CompletelyPositiveQ,$QuantumChannelUsages];
AssignUsage[TracePreservingQ,$QuantumChannelUsages];
AssignUsage[HermitianPreservingQ,$QuantumChannelUsages];
AssignUsage[UnitaryQ,$QuantumChannelUsages];
AssignUsage[UnitalQ,$QuantumChannelUsages];
AssignUsage[PauliChannelQ,$QuantumChannelUsages];


(* ::Subsection::Closed:: *)
(*Special Channels*)


Unprotect[ComChannel,AComChannel,Lindblad,LindbladDissipator,PartialTrChannel,FunctionChannel];


AssignUsage[ComChannel,$QuantumChannelUsages];
AssignUsage[AComChannel,$QuantumChannelUsages];
AssignUsage[LindbladDissipator,$QuantumChannelUsages];
AssignUsage[Lindblad,$QuantumChannelUsages];
AssignUsage[PartialTrChannel,$QuantumChannelUsages];
AssignUsage[FunctionChannel,$QuantumChannelUsages];


(* ::Subsection::Closed:: *)
(*Error Messages*)


CompletelyPositiveQ::assum = "Cannot deduce nonnegativity of eigenvalues `1` with given assumptions."


QuantumChannel::dims = "Input channel matrix needs specification of input and output dimensions.";
QuantumChannel::state = "Input must be a density matrix or pure state vector.";
QuantumChannel::plus = "QuantumChannels must be of same dimensions.";
QuantumChannel::dot = "QuantumChannel input and output dimensions are not compatible.";
QuantumChannel::tr = "Channel must be in Choi, Super or Unitary representation.";


Kraus::invalid = "Invalid set of Kraus operators.";


ProcessFidelity::dims = "Input and output dimensions of QuantumChannel must be equal.";
AverageGateFidelity::dims = "Input and output dimensions of QuantumChannel must be equal.";


ComChannel::chan = "QuantumChannel input is only valid for Unitary QuantumChannels.";
AComChannel::chan = "QuantumChannel input is only valid for Unitary QuantumChannels.";


LindbladDissipator::input = "Input must be a matrix, list of matrices, or sequence of matrices.";
Lindblad::input = "Input must be a matrix, list of matrices, or sequence of matrices.";


FunctionChannel::indims = "InputDims option must be an integer.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Predicates*)


(* ::Subsubsection::Closed:: *)
(*Transformation Predicates*)


(* ::Text:: *)
(*These predicates are used internally for the various channel transformation functions*)


KrausPairQ[set_]:=And[First[Dimensions[set]]===2,AllQ[KrausSingleQ,set]]
KrausSingleQ[set_]:=AllQ[MatrixQ,set]
KrausQ[set_]:=Or[KrausPairQ[set],KrausSingleQ[set]];
KrausUnitaryQ[set_]:=And[KrausSingleQ[set],Length[set]===1,UnitaryMatrixQ[First@set,Tolerance->10^(-12)]]


StinespringPairQ[set_]:=And[Length[set]==2,AllQ[MatrixQ,set]];
StinespringQ[set_]:=Or[MatrixQ[set],StinespringPairQ[set]]
StinespringUnitaryQ[set_,outDim_]:=And[MatrixQ[set],outDim===First[Dimensions[set]]]


SysEnvPairQ[set_]:=And[
		ListQ[set],
		Length[set]===2,
		Length[First[set]]==2,
		AllQ[MatrixQ,First[set]],
		VectorQ[Last[set]]];
SysEnvSingleQ[set_]:=And[
		ListQ[set],
		Length[set]===2,
		MatrixQ[First[set]],
		VectorQ[Last[set]]]
SysEnvQ[set_]:=Or[SysEnvSingleQ[set],SysEnvPairQ[set]];
SysEnvUnitaryQ[set_]:=And[SysEnvSingleQ[set],MemberQ[{{1},{1,1}},Dimensions@Last[set]]]


ChoiUnitaryQ[choi_]:=SameQ[1,Length[Select[Eigenvalues[choi,2],PossiblyNonzeroQ]]]


UnitaryChannelQ[chan_QuantumChannel]:=
	With[{rep=ChannelRep[chan],op=First[chan]},
	Which[
		rep===Unitary,True,
		rep===Kraus,KrausUnitaryQ[op],
		rep===SysEnv,SysEnvUnitaryQ[op],
		rep===Stinespring,StinespringUnitaryQ[op,OutputDim[chan]],
		rep===Choi,ChoiUnitaryQ[op],
		rep===Super,ChoiUnitaryQ[First@Choi[chan]],
		True,False
	]]


(* ::Subsubsection::Closed:: *)
(*Channel Property Predicates*)


Options[CompletelyPositiveQ]:={Assumptions->{}}


CompletelyPositiveQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	Block[{
		choi=First[Choi[chan]],
		assum=And[$Assumptions,OptionValue[Assumptions]],
		eigen,boole},
	If[AllQ[NumericQ,Flatten[choi]],	
		PositiveSemidefiniteMatrixQ[choi,Tolerance->10^(-12)],
		eigen=Simplify[Eigenvalues[choi],Assumptions->assum];
		boole=FullSimplify[And@@NonNegative[eigen],Assumptions->assum];
		Which[
			TrueQ[boole],True,
			TrueQ[Not@boole],False,
			True,Message[CompletelyPositiveQ::assum, Select[eigen,Not@NumericQ[#]&]]
		]
	]];


HermitianPreservingQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	With[{
		choi=First[Choi[chan]],
		assum=And[$Assumptions,OptionValue[Assumptions]]},		
		Assuming[assum,
			HermitianMatrixQ[FullSimplify[choi,Assumptions->assum]]
		]
	];


TracePreservingQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	With[{
		op=PartialTr[First[Choi[chan,Basis->"Col"]],{InputDim[chan],OutputDim[chan]},{2}],
		assum=And[$Assumptions,OptionValue[Assumptions]],
		id=IdentityMatrix[InputDim[chan]]
	},
		AllQ[FullSimplify[#*#\[Conjugate],Assumptions->assum]<10.^-12&,
			FullSimplify[Flatten[op-id],Assumptions->assum]
		]		
	]



UnitaryQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=Or[
	(ChannelRep/.ChannelParameters[chan])===Unitary,
	And[
		TracePreservingQ[chan,Assumptions->OptionValue[Assumptions]],
		Module[{unitarity=FullSimplify[Unitarity[chan],And[$Assumptions,OptionValue[Assumptions]]]},
			If[NumericQ[unitarity],
				Abs[unitarity-1]<10^-12,
				TrueQ[unitarity==1]
			]
		],
		CompletelyPositiveQ[chan,Assumptions->OptionValue[Assumptions]]
	]
];


UnitalQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	With[{
	op=PartialTr[
			First[Choi[chan,Basis->"Col"]],
			{InputDim[chan],OutputDim[chan]},{1}],
	assum=And[$Assumptions,OptionValue[Assumptions]],
	id=IdentityMatrix[OutputDim[chan]]},
		AllMatchQ[0,
			FullSimplify[Flatten[op-id],Assumptions->assum]
		]		
	]


PauliChannelQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	With[{
		op=First[Chi[chan]],
		assum=And[$Assumptions,OptionValue[Assumptions]]},
		AllMatchQ[0,
			FullSimplify[
				Flatten[op-DiagonalMatrix[Diagonal[op]]],
			Assumptions->assum]
		]
	]


(* ::Subsection::Closed:: *)
(*Options and Formatting*)


(* ::Subsubsection::Closed:: *)
(*Options*)


Options[QuantumChannel]={InputDim->Automatic,OutputDim->Automatic,Basis->Automatic};


ChannelParameters[chan_QuantumChannel]:=Last@chan
ChannelRep[chan_QuantumChannel]:=ChannelRep/.ChannelParameters[chan]
InputDim[chan_QuantumChannel]:=InputDim/.ChannelParameters[chan]
OutputDim[chan_QuantumChannel]:=OutputDim/.ChannelParameters[chan]
Basis[chan_QuantumChannel]:=Basis/.ChannelParameters[chan]


(* ::Subsubsection::Closed:: *)
(*Display formatting of channels*)


Format[chan_QuantumChannel]:=ToString[ChannelRep[chan]][First[chan],"<params>"]


(* ::Subsection::Closed:: *)
(*Constructing Channels*)


Super[m_?MatrixQ,OptionsPattern[QuantumChannel]]:=
	With[{
		dims=Dimensions[m],
		inOpt=OptionValue[InputDim],
		outOpt=OptionValue[OutputDim],
		basisOpt=OptionValue[Basis]},
		QuantumChannel[m, 
			{
				ChannelRep->Super,
				InputDim->If[inOpt===Automatic,Sqrt@Last[dims],inOpt],
				OutputDim->If[outOpt===Automatic,Sqrt@First[dims],outOpt],
				Basis->If[basisOpt===Automatic,OptionValue[Vec,Basis],basisOpt]}
		]
	]


Unitary[m_?MatrixQ,OptionsPattern[QuantumChannel]]:=
	With[{
		dims=Dimensions[m],
		inOpt=OptionValue[InputDim],
		outOpt=OptionValue[OutputDim],
		basisOpt=OptionValue[Basis]},
		QuantumChannel[m, 
			{
				ChannelRep->Unitary,
				InputDim->If[inOpt===Automatic,Last[dims],inOpt],
				OutputDim->If[outOpt===Automatic,First[dims],outOpt],
				Basis->If[basisOpt===Automatic,OptionValue[Vec,Basis],basisOpt]}
		]
	]


Kraus[kOps_?KrausQ,opts:OptionsPattern[QuantumChannel]]:=
	If[KrausUnitaryQ[kOps],
		Unitary[First[kOps],opts],
	With[{
		dims=If[KrausSingleQ[kOps],Dimensions[First[kOps]],Dimensions[kOps[[1,1]]]],
		inOpt=OptionValue[InputDim],
		outOpt=OptionValue[OutputDim],
		basisOpt=OptionValue[Basis]},
		QuantumChannel[kOps, 
			{
				ChannelRep->Kraus,
				InputDim->If[inOpt===Automatic,Last[dims],inOpt],
				OutputDim->If[outOpt===Automatic,First[dims],outOpt],
				Basis->If[basisOpt===Automatic,OptionValue[Vec,Basis],basisOpt]}
		]
	]]


Stinespring[m_?StinespringQ,opts:OptionsPattern[QuantumChannel]]:=
	With[{
		dims=If[MatrixQ[m],Dimensions[m],Dimensions[First[m]]],
		inOpt=OptionValue[InputDim],
		outOpt=OptionValue[OutputDim],
		basisOpt=OptionValue[Basis]},
	With[{dimOut=If[outOpt===Automatic,Last[dims],outOpt]},
	Which[
		StinespringUnitaryQ[m,dimOut],
			Unitary[m,opts],
		Not@IntegerQ[First[dims]/dimOut],
			Message[QuantumChannel::dims],
		True,
			QuantumChannel[m, 
				{ChannelRep->Stinespring,
				InputDim->If[inOpt===Automatic,Last[dims],inOpt],
				OutputDim->dimOut,
				Basis->If[basisOpt==Automatic,OptionValue[Vec,Basis],basisOpt]}
		]]
	]]


SysEnv[op_?SysEnvQ,opts:OptionsPattern[QuantumChannel]]:=
	If[SysEnvUnitaryQ[op],Unitary[op,opts],
	With[{
		dims=Dimensions@First@If[SysEnvPairQ[op],First@op,op],
		dEnv=Length[Flatten[Last[op]]],
		inOpt=OptionValue[InputDim],
		outOpt=OptionValue[OutputDim],
		basisOpt=OptionValue[Basis]},
		If[And[outOpt===Automatic,Not[IntegerQ[First[dims]/dEnv]]],
			Message[QuantumChannel::dims],Null];
		
		QuantumChannel[op, 
			{ChannelRep->SysEnv,
				InputDim->If[inOpt===Automatic,Last[dims]/dEnv,inOpt],
				OutputDim->If[outOpt===Automatic,First[dims]/dEnv,outOpt],
				Basis->If[basisOpt===Automatic,OptionValue[Vec,Basis],basisOpt]}
		]
	]]


Choi[m_?SquareMatrixQ,OptionsPattern[QuantumChannel]]:=
	With[{
		len=Length[m],
		inOpt=OptionValue[InputDim],
		outOpt=OptionValue[OutputDim],
		basisOpt=OptionValue[Basis]},
	If[
		Or[
			And[inOpt===Automatic,outOpt===Automatic,Not[IntegerQ[Sqrt[len]]]],
			And[Not[inOpt===Automatic],outOpt===Automatic,Not[IntegerQ[len/(Times@@inOpt)]]],
			And[inOpt===Automatic,Not[outOpt===Automatic],Not[IntegerQ[len/(Times@@outOpt)]]]],
			Message[QuantumChannel::dims],
		QuantumChannel[m, 
			{ChannelRep->Choi,
				InputDim->Which[
					Not[inOpt===Automatic],inOpt,
					And[inOpt===Automatic,Not[outOpt===Automatic]],len/(Times@@outOpt),
					True,Sqrt[len]],
				OutputDim->Which[
					Not[outOpt===Automatic],outOpt,
					And[outOpt===Automatic,Not[inOpt===Automatic]],len/(Times@@inOpt),
					True,Sqrt[len]],
				Basis->If[basisOpt===Automatic,OptionValue[Vec,Basis],basisOpt]}
			]
	]
]


Chi[m_?SquareMatrixQ,opts:OptionsPattern[QuantumChannel]]:=
	With[{
		inOpt=OptionValue[InputDim],
		outOpt=OptionValue[OutputDim],
		basisOpt=OptionValue[Basis]},
	If[basisOpt===Automatic,
			Choi[m,InputDim->inOpt,OutputDim->outOpt,Basis->"Pauli"],
			Choi[m,opts]
	]]


(* ::Subsection::Closed:: *)
(*Transforming Representations*)


(* ::Subsubsection::Closed:: *)
(*Calling methods*)


Options[TransformChannel]={ChannelRep->Automatic,InputDim->Automatic,OutputDim->Automatic,"InputBasis"->Automatic,"OutputBasis"->Automatic};


TransformChannel[chan_QuantumChannel,opts:OptionsPattern[TransformChannel]]:=
	With[{
		inRep=ChannelRep[chan],
		inBasis=Basis[chan],
		inDim=InputDim[chan],
		outDim=OutputDim[chan]},
	With[{
		outRep=If[OptionValue[ChannelRep]===Automatic,
					inRep,
					OptionValue[ChannelRep]],
		outBasis=If[OptionValue["OutputBasis"]===Automatic,
					Basis[chan],
					OptionValue["OutputBasis"]]
		},
	If[And[outRep===inRep,outBasis===inBasis],
		chan,
		QuantumChannel[
			TransformChannel[
				ChannelRep[chan]->outRep,
				First[chan],
				InputDim->inDim,
				OutputDim->outDim,
				"InputBasis"->inBasis,
				"OutputBasis"->outBasis],
			{
			ChannelRep->outRep,
			InputDim->inDim,
			OutputDim->outDim,
			Basis->outBasis}
		]
	]]]


Choi[chan_QuantumChannel,opts:OptionsPattern[QuantumChannel]]:=
	TransformChannel[chan,ChannelRep->Choi,"OutputBasis"->OptionValue[Basis]]

Chi[chan_QuantumChannel,opts:OptionsPattern[QuantumChannel]]:=
	With[{basis=OptionValue[Basis]},
		TransformChannel[chan,ChannelRep->Choi,"OutputBasis"->If[basis===Automatic,"Pauli",basis]]
	]

Super[chan_QuantumChannel,opts:OptionsPattern[QuantumChannel]]:=
	TransformChannel[chan,ChannelRep->Super,"OutputBasis"->OptionValue[Basis]]

Kraus[chan_QuantumChannel,opts:OptionsPattern[QuantumChannel]]:=
	TransformChannel[chan,ChannelRep->Kraus,"OutputBasis"->OptionValue[Basis]]

Stinespring[chan_QuantumChannel,opts:OptionsPattern[QuantumChannel]]:=
	TransformChannel[chan,ChannelRep->Stinespring,"OutputBasis"->OptionValue[Basis]]

SysEnv[chan_QuantumChannel,opts:OptionsPattern[QuantumChannel]]:=
	TransformChannel[chan,ChannelRep->SysEnv,"OutputBasis"->OptionValue[Basis]]

(*Unitary[chan_QuantumChannel,opts:OptionsPattern[QuantumChannel]]:=
	TransformChannel[chan,ChannelRep->ChannelRep[chan],"OutputBasis"->OptionValue[Basis]]*)

Unitary[chan_QuantumChannel,opts:OptionsPattern[QuantumChannel]]:=
If[UnitaryChannelQ[chan],
	TransformChannel[chan,ChannelRep->Unitary,"OutputBasis"->OptionValue[Basis]],
	TransformChannel[chan,ChannelRep->ChannelRep[chan],"OutputBasis"->OptionValue[Basis]]
]


(* ::Subsubsection::Closed:: *)
(*From Stinespring*)


StinespringPair[op_]:=If[MatrixQ[op],{op,op},op]


StinespringDims[op_]:=If[
	MatrixQ[op],
	{True,First@Dimensions@op},
	{False,First@Dimensions@First@op}]


TransformChannel[Stinespring->Stinespring,op_,opts:OptionsPattern[TransformChannel]]:=op


TransformChannel[Stinespring->SysEnv,op_,opts:OptionsPattern[TransformChannel]]:=
			ReducedUnitary[op,OptionValue[OutputDim]]


ReducedUnitary[stine_,outDim_]:=
	With[{v0=UnitVector[First[Dimensions[stine]]/outDim,1]},
		{KroneckerProduct[stine,{v0}],v0}
	]

ReducedUnitary[{stine1_,stine2_},outDim_]:=
	With[{v0=UnitVector[First[Dimensions[stine1]]/outDim,1]},
		{{KroneckerProduct[stine1,{v0}],
		KroneckerProduct[stine2,{v0}]},v0}
	]


TransformChannel[Stinespring->Super,op_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		outDim=OptionValue[OutputDim],
		basis=OptionValue["OutputBasis"],
		pair=StinespringPair[op]},
	BasisTransformation[
		Flatten[
			TensorContract[
				ArrayReshape[
					KroneckerProduct[Conjugate[Last[pair]],First[pair]],
					Flatten[{{outDim,#/outDim}&/@First[#],Last[#]}&@Transpose[Dimensions/@pair]]],
				{{2,4}}]
		,{{1,2},{3,4}}]
	,"Col"->basis]
	]


TransformChannel[Stinespring->Choi,op_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		dim=OptionValue[InputDim]*OptionValue[OutputDim],
		basis=OptionValue["OutputBasis"],
		choi=KroneckerProduct[
				Vec[First[#],Basis->"Col"],
				ConjugateTranspose@Vec[Last[#],Basis->"Col"]
			]&@StinespringPair[op]},
	BasisTransformation[
		PartialTr[
			choi,
			{dim,Length[choi]/dim},{2}],
	"Col"->basis]]


StinespringToKraus[op_,dim_,opts:OptionsPattern[TransformChannel]]:=
	Transpose[
		ArrayReshape[op,
			{OptionValue[OutputDim],dim/OptionValue[OutputDim],OptionValue[InputDim]}],
	{2,1,3}]


TransformChannel[Stinespring->Kraus,op_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		sdims=StinespringDims[op]},
	Select[
			If[First[sdims],
				StinespringToKraus[op,Last[sdims],opts],
				Map[StinespringToKraus[#,Last[sdims],opts]&,op]
			],
		AnyPossiblyNonzeroQ
	]]


TransformChannel[Stinespring->Unitary,op_,opts:OptionsPattern[TransformChannel]]:=op


(* ::Subsubsection::Closed:: *)
(*From SysEnv*)


TransformChannel[SysEnv->SysEnv,op_,opts:OptionsPattern[TransformChannel]]:=op


TransformChannel[SysEnv->Stinespring,op_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		env=KroneckerProduct[IdentityMatrix[OptionValue[InputDim]],Partition[Last[op],1]]},
	If[SysEnvSingleQ[op],
		First[op].env,
		Map[#.env&,First[op]]
	]]


TransformChannel[SysEnv->toRep_,op_,opts:OptionsPattern[TransformChannel]]:=
	With[{stine=TransformChannel[SysEnv->Stinespring,op,opts]},
		TransformChannel[Stinespring->toRep,stine,opts]
	]


TransformChannel[SysEnv->Unitary,op_,opts:OptionsPattern[TransformChannel]]:=First[op]


(* ::Subsubsection::Closed:: *)
(*From Kraus*)


KrausPair[set_]:=With[{dims=Dimensions[set]},
	Which[
		Length[dims]===3,{set,set},
		Length[dims]===4,set,
		True,Message[Kraus::invalid]
	]]


TransformChannel[Kraus->Kraus,kraus_,OptionsPattern[TransformChannel]]:=kraus


TransformChannel[Kraus->Super,kraus_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		basis=OptionValue["OutputBasis"],
		pair=KrausPair[kraus]},
	BasisTransformation[
		Total@MapThread[
			KroneckerProduct[Conjugate[#2],#1]&,
			pair,1],
	"Col"->basis]
	]


TransformChannel[Kraus->Choi,kraus_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		basis=OptionValue["OutputBasis"],
		pair=KrausPair[kraus]},
	BasisTransformation[
		Total@MapThread[
			KroneckerProduct[
				Vec[#1,Basis->"Col"],
				ConjugateTranspose@Vec[#2,Basis->"Col"]]&,
			pair],
	"Col"->basis]]


TransformChannel[Kraus->Stinespring,kraus_,opts:OptionsPattern[TransformChannel]]:=
	If[
		KrausSingleQ[kraus],
			Total@MapThread[
				KroneckerProduct[#1,Partition[#2,1]]&,
				{kraus,IdentityMatrix[Length[kraus]]}],
		Map[
			TransformChannel[Kraus->Stinespring,#,opts]&,
			kraus]
	]


TransformChannel[Kraus->SysEnv,kraus_,opts:OptionsPattern[TransformChannel]]:=
	With[
		{stine=TransformChannel[Kraus->Stinespring,kraus,opts]},
		TransformChannel[Stinespring->SysEnv,stine,opts]
	]


TransformChannel[Kraus->Unitary,op_,opts:OptionsPattern[TransformChannel]]:=First[op]


(* ::Subsubsection::Closed:: *)
(*From Choi*)


TransformChannel[Choi->Choi,mat_,opts:OptionsPattern[TransformChannel]]:= 
	BasisTransformation[mat,OptionValue["InputBasis"]->OptionValue["OutputBasis"]]


TransformChannel[Choi->Super,mat_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		dimIn=OptionValue[InputDim],
		dimOut=OptionValue[OutputDim],
		basisIn=OptionValue["InputBasis"],
		basisOut=OptionValue["OutputBasis"]},
	BasisTransformation[
		Reshuffle[
			BasisTransformation[mat,basisIn->"Col"],
			{dimIn,dimOut,dimIn,dimOut},Basis->"Col"],
		"Col"->basisOut]
	]


DevecKraus[singVals_,ops_,{dimIn_,dimOut_}]:=
	Select[
		Map[Devec[#,{dimIn,dimOut},Basis->"Col"]&,Sqrt[singVals]*ops],
		AnyPossiblyNonzeroQ]


TransformChannel[Choi->Kraus,mat_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		dimIn=OptionValue[InputDim],
		dimOut=OptionValue[OutputDim],
		svd=SingularValueDecomposition[
			BasisTransformation[mat,OptionValue["InputBasis"]->"Col"]]},
	With[{
		kraus1=DevecKraus[Diagonal[Part[svd,2]],Transpose[First[svd]],{dimIn,dimOut}],
		kraus2=DevecKraus[Diagonal[Part[svd,2]],Transpose[Last[svd]],{dimIn,dimOut}]},
	If[0===Chop@Norm@Flatten[kraus1-kraus2],
		kraus1,
		{kraus1,kraus2}
	]]];


TransformChannel[Choi->toRep_,mat_,opts:OptionsPattern[TransformChannel]]:=
	With[{kraus=TransformChannel[Choi->Kraus,mat,opts]},
		TransformChannel[Kraus->toRep,kraus,opts]
	]


(* ::Subsubsection::Closed:: *)
(*From Superoperator*)


TransformChannel[Super->Super,mat_,opts:OptionsPattern[TransformChannel]]:= 
	BasisTransformation[mat,OptionValue["InputBasis"]->OptionValue["OutputBasis"]]


TransformChannel[Super->Choi,mat_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		dimIn=OptionValue[InputDim],
		dimOut=OptionValue[OutputDim],
		basisIn=OptionValue["InputBasis"],
		basisOut=OptionValue["OutputBasis"]},
	BasisTransformation[
		Reshuffle[
			BasisTransformation[mat,basisIn->"Col"],
			{dimOut,dimOut,dimIn,dimIn},Basis->"Col"],
		"Col"->basisOut]
	]


TransformChannel[Super->toRep_,mat_,opts:OptionsPattern[TransformChannel]]:=
	With[{choi=TransformChannel[Super->Choi,mat,opts]},
		TransformChannel[Choi->toRep,choi,opts]
	]


(* ::Subsubsection::Closed:: *)
(*From Unitary*)


TransformChannel[Unitary->Unitary,U_,OptionsPattern[TransformChannel]]:=U
TransformChannel[Unitary->Stinespring,U_,OptionsPattern[TransformChannel]]:=U
TransformChannel[Unitary->Kraus,U_,OptionsPattern[TransformChannel]]:={U}
TransformChannel[Unitary->SysEnv,U_,OptionsPattern[TransformChannel]]:={U,{1}}


TransformChannel[Unitary->Super,U_,opts:OptionsPattern[TransformChannel]]:=
	BasisTransformation[
		KroneckerProduct[Conjugate[U],U],
		"Col"->OptionValue["OutputBasis"]
	]


TransformChannel[Unitary->Choi,U_,opts:OptionsPattern[TransformChannel]]:=
	BasisTransformation[
		Projector[Vec[U,Basis->"Col"]],
		"Col"->OptionValue["OutputBasis"]
	]


(* ::Subsection::Closed:: *)
(*State Evolution*)


EvolutionSuper[chan_QuantumChannel,state_]:=
	With[{
		d=Length[state],
		inDim=InputDim[chan],
		basis=Basis[chan],
		S=First[chan]},
	Which[
		And[GeneralVectorQ[state],d===inDim^2],	
			BasisTransformation[S,basis->OptionValue[Vec,Basis]].state,
		And[d===inDim,GeneralVectorQ[state]],
			Devec[First[chan].Vec[Projector[state],Basis->basis],Basis->basis],
		And[d===inDim,MatrixQ[state]],
			Devec[First[chan].Vec[state,Basis->basis],Basis->basis],		
		True,
			Message[QuantumChannel::state]
	]]


EvolutionUnitary[chan_QuantumChannel,state_]:=
	If[Length[state]===InputDim[chan],
		With[{U=First[chan]},
		Which[
			GeneralVectorQ[state],U.state,
			MatrixQ[state],U.state.ConjugateTranspose[U],
			True,Message[QuantumChannel::state]
		]],
		Super[chan][state]
	]


chan_QuantumChannel[state_]:=
	With[{rep=ChannelRep[chan]},
	Which[
		rep===Super,EvolutionSuper[chan,state],
		rep===Unitary,EvolutionUnitary[chan,state],
		True,Super[chan][state]	
	]]


(* ::Subsection::Closed:: *)
(*Channel Operations*)


(* ::Subsubsection::Closed:: *)
(*Linear Algebra*)


(* ::Text:: *)
(*Multiplication*)


QuantumChannel/:Times[val_,chan_QuantumChannel]:=With[{
	rep=ChannelRep[chan],
	inDim=InputDim[chan],
	outDim=OutputDim[chan],
	basis=Basis[chan],
	op=First[chan]},
		If[
			MemberQ[{Unitary,Choi,Super},rep],
			QuantumChannel[val*op,
				{ChannelRep->rep,InputDim->inDim,OutputDim->outDim,Basis->basis}],
			rep[val*Choi[chan]]
		]
	]


(* ::Text:: *)
(*Addition*)


CheckAdditionDims[inDim_,outDim_,chans__]:=
	And[AllMatchQ[inDim,InputDim/@{chans}],
		AllMatchQ[outDim,OutputDim/@{chans}]
	]


QuantumChannel/:Plus[chans__QuantumChannel]:=
	With[{
		rep=ChannelRep@First@{chans},
		inDim=InputDim@First@{chans},
		outDim=OutputDim@First@{chans}},
	If[
		CheckAdditionDims[inDim,outDim,chans],
		rep@QuantumChannel[
			Apply[Plus,First[Super[#,Basis->"Col"]]&/@{chans}],
			{ChannelRep->Super,
			InputDim->inDim,
			OutputDim->outDim,
			Basis->"Col"}
		],
		Message[QuantumChannel::plus]
	]]


(* ::Text:: *)
(*Composition*)


CheckDotDims[chans__]:=
	With[{inDims=Most[InputDim/@{chans}],
		outDims=Rest[OutputDim/@{chans}]},
	inDims===outDims
	]


QuantumChannel/:Dot[chans__QuantumChannel]:=
	If[CheckDotDims[chans],
		If[Not@AllQ[ChannelRep[#]===Unitary&,{chans}],
			With[{
				(*Pick the first non-unitary channel as the rep*)
				rep=First@Select[ChannelRep/@{chans},#=!=Unitary&,1],
				basis=Basis@First@{chans},
				inDim=InputDim@Last@{chans},
				outDim=OutputDim@First@{chans}
			},
			rep[QuantumChannel[
				Apply[Dot,First[Super[#,Basis->"Col"]]&/@{chans}],
				{ChannelRep->Super,
				InputDim->inDim,
				OutputDim->outDim,
				Basis->"Col"}]
			,Basis->basis]],
			(*We add a special case where all of the channels are Unitary*)
			With[{
				basis=Basis@First@{chans},
				inDim=InputDim@Last@{chans},
				outDim=OutputDim@First@{chans}
			},
			Unitary[QuantumChannel[
				Apply[Dot,First[Unitary[#,Basis->"Col"]]&/@{chans}],
				{ChannelRep->Unitary,
				InputDim->inDim,
				OutputDim->outDim,
				Basis->"Col"}]
			,Basis->basis]]
		],
		Message[QuantumChannel::dot]
	]


(* ::Subsubsection::Closed:: *)
(*Superoperator Functions*)


ChannelFunction[chan_QuantumChannel,fn_]:=
		QuantumChannel[fn@First[chan],ChannelParameters[chan]]


ChannelFunctionOpt[chan_QuantumChannel,fn_,opts___]:=ChannelFunction[chan,fn[#,opts]&]


ChannelFunction[chan_QuantumChannel,rep_,fn_Function]:=ChannelFunction[rep[chan],fn]


(* ::Text:: *)
(*Transpose, Conjugate, Conjugate Transpose*)


QuantumChannel/:Transpose[chan_QuantumChannel]:=
		With[{super=First@Super[chan,Basis->"Col"],
			inDim=InputDim[chan],
			outDim=OutputDim[chan]},
		ChannelRep[chan][
			QuantumChannel[Transpose[super],
				{ChannelRep->Super,
				InputDim->outDim,
				OutputDim->inDim,
				Basis->"Col"}]
		]]


QuantumChannel/:ConjugateTranspose[chan_QuantumChannel]:=
		With[{super=First@Super[chan,Basis->"Col"],
			inDim=InputDim[chan],
			outDim=OutputDim[chan]},
		ChannelRep[chan][
			QuantumChannel[ConjugateTranspose[super],
				{ChannelRep->Super,
				InputDim->outDim,
				OutputDim->inDim,
				Basis->"Col"}]
		]]


QuantumChannel/:Conjugate[chan_QuantumChannel]:=
		ChannelRep[chan][ChannelFunction[Super[chan,Basis->"Col"],Conjugate]]


(* ::Text:: *)
(*MatrixPower, MatrixExp, MatrixLog*)


QuantumChannel/:MatrixPower[chan_QuantumChannel,n_]:=
	ChannelRep[chan][ChannelFunction[Super[chan],MatrixPower[#,n]&]]


QuantumChannel/:MatrixExp[chan_QuantumChannel]:=
	ChannelRep[chan][ChannelFunction[Super[chan],MatrixExp]]


QuantumChannel/:MatrixLog[chan_QuantumChannel]:=
	ChannelRep[chan][ChannelFunction[Super[chan],MatrixLog]]


(* ::Subsubsection::Closed:: *)
(*Tensor Product*)


QuantumChannel/:KroneckerProduct[chan__QuantumChannel]:=CircleTimes[chan]

QuantumChannel/:CircleTimes[chans__QuantumChannel]:=
	With[{
		rep=ChannelRep@First@{chans},
		basis=Basis@First@{chans},
		chois=First[Choi[#,Basis->"Col"]]&/@{chans},
		inDims=InputDim/@{chans},
		outDims=OutputDim/@{chans}},
		rep[QuantumChannel[
			Reravel[
				CircleTimes@@chois,
				Riffle[inDims,outDims]
			],
			{ChannelRep->Choi,
			InputDim->Times@@inDims,
			OutputDim->Times@@outDims,
			Basis->"Col"}]
		,Basis->basis]
	]


(* ::Subsubsection::Closed:: *)
(*Eigenvalues*)


QuantumChannel/:Eigenvalues[chan_QuantumChannel]:=Eigenvalues[First@Choi[chan]]
QuantumChannel/:Eigenvectors[chan_QuantumChannel]:=Eigenvectors[First@Choi[chan]]
QuantumChannel/:Eigensystem[chan_QuantumChannel]:=Eigensystem[First@Choi[chan]]


(* ::Subsubsection::Closed:: *)
(*Tr*)


QuantumChannel/:Tr[chan_QuantumChannel]:=
	If[MemberQ[{Choi,Super,Unitary},ChannelRep[chan]],
		Tr@First@chan,
		Message[QuantumChannel::tr]
	]


(* ::Subsubsection::Closed:: *)
(*Simplify Etc*)


QuantumChannel/:Dimensions[chan_QuantumChannel]:={InputDim->InputDim[chan],InputDim->OutputDim[chan]}


QuantumChannel/:MatrixForm[chan_QuantumChannel]:=MatrixForm[First[chan]]


$ChannelSimplifyFunctions={N,MatrixPlot,ArrayPlot,SparseArray,Normal,Simplify,FullSimplify,Refine,ComplexExpand,FunctionExpand,PowerExpand,
	ExpToTrig,TrigToExp,TrigExpand,TrigFactor,TrigReduce,Rationalize};


Evaluate[Map[(
	QuantumChannel/:#[chan_QuantumChannel,opts___]:=ChannelFunctionOpt[chan,#,opts])&,
	$ChannelSimplifyFunctions];]


(* ::Subsection::Closed:: *)
(*Channel Functions and Metrics*)


ChannelVolume[chan_QuantumChannel]:=Det[MatrixPower[Part[Super[chan,Basis->"Pauli"],1,2;;All,2;;All],1/2]];


ProcessFidelity[chan_QuantumChannel]:=
	With[{dIn=InputDim[chan],dOut=OutputDim[chan]},
	If[dIn===dOut,
		Tr[First[Super[chan]]]/(dIn*dOut),
		Message[ProcessFidelity::dims];
	]]

ProcessFidelity[chan1_QuantumChannel,chan2_QuantumChannel]:=
	ProcessFidelity[ConjugateTranspose[Super[chan2,Basis->"Col"]].chan1]


AverageGateFidelity[chan_QuantumChannel]:=
	With[{d=InputDim[chan]},
		If[OutputDim[chan]===d,
			(1+d*ProcessFidelity[chan])/(d+1),
			Message[AverageGateFidelity::dims]
		]]

AverageGateFidelity[chan1_QuantumChannel,chan2_QuantumChannel]:=
	AverageGateFidelity[ConjugateTranspose[Super[chan2,Basis->"Col"]].Super[chan1,Basis->"Col"]]


GateFidelity[state_,chan_QuantumChannel]:=Fidelity[state,chan[state]]^2
GateFidelity[state_,chan1_QuantumChannel,chan2_QuantumChannel]:=Fidelity[chan2[state],chan1[state]]^2


EntanglementFidelity[state_,chan_QuantumChannel]:=
	With[{
		rho=Vec[If[GeneralVectorQ[state],Projector[state],state],Basis->"Col"],
		choi=First[Choi[chan,Basis->"Col"]]},
		First@Flatten[rho\[HermitianConjugate].choi.rho]
]

EntanglementFidelity[state_,chan1_QuantumChannel,chan2_QuantumChannel]:=EntanglementFidelity[state,ConjugateTranspose[Super[chan2,Basis->"Col"]].chan1]


Unitarity[chan_QuantumChannel] :=
	With[{
		Eu = First[Super[chan, Basis -> "Weyl"[InputDim[chan]]]][[2;;, 2;;]]
	},
	Chop[Tr[Eu\[HermitianConjugate].Eu]] / (First @ Dimensions @ Eu)
]


DiamondNormDistance[U_?SquareMatrixQ]:=Module[{eiglist, minangle},
	eiglist = Sort[Arg[Eigenvalues[U]]];
	minangle = Min[\[Pi],2\[Pi]-Max@Differences[{##, #1 + 2 \[Pi]} & @@ eiglist]];
	2*Sin[minangle/2]
];
DiamondNormDistance[U_?SquareMatrixQ,V_?SquareMatrixQ]:=DiamondNormDistance[U\[ConjugateTranspose].V]
GetFirstKraus[chan_]:=If[
	ChannelRep[chan]===Unitary,
	First@chan,
	With[{K=First@Kraus@chan},
		If[KrausSingleQ[K],First@K,First@First@K]
	]
]
DiamondNormDistance[U_QuantumChannel?UnitaryQ]:=DiamondNormDistance@GetFirstKraus[U]
DiamondNormDistance[U_QuantumChannel?UnitaryQ,V_QuantumChannel?UnitaryQ]:=DiamondNormDistance[GetFirstKraus[U],GetFirstKraus[V]]


(* ::Subsection::Closed:: *)
(*Special Channels*)


ComChannel[A_?MatrixQ]:=With[{dims=Dimensions[A]},
	Super[
		ProductIdentity[A,IdentityMatrix[First[dims]]]
		- ProductIdentity[IdentityMatrix[Last[dims]],A]]
	];

ComChannel[A_QuantumChannel]:=
	If[ChannelRep[A]===Unitary,
		ComChannel[First[A]],
	Message[ComChannel::chan]
	]


AComChannel[A_?MatrixQ]:=With[{dims=Dimensions[A]},
	Super[
		ProductIdentity[A,IdentityMatrix[First[dims]]]
		+ ProductIdentity[IdentityMatrix[Last[dims]],A]]
	];

AComChannel[A_QuantumChannel]:=
	If[ChannelRep[A]===Unitary,
		AComChannel[First[A]],
	Message[AComChannel::chan]
	]


LindbladDissipator[ops_]:=
Which[
	MatrixQ[ops],
		Super@Unitary[ops]-AComChannel[ConjugateTranspose[ops].ops]/2,
	AllQ[MatrixQ,ops],
		Total[LindbladDissipator/@ops],
	True,
		Message[LindbladDissipator::input]
	]

LindbladDissipator[op_QuantumChannel]:=
If[ChannelRep[op]===Unitary,
	LindbladDissipator[First[op]],
	Message[LindbladDissipator::input]
]

LindbladDissipator[ops__]:=LindbladDissipator[{ops}];


Lindblad[op_]:=
	Which[
		SquareMatrixQ[op],-I*ComChannel[op],
		And[ListQ[op],AllQ[SquareMatrixQ,op]],LindbladDissipator[op],
		True,Message[Lindblad::input]
	]


Lindblad[ops__]:=Total@Map[Lindblad,{ops}]


PartialTrChannel[sysDims_,trlist_]:=
	With[{dL=Length[sysDims]-Length[trlist]},
	Super@Flatten[
		TensorContract[
			ArrayReshape[
				IdentityMatrix[(Times@@sysDims)^2,SparseArray],
				Flatten@ConstantArray[sysDims,4]
			],
		{#,#+Length[sysDims]}&/@trlist],
		{Range[2dL],2dL+Range[1,2*Length[sysDims]]
		}]
	]


FunctionChannel[func_,opts:OptionsPattern[QuantumChannel]]:=
	With[{dim=OptionValue[InputDim]},
		If[IntegerQ[dim],
		With[{
		basis=OptionValue[Basis],
		choi=Choi[Sum[
				CircleTimes[
					UnitArray[{dim,dim},{i,j}],
					func[UnitArray[{dim,dim},{i,j}]]
				],{i,dim},{j,dim}]
			,Basis->"Col",InputDim->dim]},
		If[basis===Automatic,
			choi,
			Choi[choi,Basis->basis]
		]],
			Message[FunctionChannel::indims]
		]
	]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


Protect[Choi,Super,Chi,Kraus,Stinespring,Unitary,SysEnv];
Protect[QuantumChannel,ChannelRep,InputDim,OutputDim,Basis];
Unprotect[GateFidelity,AverageGateFidelity,EntanglementFidelity,ChannelVolume,Unitarity,DiamondNormDistance];
Protect[CompletelyPositiveQ,TracePreservingQ,HermitianPreservingQ,UnitaryQ,UnitalQ,PauliChannelQ];
Unprotect[ComChannel,AComChannel,LindbladDissipator,Lindblad,PartialTrChannel,FunctionChannel];


EndPackage[];
