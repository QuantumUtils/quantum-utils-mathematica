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


BeginPackage["QuantumChannel`",{"Tensor`","Predicates`"}];


Needs["DocTools`"]
Needs["QUOptions`"]
Needs["QuantumSystems`"]


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "QuantumChannel.nb"}]];


(* ::Section::Closed:: *)
(*Usage Declarations*)


(* ::Subsection:: *)
(*Quantum Channels*)


Unprotect[QuantumChannel,ChannelRep,InputDim,OutputDim,Basis];
Unprotect[Choi,Super,Chi,Kraus,Stinespring,Unitary,SysEnv];


AssignUsage[QuantumChannel,$Usages];
AssignUsage[Unitary,$Usages];
AssignUsage[Super,$Usages];
AssignUsage[Choi,$Usages];
AssignUsage[Chi,$Usages];
AssignUsage[Kraus,$Usages];
AssignUsage[Stinespring,$Usages];
AssignUsage[SysEnv,$Usages];


AssignUsage[InputDim,$Usages];
AssignUsage[OutputDim,$Usages];
AssignUsage[Basis,$Usages];
AssignUsage[ChannelRep,$Usages];
AssignUsage[ChannelParameters,$Usages];


(* ::Subsection:: *)
(*Channel Functions*)


Unprotect[GateFidelity,AverageGateFidelity,EntanglementFidelity,ChannelVolume];


AssignUsage[ProcessFidelity,$Usages];
AssignUsage[GateFidelity,$Usages];
AssignUsage[AverageGateFidelity,$Usages];
AssignUsage[EntanglementFidelity,$Usages];
AssignUsage[ChannelVolume,$Usages];


(* ::Subsection:: *)
(*Predicates*)


Unprotect[CompletelyPositiveQ,TracePreservingQ,HermitianPreservingQ,UnitalQ,PauliChannelQ];


AssignUsage[CompletelyPositiveQ,$Usages];
AssignUsage[TracePreservingQ,$Usages];
AssignUsage[HermitianPreservingQ,$Usages];
AssignUsage[UnitalQ,$Usages];
AssignUsage[PauliChannelQ,$Usages];


(* ::Subsection:: *)
(*Special Channels*)


Unprotect[ComChannel,AComChannel,Lindblad,LindbladDissipator,SwapChannel,PartialTrChannel];


AssignUsage[ComChannel,$Usages];
AssignUsage[AComChannel,$Usages];
AssignUsage[LindbladDissipator,$Usages];
AssignUsage[Lindblad,$Usages];
AssignUsage[SwapChannel,$Usages];
AssignUsage[PartialTrChannel,$Usages];


(* ::Subsection::Closed:: *)
(*Error Messages*)


QuantumChannel::dims = "Input channel matrix needs specification of input and output dimensions.";
QuantumChannel::state = "Input must be a density matrix or pure state vector.";
QuantumChannel::plus = "QuantumChannels must be of same dimensions.";
QuantumChannel::dot = "QuantumChannel input and output dimensions are not compatible.";


Kraus::invalid = "Invalid set of Kraus operators.";


ProcessFidelity::dims = "Input and output dimensions of QuantumChannel must be equal.";
AverageGateFidelity::dims = "Input and output dimensions of QuantumChannel must be equal.";


ComChannel::chan = "QuantumChannel input is only valid for Unitary QuantumChannels.";
AComChannel::chan = "QuantumChannel input is only valid for Unitary QuantumChannels.";


LindbladDissipator::input = "Input must be a matrix, list of matrices, or sequence of matrices.";
Lindblad::input = "Input must be a matrix, list of matrices, or sequence of matrices.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Predicates*)


KrausPairQ[set_]:=And[First[Dimensions[set]]===2,AllQ[KrausSingleQ,set]]
KrausSingleQ[set_]:=AllQ[MatrixQ,set]
KrausQ[set_]:=Or[KrausPairQ[set],KrausSingleQ[set]];


StinespringPairQ[set_]:=And[Length[set]==2,AllQ[MatrixQ,set]];
StinespringQ[set_]:=Or[MatrixQ[set],StinespringPairQ[set]]


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


Options[CompletelyPositiveQ]:={Simplify->Identity}


CompletelyPositiveQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	With[{
		choi=First[Choi[chan]],
		fun=OptionValue[Simplify]},		
	PositiveSemidefiniteMatrixQ[fun[choi]]
	];


HermitianPreservingQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	With[{
		choi=First[Choi[chan]],
		fun=OptionValue[Simplify]},		
	HermitianMatrixQ[fun[choi]]
	];


TracePreservingQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	With[{
	op=PartialTr[
			First[Choi[chan,Basis->"Col"]],
			{InputDim[chan],OutputDim[chan]},{2}],
	fun=OptionValue[Simplify],
	id=IdentityMatrix[InputDim[chan]]},
		AllMatchQ[0,fun[Flatten[op-id]]]		
	]


UnitalQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	With[{
	op=PartialTr[
			First[Choi[chan,Basis->"Col"]],
			{InputDim[chan],OutputDim[chan]},{1}],
	fun=OptionValue[Simplify],
	id=IdentityMatrix[OutputDim[chan]]},
		AllMatchQ[0,fun[Flatten[op-id]]]		
	]


PauliChannelQ[chan_QuantumChannel,opts:OptionsPattern[CompletelyPositiveQ]]:=
	With[{op=Chi[chan],
	fun=OptionValue[Simplify]},
		AllMatchQ[0,Flatten[op-DiagonalMatrix[Diagonal[op]]]
		]]


(* ::Subsection::Closed:: *)
(*Options and Formatting*)


(* ::Subsubsection:: *)
(*Options*)


ChannelParameters[chan_QuantumChannel]:=Last@chan
ChannelRep[chan_QuantumChannel]:=ChannelRep/.ChannelParameters[chan]
InputDim[chan_QuantumChannel]:=InputDim/.ChannelParameters[chan]
OutputDim[chan_QuantumChannel]:=OutputDim/.ChannelParameters[chan]
Basis[chan_QuantumChannel]:=Basis/.ChannelParameters[chan]


(* ::Subsubsection:: *)
(*Display formatting of channels*)


Format[chan_QuantumChannel]:=ToString[ChannelRep[chan]][First[chan],"<params>"]


(* ::Subsection:: *)
(*Constructing Channels*)


Options[QuantumChannel]={InputDim->Automatic,OutputDim->Automatic,Basis->Automatic};


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


Kraus[kOps_?KrausQ,OptionsPattern[QuantumChannel]]:=
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
	]


Stinespring[m_?StinespringQ,OptionsPattern[QuantumChannel]]:=
	With[{
		dims=If[MatrixQ[m],Dimensions[m],Dimensions[First[m]]],
		inOpt=OptionValue[InputDim],
		outOpt=OptionValue[OutputDim],
		basisOpt=OptionValue[Basis]},
		If[And[outOpt==Automatic,Not[IntegerQ[First[dims]/Last[dims]]]],
			Message[QuantumChannel::dims],Null];
		QuantumChannel[m, 
			{
				ChannelRep->Stinespring,
				InputDim->If[inOpt===Automatic,Last[dims],inOpt],
				OutputDim->If[outOpt===Automatic,First[dims]/Last[dims],outOpt],
				Basis->If[basisOpt==Automatic,OptionValue[Vec,Basis],basisOpt]}
		]
	]


SysEnv[op_?SysEnvQ,OptionsPattern[QuantumChannel]]:=
	With[{
		dims=Dimensions[First[op]],
		dEnv=Length[Flatten[Last[op]]],
		inOpt=OptionValue[InputDim],
		outOpt=OptionValue[OutputDim],
		basisOpt=OptionValue[Basis]},
		If[And[outOpt===Automatic,Not[IntegerQ[First[dims]/dEnv]]],
			Message[QuantumChannel::dims],Null];
		
		QuantumChannel[SparseArray[op], 
			{ChannelRep->SysEnv,
				InputDim->If[inOpt===Automatic,Last[dims]/dEnv,inOpt],
				OutputDim->If[outOpt===Automatic,First[dims]/dEnv,outOpt],
				Basis->If[basisOpt===Automatic,OptionValue[Vec,Basis],basisOpt]}
		]
	]


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

Unitary[chan_QuantumChannel,opts:OptionsPattern[QuantumChannel]]:=
	TransformChannel[chan,ChannelRep->ChannelRep[chan],"OutputBasis"->OptionValue[Basis]]


(* ::Subsubsection:: *)
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
	With[{v0=UnitVector[Last[Dimensions[stine]]/outDim,1]},
		{KroneckerProduct[stine,{v0}],v0}
	]

ReducedUnitary[{stine1_,stine2_},outDim_]:=
	With[{v0=UnitVector[Last[Dimensions[stine1]]/outDim,1]},
		{{KroneckerProduct[stine1,{v0}]
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


TransformChannel[Stinespring->Kraus,op_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		dims=StinespringDims[op],
		outDim=OptionValue[OutputDim],
		inDim=OptionValue[InputDim]},
	Flatten[
		If[First[dims],Apply,Map][ArrayReshape[#,{outDim,Last[dims]/outDim,inDim}]&,op],
		{{2},{1},{3}}
	]]


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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
			TransformChannel[Kraus->Stinespring][#,opts]&,
			kraus]
	]


TransformChannel[Kraus->SysEnv,kraus_,opts:OptionsPattern[TransformChannel]]:=
	With[
		{stine=TransformChannel[Kraus->Stinespring,kraus,opts]},
		TransformChannel[Stinespring->SysEnv,stine,opts]
	]


(* ::Subsubsection:: *)
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


DevecKraus[weights_,ops_]:=
	Select[
		Map[Devec[#,Basis->"Col"]&,weights*ops],
	Not[AllMatchQ[0,Flatten[#]]]&]


TransformChannel[Choi->Kraus,mat_,opts:OptionsPattern[TransformChannel]]:=
	With[{
		svd=SingularValueDecomposition[
			BasisTransformation[mat,OptionValue["InputBasis"]->"Col"]]},
	With[{
		kraus1=Sort[DevecKraus[Sqrt@Diagonal[Part[svd,2]],Transpose[First[svd]]]],
		kraus2=Sort[DevecKraus[Sqrt@Diagonal[Part[svd,2]],Transpose[Last[svd]]]]},
	If[kraus1===kraus2,
		kraus1,
		{kraus1,kraus2}
	]]];


TransformChannel[Choi->toRep_,mat_,opts:OptionsPattern[TransformChannel]]:=
	With[{kraus=TransformChannel[Choi->Kraus,mat,opts]},
		TransformChannel[Kraus->toRep,kraus,opts]
	]


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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
		And[d===inDim,MatrixQ[state]],
			Devec[First[chan].Vec[state,Basis->basis],Basis->basis],
		And[d===inDim,GeneralVectorQ[state]],
			Devec[First[chan].Vec[Projector[state],Basis->basis],Basis->basis],
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


(* ::Subsection:: *)
(*Channel Operations*)


(* ::Subsubsection:: *)
(*Linear Algebra*)


(* ::Text:: *)
(*Multiplication*)


QuantumChannel/:Times[val_,chan_QuantumChannel]:=With[{
	rep=ChannelRep[chan],
	params=ChannelParameters[chan],
	op=First[chan]},
		If[
			MemberQ[{Unitary,Choi,Super},rep],
			QuantumChannel[val*op,params],
			rep[val*Choi[chan]]
		]
	]


CheckAdditionDims[inDim_,outDim_,chans__]:=
	And[AllMatchQ[inDim,InputDim/@{chans}],
		AllMatchQ[outDim,OutputDim/@{chans}]
	]


(* ::Text:: *)
(*Addition*)


QuantumChannel/:Plus[chans__QuantumChannel]:=
	With[{
		rep=ChannelRep@First@{chans},
		inDim=InputDim@First@{chans},
		outDim=OutputDim@First@{chans}},
	If[
		CheckAdditionDims[inDim,outDim,chans],
		QuantumChannel[
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
	Print[inDims];
	Print[outDims];
	inDims===outDims
	]


QuantumChannel/:Dot[chans__QuantumChannel]:=
	If[CheckDotDims[chans],
		With[{
		rep=ChannelRep@First@{chans},
		basis=Basis@First@{chans},
		inDim=InputDim@First@{chans},
		outDim=OutputDim@Last@{chans}},
		rep[QuantumChannel[
			Apply[Dot,First[Super[#,Basis->"Col"]]&/@{chans}],
			{ChannelRep->Super,
			InputDim->inDim,
			OutputDim->outDim,
			Basis->"Col"}]
		,Basis->basis]],
		Message[QuantumChannel::dot]	
	]


(* ::Subsubsection:: *)
(*Superoperator Functions*)


ChannelFunction[chan_QuantumChannel,fn_]:=
		QuantumChannel[fn@First[chan],ChannelParameters[chan]]


ChannelFunction[chan_QuantumChannel,rep_,fn_Function]:=ChannelFunction[rep[chan],fn]


(* ::Text:: *)
(*Transpose, Conjugate, Conjugate Transpose*)


QuantumChannel/:Transpose[chan_QuantumChannel]:=
		ChannelRep[chan][ChannelFunction[Super[chan,Basis->"Col"],Transpose]]


QuantumChannel/:ConjugateTranspose[chan_QuantumChannel]:=
		ChannelRep[chan][ChannelFunction[Super[chan,Basis->"Col"],ConjugateTranspose]]


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


QuantumChannel/:KroneckerProduct[chan_QuantumChannel]:=chan
QuantumChannel/:KroneckerProduct[chans__QuantumChannel]:=
	With[{
		rep=ChannelRep@First@{chans},
		basis=Basis@First@{chans},
		chois=First[Choi[#,Basis->"Col"]]&/@{chans},
		inDims=InputDim/@{chans},
		outDims=OutputDim/@{chans}},
		rep[QuantumChannel[
			Reravel[
				KroneckerProduct@@chois,
				Riffle[inDims,outDims]
			],
			{ChannelRep->Choi,
			InputDim->Times@@inDims,
			OutputDim->Times@@outDims,
			Basis->"Col"}]
		,Basis->basis]
	]
QuantumChannel/:CircleTimes[chans__QuantumChannel]:=KroneckerProduct[chans]


(* ::Subsubsection::Closed:: *)
(*Eigenvalues*)


QuantumChannel/:Eigenvalues[chan_QuantumChannel]:=Eigenvalues[First@Choi[chan]]
QuantumChannel/:Eigenvectors[chan_QuantumChannel]:=Eigenvectors[First@Choi[chan]]
QuantumChannel/:Eigensystem[chan_QuantumChannel]:=Eigensystem[First@Choi[chan]]


(* ::Subsubsection::Closed:: *)
(*Simplify Etc*)


QuantumChannel/:MatrixForm[chan_QuantumChannel]:=MatrixForm[First[chan]]


$ChannelFunctions={MatrixPlot,ArrayPlot,SparseArray,Normal,Simplify,FullSimplify,Refine,ComplexExpand,FunctionExpand,PowerExpand,
	ExpToTrig,TrigToExp,TrigExpand,TrigFactor,TrigReduce};


Map[Function[func,
QuantumChannel/:func[chan_QuantumChannel,opts___]:=
	ChannelFunction[chan,func[#,opts]&]],$ChannelSimplifyFunctions];


(* ::Subsection::Closed:: *)
(*Fidelity and Volume*)


ChannelVolume[chan_QuantumChannel]:=Det[MatrixPower[Part[Super[chan,Basis->"Pauli"],1,2;;All,2;;All],1/2]];


ProcessFidelity[chan_QuantumChannel]:=
	With[{dIn=InputDim[chan],dOut=OutputDim[chan]},
	If[dIn===dOut,
		Tr[First[Super[chan]]]/(dIn*dOut),
		Message[ProcessFidelity::dims];
	]]

ProcessFidelity[chan1_QuantumChannel,chan2_QuantumChannel]:=
	ProcessFidelity[ConjugateTranspose[Super[chan2]].chan1]


AverageGateFidelity[chan_QuantumChannel]:=
	With[{d=InputDim[chan]},
		If[OutputDim[chan]===d,
			(1+d*ProcessFidelity[chan])/(d+1),
			Message[AverageGateFidelity::dims]
		]]

AverageGateFidelity[chan1_QuantumChannel,chan2_QuantumChannel]:=
	AverageGateFidelity[ConjugateTranspose[Super[chan2]].Super[chan1]]


GateFidelity[state_,chan_QuantumChannel]:=Fidelity[state,chan[state]]
GateFidelity[state_,chan1_QuantumChannel,chan2_QuantumChannel]:=Fidelity[chan2[state],chan1[state]]


EntanglementFidelity[state_,chan_QuantumChannel]:=
	With[{
		rho=Vec[If[GeneralVectorQ[state],Projector[state],state],Basis->"Col"],
		choi=First[Choi[chan,Basis->"Col"]]},
		First@Flatten[rho\[HermitianConjugate].choi.rho]
]

EntanglementFidelity[state_,chan1_QuantumChannel,chan2_QuantumChannel]:=EntanglementFidelity[state,ConjugateTranspose[Super[chan2]].chan2]


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
		Unitary[ops]-AComChannel[ConjugateTranspose[ops].ops]/2,
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


SwapChannel[output_List,dims_List]:=
	Super[Flatten[
	ArrayReshape[
		IdentityMatrix[(Times@@dims)^2,SparseArray]
	,Flatten[ConstantArray[dims,4]]]
	,{Join[output,Length[dims]+output],2*Length[dims]+Range[2*Length[dims]]}]]


SwapChannel[output_List,d_Integer:2]:=SwapChannel[output,ConstantArray[d,Length[output]]]


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


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section:: *)
(*Unit Testing*)


(* ::Section::Closed:: *)
(*End Package*)


Protect[Choi,Super,Chi,Kraus,Stinespring,Unitary,SysEnv];
Protect[QuantumChannel,ChannelRep,InputDim,OutputDim,Basis];
Unprotect[GateFidelity,AverageGateFidelity,EntanglementFidelity,ChannelVolume];
Protect[CompletelyPositiveQ,TracePreservingQ,HermitianPreservingQ,UnitalQ,PauliChannelQ];
Unprotect[ComChannel,AComChannel,LindbladDissipator,Lindblad,SwapChannel,PartialTrChannel];


EndPackage[];
