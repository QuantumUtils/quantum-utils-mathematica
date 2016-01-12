(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Quantum Channel Unit Tests*)


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


BeginPackage["QuantumChannelTests`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];
Needs["QuantumChannel`"];


(* ::Section:: *)
(*Results*)


Begin["`UnitTests`"];


$RegisteredTests={};
$TestResults := RunTest[$RegisteredTests];


End[];


(* ::Section:: *)
(*Unit Tests*)


Begin["`UnitTests`"];


(* ::Subsection::Closed:: *)
(*Channels*)


(* ::Subsubsection::Closed:: *)
(*Representations*)


TestCase[$RegisteredTests,"QuantumChannel:Unitary",
	SameQ[
		Unitary[PauliMatrix[1]],
		QuantumChannel[PauliMatrix[1],{ChannelRep->Unitary,InputDim->2,OutputDim->2,Basis->"Col"}]
	]];


TestCase[$RegisteredTests,"QuantumChannel:Super",
	SameQ[
		Super[IdentityMatrix[4]],
		QuantumChannel[IdentityMatrix[4],{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]
	]];


TestCase[$RegisteredTests,"QuantumChannel:Choi",
	And[
		SameQ[
			Choi[IdentityMatrix[4]],
			QuantumChannel[IdentityMatrix[4],
				{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]],
		SameQ[
			Choi[IdentityMatrix[8],InputDim->4],
			QuantumChannel[IdentityMatrix[8],
				{ChannelRep->Choi,InputDim->4,OutputDim->2,Basis->"Col"}]]
	]];


TestCase[$RegisteredTests,"QuantumChannel:Chi",
	SameQ[
		Chi[IdentityMatrix[4]],
		QuantumChannel[IdentityMatrix[4],
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];


TestCase[$RegisteredTests,"QuantumChannel:Kraus",
	SameQ[
		Kraus[Table[PauliMatrix[j],{j,4}]],
		QuantumChannel[Table[PauliMatrix[j],{j,4}],
			{ChannelRep->Kraus,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];


TestCase[$RegisteredTests,"QuantumChannel:Stinespring",
	Module[{a,b},
	And[
	SameQ[
		Stinespring[Array[a,{8,2}],OutputDim->2],
		QuantumChannel[Array[a,{8,2}],
			{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}]],
	SameQ[
		Stinespring[{Array[a,{8,2}],Array[b,{8,2}]},OutputDim->2],
		QuantumChannel[{Array[a,{8,2}],Array[b,{8,2}]},
			{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}]]

	]]];


TestCase[$RegisteredTests,"QuantumChannel:SysEnv",
	Module[{a,b},
	And[
		SameQ[
		SysEnv[{Array[a,{4,4}],{1,0}}],
		QuantumChannel[{Array[a,{4,4}],{1,0}},
			{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}]],
		SameQ[
		SysEnv[{{Array[a,{4,4}],Array[b,{4,4}]},{1,0}}],
		QuantumChannel[{{Array[a,{4,4}],Array[b,{4,4}]},{1,0}},
			{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}]]
	]]];


TestCase[$RegisteredTests,"QuantumChannel:ChannelParameters",
	SameQ[
		ChannelParameters[QuantumChannel[IdentityMatrix[4],
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]],
		{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}
	]];


TestCase[$RegisteredTests,"QuantumChannel:ChannelRep",
	SameQ[
		ChannelRep[QuantumChannel[IdentityMatrix[4],
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]],
		Super
	]];


TestCase[$RegisteredTests,"QuantumChannel:ChannelRep",
	SameQ[
		ChannelRep[QuantumChannel[IdentityMatrix[4],
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]],
		Super
	]];


TestCase[$RegisteredTests,"QuantumChannel:InputDim",
	SameQ[
		InputDim[QuantumChannel[IdentityMatrix[4],
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]],
		2
	]];


TestCase[$RegisteredTests,"QuantumChannel:OutputDim",
	SameQ[
		OutputDim[QuantumChannel[IdentityMatrix[4],
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]],
		2
	]];


TestCase[$RegisteredTests,"QuantumChannel:Basis",
	SameQ[
		Basis[QuantumChannel[IdentityMatrix[4],
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]],
		"Col"
	]];


(* ::Subsubsection::Closed:: *)
(*Evolution*)


TestCase[$RegisteredTests,"QuantumChannel:UnitaryEvolution",
	With[{chan=QuantumChannel[
				PauliMatrix[1],
				{ChannelRep->Unitary,InputDim->2,OutputDim->2,Basis->"Col"}]},
	And[
		chan[{{1},{0}}]==={{0},{1}},
		chan[{1,0}]==={0,1},
		chan[{{1,0},{0,0}}]==={{0,0},{0,1}},
		chan[{1,0,0,0}]==={0,0,0,1},
		chan[{{1},{0},{0},{0}}]==={{0},{0},{0},{1}}
	]]];


TestCase[$RegisteredTests,"QuantumChannel:SuperEvolution",
	With[{
		chan=QuantumChannel[
			{{1,0,0,1},{0,0,0,0},{0,0,0,0},{1,0,0,1}}/2,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}],
		out=IdentityMatrix[2]/2},
	And[
		chan[{{1},{0}}]===out,
		chan[{1,0}]===out,
		chan[{{1,0},{0,0}}]===out,
		chan[{1,0,0,0}]==={1,0,0,1}/2,
		chan[{{1},{0},{0},{0}}]==={{1},{0},{0},{1}}/2
	]]];


TestCase[$RegisteredTests,"QuantumChannel:ChoiEvolution",
	With[{
		chan=QuantumChannel[
				IdentityMatrix[4]/2,
				{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}],	
		out=IdentityMatrix[2]/2},
	And[
		chan[{{1},{0}}]===out,
		chan[{1,0}]===out,
		chan[{{1,0},{0,0}}]===out,
		chan[{1,0,0,0}]==={1,0,0,1}/2,
		chan[{{1},{0},{0},{0}}]==={{1},{0},{0},{1}}/2
	]]];


TestCase[$RegisteredTests,"QuantumChannel:KrausEvolution",
	With[{
		chan=QuantumChannel[
				(PauliMatrix/@Range[4])/2,
				{ChannelRep->Kraus,InputDim->2,OutputDim->2,Basis->"Col"}],	
		out=IdentityMatrix[2]/2},
	And[
		chan[{{1},{0}}]===out,
		chan[{1,0}]===out,
		chan[{{1,0},{0,0}}]===out,
		chan[{1,0,0,0}]==={1,0,0,1}/2,
		chan[{{1},{0},{0},{0}}]==={{1},{0},{0},{1}}/2
	]]];


TestCase[$RegisteredTests,"QuantumChannel:SysEnvEvolution",
	With[{
		chan=QuantumChannel[
				{{{0,0,1,0},{0,1,0,0},{1,0,0,0},{0,0,1,0}},{1,0}},
				{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}],	
		out={{0,0},{0,1}}},
	And[
		chan[{{1},{0}}]===out,
		chan[{1,0}]===out,
		chan[{{1,0},{0,0}}]===out,
		chan[{1,0,0,0}]==={0,0,0,1},
		chan[{{1},{0},{0},{0}}]==={{0},{0},{0},{1}}
	]]];


TestCase[$RegisteredTests,"QuantumChannel:StinespringEvolution",
	With[{
		chan=QuantumChannel[
				{{0,0},{0,0},{0,1},{1,0},{0,1},{1,0},{0,0},{0,0}}/Sqrt[2],
				{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}],	
		out=IdentityMatrix[2]/2},
	And[
		chan[{{1},{0}}]===out,
		chan[{1,0}]===out,
		chan[{{1,0},{0,0}}]===out,
		chan[{1,0,0,0}]==={1,0,0,1}/2,
		chan[{{1},{0},{0},{0}}]==={{1},{0},{0},{1}}/2
	]]];


(* ::Subsection::Closed:: *)
(*Channel Transformations*)


(* ::Subsubsection::Closed:: *)
(*To/From Unitary*)


With[{chan=QuantumChannel[PauliMatrix[1],
			{ChannelRep->Unitary,InputDim->2,OutputDim->2,Basis->"Col"}]},

TestCase[$RegisteredTests,"QuantumChannel:UnitaryToUnitary",
	SameQ[Unitary[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:UnitaryToSuper",
	SameQ[Super[chan],
		QuantumChannel[
			{{0,0,0,1},{0,0,1,0},{0,1,0,0},{1,0,0,0}},
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]]];

TestCase[$RegisteredTests,"QuantumChannel:UnitaryToChoi",
	SameQ[Choi[chan],
		QuantumChannel[
			{{0,0,0,0},{0,1,1,0},{0,1,1,0},{0,0,0,0}},
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]]];

TestCase[$RegisteredTests,"QuantumChannel:UnitaryToChi",
	SameQ[Chi[chan],
		QuantumChannel[
			{{0,0,0,0},{0,2,0,0},{0,0,0,0},{0,0,0,0}},
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Pauli"}]]];

TestCase[$RegisteredTests,"QuantumChannel:UnitaryToKraus",
	SameQ[Kraus[chan],
		QuantumChannel[
			{{{0,1},{1,0}}},
			{ChannelRep->Kraus,InputDim->2,OutputDim->2,Basis->"Col"}]]];

TestCase[$RegisteredTests,"QuantumChannel:UnitaryToStinespring",
	SameQ[Stinespring[chan],
		QuantumChannel[
			{{0,1},{1,0}},
			{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}]]];

TestCase[$RegisteredTests,"QuantumChannel:UnitaryToSysEnv",
	SameQ[SysEnv[chan],
		QuantumChannel[
			{{{0,1},{1,0}},{1}},
			{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}]]];
];


With[{chan=QuantumChannel[PauliMatrix[3],
			{ChannelRep->Unitary,InputDim->2,OutputDim->2,Basis->"Col"}]},

TestCase[$RegisteredTests,"QuantumChannel:SuperToUnitary",
	SameQ[Unitary[Super[chan]],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:ChoiToUnitary",
	SameQ[Unitary[Choi[chan]],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:KrausToUnitary",
	SameQ[Unitary[Kraus[chan]],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToUnitary",
	SameQ[Unitary[Stinespring[chan]],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToUnitary",
	SameQ[Unitary[SysEnv[chan]],chan]
	];
];


(* ::Subsubsection::Closed:: *)
(*From Super*)


With[{chan=QuantumChannel[
			{{3,0,0,1},{0,3,1,0},{0,1,3,0},{1,0,0,3}}/4,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]},

TestCase[$RegisteredTests,"QuantumChannel:SuperToNotUnitary",
	SameQ[Unitary[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:SuperToSuper",
	SameQ[Super[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:SuperToPauliSuper",
	SameQ[Super[chan,Basis->"Pauli"],
		QuantumChannel[
			{{2,0,0,0},{0,2,0,0},{0,0,1,0},{0,0,0,1}}/2,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];
TestCase[$RegisteredTests,"QuantumChannel:SuperToChoi",
	SameQ[Choi[chan],
		QuantumChannel[
			{{3,0,0,3},{0,1,1,0},{0,1,1,0},{3,0,0,3}}/4,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:SuperToChi",
	SameQ[Choi[chan,Basis->"Pauli"],
		QuantumChannel[
			{{3,0,0,0},{0,1,0,0},{0,0,0,0},{0,0,0,0}}/2,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:SuperToKraus",
	SameQ[Kraus[chan],
		QuantumChannel[
			{{{Sqrt[3],0},{0,Sqrt[3]}}/2,{{0,1},{1,0}}/2},
			{ChannelRep->Kraus,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:SuperToStinespring",
	SameQ[Stinespring[chan],
		QuantumChannel[
			{{Sqrt[3],0},{0,1},{0,Sqrt[3]},{1,0}}/2,
			{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:SuperToSysEnv",
	SameQ[SysEnv[chan],
		QuantumChannel[
			{{{Sqrt[3],0,0,0},{0,0,1,0},{0,0,Sqrt[3],0},{1,0,0,0}}/2,{1,0}},
			{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];
];


(* ::Subsubsection::Closed:: *)
(*From Choi*)


With[{chan=QuantumChannel[
			{{3,0,0,3},{0,1,1,0},{0,1,1,0},{3,0,0,3}}/4,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]},

TestCase[$RegisteredTests,"QuantumChannel:ChoiToNotUnitary",
	SameQ[Unitary[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:ChoiToSuper",
	SameQ[Super[chan],
		QuantumChannel[
			{{3,0,0,1},{0,3,1,0},{0,1,3,0},{1,0,0,3}}/4,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:ChoiToPauliSuper",
	SameQ[Super[chan,Basis->"Pauli"],
		QuantumChannel[
			{{2,0,0,0},{0,2,0,0},{0,0,1,0},{0,0,0,1}}/2,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];
TestCase[$RegisteredTests,"QuantumChannel:ChoiToChoi",
	SameQ[Choi[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:ChoiToChi",
	SameQ[Choi[chan,Basis->"Pauli"],
		QuantumChannel[
			{{3,0,0,0},{0,1,0,0},{0,0,0,0},{0,0,0,0}}/2,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:ChoiToKraus",
	SameQ[Kraus[chan],
		QuantumChannel[
			{{{Sqrt[3],0},{0,Sqrt[3]}}/2,{{0,1},{1,0}}/2},
			{ChannelRep->Kraus,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:ChoiToStinespring",
	SameQ[Stinespring[chan],
		QuantumChannel[
			{{Sqrt[3],0},{0,1},{0,Sqrt[3]},{1,0}}/2,
			{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:ChoiToSysEnv",
	SameQ[SysEnv[chan],
		QuantumChannel[
			{{{Sqrt[3],0,0,0},{0,0,1,0},{0,0,Sqrt[3],0},{1,0,0,0}}/2,{1,0}},
			{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];
];


(* ::Subsubsection::Closed:: *)
(*From Kraus*)


With[{chan=QuantumChannel[
			{Sqrt[3]*{{1,0},{0,1}}/2,{{0,1},{1,0}}/2},
			{ChannelRep->Kraus,InputDim->2,OutputDim->2,Basis->"Col"}]},

TestCase[$RegisteredTests,"QuantumChannel:KrausToNotUnitary",
	SameQ[Unitary[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:KrausToSuper",
	SameQ[Super[chan],
		QuantumChannel[
			{{3,0,0,1},{0,3,1,0},{0,1,3,0},{1,0,0,3}}/4,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:KrausToPauliSuper",
	SameQ[Super[chan,Basis->"Pauli"],
		QuantumChannel[
			{{2,0,0,0},{0,2,0,0},{0,0,1,0},{0,0,0,1}}/2,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];
TestCase[$RegisteredTests,"QuantumChannel:KrausToChoi",
	SameQ[Choi[chan],
		QuantumChannel[
			{{3,0,0,3},{0,1,1,0},{0,1,1,0},{3,0,0,3}}/4,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:KrausToChi",
	SameQ[Choi[chan,Basis->"Pauli"],
		QuantumChannel[
			{{3,0,0,0},{0,1,0,0},{0,0,0,0},{0,0,0,0}}/2,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:KrausToKraus",
	SameQ[Kraus[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:KrausToStinespring",
	SameQ[Stinespring[chan],
		QuantumChannel[
			{{Sqrt[3],0},{0,1},{0,Sqrt[3]},{1,0}}/2,
			{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:KrausToSysEnv",
	SameQ[SysEnv[chan],
		QuantumChannel[
			{{{Sqrt[3],0,0,0},{0,0,1,0},{0,0,Sqrt[3],0},{1,0,0,0}}/2,{1,0}},
			{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];
];


(* ::Subsubsection::Closed:: *)
(*From Stinespring*)


With[{chan=QuantumChannel[
			{{Sqrt[3],0},{0,1},{0,Sqrt[3]},{1,0}}/2,
			{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}]},

TestCase[$RegisteredTests,"QuantumChannel:StinespringToNotUnitary",
	SameQ[Unitary[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToSuper",
	SameQ[Super[chan],
		QuantumChannel[
			{{3,0,0,1},{0,3,1,0},{0,1,3,0},{1,0,0,3}}/4,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToPauliSuper",
	SameQ[Super[chan,Basis->"Pauli"],
		QuantumChannel[
			{{2,0,0,0},{0,2,0,0},{0,0,1,0},{0,0,0,1}}/2,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];
TestCase[$RegisteredTests,"QuantumChannel:StinespringToChoi",
	SameQ[Choi[chan],
		QuantumChannel[
			{{3,0,0,3},{0,1,1,0},{0,1,1,0},{3,0,0,3}}/4,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToChi",
	SameQ[Choi[chan,Basis->"Pauli"],
		QuantumChannel[
			{{3,0,0,0},{0,1,0,0},{0,0,0,0},{0,0,0,0}}/2,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToKraus",
	SameQ[Kraus[chan],
		QuantumChannel[
			{Sqrt[3]*{{1,0},{0,1}}/2,{{0,1},{1,0}}/2},
			{ChannelRep->Kraus,InputDim->2,OutputDim->2,Basis->"Col"}]
	]];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToStinespring",
	SameQ[Stinespring[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToSysEnv",
	SameQ[SysEnv[chan],
		QuantumChannel[
			{{{Sqrt[3],0,0,0},{0,0,1,0},{0,0,Sqrt[3],0},{1,0,0,0}}/2,{1,0}},
			{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];
];


(* ::Subsubsection::Closed:: *)
(*From SysEnv*)


With[{chan=QuantumChannel[
			{{{Sqrt[3],0,0,0},{0,0,1,0},{0,0,Sqrt[3],0},{1,0,0,0}}/2,{1,0}},
			{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}]},

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToNotUnitary",
	SameQ[Unitary[chan],chan]
	];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToSuper",
	SameQ[Super[chan],
		QuantumChannel[
			{{3,0,0,1},{0,3,1,0},{0,1,3,0},{1,0,0,3}}/4,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToPauliSuper",
	SameQ[Super[chan,Basis->"Pauli"],
		QuantumChannel[
			{{2,0,0,0},{0,2,0,0},{0,0,1,0},{0,0,0,1}}/2,
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];
TestCase[$RegisteredTests,"QuantumChannel:SysEnvToChoi",
	SameQ[Choi[chan],
		QuantumChannel[
			{{3,0,0,3},{0,1,1,0},{0,1,1,0},{3,0,0,3}}/4,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToChi",
	SameQ[Choi[chan,Basis->"Pauli"],
		QuantumChannel[
			{{3,0,0,0},{0,1,0,0},{0,0,0,0},{0,0,0,0}}/2,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Pauli"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToKraus",
	SameQ[Kraus[chan],
		QuantumChannel[
			{Sqrt[3]*{{1,0},{0,1}}/2,{{0,1},{1,0}}/2},
			{ChannelRep->Kraus,InputDim->2,OutputDim->2,Basis->"Col"}]
	]];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToStinespring",
	SameQ[Stinespring[chan],
		QuantumChannel[
			{{Sqrt[3],0},{0,1},{0,Sqrt[3]},{1,0}}/2,
			{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}]]
	];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToSysEnv",
	SameQ[SysEnv[chan],chan]
	];
];


(* ::Subsubsection::Closed:: *)
(*Non-CP Transformations*)


With[{
	choi=QuantumChannel[
			{{-1,0,0,-1},{0,1,1,0},{0,1,1,0},{-1,0,0,-1}},
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}],
	super=QuantumChannel[
			{{-1,0,0,1},{0,-1,1,0},{0,1,-1,0},{1,0,0,-1}},
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}],
	kraus=QuantumChannel[
			{{{{-1,0},{0,-1}},{{0,1},{1,0}}},
			{{{1,0},{0,1}},{{0,1},{1,0}}}},
			{ChannelRep->Kraus,InputDim->2,OutputDim->2,Basis->"Col"}],
	stine=QuantumChannel[
			{{{-1,0},{0,1},{0,-1},{1,0}},{{1,0},{0,1},{0,1},{1,0}}},
			{ChannelRep->Stinespring,InputDim->2,OutputDim->2,Basis->"Col"}],
	syse=QuantumChannel[
			{{{{-1,0,0,0},{0,0,1,0},{0,0,-1,0},{1,0,0,0}},
			{{1,0,0,0},{0,0,1,0},{0,0,1,0},{1,0,0,0}}},{1,0}},
			{ChannelRep->SysEnv,InputDim->2,OutputDim->2,Basis->"Col"}],
	rho=Array["a",{2,2}]
		},

TestCase[$RegisteredTests,"QuantumChannel:ChoiToKrausNonCP",
	Kraus[choi]===kraus];

TestCase[$RegisteredTests,"QuantumChannel:ChoiToStinespringNonCP",
	Stinespring[choi]===stine];

TestCase[$RegisteredTests,"QuantumChannel:ChoiToSysEnvNonCP",
	SysEnv[choi]===syse];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToChoiNonCP",
	Choi[syse]===choi];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToSuperNonCP",
	Super[syse]===super];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToKrausNonCP",
	Kraus[syse]===kraus];

TestCase[$RegisteredTests,"QuantumChannel:SysEnvToStinespringNonCP",
	Stinespring[syse]===stine];

TestCase[$RegisteredTests,"QuantumChannel:KrausToChoiNonCP",
	Choi[kraus]===choi];

TestCase[$RegisteredTests,"QuantumChannel:KrausToSuperNonCP",
	Super[kraus]===super];

TestCase[$RegisteredTests,"QuantumChannel:KrausToSysEnvNonCP",
	SysEnv[kraus]===syse];

TestCase[$RegisteredTests,"QuantumChannel:KrausToStinespringNonCP",
	Stinespring[kraus]===stine];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToChoiNonCP",
	Choi[stine]===choi];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToSuperNonCP",
	Super[stine]===super];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToSysEnvNonCP",
	SysEnv[stine]===syse];

TestCase[$RegisteredTests,"QuantumChannel:StinespringToKrausNonCP",
	Kraus[stine]===kraus];
]


(* ::Subsection::Closed:: *)
(*Channel Operations*)


(* ::Subsubsection::Closed:: *)
(*Linear Operations*)


TestCase[$RegisteredTests,"QuantumChannel:Times",
	With[{op=Array["a",{4,4}]},
	And[
		10*Choi[op]===Choi[10*op],
		10*Super[op]===Super[10*op],
		10*Unitary[op]===Unitary[10*op]]
	]];


TestCase[$RegisteredTests,"QuantumChannel:Plus",
	With[{op=Array["a",{4,4}]},
	And[
		Super[op]+Super[op]===Super[op+op],
		Choi[op]+Choi[op]===Choi[op+op]
	]
	]];


TestCase[$RegisteredTests,"QuantumChannel:Dot",
	With[{
		op1=Array["a",{4,4}],
		op2=Array["b",{9,4}],
		op3=Array["c",{9,9}]},
	Super[op3].Super[op2].Super[op1]===Super[op3.op2.op1]
	]];


TestCase[$RegisteredTests,"QuantumChannel:CircleTimes",
	With[{X=PauliMatrix[1],Z=PauliMatrix[3],id=PauliMatrix[0]},
		CircleTimes[Super[KroneckerProduct[X,X]],
		Super[KroneckerProduct[Z,Z]],
		Super[KroneckerProduct[id,id]]]===
		Super[KroneckerProduct[X,Z,id,X,Z,id]]
	]];


(* ::Subsubsection::Closed:: *)
(*Conjugate Transpose*)


Module[{chan, op=Array["a",{4,4}]},
chan=QuantumChannel[op,
	{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}];

TestCase[$RegisteredTests,"QuantumChannel:Transpose",
	Transpose[chan]===
	QuantumChannel[Transpose@op,
		{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]
	];

TestCase[$RegisteredTests,"QuantumChannel:Conjugate",
	Conjugate[chan]===
	QuantumChannel[Conjugate@op,
		{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]
	];

TestCase[$RegisteredTests,"QuantumChannel:ConjugateTranspose",
	ConjugateTranspose[chan]===
	QuantumChannel[ConjugateTranspose@op,
		{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]
	];

TestCase[$RegisteredTests,"QuantumChannel:TrSuper",
	Tr[chan]===Tr[op]
	];
]


(* ::Subsubsection::Closed:: *)
(*Super ops*)


Module[{chan,op={{1,0,0,3},{0,1,3,0},{0,3,1,0},{3,0,0,1}}/4},

chan=QuantumChannel[op,
				{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}];

TestCase[$RegisteredTests,"QuantumChannel:MatrixPower",
	MatrixPower[chan,2]===
	QuantumChannel[MatrixPower[op,2],
		{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]
	];

TestCase[$RegisteredTests,"QuantumChannel:MatrixExp",
	MatrixExp[chan]===
	QuantumChannel[MatrixExp[op],
		{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]
	];

TestCase[$RegisteredTests,"QuantumChannel:MatrixLog",
	MatrixLog[chan]===
	QuantumChannel[MatrixLog[op],
		{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]
	];
]


(* ::Subsubsection::Closed:: *)
(*Eigenvalues*)


Module[{chan,op={{1,0,0,1},{0,3,3,0},{0,3,3,0},{1,0,0,1}}/4},

chan=QuantumChannel[op,
				{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}];

TestCase[$RegisteredTests,"QuantumChannel:Eigenvalues",
	Eigenvalues[chan]==={3/2,1/2,0,0}
	];

TestCase[$RegisteredTests,"QuantumChannel:Eigenvectors",
	Eigenvectors[chan]==={{0,1,1,0},{1,0,0,1},{-1,0,0,1},{0,-1,1,0}}
	];

TestCase[$RegisteredTests,"QuantumChannel:Eigensystem",
	Eigensystem[chan]==={{3/2,1/2,0,0},{{0,1,1,0},{1,0,0,1},{-1,0,0,1},{0,-1,1,0}}}
	];

TestCase[$RegisteredTests,"QuantumChannel:TrChoi",
	Tr[chan]===Tr[op]
	];

]


(* ::Subsubsection::Closed:: *)
(*Simplify Functions*)


TestCase[$RegisteredTests,"QuantumChannel:SimplifyFunctions",
	Module[{a,b},
	And[
		SameQ[
			FullSimplify[Super[{{Abs[a],0},{0,Abs[b]}}],a>=0&&b>=0],
			Super[{{a,0},{0,b}}]],
		SameQ[
			ExpToTrig[Super[{{Exp[I*a],0},{0,1}}]],
			Super[{{Cos[a]+I*Sin[a],0},{0,1}}]],
		SameQ[
			TrigToExp[Super[{{2*Cos[a],0},{0,1}}]],
			Super[{{Exp[I*a]+Exp[-I*a],0},{0,1}}]]
	]]];


(* ::Subsection::Closed:: *)
(*Channel Functions and Metrics*)


Module[{a,b,chan1,chan2},
	chan1=QuantumChannel[DiagonalMatrix@Array[a,4],
		{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}];
	chan2=QuantumChannel[DiagonalMatrix@Array[b,4],
		{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}];

TestCase[$RegisteredTests,"QuantumChannel:ProcessFidelity",
	And[
		ProcessFidelity[chan1]===Total@Array[a,4]/4,
		ProcessFidelity[chan1,chan2]===Array[a,4].Conjugate[Array[b,4]]/4
	]];


TestCase[$RegisteredTests,"QuantumChannel:GateFidelity",
	And[
	AllMatchQ[Abs[a[1]b[1]],
		Map[FullSimplify@GateFidelity[#,chan1,chan2]&,
			{{1,0},{{1},{0}},{{1,0},{0,0}}}]],
	{a[1],a[1],Abs[a[1]]}===
		Map[FullSimplify@GateFidelity[#,chan1]&,
			{{1,0},{{1},{0}},{{1,0},{0,0}}}]
	]];

TestCase[$RegisteredTests,"QuantumChannel:AverageGateFidelity",
	And[
		AllMatchQ[0,
			{FullSimplify[1/6 (2+Total@Array[a,4])-AverageGateFidelity[chan1]],
			FullSimplify[1/6 (2+Array[a,4].Conjugate[Array[b,4]])
				-AverageGateFidelity[chan1,chan2]]}]		
	]];

TestCase[$RegisteredTests,"QuantumChannel:EntanglementFidelity",
	And[
	AllMatchQ[a[1]Conjugate[b[1]],
		Map[FullSimplify@EntanglementFidelity[#,chan1,chan2]&,
			{{1,0},{{1},{0}},{{1,0},{0,0}}}]],
	AllMatchQ[a[1],
		Map[FullSimplify@EntanglementFidelity[#,chan1]&,
			{{1,0},{{1},{0}},{{1,0},{0,0}}}]
	]]];

];


TestCase[$RegisteredTests, "QuantumChannel:DiamondNormDistance",
	And[
		Module[{\[Theta]},FullSimplify[DiamondNormDistance[Unitary[MatrixExp[-I \[Theta] {{0,1},{1,0}}/2]]]-2*Sin[\[Theta]/2],Assumptions->0<=\[Theta]<2\[Pi]]==0]
	]
]


TestCase[$RegisteredTests, "QuantumChannel:Unitarity",
	And[
		Unitarity[
			QuantumChannel[{{0,1},{1,0}},
				{ChannelRep->Unitary,InputDim->2,OutputDim->2,Basis->"Col"}]
			] == 1,
		Unitarity[
			QuantumChannel[DiagonalMatrix[{1,0,0,0}],
				{ChannelRep->Super, InputDim->2, OutputDim->2, Basis -> "Pauli"}]
			] == 0,
		Unitarity[
			QuantumChannel[{
				{1, 0, 0, 1},
				{0, 0, 0, 0},
				{0, 0, 0, 0},
				{0, 0, 0, 0}},
				{ChannelRep->Super, InputDim->2, OutputDim->2, Basis -> "Pauli"}]
			] == 0,
		Unitarity[
			QuantumChannel[{Sqrt[1/2]{{1,0},{0,1}},Sqrt[1/2]{{0,1},{1,0}}},
				{ChannelRep->Kraus, InputDim->2, OutputDim->2, Basis->"Col"}]
			] == 1/3
	]
]


TestCase[$RegisteredTests,"QuantumChannel:ChannelVolume",
	Module[{a},
	FullSimplify[
		ChannelVolume[QuantumChannel[DiagonalMatrix@Array[a,4],
			{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Pauli"}]]-Sqrt[a[2]]Sqrt[a[3]]Sqrt[a[4]]]===0
	]];


(* ::Subsection::Closed:: *)
(*Channel Predicates*)


TestCase[$RegisteredTests,"QuantumChannel:CompletelyPositiveQ",
	And[
		Not@CompletelyPositiveQ@QuantumChannel[
			{{1,0,0,0},{0,0,1,0},{0,1,0,0},{0,0,0,1}},
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}],
		CompletelyPositiveQ@QuantumChannel[
			IdentityMatrix[4]/2,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]
	]];


TestCase[$RegisteredTests,"QuantumChannel:TracePreservingQ",
	And[
		TracePreservingQ@QuantumChannel[
			IdentityMatrix[4]/2,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}],
		Not@TracePreservingQ@QuantumChannel[
			{{1,0,0,0},{0,1,0,0},{0,0,0,0},{0,0,0,0}},
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]
	]];


TestCase[$RegisteredTests,"QuantumChannel:HermitianPreservingQ",
	And[
		HermitianPreservingQ@QuantumChannel[
			IdentityMatrix[4]/2,
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}],
		Not@HermitianPreservingQ@QuantumChannel[
			{{1,0,0,1},{0,0,0,0},{0,0,0,0},{0,0,0,1}},
			{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]
	]];


TestCase[$RegisteredTests,"QuantumChannel:UnitaryQ",
	Module[{a,b,p0,p1,p2,p3},
	And[
		UnitaryQ[Unitary[{{0,1},{1,0}}]],
		UnitaryQ[Kraus[{{{Cos[a],Sin[a]},{-Sin[a],Cos[a]}}}],Assumptions->a\[Element]Reals],
		UnitaryQ[Kraus[{{{a,b},{-b\[Conjugate],a\[Conjugate]}}}],Assumptions->Abs[a]^2+Abs[b]^2==1],
		Not@UnitaryQ[Kraus[{{{a,b},{-b\[Conjugate],a\[Conjugate]}}}],Assumptions->Abs[a]^2+Abs[b]^2==2],
		Not@UnitaryQ[Chi[DiagonalMatrix[{p0,p1,p2,p3}]]],
		UnitaryQ[Chi[DiagonalMatrix[{2,0,0,0}]]],
		UnitaryQ[Kraus[{{{-0.15655368690078914`+ 0.3430921066545964` I,-0.2348331888076826`- 0.05351542084751404` I,0.15434364097586445` -0.7761828151024415` I,0.4126091294603258` +0.05692281573291967` I},{-0.20161584271439267`+ 0.19817760822741248` I,-0.16484366732378541`- 0.4753872264482968` I,-0.3635752839999726`+ 0.43813126093787663` I,0.5287549284414077` +0.2513612431126086` I},{0.8487976714138722` +0.03598473246540806` I,-0.10339410593987132`- 0.09547646392229023` I,0.10895450613616803` -0.029258702335285096` I,0.05717734865987785` +0.49238715504781155` I},{-0.042404220777264696`- 0.23303418445576338` I,-0.6832106295507965`- 0.44971119932859627` I,-0.12711459846245918`- 0.14382980174948` I,-0.4835653853512809`- 0.06480284622646271` I}}}]],
		Not@UnitaryQ[Kraus[{{{0.062322322567051225` +0.4499103526192444` I,-0.0030534708049662967`+ 0.7475381561426192` I,-0.36536281946601035`- 0.3184085336398186` I},{-0.7251449050775644`- 0.06243481298569527` I,0.39685356189665594` +0.36971092879610457` I,0.2807315180487294` +0.31189365989100587` I},{0.5135328388272068` -0.0157285386936368` I,-0.03753268853883381`+ 0.3815622600004384` I,0.7607430120380767` +0.10152952864202755` I}}/Sqrt[2],{{-0.3385280862506501`- 0.11148473131686024` I,-0.09839679668003078`- 0.20068656570790783` I,0.7898784700625915` +0.44621168158367813` I},{0.47420455444381676` -0.7834563570746536` I,0.11075193848722416` +0.007811711947949143` I,0.2662964187271937` -0.2794376133502603` I},{0.18364835205881913` +0.023861043414192033` I,-0.444049899273856`- 0.8605438077054051` I,-0.14114688592590158`- 0.08980830675081206` I}}/Sqrt[2]}]]
	]]
	];


TestCase[$RegisteredTests,"QuantumChannel:UnitalQ",
	And[
		Not@UnitalQ[
			QuantumChannel[
				{{1,0,0,0},{0,0,0,0},{0,0,1,0},{0,0,0,0}},
				{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]],
		UnitalQ[
			QuantumChannel[
				DiagonalMatrix[{1/4,1/4,3/4,3/4}],
				{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]]
	]];


TestCase[$RegisteredTests,"QuantumChannel:PauliChannelQ",
	And[
		PauliChannelQ[
			QuantumChannel[
				DiagonalMatrix[{1/4,1/4,3/4,3/4}],
				{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Pauli"}]],
		Not@PauliChannelQ[
			QuantumChannel[
				DiagonalMatrix[{1/4,1/4,3/4,3/4}],
				{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]]
	]];


(* ::Subsection::Closed:: *)
(*Special Channels*)


TestCase[$RegisteredTests,"QuantumChannel:ComChannel",
	With[{comY=QuantumChannel[
				{{0,-I,-I,0},{I,0,0,-I},{I,0,0,-I},{0,I,I,0}},
				{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]},
	And[
		SameQ[ComChannel[PauliMatrix[2]],comY],
		SameQ[ComChannel[Unitary[PauliMatrix[2]]],comY]
	]]];


TestCase[$RegisteredTests,"QuantumChannel:AComChannel",
	With[{acomY=QuantumChannel[
				{{0,-I,I,0},{I,0,0,I},{-I,0,0,-I},{0,-I,I,0}},
				{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]},
	And[
		SameQ[AComChannel[PauliMatrix[2]],acomY],
		SameQ[AComChannel[Unitary[PauliMatrix[2]]],acomY]
	]]];


TestCase[$RegisteredTests,"QuantumChannel:LindbladDissipator",
	And[
		SameQ[
			LindbladDissipator[PauliMatrix[1]],
			QuantumChannel[
				{{-1,0,0,1},{0,-1,1,0},{0,1,-1,0},{1,0,0,-1}},
				{ChannelRep->Super,InputDim->2,OutputDim->2,Basis->"Col"}]
		],
		SameQ[
			LindbladDissipator[PauliMatrix[1],PauliMatrix[2]],
			LindbladDissipator[PauliMatrix[1]]+LindbladDissipator[PauliMatrix[2]]
		],
		SameQ[
			LindbladDissipator[{PauliMatrix[1],PauliMatrix[2]}],
			LindbladDissipator[PauliMatrix[1]]+LindbladDissipator[PauliMatrix[2]]
		]
	]];


TestCase[$RegisteredTests,"QuantumChannel:Lindblad",
	SameQ[
		Lindblad[PauliMatrix[1],PauliMatrix[2],{PauliMatrix[1],PauliMatrix[3]}],
		-I*ComChannel[PauliMatrix[1]+PauliMatrix[2]]
		+LindbladDissipator[PauliMatrix[1],PauliMatrix[3]]
	]];


TestCase[$RegisteredTests,"QuantumChannel:PartialTrChannel",
	With[{chan=PartialTrChannel[{2,2},{1}]},
	And[
		SameQ[
			ArrayRules[First@chan],
			{{1,1}->1,{1,11}->1,{2,2}->1,{2,12}->1,
			{3,5}->1,{3,15}->1,{4,6}->1,{4,16}->1,{_,_}->0}],
		SameQ[Part[chan,2],{ChannelRep->Super,InputDim->4,OutputDim->2,Basis->"Col"}]
	]]];


TestCase[$RegisteredTests,"QuantumChannel:FunctionChannel",
	And[
		SameQ[
			FunctionChannel[Tr,InputDim->3],
			QuantumChannel[
				IdentityMatrix[3],
				{ChannelRep->Choi,InputDim->3,OutputDim->1,Basis->"Col"}]
		],
		SameQ[
			FunctionChannel[Transpose,InputDim->2],
			QuantumChannel[
				{{1,0,0,0},{0,0,1,0},{0,1,0,0},{0,0,0,1}},
				{ChannelRep->Choi,InputDim->2,OutputDim->2,Basis->"Col"}]
		]
	]];


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


EndPackage[];
