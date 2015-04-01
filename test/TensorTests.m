(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Predicates Unit Tests*)


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


BeginPackage["TensorTests`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];
Needs["Tensor`"];


(* ::Section::Closed:: *)
(*Results*)


Begin["`UnitTests`"];


$RegisteredTests={};
$TestResults := RunTest[$RegisteredTests];


End[];


(* ::Section::Closed:: *)
(*Unit Tests*)


Begin["`UnitTests`"];


(* ::Subsection::Closed:: *)
(*Matrices and Operations*)


TestCase[$RegisteredTests,"Tensor:CircleTimes", 
	And[
		Module[{a},CircleTimes[a->3]===CircleTimes[a,a,a]],
		CircleTimes[{1,0},{1,0}]==={1,0,0,0},
		CircleTimes[{1,0},{{1,0},{0,0}}]==={{1,0},{0,0},{0,0},{0,0}},
		Module[{b},CircleTimes[3,b->2,{1,0}]===3*CircleTimes[b,b,{1,0}]]
	]];


TestCase[$RegisteredTests,"Tensor:\[DoubleStruckOne]", And[Subscript[\[DoubleStruckOne], 0]==={{}}, Subscript[\[DoubleStruckOne], 3]===IdentityMatrix[3]]];


TestCase[$RegisteredTests,"Tensor:BlockMatrix", 
	SameQ[
		BlockMatrix[{{1,1},{1,1}},{{2,2},{2,2}}],
		{{1,1,0,0},{1,1,0,0},{0,0,2,2},{0,0,2,2}}
	]];


TestCase[$RegisteredTests,"Tensor:UnitArray", UnitArray[{2,2},{1,1}]==={{1,0},{0,0}}];


TestCase[$RegisteredTests,"Tensor:TensorFactorPermutations", 
	Module[{a,b},
		AllMatchQ[
			CircleTimes[a,a,b]+CircleTimes[a,b,a]+CircleTimes[b,a,a],
			{TensorFactorPermutations[{a,2},{b,1}],
			TensorFactorPermutations[a->2,b->1]}]
		
	]];


TestCase[$RegisteredTests,"Tensor:SwapMatrix",
	And[
		SwapMatrix[2,{2,1}]==={{1,0,0,0},{0,0,1,0},{0,1,0,0},{0,0,0,1}},
		SwapMatrix[2,{2,1}]===SwapMatrix[{2,2},{2,1}]
	]];


TestCase[$RegisteredTests,"Tensor:Com",
	Module[{a,b},
	And[
		Com[a,b,1]===Com[a,b],
		Com[a,b,0]===b,
		AllMatchQ[0,{Com[1,b],Com[b,\[Pi]],Com[a,Zeta[3],3],Com[1/2,b,2]}],
		Com[PauliMatrix[1],PauliMatrix[2]]===2*I*PauliMatrix[3],
		Com[PauliMatrix[1],PauliMatrix[2],5]===32*I*PauliMatrix[3]
	]]];


TestCase[$RegisteredTests,"Tensor:ACom",
	Module[{a,b},
	And[
		ACom[a,b,1]===ACom[a,b],
		ACom[a,b,0]===b,
		Norm[ACom[PauliMatrix[1],PauliMatrix[2]]]===0,
		Norm[ACom[PauliMatrix[1],PauliMatrix[2],5]]===0
	]]];


TestCase[$RegisteredTests,"Tensor:OuterProduct", 
	Module[{a,b},
		SameQ[
			OuterProduct[Array[a,2],Array[b,2]],
			{{a[1] Conjugate[b[1]],a[1] Conjugate[b[2]]},
			{a[2] Conjugate[b[1]],a[2] Conjugate[b[2]]}}]
	]];


TestCase[$RegisteredTests,"Tensor:Projector", 
	With[{m={{1,0},{0,0}}},
		And[
			Projector[{1,0}]===m,
			Projector[{{1,0}}]===m,
			Projector[{{1},{0}}]===m
		]
	]];


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Manipulations*)


TestCase[$RegisteredTests,"Tensor:MatrixToTensor",
	Module[{a,b},
		SameQ[
			Dimensions[
			MatrixToTensor[
				KroneckerProduct[Array[a,{2,3}],Array[b,{4,5}]],
				{{2,4},{3,5}}]],
			{2,4,3,5}]
	]]; 


TestCase[$RegisteredTests,"Tensor:MatrixTranspose",
	SameQ[
		MatrixTranspose[IdentityMatrix[4],{2,2},{2,1}],
		{{1,0,0,0},{0,0,1,0},{0,1,0,0},{0,0,0,1}}]
	];


TestCase[$RegisteredTests,"Tensor:Swap",
	Module[{a,b},
		0===Norm[Swap[
				KroneckerProduct[Array[a,{2,2}],Array[b,{2,2}]],{2,1}]
				-KroneckerProduct[Array[b,{2,2}],Array[a,{2,2}]]]
	]];


TestCase[$RegisteredTests,"Tensor:Reshuffle",
	And[
		SameQ[
			Reshuffle[IdentityMatrix[4],{2,2,2,2}],
			{{1,0,0,1},{0,0,0,0},{0,0,0,0},{1,0,0,1}}],
		SameQ[
			Reshuffle[IdentityMatrix[4],{2,2,2,2},Basis->"Row"],
			{{1,0,0,1},{0,0,0,0},{0,0,0,0},{1,0,0,1}}]
	]];


TestCase[$RegisteredTests,"Tensor:Unravel",
	With[{X=PauliMatrix[1],Y=PauliMatrix[2],Z=PauliMatrix[3]},
	And[
		Unravel[KroneckerProduct[X,Y,Z,X,Y,Z],2]===KroneckerProduct[X,X,Y,Y,Z,Z],
		Unravel[KroneckerProduct[X,Y,Z,X,Y,Z],{2,2,2}]===KroneckerProduct[X,X,Y,Y,Z,Z],
		Unravel[KroneckerProduct[X,Y,Z,X,Y,Z]]===KroneckerProduct[X,X,Y,Y,Z,Z]
	]]];


TestCase[$RegisteredTests,"Tensor:Reravel",
	With[{X=PauliMatrix[1],Y=PauliMatrix[2],Z=PauliMatrix[3]},
	And[
		Reravel[KroneckerProduct[X,X,Y,Y,Z,Z],2]===KroneckerProduct[X,Y,Z,X,Y,Z],
		Reravel[KroneckerProduct[X,X,Y,Y,Z,Z],{2,2,2}]===KroneckerProduct[X,Y,Z,X,Y,Z],
		Reravel[KroneckerProduct[X,X,Y,Y,Z,Z]]===KroneckerProduct[X,Y,Z,X,Y,Z]
	]]];


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Contractions*)


TestCase[$RegisteredTests,"Tensor:PartialTr", 
	Module[{a,b},
		Norm@Simplify[
			PartialTr[KroneckerProduct[Array[a,{2,2}],Array[b,{3,3}]],{2,3},{2}]
			-Tr[Array[b,{3,3}]]*Array[a,{2,2}]
			]===0
	]];


TestCase[$RegisteredTests,"Tensor:TensorPairContract",
	Module[{a,b},
		SameQ[
			TensorPairContract[Array[a,{2,3}],Array[b,{3,4}],{{2,1}}],
			Array[a,{2,3}].Array[b,{3,4}]]
	]];


TestCase[$RegisteredTests,"Tensor:MatrixContract", 
	Module[{a,b},
		Norm@Simplify[
			MatrixContract[KroneckerProduct[Array[a,{2,2}],Array[b,{3,3}]],{2,3},{{2,4}}]
			-Tr[Array[b,{3,3}]]*Array[a,{2,2}]
			]===0
	]];


TestCase[$RegisteredTests,"Tensor:MatrixPairContract",
	Module[{a,b},
		Norm@Simplify[
			MatrixPairContract[
				{KroneckerProduct[Array[a,{2,2}],Array[b,{3,3}]],{2,3}},
				{{{1,0},{0,0}},{2}},{{1,1},{3,2}}]
			-a[1,1]*Array[b,{3,3}]
			]===0
	]];


(* ::Subsection::Closed:: *)
(*Matrix Bases*)


TestCase[$RegisteredTests,"Tensor:Basis",
	Basis["PO"]===PauliMatrix/@Range[0,3]];


TestCase[$RegisteredTests,"Tensor:BasisLabels",
	BasisLabels["PO",2]==={
		"I"\[CircleTimes]"I","I"\[CircleTimes]"X","I"\[CircleTimes]"Y","I"\[CircleTimes]"Z",
		"X"\[CircleTimes]"I","X"\[CircleTimes]"X","X"\[CircleTimes]"Y","X"\[CircleTimes]"Z",
		"Y"\[CircleTimes]"I","Y"\[CircleTimes]"X","Y"\[CircleTimes]"Y","Y"\[CircleTimes]"Z",
		"Z"\[CircleTimes]"I","Z"\[CircleTimes]"X","Z"\[CircleTimes]"Y","Z"\[CircleTimes]"Z"}];


TestCase[$RegisteredTests,"Tensor:ExpressInBasis",
	ExpressInBasis[PauliMatrix[1]]==={0,1,0,0}];


(* ::Subsection::Closed:: *)
(*Vectorization*)


TestCase[$RegisteredTests,"Tensor:Vec",
	Module[{a},
		And[
			Vec[{{a[1],a[2]},{a[3],a[4]}}]==={{a[1]},{a[3]},{a[2]},{a[4]}},
			Vec[{{a[1],a[2]},{a[3],a[4]}},Basis->"Row"]==={{a[1]},{a[2]},{a[3]},{a[4]}},
			Vec[PauliMatrix[1],Basis->"Pauli"]==={{0},{Sqrt[2]},{0},{0}}
		]
	]];


TestCase[$RegisteredTests,"Tensor:Devec",
	Module[{a},
		And[
			Devec[{{a[1]},{a[3]},{a[2]},{a[4]}}]==={{a[1],a[2]},{a[3],a[4]}},
			Devec[{{a[1]},{a[2]},{a[3]},{a[4]}},Basis->"Row"]==={{a[1],a[2]},{a[3],a[4]}},
			Devec[{{0},{Sqrt[2]},{0},{0}},Basis->"Pauli"]===PauliMatrix[1]
		]
	]];


TestCase[$RegisteredTests,"Tensor:ProductIdentity", 
	And[
		SameQ[
			ProductIdentity[PauliMatrix[1],PauliMatrix[2]],
			-1*KroneckerProduct[PauliMatrix[2],PauliMatrix[1]]],
		SameQ[
			ProductIdentity[PauliMatrix[1],PauliMatrix[2],Basis->"Row"],
			-1*KroneckerProduct[PauliMatrix[1],PauliMatrix[2]]]
	]];


TestCase[$RegisteredTests,"Tensor:BasisMatrix",
	BasisMatrix["Col"->"Pauli"]==={{1,0,0,1},{0,1,1,0},{0,-I,I,0},{1,0,0,-1}}/Sqrt[2]];


TestCase[$RegisteredTests,"Tensor:BasisTransformation",
	BasisTransformation[{0,1,1,0},"Col"->"Pauli"]==={0,Sqrt[2],0,0}];


(* ::Subsection::Closed:: *)
(*Tensor Product Parser*)


TestCase[$RegisteredTests,"Tensor:TP",
	And[
		TP["XXX"]===CircleTimes[PauliMatrix[1],PauliMatrix[1],PauliMatrix[1]],
		Module[{f},TP["ab",Replace->{"a"->"A","b"->"B"},Method->f]===f["A","B"]]
	]];


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


EndPackage[];
