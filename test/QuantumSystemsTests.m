(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Quantum Systems Unit Tests*)


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


BeginPackage["QuantumSystemsTests`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];
Needs["QuantumSystems`"];


(* ::Section::Closed:: *)
(*Results*)


Begin["`UnitTests`"];


$RegisteredTests={};
$TestResults := RunTest[$RegisteredTests];


End[];


(* ::Section:: *)
(*Unit Tests*)


Begin["`UnitTests`"];


(* ::Subsection:: *)
(*States and Operators*)


(* ::Subsubsection::Closed:: *)
(*Spin Operators*)


TestCase[$RegisteredTests,"QuantumSystems:SpinNumbers",
	And[
		Spin[0]===Spin["I"],
		Spin[1]===Spin["X"],
		Spin[2]===Spin["Y"],
		Spin[3]===Spin["Z"]
	]];


TestCase[$RegisteredTests,"QuantumSystems:SpinMatrix",
	And[
		Spin["I"][1/2]===IdentityMatrix[2],
		Spin["X"][1/2]==={{0,1},{1,0}}/2,
		Spin["Y"][1/2]==={{0,-I},{I,0}}/2,
		Spin["Z"][1/2]==={{1,0},{0,-1}}/2,
		Spin["P"][1/2]==={{0,1},{0,0}},
		Spin["M"][1/2]==={{0,0},{1,0}},
		Spin["I"][1]===IdentityMatrix[3],
		Spin["X"][1]==={{0,1,0},{1,0,1},{0,1,0}}/Sqrt[2],
		Spin["Y"][1]==={{0,-I,0},{I,0,-I},{0,I,0}}/Sqrt[2],
		Spin["Z"][1]==={{1,0,0},{0,0,0},{0,0,-1}},
		Spin["P"][1]==={{0,1,0},{0,0,1},{0,0,0}}*Sqrt[2],
		Spin["M"][1]==={{0,0,0},{1,0,0},{0,1,0}}*Sqrt[2],
		Spin["XX+YY-2ZZ"][1/2]==={{-1,0,0,0},{0,1,1,0},{0,1,1,0},{0,0,0,-1}}/2
	]];


TestCase[$RegisteredTests,"QuantumSystems:SpinSparseArray",
	And[
		Spin["I"][1/2,SparseArray]==IdentityMatrix[2,SparseArray],
		Spin["X"][1/2,SparseArray]==SparseArray[{{0,1},{1,0}}/2],
		Spin["Y"][1/2,SparseArray]==SparseArray[{{0,-I},{I,0}}/2],
		Spin["Z"][1/2,SparseArray]==SparseArray[{{1,0},{0,-1}}/2],
		Spin["P"][1/2,SparseArray]==SparseArray[{{0,1},{0,0}}],
		Spin["M"][1/2,SparseArray]==SparseArray[{{0,0},{1,0}}],
		Spin["I"][1,SparseArray]==IdentityMatrix[3,SparseArray],
		Spin["X"][1,SparseArray]==SparseArray[{{0,1,0},{1,0,1},{0,1,0}}/Sqrt[2]],
		Spin["Y"][1,SparseArray]==SparseArray[{{0,-I,0},{I,0,-I},{0,I,0}}/Sqrt[2]],
		Spin["Z"][1,SparseArray]==SparseArray[{{1,0,0},{0,0,0},{0,0,-1}}],
		Spin["P"][1,SparseArray]==SparseArray[{{0,1,0},{0,0,1},{0,0,0}}*Sqrt[2]],
		Spin["M"][1,SparseArray]==SparseArray[{{0,0,0},{1,0,0},{0,1,0}}*Sqrt[2]],
		Spin["XX+YY-2ZZ"][1/2,SparseArray]==SparseArray[{{-1,0,0,0},{0,1,1,0},{0,1,1,0},{0,0,0,-1}}/2]
	]];


(* ::Subsubsection::Closed:: *)
(*Cavity Operators*)


TestCase[$RegisteredTests,"QuantumSystems:CavityMatrix",
	And[
		Cavity["I"][2]===IdentityMatrix[2],
		Cavity["a"][2]==={{0,1},{0,0}},
		Cavity["c"][2]==={{0,0},{1,0}},
		Cavity["n"][2]==={{0,0},{0,1}},
		Cavity["I"][4]===IdentityMatrix[4],
		Cavity["a"][4]===DiagonalMatrix[Sqrt[{1,2,3}],1],
		Cavity["c"][4]===DiagonalMatrix[Sqrt[{1,2,3}],-1],
		Cavity["n"][4]===DiagonalMatrix[{0,1,2,3}],
		Cavity["ac+ca"][2]==={{0,0,0,0},{0,0,1,0},{0,1,0,0},{0,0,0,0}}
	]];


TestCase[$RegisteredTests,"QuantumSystems:CavitySparseArray",
	And[
		Cavity["I"][2,SparseArray]==IdentityMatrix[2,SparseArray],
		Cavity["a"][2,SparseArray]==SparseArray[{{0,1},{0,0}}],
		Cavity["c"][2,SparseArray]==SparseArray[{{0,0},{1,0}}],
		Cavity["n"][2,SparseArray]==SparseArray[{{0,0},{0,1}}],
		Cavity["I"][4,SparseArray]==IdentityMatrix[4,SparseArray],
		Cavity["a"][4,SparseArray]==SparseArray[DiagonalMatrix[Sqrt[{1,2,3}],1]],
		Cavity["c"][4,SparseArray]==SparseArray[DiagonalMatrix[Sqrt[{1,2,3}],-1]],
		Cavity["n"][4,SparseArray]==SparseArray[DiagonalMatrix[{0,1,2,3}]],
		Cavity["ac+ca"][2,SparseArray]==SparseArray[{{0,0,0,0},{0,0,1,0},{0,1,0,0},{0,0,0,0}}]
	]];


(* ::Subsubsection::Closed:: *)
(*Quantum States*)


TestCase[$RegisteredTests,"QuantumSystems:QStateBloch",
	And[
		QState[{"x","y","z"}]=={{1+"z","x"-I "y"},{"x"+I "y",1-"z"}}/2,
		QState[{"x1","y1","z2"},{"x2","y2","z2"}]=={
			{(1+"z2")^2,("x2"-I "y2") (1+"z2"),("x1"-I "y1") (1+"z2"),("x1"-I "y1") ("x2"-I "y2")},
			{("x2"+I "y2") (1+"z2"),(1-"z2") (1+"z2"),("x1"-I "y1") ("x2"+I "y2"),("x1"-I "y1") (1-"z2")},
			{("x1"+I "y1") (1+"z2"),("x1"+I "y1") ("x2"-I "y2"),(1-"z2") (1+"z2"),("x2"-I "y2") (1-"z2")},
			{("x1"+I "y1") ("x2"+I "y2"),("x1"+I "y1") (1-"z2"),("x2"+I "y2") (1-"z2"),(1-"z2")^2}}/4
	]];


TestCase[$RegisteredTests,"QuantumSystems:QStateVector",
	And[
		AllMatchQ[{1,0}, QState[#,VectorQ->True]&/@{"Zp","H"}],
		AllMatchQ[{0,1}, QState[#,VectorQ->True]&/@{"Zm","V"}],
		AllMatchQ[{1,1}/Sqrt[2], QState[#,VectorQ->True]&/@{"Xp","D"}],
		AllMatchQ[{1,-1}/Sqrt[2], QState[#,VectorQ->True]&/@{"Xm","A"}],
		AllMatchQ[{1,I}/Sqrt[2], QState[#,VectorQ->True]&/@{"Yp","R"}],
		AllMatchQ[{1,-I}/Sqrt[2], QState[#,VectorQ->True]&/@{"Ym","L"}],
		AllMatchQ[{1,0,0,1}/Sqrt[2],QState[#,VectorQ->True]&/@{"B1","Bell1"}],
		AllMatchQ[{0,1,1,0}/Sqrt[2],QState[#,VectorQ->True]&/@{"B2","Bell2"}],
		AllMatchQ[{0,1,-1,0}/Sqrt[2],QState[#,VectorQ->True]&/@{"B3","Bell3"}],
		AllMatchQ[{1,0,0,-1}/Sqrt[2],QState[#,VectorQ->True]&/@{"B4","Bell4"}]
	]];


TestCase[$RegisteredTests,"QuantumSystems:QStateGeneralVector",
	And[
		AllMatchQ[{{1},{0}}, QState[#,ColumnVectorQ->True]&/@{"Zp","H"}],
		AllMatchQ[{{0},{1}}, QState[#,ColumnVectorQ->True]&/@{"Zm","V"}],
		AllMatchQ[{{1},{1}}/Sqrt[2], QState[#,ColumnVectorQ->True]&/@{"Xp","D"}],
		AllMatchQ[{{1},{-1}}/Sqrt[2], QState[#,ColumnVectorQ->True]&/@{"Xm","A"}],
		AllMatchQ[{{1},{I}}/Sqrt[2], QState[#,ColumnVectorQ->True]&/@{"Yp","R"}],
		AllMatchQ[{{1},{-I}}/Sqrt[2], QState[#,ColumnVectorQ->True]&/@{"Ym","L"}],
		AllMatchQ[{{1},{0},{0},{1}}/Sqrt[2],QState[#,ColumnVectorQ->True]&/@{"B1","Bell1"}],
		AllMatchQ[{{0},{1},{1},{0}}/Sqrt[2],QState[#,ColumnVectorQ->True]&/@{"B2","Bell2"}],
		AllMatchQ[{{0},{1},{-1},{0}}/Sqrt[2],QState[#,ColumnVectorQ->True]&/@{"B3","Bell3"}],
		AllMatchQ[{{1},{0},{0},{-1}}/Sqrt[2],QState[#,ColumnVectorQ->True]&/@{"B4","Bell4"}]
	]];


TestCase[$RegisteredTests,"QuantumSystems:QStateDensity",
	And[
		QState["I"]==IdentityMatrix[2]/2,
		AllMatchQ[{{1,0},{0,0}}, QState[#]&/@{"Zp","H"}],
		AllMatchQ[{{0,0},{0,1}}, QState[#]&/@{"Zm","V"}],
		AllMatchQ[{{1,1},{1,1}}/2, QState[#]&/@{"Xp","D"}],
		AllMatchQ[{{1,-1},{-1,1}}/2, QState[#]&/@{"Xm","A"}],
		AllMatchQ[{{1,-I},{I,1}}/2, QState[#]&/@{"Yp","R"}],
		AllMatchQ[{{1,I},{-I,1}}/2, QState[#]&/@{"Ym","L"}],
		AllMatchQ[{{1,0,0,1},{0,0,0,0},{0,0,0,0},{1,0,0,1}}/2,
			QState[#]&/@{"B1","Bell1"}],
		AllMatchQ[{{0,0,0,0},{0,1,1,0},{0,1,1,0},{0,0,0,0}}/2, 
			QState[#]&/@{"B2","Bell2"}],
		AllMatchQ[{{0,0,0,0},{0,1,-1,0},{0,-1,1,0},{0,0,0,0}}/2,
			QState[#]&/@{"B3","Bell3"}],
		AllMatchQ[{{1,0,0,-1},{0,0,0,0},{0,0,0,0},{-1,0,0,1}}/2,
			QState[#]&/@{"B4","Bell4"}]
	]];


(* ::Subsubsection::Closed:: *)
(*Bra-Ket Notation*)


TestCase[$RegisteredTests,"QuantumSystems:KetForm",
	And[
		AllMatchQ[Ket[Subscript[0,2],Subscript[0,2]],KetForm/@{{1,0,0,0},{{1},{0},{0},{0}}}],
		KetForm[{{1,0,0,0}}]===Bra[Subscript[0,2],Subscript[0,2]],
		SameQ[KetForm[{"a",0,0,0,0,"b"},{2,3}],
			"a"*Ket[Subscript[0,2],Subscript[0,3]]+"b"*Ket[Subscript[1,2],Subscript[2,3]]],
		SameQ[KetForm[Array["a",{2,2}]],
			"a"[1,1]*KetBra[{Subscript[0,2]},{Subscript[0,2]}]+
			"a"[1,2]*KetBra[{Subscript[0,2]},{Subscript[1,2]}]+
			"a"[2,1]*KetBra[{Subscript[1,2]},{Subscript[0,2]}]+
			"a"[2,2]*KetBra[{Subscript[1,2]},{Subscript[1,2]}]]
	]];


(* ::Subsubsection:: *)
(*Vec Form*)


TestCase[$RegisteredTests,"QuantumSystems:VecForm",
	And[
		SameQ[{{1},{0},{0},{0}},VecForm@Ket[Subscript[0,2],Subscript[0,2]]],
		SameQ[{{1,0,0,0}},VecForm@Bra[Subscript[0,2],Subscript[0,2]]],
		SameQ[{{"a"},{0},{0},{0},{0},{"b"}},
			VecForm["a"*Ket[Subscript[0,2],Subscript[0,3]]+"b"*Ket[Subscript[1,2],Subscript[2,3]]]],
		SameQ[Array["a",{2,2}],VecForm[
			"a"[1,1]*KetBra[{Subscript[0,2]},{Subscript[0,2]}]+
			"a"[1,2]*KetBra[{Subscript[0,2]},{Subscript[1,2]}]+
			"a"[2,1]*KetBra[{Subscript[1,2]},{Subscript[0,2]}]+
			"a"[2,2]*KetBra[{Subscript[1,2]},{Subscript[1,2]}]]],
		SameQ[VecForm[Ket[Subscript[0,2],Subscript[0,2]].Bra[Subscript[0,2],Subscript[0,2]]],
			DiagonalMatrix[{1,0,0,0}]],
		SameQ[VecForm[Spin["Z"],Spin->1],{{1,0,0},{0,0,0},{0,0,-1}}],
		SameQ[VecForm[CircleTimes[Spin["P"],Cavity["a"]],Cavity->3],
			{{0,0,0,0,1,0},{0,0,0,0,0,Sqrt[2]},{0,0,0,0,0,0},
			{0,0,0,0,0,0},{0,0,0,0,0,0},{0,0,0,0,0,0}}]
	]];


(* ::Subsubsection:: *)
(*Rotating Frame Conversions*)


TestCase[$RegisteredTests, "QuantumSystems:RotatingFrameConversions:RotatingHamiltonian:TransformToItselfInvariant",
	Module[{BaseHamiltonian, RandomTime, RandomMatrixSize},
		RandomMatrixSize = RandomInteger[{1, 10}];
		BaseHamiltonian = RandomHermitian[RandomMatrixSize];
		RandomTime = RandomInteger[{1, 100}];
		Chop[EffectiveHamiltonian[BaseHamiltonian, BaseHamiltonian][RandomTime]] ==
			ConstantArray[0, {RandomMatrixSize, RandomMatrixSize}]
	]
];


TestCase[$RegisteredTests, "QuantumSystems:RotatingFrameConversions:RotatingHamiltonian:FunctionOfTimeInvariant",
	Module[{BaseHamiltonian, RandomTime, RandomMatrixSize, HamiltonianFunction},
		RandomMatrixSize = RandomInteger[{1, 10}];
		BaseHamiltonian = RandomHermitian[RandomMatrixSize];
		RandomTime = RandomInteger[{1, 100}];
		HamiltonianFunction = Function[{time}, BaseHamiltonian];
		Chop[EffectiveHamiltonian[HamiltonianFunction, BaseHamiltonian][RandomTime]] == 
		ConstantArray[0, {RandomMatrixSize, RandomMatrixSize}]
	]
];
		


(* ::Subsection::Closed:: *)
(*Symbolic Evaluation*)


TestCase[$RegisteredTests,"QuantumSystems:QExpand",
	SameQ[QExpand[Spin["3XX+2YY"]]
		3CircleTimes[Spin["X"],Spin["X"]]+2*CircleTimes[Spin["Y"],Spin["Y"]]],
	SameQ[QExpand[Cavity["ac-ca"]]
		CircleTimes[Spin["a"],Spin["c"]]-CircleTimes[Spin["c"],Spin["a"]]]
	];


TestCase[$RegisteredTests,"QuantumSystems:QSimplify:Spin",
	And[
		AllMatchQ[
			-I*Spin["Z"],
			{QSimplify[Com[Spin["Y"],Spin["X"]]],
			QSimplify[Com[Spin["Y"],Spin["X"]],"SpinAlgebra"->"PM"],
			QSimplify[Com[Spin["Y"],Spin["X"]],"SpinAlgebra"->"XY"]}],
		AllMatchQ[
			-I*Spin["X"],
			{QSimplify[Com[Spin["Z"],Spin["Y"]]],
			QSimplify[Com[Spin["Z"],Spin["Y"]],"SpinAlgebra"->"XY"]}],
		AllMatchQ[
			-I*Spin["Y"],
			{QSimplify[Com[Spin["X"],Spin["Z"]]],
			QSimplify[Com[Spin["X"],Spin["Z"]],"SpinAlgebra"->"XY"]}],
		AllMatchQ[
			Spin["P"],
			{QSimplify[Com[Spin["Z"],Spin["P"]]],
			QSimplify[Com[Spin["Z"],Spin["P"]],"SpinAlgebra"->"PM"]}],
		SameQ[
			Spin["X"]+I*Spin["Y"],
			QSimplify[Com[Spin["Z"],Spin["P"]],"SpinAlgebra"->"XY"]],
		AllMatchQ[
			-Spin["M"],
			{QSimplify[Com[Spin["Z"],Spin["M"]]],
			QSimplify[Com[Spin["Z"],Spin["M"]],"SpinAlgebra"->"PM"]}],
		SameQ[
			-Spin["X"]+I*Spin["Y"],
			QSimplify[Com[Spin["Z"],Spin["M"]],"SpinAlgebra"->"XY"]],
		AllMatchQ[
			Spin["I"]/4,
			QSimplify[Spin[#]^2,"SpinHalf"->True]&/@{"X","Y","Z"}],
		SameQ[
			QSimplify[Spin["M"].Spin["Z"],"OrderSpin"->True],
			Spin["Z"].Spin["M"]+Spin["M"]],
		SameQ[
			QSimplify[Spin["P"].Spin["Z"],"OrderSpin"->True],
			Spin["Z"].Spin["P"]-Spin["P"]],
		SameQ[
			QSimplify[Spin["M"].Spin["P"],"OrderSpin"->True],
			Spin["P"].Spin["M"]-2 Spin["Z"]]
	]];


TestCase[$RegisteredTests,"QuantumSystems:QSimplify:Cavity",
	And[
		QSimplify@Com[Cavity["c"],Cavity["a"]]===-Cavity["I"],
		QSimplify@Com[Cavity["a"],Cavity["n"]]===Cavity["a"],
		QSimplify@Com[Cavity["c"],Cavity["n"]]===-Cavity["c"],
		AllMatchQ[
			"\[Alpha]" Cavity["c"]+Cavity["a"] Conjugate["\[Alpha]"],
		QSimplify[Com[Cavity["n"],"\[Alpha]"Cavity["c"]-Conjugate["\[Alpha]"]Cavity["a"],#]]&/@{1,3}]
	]];


TestCase[$RegisteredTests,"QuantumSystems:QSimplify:SpinCavity",
	With[{
		h0="w1"Spin["Z"]\[CircleTimes]Cavity["I"]+"w2"Spin["I"]\[CircleTimes]Cavity["n"],
		hJC=QSimplify[ConjugateTranspose[#]+#&[Spin["P"]\[CircleTimes]Cavity["a"]]],
		hJCm=QSimplify[ConjugateTranspose[#]-#&[Spin["P"]\[CircleTimes]Cavity["a"]]]},
	And[
		QSimplify@Com[h0,hJC,2]==("w1"-"w2")^2*hJC,
		QSimplify@Com[h0,hJC,3]==-("w1"-"w2")^3*hJCm,
		QSimplify@Com[h0,hJC,4]==("w1"-"w2")^4*hJC
	]]];


TestCase[$RegisteredTests,"QuantumSystems:QSimplify:Algebra",
	And[
		QSimplify[CircleTimes["\[Omega]"*Op["a"],"\[Lambda]"*Op["b"]]]=="\[Omega]"*"\[Lambda]"*CircleTimes[Op["a"],Op["b"]],
		QSimplify[Dot["\[Omega]"*Op["a"],"\[Lambda]"*Op["b"]]]=="\[Omega]"*"\[Lambda]"*Dot[Op["a"],Op["b"]],
		QSimplify[Com["\[Omega]"*Op["a"],"\[Lambda]"*Op["b"]]]=="\[Omega]"*"\[Lambda]"*Com[Op["a"],Op["b"]],
		QSimplify[CircleTimes["\[Omega]"*Op["a"]+"\[Lambda]"*Op["b"],"\[Mu]"*Op["c"]]]=="\[Mu]"("\[Omega]"*CircleTimes[Op["a"],Op["c"]]+"\[Lambda]"*CircleTimes[Op["b"],Op["c"]]),
		QSimplify[Dot["\[Omega]"*Op["a"]+"\[Lambda]"*Op["b"],"\[Mu]"*Op["c"]]]=="\[Mu]"("\[Omega]"*Op["a"].Op["c"]+"\[Lambda]"*Op["b"].Op["c"]),
		QSimplify[Com["\[Omega]"*Op["a"]+"\[Lambda]"*Op["b"],"\[Mu]"*Op["c"]]]=="\[Mu]"("\[Omega]"*Com[Op["a"],Op["c"]]+"\[Lambda]"*Com[Op["b"],Op["c"]])
	]];


TestCase[$RegisteredTests,"QuantumSystems:QSimplifyRules",
	And[
		SameQ[
			QSimplify[Com[Dot[Op["I"]^3].Op["A"],Op["B"]],
				QSimplifyRules[Op,{Op["A"],Op["B"]},Identity->True,Com->{{Op["C"]}}]],
			Op["C"]],
		SameQ[
			QSimplify[Com[Dot[Op["id"]^3].Op["A"],Op["B"]],
				QSimplifyRules[Op,{Op["A"],Op["B"]},Com->{{Op["C"]}},Identity->Op["id"]]],
			Op["C"]],
		SameQ[
			QSimplify[Op["B"].Op["A"],
				QSimplifyRules[Op,{Op["A"],Op["B"]},Com->{{Op["C"]}},"NormalOrder"->True]],
			Op["A"].Op["B"]-Op["C"]],
		SameQ[
			QSimplify[Op["B"].Op["A"],
				QSimplifyRules[Op,{Op["A"],Op["B"]},Com->{{Op["C"]}},"NormalOrder"->{Op["B"],Op["A"]}]],
			Op["B"].Op["A"]]
	]];


(* ::Subsection::Closed:: *)
(*Quantum Gates*)


(* ::Text:: *)
(*Still need to test all the possible configurations*)


TestCase[$RegisteredTests,"QuantumSystems:CGate",
	And[
		AllMatchQ[
			{{1,0,0,0},{0,1,0,0},{0,0,0,1},{0,0,1,0}},
			{CGate[PauliMatrix[1],2,1],CGate[PauliMatrix[1],2,{1}],
			CGate[{PauliMatrix[1]},{2},1],CGate[{2,2},PauliMatrix[1],2,1],
			CGate[{2,2},PauliMatrix[1],2,{1}],CGate[{2,2},{PauliMatrix[1]},{2},1],
			CGate[PauliMatrix[1],2,1,Control->1],CGate[PauliMatrix[1],2,1,Control->{1}]
			CGate[{2,2},PauliMatrix[1],2,1,Control->1],CGate[{2,2},PauliMatrix[1],2,1,Control->{1}]}
		],
		AllMatchQ[
			{{1,0,0,0,0,0,0,0},{0,1,0,0,0,0,0,0},{0,0,1,0,0,0,0,0},{0,0,0,1,0,0,0,0},
			{0,0,0,0,1,0,0,0},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,0,1},{0,0,0,0,0,0,1,0}},
			{CGate[PauliMatrix[1],3,{1,2}],CGate[{PauliMatrix[1]},{3},{1,2}],
			CGate[{2,2,2},PauliMatrix[1],3,{1,2}],CGate[{2,2,2},{PauliMatrix[1]},{3},{1,2}],
			CGate[PauliMatrix[1],3,{1,2},Control->1],CGate[PauliMatrix[1],3,{1,2},Control->{1,1}],
			CGate[{2,2,2},PauliMatrix[1],3,{1,2},Control->1],CGate[{2,2,2},PauliMatrix[1],3,{1,2},Control->{1,1}]}
		],
		AllMatchQ[
			{{1,0,0,0,0,0,0,0},{0,1,0,0,0,0,0,0},{0,0,0,1,0,0,0,0},{0,0,1,0,0,0,0,0},
			{0,0,0,0,1,0,0,0},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,1}},
			{CGate[{2,2,2},PauliMatrix[1],3,{1,2},Control->{0,1}],
			CGate[PauliMatrix[1],3,{1,2},Control->{0,1}],
			CGate[{PauliMatrix[1]},{3},{1,2},Control->{0,1}]}
		],
		AllMatchQ[
			{{1,0,0,0,0,0,0,0},{0,1,0,0,0,0,0,0},{0,0,1,0,0,0,0,0},{0,0,0,1,0,0,0,0},
			{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,-1},{0,0,0,0,1,0,0,0},{0,0,0,0,0,-1,0,0}},
			{CGate[{PauliMatrix[1],PauliMatrix[3]},{2,3},1],
			CGate[{2,2,2},{PauliMatrix[1],PauliMatrix[3]},{2,3},1],
			CGate[{2,2,2},{PauliMatrix[1],PauliMatrix[3]},{2,3},{1}],
			CGate[{2,2,2},{PauliMatrix[1],PauliMatrix[3]},{2,3},1,Control->1]}
		]
	]];


(* ::Subsection::Closed:: *)
(*State Measures*)


TestCase[$RegisteredTests,"QuantumSystems:EntropyH",
	And[
		EntropyH[{1,1,1,1}/4]===2,
		EntropyH[{1,1}/2]===1,
		EntropyH[{1,0}]===0
	]];


TestCase[$RegisteredTests,"QuantumSystems:EntropyS",
	And[
		EntropyS[IdentityMatrix[2]/2]===1,
		EntropyS[DiagonalMatrix[{1,0}]]===0
	]];


TestCase[$RegisteredTests,"QuantumSystems:MutualInformationS",
	With[{bell={{1,0,0,1},{0,0,0,0},{0,0,0,0},{1,0,0,1}}/2,z={{1,0},{0,0}}},
	And[
		MutualInformationS[bell]===2,
		MutualInformationS[KroneckerProduct[z,z]]===0,
		MutualInformationS[KroneckerProduct[z,bell],{2,4}]===0,
		MutualInformationS[KroneckerProduct[z,bell],{4,2}]===2
	]]];


TestCase[$RegisteredTests,"QuantumSystems:RelativeEntropyS",
	And[
		RelativeEntropyS[{{1,0},{0,0}},{{1,1},{1,1}}/2]===DirectedInfinity[1],
		RelativeEntropyS[{{1,0},{0,0}},{{1,0},{0,1}}/2]===1,
		RelativeEntropyS[{{1,0},{0,1}}/2,{{1,0},{0,1}}/2]===0
	]];


TestCase[$RegisteredTests,"QuantumSystems:PNorm",
	And[
		0===Norm[PNorm[DiagonalMatrix[{3,1}/4],2]-Sqrt@Total[{9,1}/16]],
		1===PNorm[{{1-1/2,0},{0,-1/2}},1],
		2===PNorm[{{1,0},{0,-1}},1]
	]];


TestCase[$RegisteredTests,"QuantumSystems:Purity",
	And[
		Purity[IdentityMatrix[2]/2]===1/2,
		Purity[IdentityMatrix[2]/2,Normalize->True]===0,
		Purity[DiagonalMatrix[{1/3,2/3}]]===5/9,
		Purity[{{1,0},{0,0}}]===1
	]];


TestCase[$RegisteredTests,"QuantumSystems:Fidelity",
	With[{
		v1={1,0}, v2={1,1}/Sqrt[2], v3={0,1},
		m1={{1,0},{0,0}}, m2={{1,1},{1,1}}/2, m3={{0,0},{0,1}}},
	And[
		AllMatchQ[1/Sqrt[2],{Fidelity[v1,v2],Fidelity[v1,m2],Fidelity[m1,v2],Fidelity[m1,m2]}],
		AllMatchQ[1,{Fidelity[v2,v2],Fidelity[v2,m2],Fidelity[m2,v2],Fidelity[m2,m2]}],
		AllMatchQ[0,{Fidelity[v1,v3],Fidelity[v1,m3],Fidelity[m1,v3],Fidelity[m1,m3]}]
	]]];


TestCase[$RegisteredTests,"QuantumSystems:EntangledQ",
	And[
		EntangledQ[{{1,0,0,1},{0,0,0,0},{0,0,0,0},{1,0,0,1}}/2],
		Not@EntangledQ[DiagonalMatrix[{1,0,0,0}]]
	]];


TestCase[$RegisteredTests,"QuantumSystems:Concurrence",
	And[
		Concurrence[{{3,0,0,2},{0,1,0,0},{0,0,1,0},{2,0,0,3}}/8]===1/4,
		Concurrence[{{1,0,0,1},{0,0,0,0},{0,0,0,0},{1,0,0,1}}/2]===1,
		Concurrence[{{2,0,0,1},{0,1,0,0},{0,0,1,0},{1,0,0,2}}/6]===0
	]];


TestCase[$RegisteredTests,"QuantumSystems:EntanglementF",
	And[
		EntanglementF[{{1,0,0,1},{0,0,0,0},{0,0,0,0},{1,0,0,1}}/2]===1,
		EntanglementF[{{2,0,0,1},{0,1,0,0},{0,0,1,0},{1,0,0,2}}/6]===0
	]];


(* ::Subsection:: *)
(*Random Matrices*)


TestCase[$RegisteredTests,"QuantumSystems:RandomDensity",
	And[
		AllQ[And[Chop[Tr[#]-1]===0,AllQ[#>=0&,Chop@Eigenvalues[#]]]&,
			{RandomDensity[4],RandomDensity[4,1],
			RandomDensity[4,"HS"],RandomDensity[4,1,"HS"],
			RandomDensity[4,"Bures"],RandomDensity[4,1,"Bures"]}],
		Norm@Chop[Eigenvalues[RandomDensity[4,1]]-{1,0,0,0}]===0
	]];


TestCase[$RegisteredTests,"QuantumSystems:RandomUnitary",
	0===Norm@Chop[IdentityMatrix[4]-ConjugateTranspose[#].#&@RandomUnitary[4]]
	];


TestCase[$RegisteredTests,"QuantumSystems:RandomHermitian",
	0===Norm@Chop[#-ConjugateTranspose[#]&@RandomHermitian[4]]
	];


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


EndPackage[];
