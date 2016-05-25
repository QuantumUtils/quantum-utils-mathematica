(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Quantum Systems Package*)


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


BeginPackage["QuantumSystems`",{"QUDoc`","Predicates`","Tensor`"}];


Needs["QUDevTools`"]


$QuantumSystemsUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "QuantumSystems.nb"}]];


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection::Closed:: *)
(*States, Operators and Gates*)


Unprotect[Spin,Cavity,QState,CGate,KetForm,VecForm,Ket,Bra,KetBra];


AssignUsage[Spin,$QuantumSystemsUsages];
AssignUsage[Cavity,$QuantumSystemsUsages];
AssignUsage[QState,$QuantumSystemsUsages];
AssignUsage[KetForm,$QuantumSystemsUsages];
AssignUsage[VecForm,$QuantumSystemsUsages];
AssignUsage[CGate,$QuantumSystemsUsages];


(* ::Subsection::Closed:: *)
(*Symbolic Evaluation*)


Unprotect[Op,QPower,QExpand,QPowerExpand,QSimplifyRules,QSimplify,ClearQSimplifyCache];


AssignUsage[QExpand,$QuantumSystemsUsages];
AssignUsage[QPower,$QuantumSystemsUsages];
AssignUsage[QPowerExpand,$QuantumSystemsUsages];
AssignUsage[QSimplifyRules,$QuantumSystemsUsages];
AssignUsage[QSimplify,$QuantumSystemsUsages];
AssignUsage[ClearQSimplifyCache,$QuantumSystemsUsages];


(* ::Subsection::Closed:: *)
(*State Measures*)


Unprotect[EntropyH,EntropyS,RelativeEntropyS,MutualInformationS];
Unprotect[Purity,PNorm,Fidelity,EntangledQ,Concurrence,EntanglementF];


AssignUsage[EntropyH,$QuantumSystemsUsages];
AssignUsage[EntropyS,$QuantumSystemsUsages];
AssignUsage[RelativeEntropyS,$QuantumSystemsUsages];
AssignUsage[MutualInformationS,$QuantumSystemsUsages];


AssignUsage[Purity,$QuantumSystemsUsages];
AssignUsage[PNorm,$QuantumSystemsUsages];
AssignUsage[Fidelity,$QuantumSystemsUsages];


AssignUsage[EntangledQ,$QuantumSystemsUsages];
AssignUsage[Concurrence,$QuantumSystemsUsages];
AssignUsage[EntanglementF,$QuantumSystemsUsages];


(* ::Subsection::Closed:: *)
(*Random Matrices*)


Unprotect[RandomUnitary,RandomDensity,RandomHermitian];


AssignUsage[RandomUnitary,$QuantumSystemsUsages];
AssignUsage[RandomDensity,$QuantumSystemsUsages];
AssignUsage[RandomHermitian,$QuantumSystemsUsages];


(* ::Subsection::Closed:: *)
(*Error Messages*)


(* ::Subsubsection::Closed:: *)
(*States and Operators*)


Spin::spin = "Total spin value must be an non-negative integer or half-integer; `1` received.";
Cavity::dim = "Cavity dimension must be a positive integer.";


VecForm::fail = "Unable to parse input.";
KetForm::input = "Input must be a rank 1 or 2 array.";


(* ::Subsubsection::Closed:: *)
(*Symbolic Evaluation*)


QSimplifyRules::alg = "Commutator algebra matrix must be upper triangular, lower triangular, or skew-symmetric.";
QSimplifyRules::ragls = "Invalid algebra input list.";
QSimplifyRules::comlen = "Commutator algebra matrix does not match number of operators.";
QSimplifyRules::dotlen = "Dot matrix does not match number of operators.";


(* ::Subsubsection::Closed:: *)
(*Quantum Gates*)


CGate::dims = "Input dimensions must be an integer or list of integers";
CGate::targctrl = "Targets and Control subsystem lists must not intersect.";
CGate::gates = "Gate must be matrix or a list of matrices of same length as list of targets.";
CGate::ctrlval = "Control values must be integer or list of integers same length as controls list.";
CGate::ctrldim = "The dimension of the control system is must be greater than 1."


(* ::Subsubsection::Closed:: *)
(*Measures*)


Fidelity::input = "Input must be satisfy either SquareMatrixQ or GeneralVectorQ.";


EntropyS::sqmat = "Input must be a square matrix.";
MutualInformationS::sqmat = "Input must be a square matrix.";
MutualInformationS::dims = "Computed subsystem dimensions not integers manually specify dimensions.";
RelativeEntropyS::sqmat = "Input must be a square matrix.";


Purity::sqmat = "Input must be a square matrix.";


EntangledQ::input = "Input must be satisfy either SquareMatrixQ or GeneralVectorQ.";


Concurrence::input = "Input must be satisfy either SquareMatrixQ or GeneralVectorQ.";
Concurrence::dim = "Concurrence currently only works for 2-qubit states.";
EntanglementF::input = "Input must be satisfy either SquareMatrixQ or GeneralVectorQ.";
EntanglementF::dim = "Concurrence currently only works for 2-qubit states.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*States and Operators*)


(* ::Subsubsection::Closed:: *)
(*Spin Operators*)


SpinQ[S_]:=IntegerQ[2*S]&&NonNegative[2*S]


Clear[SpinZ,SpinP]

SpinZ[S_]:=SpinZ[S]=DiagonalMatrix[SparseArray[Range[S,-S,-1]]];

SpinP[S_]:=SpinP[S]=SparseArray[{#,#+1}->Sqrt[(1+2 S-#) #]&/@Range[2S],{2S+1,2S+1}];

SpinM[S_]:=Transpose[SpinP[S]];

SpinX[S_]:=(SpinP[S]+SpinM[S])/2

SpinY[S_]:=(SpinM[S]-SpinP[S])*I/2

SpinI[S_]:=IdentityMatrix[2S+1,SparseArray];


Clear[SpinTPRules]
SpinTPRules[S_]:= SpinTPRules[S] = 
	MapThread[Rule,
			{{"I","X","Y","Z","P","M"},
			If[S==="Symbolic",
				{Spin["I"],Spin["X"],Spin["Y"],Spin["Z"],Spin["P"],Spin["M"]},
				{SpinI[S],SpinX[S],SpinY[S],SpinZ[S],SpinP[S],SpinM[S]}
			]}
	]


SetAttributes[Spin,HoldAllComplete]

Spin[expr_][S_,SparseArray]:=With[
	{spin=Rationalize[S]},
	If[SpinQ[spin],
		TP[expr,Replace->SpinTPRules[S]],
		Message[Spin::spin,spin]
	]]

Spin[expr_][S_]:=Normal[Spin[expr][S,SparseArray]]
Spin[expr_][S_,Identity]:=Spin[expr][S]
Spin[expr_]["Symbolic"]:=TP[expr,Replace->SpinTPRules["Symbolic"]]


(* ::Text:: *)
(*Short hand for single spin operators*)


Spin[0]:=Spin["I"]
Spin[1]:=Spin["X"]
Spin[2]:=Spin["Y"]
Spin[3]:=Spin["Z"]


(* ::Text:: *)
(*Display Formatting*)


SpinFormatting[Spin[op_]]:=
	With[{str=ToString@Unevaluated[op]},
		Which[
			str=="P", Subscript[Style["S",Italic],"+"],
			str=="M", Subscript[Style["S",Italic],"-"],
			str=="X", Subscript[Style["S",Italic],"x"],
			str=="Y", Subscript[Style["S",Italic],"y"],
			str=="Z", Subscript[Style["S",Italic],"z"],
			str=="I", Subscript["\[DoubleStruckOne]","s"],
			True,"Spin"[str]
	]]


Format[Spin[op_],TraditionalForm]:=SpinFormatting[Spin[op]]


(* ::Subsubsection::Closed:: *)
(*Cavity Operators*)


Clear[CavityA,CavityN]
CavityI[n_Integer]:= IdentityMatrix[n,SparseArray]
CavityA[n_Integer]:= CavityA[n] = DiagonalMatrix[SparseArray@Sqrt[Range[n-1]],1]
CavityC[n_Integer]:= Transpose[CavityA[n]]
CavityN[n_Integer]:= CavityN[n] = DiagonalMatrix[SparseArray@Range[0,n-1]]


CavityTPRules[n_]:= CavityTPRules[n] =	 
	MapThread[Rule,
			{{"I","a","c","n","N"},
			If[n==="Symbolic",
				{Cavity["I"],Cavity["a"],Cavity["c"],Cavity["n"],Cavity["n"]},
				{CavityI[n],CavityA[n],CavityC[n],CavityN[n],CavityN[n]}
			]}
	]


SetAttributes[Cavity,HoldAllComplete]
Cavity[expr_][n_Integer,SparseArray]:=
	If[Positive[n],
		TP[expr,Replace->CavityTPRules[n]],
		Message[Cavity::int]
	]

Cavity[expr_][n_Integer]:=Normal[Cavity[expr][n,SparseArray]]
Cavity[expr_][n_Integer,Identity]:=Cavity[expr][n]
Cavity[expr_]["Symbolic"]:=TP[expr,Replace->CavityTPRules["Symbolic"]]


(* ::Text:: *)
(*Display Formatting*)


CavityFormatting[Cavity[op_]]:=
	With[{str=ToString@Unevaluated[op]},
		Which[
			str=="a", Style["a",Italic],
			str=="c", Superscript[Style["a",Italic],"\[Dagger]"],
			str=="n"||str=="N", Style["N",Italic],
			str=="I", Subscript["\[DoubleStruckOne]","c"],
			True, "Cavity"[str]
	]]


Format[Cavity[op_],TraditionalForm]:=CavityFormatting[Cavity[op]]


(* ::Subsubsection::Closed:: *)
(*Quantum States*)


SetAttributes[QState,HoldAllComplete];


Options[QState]:={VectorQ->False, ColumnVectorQ->False};


QState[expr__,opts:OptionsPattern[]]:=
	Which[
		AllMatchQ[{_,_,_},{expr}],
			CircleTimes@@Map[QStateBloch,{expr}],
		TrueQ[OptionValue[VectorQ]],
			TP[Unevaluated[expr],Replace->$QStateVecRules],
		TrueQ[OptionValue[ColumnVectorQ]],
			Partition[TP[Unevaluated[expr],Replace->$QStateVecRules],1],
		True,TP[Unevaluated[expr],Replace->$QStateMatRules]
	]


QStateBloch[{x_,y_,z_}]:={{1+z,x-I*y},{x+I*y,1-z}}/2;


(* ::Text:: *)
(*Rules for state vectors*)


$QStateVecZp={1,0};
$QStateVecZm={0,1};
$QStateVecXp={1,1}/Sqrt[2];
$QStateVecXm={1,-1}/Sqrt[2];
$QStateVecYp={1,I}/Sqrt[2];
$QStateVecYm={1,-I}/Sqrt[2];
$QStateVecBell1={1,0,0,1}/Sqrt[2];
$QStateVecBell2={0,1,1,0}/Sqrt[2];
$QStateVecBell3={0,1,-1,0}/Sqrt[2];
$QStateVecBell4={1,0,0,-1}/Sqrt[2];


$QStateVecRules={
"Zp"->$QStateVecZp, "H"->$QStateVecZp,
"Zm"->$QStateVecZm, "V"->$QStateVecZm,
"Xp"->$QStateVecXp, "D"->$QStateVecXp,
"Xm"->$QStateVecXm, "A"->$QStateVecXm,
"Yp"->$QStateVecYp, "R"->$QStateVecYp,
"Ym"->$QStateVecYm, "L"->$QStateVecYm,
"Bell1"->$QStateVecBell1, "B1"->$QStateVecBell1,
"Bell2"->$QStateVecBell2, "B2"->$QStateVecBell2,
"Bell3"->$QStateVecBell3, "B3"->$QStateVecBell3,
"Bell4"->$QStateVecBell4, "B4"->$QStateVecBell4};


(* ::Text:: *)
(*Rules for density matrices*)


$QStateMatZp=Projector[$QStateVecZp];
$QStateMatZm=Projector[$QStateVecZm];
$QStateMatXp=Projector[$QStateVecXp];
$QStateMatXm=Projector[$QStateVecXm];
$QStateMatYp=Projector[$QStateVecYp];
$QStateMatYm=Projector[$QStateVecYm];
$QStateMatBell1=Projector[$QStateVecBell1];
$QStateMatBell2=Projector[$QStateVecBell2];
$QStateMatBell3=Projector[$QStateVecBell3];
$QStateMatBell4=Projector[$QStateVecBell4];
$QStateMixed={{1/2,0},{0,1/2}};


$QStateMatRules={
"I"->$QStateMixed,
"Zp"->$QStateMatZp, "H"->$QStateMatZp,
"Zm"->$QStateMatZm, "V"->$QStateMatZm,
"Xp"->$QStateMatXp, "D"->$QStateMatXp,
"Xm"->$QStateMatXm, "A"->$QStateMatXm,
"Yp"->$QStateMatYp, "R"->$QStateMatYp,
"Ym"->$QStateMatYm, "L"->$QStateMatYm,
"Bell1"->$QStateMatBell1, "B1"->$QStateMatBell1,
"Bell2"->$QStateMatBell2, "B2"->$QStateMatBell2,
"Bell3"->$QStateMatBell3, "B3"->$QStateMatBell3,
"Bell4"->$QStateMatBell4, "B4"->$QStateMatBell4};


(* ::Subsubsection::Closed:: *)
(*Bra-Ket Notation*)


(* ::Text:: *)
(*Converting arrays to Bra-Ket Notation.*)


KetForm[op_,subdims___]:=
	With[{dims=Dimensions[op]},
	Which[
		GeneralVectorQ[op],
			KetFormVector[Ket,op,subdims],
		RowVectorQ[op],
			KetFormVector[Bra,op,subdims],
		MatrixQ[op],
			KetFormMatrix[op,subdims],
		True,
			Message[KetForm::input]
	]]


(* ::Text:: *)
(*Display Formatting*)


Format[KetBra[{a__},{b__}]]:=
	DisplayForm[
	RowBox[{"\[LeftBracketingBar]",
		Sequence@@Map[AdjustmentBox[#,BoxMargins->{{0,0},{0.5,0.5}}]&,{a}],
		AdjustmentBox["\[RightAngleBracket]",BoxMargins->{{0,-0.5},{0,0}}],
		"\[LeftAngleBracket]",
		Sequence@@Map[AdjustmentBox[#,BoxMargins->{{0,0},{0.5,0.5}}]&,{b}],
		"\[RightBracketingBar]"}]]


(* ::Text:: *)
(*Conversion Utility functions*)


KetBasis[ds_List]:=Tuples@Map[Table[Subscript[i, #],{i,0,#-1}]&,ds]


KetAutoDim[d_]:=
	Which[
		IntegerQ[Log[2,d]],
			ConstantArray[2,Log[2,d]],
		IntegerQ[Log[3,d]],
			ConstantArray[3,Log[3,d]],
		True,d]


(* ::Text:: *)
(*Converting vectors to KetForm*)


KetFormVector[ket_,vec_,ds_List]:=Flatten[vec].(ket@@@KetBasis[ds]);
KetFormVector[ket_,vec_,d_Integer]:=
	With[{ds=ConstantArray[d,Log[d,Length[Flatten[vec]]]]},
		KetFormVector[ket,vec,ds]
	]
KetFormVector[ket_,vec_]:=
	With[{d=KetAutoDim[Length[Flatten[vec]]]},
		KetFormVector[ket,vec,d]
	]


(* ::Text:: *)
(*Converting matrices to KetForm*)


KetFormMatrix[mat_,dsL_List,dsR_List]:=
Flatten[mat].(KetBra@@@Tuples[{KetBasis[dsL],KetBasis[dsR]}]);

KetFormMatrix[mat_,ds_List]:=KetFormMatrix[mat,ds,ds];

KetFormMatrix[mat_,d_Integer]:=
	With[{n=Log[d,Length[mat]]},
		If[IntegerQ[Log[d,Length[mat]]],
			KetFormMatrix[mat,ConstantArray[d,n]],
			KetFormMatrix[mat,{First[Dimensions[mat]]},{Last[Dimensions[mat]]}]]
	];

KetFormMatrix[mat_]:=
	With[{dims=KetAutoDim/@Dimensions[mat]},
		KetFormMatrix[mat,Sequence@@dims]
	]


(* ::Subsubsection::Closed:: *)
(*Vec Form*)


(* ::Text:: *)
(*Converting Spin, Cavity and Bra-Ket notation to arrays.*)


KetDimensions[Subscript[num_,dim_]]:={dim,1+num};
KetDimensions[num_]:={2,1+num};


Options[VecForm]:={Spin->1/2,Cavity->2,SparseArray->False};


VecForm[obj_,opts:OptionsPattern[VecForm]]:=
	With[{f=If[OptionValue[SparseArray],SparseArray,Identity]},
	Which[
		(* Linear Algebra *)
		NumericQ[obj],obj,
		ListQ[obj],f@obj,
		StringQ[obj],obj,
		MatchQ[obj,Plus[_,__]],
			VecForm[First[obj],opts]+VecForm[Rest[obj],opts],
		MatchQ[obj,Times[_?CoefficientQ,__]],
			obj/.{Times[a_?CoefficientQ,b__]:>Times[a,VecForm[Times[b],opts]]},
		MatchQ[obj,CircleTimes[_,__]],
			CircleTimes@@Map[VecForm[#,opts]&,List@@obj],
		MatchQ[obj,Dot[_,__]],
			Dot@@Map[VecForm[#,opts]&,List@@obj],
		MatchQ[obj,QPower[_,_]],
			MatrixPower[VecForm[First@obj,opts],VecForm[Last@obj,opts]],
		(* Bra-Ket *)
		MatchQ[obj,Ket[{__}]],
			Partition[CircleTimes@@Apply[Composition[f,UnitVector],Map[KetDimensions,First@obj],{1}],1],
		MatchQ[obj,Ket[__]],
			Partition[CircleTimes@@Apply[Composition[f,UnitVector],Map[KetDimensions,List@@obj],{1}],1],
		MatchQ[obj,Bra[{__}]],
			ConjugateTranspose[VecForm[Ket@@First[obj],opts]],
		MatchQ[obj,Bra[__]],
			ConjugateTranspose[VecForm[Ket@@obj,opts]],
		MatchQ[obj,KetBra[{__},{__}]],
			CircleTimes[VecForm[Ket@@First[obj]],VecForm[Bra@@Last[obj],opts]],
		(* Spin-Cavity *)
		MatchQ[obj,Spin[__]],
			obj[OptionValue[Spin],f],
		MatchQ[obj,Cavity[__]],
			obj[OptionValue[Cavity],f],
		(* Symbolic *)
		CoefficientQ[obj],obj,
		(* Failure *)
		True,
			Message[VecForm::fail]
]]

VecForm[a__,opts:OptionsPattern[VecForm]]:=Map[VecForm[#,opts]&,{a}]


(* ::Subsection::Closed:: *)
(*Symbolic Evaluation*)


(* ::Subsubsection::Closed:: *)
(*Symbolic Operators*)


(* ::Text:: *)
(*Use container Op for symbolic operators*)


(* ::Text:: *)
(*QPower for powers of symbolic operators*)


QPower[a_]:=a
QPower[arg_,1]:=arg
QPower[arg_?CoefficientQ,n_]:=Power[arg,n]
QPower[arg_?MatrixQ,n_]:=MatrixPower[arg,n]
QPower[QPower[arg_,m_],n_]:=QPower[arg,m+n]
QPower[Times[x_?CoefficientQ,xs__],n_]:=QPower[x,n]*QPower[Times[xs],n]
QPower[Times[xs__,x_?CoefficientQ],n_]:=QPower[x,n]*QPower[Times[xs],n]
QPower[CircleTimes[a_,b__],n_]:=CircleTimes[QPower[a,n],QPower[CircleTimes[b],n]]


(* ::Text:: *)
(*Hide QPower from displayed equations*)


Format[QPower[arg_,n_]]:=arg^n;


(* ::Text:: *)
(*Parsing symbolic Spin-Cavity expressions*)


QExpand[expr_]:=ReplaceRepeated[expr,{Spin[arg_]:> Spin[arg]["Symbolic"],Cavity[arg_]:> Cavity[arg]["Symbolic"]}]


(* ::Subsubsection::Closed:: *)
(*QSimplify*)


Options[QSimplify]:={
	Spin->True,
	Cavity->True,
	"SpinAlgebra"->Automatic,
	"CavityAlgebra"->"n",
	"SpinHalf"->False,
	"OrderSpin"->False,
	"OrderCavity"->True,
	TimeConstraint->1,
	QPowerExpand->True};


QSimplify[expr_,rules_?ListQ,opts:OptionsPattern[QSimplify]]:= QSimplifyCached[expr,rules,opts]
QSimplify[expr_,opts:OptionsPattern[QSimplify]]:= QSimplify[expr,{},opts]


QSimplify[Com[a_,b_,n_?Positive],rules_?ListQ,opts:OptionsPattern[]]:=QSimplify[Com[a,QSimplify[Com[a,b],opts],n-1],rules,opts]
QSimplify[ACom[a_,b_,n_?Positive],rules_?ListQ,opts:OptionsPattern[]]:=QSimplify[ACom[a,QSimplify[ACom[a,b],opts],n-1],rules,opts]


QPowerExpand[expr_]:=expr//.{QPower[Plus[a_,b__],n_Integer]:>Dot@@ConstantArray[Plus[a,b],n]}
QPowerExpand[expr_,nMax_Integer]:=expr//.{QPower[Plus[a_,b__],n_?(IntegerQ[#]&&#<=nMax&)]:>Dot@@ConstantArray[Plus[a,b],n]}


(* ::Text:: *)
(*Memoized function for QSimplify*)


Clear[QSimplifyCached];


QSimplifyCached[expr_,rules_,opts:OptionsPattern[QSimplify]]:=
	QSimplifyCached[expr,rules,opts]=
	With[{
		replaceRules=Join[QSimplifyDefaultRules[opts],rules],
		qexpand=OptionValue[QPowerExpand]},
	With[{
		OuterSimplify=Which[
						MemberQ[{True,All,\[Infinity]},qexpand],
							Simplify[QPowerExpand[#],TimeConstraint->OptionValue[TimeConstraint]]&,
						IntegerQ[qexpand],
							Simplify[QPowerExpand[#,qexpand],TimeConstraint->OptionValue[TimeConstraint]]&,
						True,
							Simplify[#,TimeConstraint->OptionValue[TimeConstraint]]&
						]},
		FixedPoint[
			OuterSimplify@ReplaceRepeated[#,Join[QSimplifyDefaultRules[opts],rules]]&,
			Simplify[QExpand[expr],TimeConstraint->OptionValue[TimeConstraint]]
		]
	]]


(* ::Text:: *)
(*Function to clear cached QSimplify values*)


ClearQSimplifyCache[]:=(
	DownValues[QSimplifyCached]=Part[DownValues[QSimplifyCached],{-1}];
	DownValues[QSimplifyDefaultRules]=Part[DownValues[QSimplifyDefaultRules],{-1}];)


(* ::Text:: *)
(*Default replacement rules for QSimplify*)


Clear[QSimplifyDefaultRules]


QSimplifyDefaultRules[opts:OptionsPattern[QSimplify]]:=
	QSimplifyDefaultRules[opts]=
		Join[
			$QSimplifyLinearAlgebra,
			QSimplifySpinRules[opts],
			QSimplifyCavityRules[opts],
			QSimplifyPower[Op]
		]


QSimplifySpinRules[opts:OptionsPattern[QSimplify]]:=
	If[OptionValue[Spin],
		Join[
			If[OptionValue["SpinHalf"],$QSimplifySpinHalf,{}],
			Which[
				OptionValue["SpinAlgebra"]==="PM", $QSimplifySpinPM,
				OptionValue["SpinAlgebra"]==="XY", $QSimplifySpinXY,
				True,{}],
			$QSimplifySpin,
			If[OptionValue["OrderSpin"],$QSimplifySpinOrdering,{}]
		],{}]


QSimplifyCavityRules[opts:OptionsPattern[QSimplify]]:=
	If[OptionValue[Cavity],
		Join[
			If[OptionValue["OrderCavity"],$QSimplifyCavityOrdering,{}],
			Which[
				OptionValue["CavityAlgebra"]==="n", $QSimplifyCavityN,
				OptionValue["CavityAlgebra"]==="ac", $QSimplifyCavityAC,
				True,{}],
			$QSimplifyCavity,{}
		],{}]


(* ::Subsubsection::Closed:: *)
(*QSimplifyRules*)


(* ::Text:: *)
(*Function for constructing QSimplify Rules*)


Options[QSimplifyRules]:={
	Protect->True,
	Power->True,
	Identity->None,
	Com->None,
	Dot->None,
	Transpose->None,
	Conjugate->None,
	ConjugateTranspose->None,
	"NormalOrder"->False
	};


QSimplifyRules[opLabel_,ops_?ListQ,opts:OptionsPattern[QSimplifyRules]]:=
	With[{
		attr=OptionValue[Protect],
		id=OptionValue[Identity],
		order=OptionValue["NormalOrder"],
		comAlg=OptionValue[Com],
		ctOps=OptionValue[ConjugateTranspose],
		tOps=OptionValue[Transpose],
		cOps=OptionValue[Conjugate],
		dotAlg=OptionValue[Dot]},
	If[MemberQ[{Automatic,True},attr],Protect[opLabel]];
	Join[
		If[MemberQ[{Automatic,True},OptionValue[Power]],
			QSimplifyPower[opLabel],
			{}],
		Which[
			MemberQ[{Automatic,True},id],QSimplifyIdentity[opLabel["I"]],
			MemberQ[{False,None},id],{},
			True, QSimplifyIdentity[id]],
		If[MemberQ[{False,None},comAlg],
			{},
			QSimplifyCom[ops,comAlg]],
		If[MemberQ[{False,None},ctOps],
			{},
			QSimplifyFunction[ConjugateTranspose,ops,ctOps]],
		If[MemberQ[{False,None},tOps],
			{},
			QSimplifyFunction[Transpose,ops,tOps]],
		If[MemberQ[{False,None},cOps],
			{},
			QSimplifyFunction[Conjugate,ops,cOps]],
		If[MemberQ[{False,None},dotAlg],
			{},
			QSimplifyDot[ops,dotAlg]],
		Which[
			MemberQ[{False,None},order],{},
			MemberQ[{Automatic,True},order],QSimplifyNormalOrder[opLabel,ops],
			True,QSimplifyNormalOrder[opLabel,order]]
	]]


(* ::Subsubsection::Closed:: *)
(*QSimplifyRules Constructors*)


(* ::Text:: *)
(*General function constructor*)


QSimplifyFunction[fn_,ops_,opsFn_]:=
	Select[
		MapThread[fn[#1]:>#2&,{ops,opsFn}],
		Not[MatchQ[#,_:>None]]&
	]


(* ::Text:: *)
(*Commutator constructors*)


QSimplifyCom[ops_List,algebra_]:=
	With[{mat=QSimplifyComMatrix[algebra]},
	If[Length[mat]===Length[ops],
		Select[
			Flatten[
				MapThread[
					RuleDelayed,
					{Outer[Com[#1,#2]&,ops,ops],
					QSimplifyComMatrix[algebra]},2]
			],
		Not[MatchQ[#,_:>None]]||Not[MatchQ[#,0:>_]]&],
		Message[QSimplifyRules::comlen]
	]]


QSimplifyComMatrix[algebra_]:=
	Which[
		And[ListQ[algebra],{1}===Dimensions[algebra]],
			QSimplifyComMatrixRagged[{algebra}],
		And[MatrixQ[algebra],{1,1}===Dimensions[algebra]],
			QSimplifyComMatrixRagged[algebra],
		MatrixQ[algebra],
			QSimplifyComMatrixSkew[algebra],
		ListQ[algebra],
			QSimplifyComMatrixRagged[algebra],
		True,
			QSimplifyComMatrixRagged[{{algebra}}]
	]


(* ::Text:: *)
(*Constructing algebra matrix for skew, upper or lower triangular matrices*)


QSimplifyComMatrixSkew[mat_?MatrixQ]:=
With[{utri=UpperTriangularize[mat,1],ltri=LowerTriangularize[mat,-1]},
Which[
	AllMatchQ[0,Flatten[utri]], ltri-Transpose[ltri],
	AllMatchQ[0,Flatten[ltri]], utri-Transpose[utri],
	AllMatchQ[0,Flatten[utri+Transpose[ltri]]], utri+ltri,
	True, Message[QSimplifyRules::alg]
]]


(* ::Text:: *)
(*Constructing algebra matrix for ragged lists*)


QSimplifyComMatrixRagged[raggedls_]:=
	With[{d=Length[First@raggedls]},
	If[Flatten[Dimensions/@raggedls]===Range[d,1,-1],
		QSimplifyComMatrixSkew[PadRight[PadLeft[raggedls,{d,d+1}],{d+1,d+1}]],
		Message[QSimplifyRules::ragls]]
	]


(* ::Text:: *)
(*Dot constructors*)


QSimplifyDot[ops_,algebra_]:=
	If[Dimensions[algebra]==={#,#}&@Length[ops],
		Select[
			Flatten[
				MapThread[
					RuleDelayed,
					{Outer[Dot[#1,#2]&,ops,ops],algebra}
				,2]
			],
		Not[MatchQ[#,_:>None]]&],
		Message[QSimplifyRules::dotlen]
	]


(* ::Text:: *)
(*Identity operator constructor*)


QSimplifyIdentity[id_]:={
	Dot[id,op_]:> op,
	Dot[id,QPower[op_,m_]]:> QPower[op,m],
	Dot[op_,id]:> op,
	Dot[QPower[op_,m_],id]:> QPower[op,m],
	QPower[id,n_]:> id,
	Com[id,s_]:> 0,
	Com[s_,id]:> 0,
	ConjugateTranspose[id]:> id,
	Transpose[id]:> id,
	Conjugate[id]:> id
	};


(* ::Text:: *)
(*Operator power constructor*)


QSimplifyPower[container_]:={
	Power[op_container,n_]:> QPower[op,n],
	MatrixPower[op_container,n_]:> QPower[op,n]
	};


(* ::Text:: *)
(*Normal Ordering constructors*)


NormalOrderRight[opls_List,op_]:=Join[
	Map[{
	op.#:>#.op+Com[op,#],
	op.QPower[#,m_]:> QPower[#,m].op+Com[op,QPower[#,m]],
	QPower[op,n_].#:> #.QPower[op,n]+Com[QPower[op,n],#],
	QPower[op,n_].QPower[#,m_]:> QPower[#,m].QPower[op,n]+Com[QPower[op,n],QPower[#,m]]
	}&,opls]];


NormalOrderRight[container_,op_]:={
	op.container[s_]:>container[s].op+Com[op,container[s]],
	op.QPower[container[s_],m_]:> QPower[container[s],m].op+Com[op,QPower[container[s],m]],
	QPower[op,n_].container[s_]:> container[s].QPower[op,n]+Com[QPower[op,n],container[s]],
	QPower[op,n_].QPower[container[s_],m_]:> QPower[container[s],m].QPower[op,n]+Com[QPower[op,n],QPower[container[s],m]]
	};


NormalOrderLeft[opls_List,op_]:=Join[
	Map[{
	#.op:> op.#+Com[#,op],
	#.QPower[op,n_]:> QPower[op,n].#+Com[#,QPower[op,n]],
	QPower[#,m_].op:> op.QPower[#,m]+Com[QPower[#,m],op],
	QPower[#,m_].QPower[op,n_]:> QPower[op,n].QPower[#,m]+Com[QPower[#,m],QPower[op,n]]
	}&,opls]];


NormalOrderLeft[container_,op_]:={
	container[s_].op:> op.container[s]+Com[container[s],op],
	container[s_].QPower[op,n_]:> QPower[op,n].container[s]+Com[container[s],QPower[op,n]],
	QPower[container[s_],m_].op:> op.QPower[container[s],m]+Com[QPower[container[s],m],op],
	QPower[container[s_],m_].QPower[op,n_]:> QPower[op,n].QPower[container[s],m]+Com[QPower[container[s],m],QPower[op,n]]
};


NormalOrderList[ops_]:=
	If[Length[ops]<2,{},
		Flatten@Join[
			NormalOrderLeft[Rest[ops],First[ops]],
			NormalOrderRight[ops[[2;;-2]],Last[ops]],
			NormalOrderList[ops[[2;;-2]]]
		]
	];


QSimplifyNormalOrder[container_,ops_List]:=
Join[
	NormalOrderLeft[container,First[ops]],
	NormalOrderRight[container,Last[ops]],
	NormalOrderList[ops[[2;;-2]]]
];


(* ::Subsubsection::Closed:: *)
(*Linear Algebra Rules*)


$QSimplifyLinearAlgebra={
(* Times *)
Times[Plus[a_,b__],c_]:>Expand[Times[Plus[a,b],c]],
(* Dot *)
Dot[a_,QPower[a_,n_]]:> QPower[a,n+1],
Dot[a_,Plus[b_,c__]]:> Dot[a,b]+Dot[a,Plus[c]],
Dot[a_,a_]:> QPower[a,2],
Dot[QPower[a_,n_],a_]:> QPower[a,n+1],
Dot[QPower[a_,m_],QPower[a_,n_]]:> QPower[a,n+m],
Dot[Plus[a_,b__],c_]:> Dot[a,c]+Dot[Plus[b],c],
Dot[a_,n_?CoefficientQ]:> n*Dot[a],
Dot[n_?CoefficientQ,b_]:> n*Dot[b],
Dot[a_,Times[b_?CoefficientQ,c__]]:> b*Dot[a,Times[c]],
Dot[a_,Times[c__,b_?CoefficientQ]]:> b*Dot[a,Times[c]],
Dot[Times[a_?CoefficientQ,b__],c_]:> a*Dot[Times[b],c],
Dot[Times[b__,a_?CoefficientQ],c_]:> a*Dot[Times[b],c],
Dot[CircleTimes[a1_,b1__],CircleTimes[a2_,b2__]]:> CircleTimes@@MapThread[Dot,{{a1,b1},{a2,b2}}],
(* CircleTimes *)
KroneckerProduct[a_,b__]:> CircleTimes[a,b],
CircleTimes[Plus[a_,b__],c_]:> Plus@@Map[CircleTimes[#,c]&,{a,b}],
CircleTimes[c_,Plus[a_,b__]]:> Plus@@Map[CircleTimes[c,#]&,{a,b}],
CircleTimes[Times[a_?CoefficientQ,b__],c_]:> a*CircleTimes[Times[b],c],
CircleTimes[Times[b__,a_?CoefficientQ],c_]:> a*CircleTimes[Times[b],c],
CircleTimes[c_,Times[a_?CoefficientQ,b__]]:> a*CircleTimes[c,Times[b]],
CircleTimes[c_,Times[b__,a_?CoefficientQ]]:> a*CircleTimes[c,Times[b]],
(* Conjugate *)
Conjugate[Plus[a_,b__]]:>Plus@@Map[Conjugate,{a,b}],
Conjugate[Times[a_,b__]]:>Times@@Map[Conjugate,{a,b}],
Conjugate[Dot[a_,b__]]:>Dot@@Map[Conjugate,{a,b}],
Conjugate[CircleTimes[a_,b__]]:>CircleTimes@@Map[Conjugate,{a,b}],
Conjugate[Transpose[a_]]:> Transpose[Conjugate[a]],
(* Transpose *)
Transpose[a_?CoefficientQ]:> a,
Transpose[Transpose[a_]]:> a,
Transpose[Plus[a_,b__]]:> Plus@@Map[Transpose,{a,b}],
Transpose[Times[a_,b__]]:> Times@@Map[Transpose,{a,b}],
Transpose[Dot[a_,b__]]:> Dot@@Map[Transpose,Reverse[{a,b}]],
Transpose[CircleTimes[a_,b__]]:>CircleTimes@@Map[Transpose,{a,b}],
(* ConjugateTranspose *)
ConjugateTranspose[a_?CoefficientQ]:> Conjugate[a],
ConjugateTranspose[ConjugateTranspose[a_]]:> a,
ConjugateTranspose[a_]:> Transpose[Conjugate[a]],
(* Com *)
Com[a_?CoefficientQ,b_]:> 0,
Com[a_,b_?CoefficientQ]:> 0,
Com[Plus[a_,b__],c_]:> Plus@@Map[Com[#,c]&,{a,b}],
Com[c_,Plus[a_,b__]]:> Plus@@Map[Com[c,#]&,{a,b}],
Com[Times[a_?CoefficientQ,b__],c_]:> a*Com[Times[b],c],
Com[Times[b__,a_?CoefficientQ],c_]:> a*Com[Times[b],c],
Com[c_,Times[a_?CoefficientQ,b__]]:> a*Com[c,Times[b]],
Com[c_,Times[b__,a_?CoefficientQ]]:> a*Com[c,Times[b]],
Com[Dot[a_,b__],c_]:> Dot[a,Com[Dot[b],c]]+Dot[Com[a,c],Dot[b]],
Com[a_,Dot[b_,c__]]:> Dot[Com[a,b],Dot[c]]+Dot[b,Com[a,Dot[c]]],
Com[QPower[a_,n_],b_]:> Dot[a,Com[QPower[a,n-1],b]]+Dot[Com[a,b],QPower[a,n-1]],
Com[a_,QPower[b_,n_]]:> Dot[b,Com[a,QPower[b,n-1]]]+Dot[Com[a,b],QPower[b,n-1]],
Com[CircleTimes[a1_,b1__],CircleTimes[a2_,b2__]]:> 
	CircleTimes[Com[a1,a2],Dot[CircleTimes[b1],CircleTimes[b2]]]
	+CircleTimes[Dot[a1,a2],Com[CircleTimes[b1],CircleTimes[b2]]],
Com[a_,b_,n_?Positive]:> Com[a,Com[a,b],n-1],
(* ACom *)
ACom[a_,b_]:> a.b+b.a,
ACom[a_,b_,n_?Positive]:> ACom[a,ACom[a,b],n-1]
	};


(* ::Subsubsection::Closed:: *)
(*Spin Rules*)


$QSimplifySpin=
QSimplifyRules[
	Spin,
	{Spin["X"],Spin["Y"],Spin["Z"],Spin["P"],Spin["M"]},
	Protect->False,
	Power->True,
	Identity->True,
	Com->{{I*Spin["Z"],-I*Spin["Y"],-Spin["Z"],Spin["Z"]},
		{I*Spin["X"],-I*Spin["Z"],-I*Spin["Z"]},
		{Spin["P"],-Spin["M"]},
		{2Spin["Z"]}},
	ConjugateTranspose->{Spin["X"],Spin["Y"],Spin["Z"],Spin["M"],Spin["P"]},
	Transpose->{Spin["X"],-Spin["Y"],Spin["Z"],Spin["M"],Spin["P"]},
	Conjugate->{Spin["X"],-Spin["Y"],Spin["Z"],Spin["P"],Spin["M"]}
	]


(* ::Text:: *)
(*Spin Normal Ordering*)


$QSimplifySpinOrdering=
	QSimplifyNormalOrder[
		Spin,
		{Spin["Z"],Spin["X"],Spin["Y"],Spin["P"],Spin["M"]}]


(* ::Text:: *)
(*Spin Algebra Convention*)


(* X and Y expand to P and M *)
$QSimplifySpinPM={
	Spin["X"]:> (Spin["P"]+Spin["M"])/2,
	Spin["Y"]:> (-I*Spin["P"]+I*Spin["M"])/2
	};
(* P and M expand to X and Y *)
$QSimplifySpinXY={
	Spin["P"]:> Spin["X"]+I*Spin["Y"],
	Spin["M"]:> Spin["X"]-I*Spin["Y"]
	};


(* ::Text:: *)
(*Additional Spin 1/2 rules*)


$spinHalfDotMatrix=
	{{Spin["I"]/4,I*Spin["Z"]/2, -I*Spin["Y"]/2,Spin["I"]/4-Spin["Z"]/2,Spin["I"]/4+Spin["Z"]/2},
	{-I*Spin["Z"]/2,Spin["I"]/4,I*Spin["X"]/2,I*Spin["I"]/4-I*Spin["Z"]/2,-I*Spin["I"]/4-I*Spin["Z"]/2},
	{I*Spin["Y"]/2,-I*Spin["X"]/2,Spin["I"]/4,Spin["P"]/2,-Spin["M"]/2},
	{Spin["I"][1/2]/4+Spin["Z"][1/2]/2,I*Spin["I"][1/2]/4+I*Spin["Z"][1/2]/2,-Spin["P"]/2,0,Spin["Z"]+Spin["I"]/2},
	{Spin["I"][1/2]/4-Spin["Z"][1/2]/2,-I*Spin["I"][1/2]/4+I*Spin["Z"][1/2]/2,Spin["M"]/2,-Spin["Z"]+Spin["I"]/2,0}};


$QSimplifySpinHalfDot=QSimplifyDot[{Spin["X"],Spin["Y"],Spin["Z"],Spin["P"],Spin["M"]},$spinHalfDotMatrix];


$QSimplifySpinACom={
	ACom[Spin["X"],Spin["Y"]]:> 0,
	ACom[Spin["X"],Spin["Z"]]:> 0,
	ACom[Spin["Y"],Spin["X"]]:> 0,
	ACom[Spin["Y"],Spin["Z"]]:> 0,
	ACom[Spin["Z"],Spin["X"]]:> 0,
	ACom[Spin["Z"],Spin["Y"]]:> 0};


$QSimplifySpinHalfPower={
	QPower[Spin["X"],n_?Positive]:> If[EvenQ[n],Spin["I"]/2^n,Spin["X"]/2^(n-1)],
	QPower[Spin["Y"],n_?Positive]:> If[EvenQ[n],Spin["I"]/2^n,Spin["Y"]/2^(n-1)],
	QPower[Spin["Z"],n_?Positive]:> If[EvenQ[n],Spin["I"]/2^n,Spin["Z"]/2^(n-1)],
	QPower[Spin["P"]+Spin["M"],n_?Positive]:> If[EvenQ[n],Spin["I"],Spin["P"]+Spin["M"]],
	QPower[Spin["P"]-Spin["M"],n_?Positive]:> If[EvenQ[n],Spin["I"],Spin["P"]-Spin["M"]],
	QPower[Spin["M"]-Spin["P"],n_?Positive]:> If[EvenQ[n],Spin["I"],Spin["M"]-Spin["P"]],
	QPower[Spin["P"],n_?Positive]:> If[n===1,Spin["P"],0],
	QPower[Spin["M"],n_?Positive]:> If[n===1,Spin["M"],0]};


$QSimplifySpinHalf=Join[
	$QSimplifySpinACom,
	$QSimplifySpinHalfDot,
	$QSimplifySpinHalfPower];


(* ::Subsubsection::Closed:: *)
(*Cavity Rules*)


$QSimplifyCavity=
	QSimplifyRules[
		Cavity,
		{Cavity["a"],Cavity["c"],Cavity["n"]},
		Protect->False,
		Power->True,
		Identity->True,
		Com->{{Cavity["I"],Cavity["a"]},{-Cavity["c"]}},
		ConjugateTranspose->{Cavity["c"],Cavity["a"],Cavity["n"]},
		Transpose->{Cavity["c"],Cavity["a"],Cavity["n"]},
		Conjugate->{Cavity["a"],Cavity["c"],Cavity["n"]}
	]


(* ::Text:: *)
(*Normal Ordering of operators*)


$QSimplifyCavityOrdering=
	QSimplifyNormalOrder[
		Cavity,
		{Cavity["c"],Cavity["a"],Cavity["n"]}]


(* ::Text:: *)
(*Replace ladder operators with number operator where possible.*)


$QSimplifyCavityN={
	Cavity["c"].Cavity["a"]:> Cavity["n"],
	Cavity["c"].QPower[Cavity["a"],m_?Positive]:> Cavity["n"].QPower[Cavity["a"],m-1],
	QPower[Cavity["c"],m_?Positive].Cavity["a"]:> QPower[Cavity["c"],m-1].Cavity["n"],
	QPower[Cavity["c"],m_?Positive].QPower[Cavity["a"],n_?Positive]:> 
		Which[
			m==n,QPower[Cavity["n"],m],
			m>n, QPower[Cavity["c"],m-n].QPower[Cavity["n"],n],
			m<n,QPower[Cavity["n"],m].QPower[Cavity["a"],n-m]]
};


(* ::Text:: *)
(*Replace cavity number operators with ladder operators*)


$QSimplifyCavityAC={Cavity["n"]:> Cavity["c"].Cavity["a"]};


(* ::Subsection::Closed:: *)
(*Quantum Gates*)


(* ::Subsubsection:: *)
(*Controlled Gates*)


Options[CGate]={Control->1};


CGateGuard=Or[MatrixQ[#],And[ListQ[#],AllQ[MatrixQ,#]]]&;


CGate[gate_?CGateGuard,targ_,ctrl_,opts:OptionsPattern[]]:=
	With[{
		d=If[MatrixQ[gate],
			Length[gate],
			Length[First[gate]]]},
		CGate[d,gate,targ,ctrl,opts]
	]


CGate[dims_,gate_,targ_,ctrl_,opts:OptionsPattern[]]:=
	If[
		CGateArgTests[dims,gate,targ,ctrl],
		If[IntegerQ[dims],
			CGate[ConstantArray[dims,Max[targ,ctrl]],gate,targ,ctrl,opts],
		With[{
			gates=If[MatrixQ[gate],{gate},gate],
			targs=If[IntegerQ[targ],{targ},targ],
			ctrls=If[IntegerQ[ctrl],{ctrl},ctrl]},
			CGateConstructor[dims,gates,targs,ctrls,CGateControl[ctrls,OptionValue[Control]]]
		]]	
	,Null]


(* ::Subsubsection:: *)
(*Controlled-Gate Utility Functions*)


(* ::Text:: *)
(*Extacting option value*)


CGateControl[ctrls_List,ctrlval_]:=
	Which[
		IntegerQ[ctrlval],
			ConstantArray[ctrlval,Length[ctrls]],
		And[
			AllQ[IntegerQ,ctrlval],
			Length[ctrlval]===Length[ctrls]],
				ctrlval,
		True,Message[CGate::ctrlval]]


(* ::Text:: *)
(*Tests if inputs are valid and returns appropriate error message if not.*)


CGateArgTests[dims_,gate_,targs_,ctrls_]:=
	Which[		
		Not[Or[IntegerQ[dims],AllQ[IntegerQ,dims]]],
			Message[CGate::dims];False,
		dims===1,
			Message[CGate::ctrldim];False,
		And[ListQ[dims],Not[Cases[Part[dims,Flatten[{ctrls}]],1]==={}]],
			Message[CGate::ctrldim];False,
		Not[Intersection[Flatten[{targs}],Flatten[{ctrls}]]==={}],
			Message[CGate::targctrl];False,
		Not[Or[
				And[AllQ[MatrixQ,gate],Length[gate]===Length[Flatten[{targs}]]],
				And[MatrixQ[gate],IntegerQ[targs]]]],
			Message[CGate::gates]; False,
		True, True
		]


(* ::Text:: *)
(*Constructs a piece of a controlled gate for a set of gates on a set of targets for a set of control vals on a set of controls.*)


CGatePart[dims_,{gates_,targs_},{ctrls_,ctrlvals_}]:=
	With[{
		ids=Complement[Range[Length[dims]],ctrls,targs]},
	CircleTimes@@Permute[
			Join[gates,
				MapThread[
					Projector[UnitVector[#1,#2+1]]&,
					{Part[dims,ctrls],ctrlvals}],
				Map[IdentityMatrix,Part[dims,ids]]
			],
			Flatten[{targs,ctrls,ids}]]
	]


(* ::Text:: *)
(*Function for constructing the actual controlled gate.*)


CGateConstructor[dims_,gates_List,targs_List,ctrls_List,ctrlvals_List]:=
	Block[{dtargs,dctrls,notctrls,gateOp,idOp},
		dtargs=Part[dims,targs];
		dctrls=Part[dims,ctrls];
		notctrls=Complement[Tuples[Range[0,#-1]&/@dctrls],{ctrlvals}];
		gateOp=CGatePart[dims,
					{gates,targs},
					{ctrls,ctrlvals}];
		idOp=Total@Map[
				CGatePart[dims,
					{IdentityMatrix/@dtargs,targs},
					{ctrls,#}]&,
					notctrls];
		gateOp+idOp
]


(* ::Subsection::Closed:: *)
(*State Measures*)


(* ::Subsubsection::Closed:: *)
(*Entropy*)


LogSafeZero[b_:2, x_] := Piecewise[{{0, x == 0}, {x Log[b, x], x != 0}}]


EntropyH[{ps__}]:= Plus@@Map[-LogSafeZero[2,#]&,{ps}]


EntropyS[mat_]:= 
	If[SquareMatrixQ[mat],
		EntropyH[Eigenvalues[mat]],
		Message[EntropyS::sqmat]
	]


MutualInformationS[mat_, {dA_, dB_}]:= 
	If[SquareMatrixQ[mat],
		(EntropyS[PartialTr[mat, {dA, dB},{1}]]
		+EntropyS[PartialTr[mat, {dA, dB},{2}]]
		-EntropyS[mat]),
		Message[MutualInformationS::sqmat]]


MutualInformationS[mat_]:=With[{d=Sqrt@Length[mat]},
	If[IntegerQ[d],
		MutualInformationS[mat,{d,d}],
		Message[MutualInformationS::dims]	
	]]


(* ::Text:: *)
(*Function for computing the matrix-log of singular matrices. It returns -\[Infinity] for zero eigenvalues*)


MatrixLogZero[base_,A_]:=
	With[{sys=Eigensystem[A]},
		Total[
			ReplaceAll[
				Log[base,First[sys]],
				{DirectedInfinity[-1]->-"\[Infinity]",DirectedInfinity[1]->"\[Infinity]"}
			]*Map[Composition[Projector,Normalize],Last[sys]]
	]]


RelativeEntropyS[A_,B_]:=
	If[AllQ[SquareMatrixQ,{A,B}],
		ReplaceAll[
			-EntropyS[A]-Tr[A.MatrixLogZero[2,B]],
			"\[Infinity]"->DirectedInfinity[1]
		],
	Message[RelativeEntropyS::sqmat]
]



(* ::Subsubsection::Closed:: *)
(*Norms*)


TrNorm[A_]:=Total@SingularValueList[A]


PNorm[A_]:=Norm[A];
PNorm[A_,1]:=If[MatrixQ[A],Total@SingularValueList[A],Norm[A,1]];
PNorm[A_,2]:=If[MatrixQ[A],Norm[A,"Frobenius"],Norm[A,2]];
PNorm[A_,p_]:=If[MatrixQ[A],Norm[SingularValueList[A],p],Norm[A,p]];


(* ::Subsubsection::Closed:: *)
(*Purity and Fidelity*)


Purity[A_]:=
	If[SquareMatrixQ[A],
		Tr[A\[HermitianConjugate].A],
		Message[Purity::sqmat]
	]
Purity[A_?MatrixQ,Normalize->True]:=With[{d=Length[A]},d/(d-1) (Purity[A]-1/d)];


Fidelity[A_,B_]:=With[{
		av=GeneralVectorQ[A],
		bv=GeneralVectorQ[B],
		am=MatrixQ[A],
		bm=MatrixQ[B]},
	Which[
		And[av,bv],Abs[Conjugate[Flatten[A]].Flatten[B]],
		And[av,Not[bv],bm], Sqrt[Tr[B.Projector[A]]],
		And[am,Not[av],bv], Sqrt[Tr[A.Projector[B]]],
		And[am,bm],Total@SingularValueList[MatrixPower[A,1/2].MatrixPower[B,1/2]],
		True,Message[Fidelity::input]
	]]


(* ::Subsubsection::Closed:: *)
(*Entanglement Measures*)


EntangledQ[op_,{da_Integer,db_Integer},fn_]:=
	Which[
		GeneralVectorQ[op],EntangledQ[Projector[op],{da,db},fn],
		SquareMatrixQ[op],
			With[{vals=fn[Eigenvalues[MatrixTranspose[op,{{da,db},{da,db}},{1,4,2,3}]]]},
			If[AnyQ[Negative,vals],True,
			If[Length[op]<=6,False,"Indeterminate"]]],
		True,Message[EntangledQ::input]
	]

EntangledQ[op_,{da_Integer,db_Integer}]:=EntangledQ[op,{da,db},Identity]
EntangledQ[op_,fn_]:=With[{d=Sqrt@Length[op]},EntangledQ[op,{d,d},fn]]
EntangledQ[op_]:=EntangledQ[op,Identity]


Concurrence[op_]:=
	If[Length[op]===4,
		Which[
			GeneralVectorQ[op],
				Concurrence[Projector[op]],
			SquareMatrixQ[op],
				With[{vals=Sqrt@Sort[Chop@Eigenvalues[op.TP["YY"].Conjugate[op].TP["YY"]],Greater]},
				Max[{0,(First[vals]-Total@Rest[vals])}]],
			True, Message[Concurrence::input]
		], Message[Concurrence::dim]
	]


EntanglementF[op_]:=
	If[Length[op]===4,
		Which[
			GeneralVectorQ[op],
				EntanglementF[Projector[op]],
			SquareMatrixQ[op],
				With[{val=(1+Sqrt[1-Concurrence[op]^2])/2},
				(-LogSafeZero[val]-LogSafeZero[1-val])],
			True, Message[EntanglementF::input]
		], Message[EntanglementF::dim]
	]


(* ::Subsection::Closed:: *)
(*Random Matrices*)


(* ::Text:: *)
(*Needs updating to being random with respect to proper measures*)


RandomNormal[]:=RandomVariate[NormalDistribution[0,1]]
RandomNormal[dims_]:=RandomVariate[NormalDistribution[0,1],dims]


GinibreMatrix[n_,r_]:=RandomNormal[{n,r}]+I*RandomNormal[{n,r}]


RandomUnitary[n_]:=Orthogonalize[GinibreMatrix[n,n]]


RandomDensity[n_]:=RandomDensity[n,n]
RandomDensity[n_,rank_]:=RandomDensity[n,rank,"HS"]

RandomDensity[n_,rank_,"HS"]:=
	With[{G=GinibreMatrix[n,rank]},
	#/Tr[#]&[G.ConjugateTranspose[G]]
	]

RandomDensity[n_,"HS"]:=RandomDensity[n,n,"HS"]


RandomDensity[n_,rank_,"Bures"]:=
	With[{
		G=GinibreMatrix[n,rank],
		U=RandomUnitary[n],
		id=IdentityMatrix[n]},
	#/Tr[#]&[(id+U).G.ConjugateTranspose[G].(id+ConjugateTranspose[U])]
	]

RandomDensity[n_,"Bures"]:=RandomDensity[n,n,"Bures"]


RandomHermitian[n_,tr_:1]:=With[
	{A=GinibreMatrix[n,n]RandomComplex[{0,1+I},{n,n}]},
	tr*#/Tr[MatrixPower[#.#,1/2]]&[A+ConjugateTranspose[A]]
];


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


Protect[Spin,Cavity,QState,KetForm,VecForm,Ket,Bra,KetBra];
Protect[Op,QPower,QExpand,QPowerExpand,QSimplifyRules,QSimplify,ClearQSimplifyCache];
Protect[CGate];
Protect[EntropyH,EntropyS,RelativeEntropyS,MutualInformationS];
Protect[Purity,PNorm,Fidelity,EntangledQ,Concurrence,EntanglementF];
Protect[RandomUnitary,RandomDensity,RandomHermitian];


EndPackage[];
