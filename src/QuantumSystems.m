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


(* ::Subsection:: *)
(*Preamble*)


BeginPackage["QuantumSystems`",{"Predicates`","Tensor`"}];


Needs["UnitTesting`"];
Needs["QUDevTools`"]


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "QuantumSystems.nb"}]];


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection::Closed:: *)
(*States, Operators and Gates*)


Unprotect[Spin,Cavity,QState,CGate,KetForm,VecForm,Ket,Bra,KetBra];


AssignUsage[Spin,$Usages];
AssignUsage[Cavity,$Usages];
AssignUsage[QState,$Usages];
AssignUsage[KetForm,$Usages];
AssignUsage[VecForm,$Usages];
AssignUsage[CGate,$Usages];


(* ::Subsection::Closed:: *)
(*Symbolic Evaluation*)


Unprotect[QPower,QExpand,QSimplify,ClearQSimplifyCache];


AssignUsage[QExpand,$Usages];
AssignUsage[QPower,$Usages];
AssignUsage[QSimplify,$Usages];
AssignUsage[ClearQSimplifyCache,$Usages];


(* ::Subsection::Closed:: *)
(*State Measures*)


Unprotect[EntropyH,EntropyS,RelativeEntropyS,MutualInformationS];
Unprotect[Purity,PNorm,Fidelity,EntangledQ,Concurrence,EntanglementF];


AssignUsage[EntropyH,$Usages];
AssignUsage[EntropyS,$Usages];
AssignUsage[RelativeEntropyS,$Usages];
AssignUsage[MutualInformationS,$Usages];


AssignUsage[Purity,$Usages];
AssignUsage[PNorm,$Usages];
AssignUsage[Fidelity,$Usages];


AssignUsage[EntangledQ,$Usages];
AssignUsage[Concurrence,$Usages];
AssignUsage[EntanglementF,$Usages];


(* ::Subsection::Closed:: *)
(*Random Matrices*)


Unprotect[RandomUnitary,RandomDensity,RandomHermitian];


AssignUsage[RandomUnitary,$Usages];
AssignUsage[RandomDensity,$Usages];
AssignUsage[RandomHermitian,$Usages];


(* ::Subsection:: *)
(*Error Messages*)


(* ::Subsubsection::Closed:: *)
(*States and Operators*)


Spin::spin = "Total spin value must be an non-negative integer or half-integer; `1` received.";
Cavity::dim = "Cavity dimension must be a positive integer.";


VecForm::fail = "Unable to parse input.";
KetForm::input = "Input must be a rank 1 or 2 array.";


(* ::Subsubsection::Closed:: *)
(*Quantum Gates*)


CGate::dims = "Input dimensions must be an integer or list of integers";
CGate::targctrl = "Targets and Control subsystem lists must not intersect.";
CGate::gates = "Gate must be matrix or a list of matrices of same length as list of targets.";
CGate::ctrlval = "Control values must be integer or list of integers same length as controls list.";


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


(* ::Subsection:: *)
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


Format[Spin[op_]]:=
	With[{str=ToString@Unevaluated[op]},
		Which[
			str=="P", Subscript[Style["S",Italic],"+"],
			str=="M", Subscript[Style["S",Italic],"-"],
			str=="Z"||str=="X"||str=="Y", Subscript[Style["S",Italic],str],
			str=="I", Subscript["\[DoubleStruckOne]","S"],
			True,"Spin"[str]
	]]


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


Format[Cavity[op_]]:=
	With[{str=ToString@Unevaluated[op]},
		Which[
			str=="a", Style["a",Italic],
			str=="c", Superscript[Style["a",Italic],"\[Dagger]"],
			str=="n"||str=="N", Style["N",Italic],
			str=="I", Subscript["\[DoubleStruckOne]","c"],
			True, "Cavity"[str]
	]]


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
			Partition[CircleTimes@@Apply[f@*UnitVector,Map[KetDimensions,First@obj],{1}],1],
		MatchQ[obj,Ket[__]],
			Partition[CircleTimes@@Apply[f@*UnitVector,Map[KetDimensions,List@@obj],{1}],1],
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


(* ::Subsection:: *)
(*Symbolic Evaluation*)


(* ::Subsubsection::Closed:: *)
(*QPower*)


QPower[a_]:=a
QPower[arg_,1]:=arg
QPower[arg_?CoefficientQ,n_]:=Power[arg,n]
QPower[arg_?MatrixQ,n_]:=MatrixPower[arg,n]
QPower[QPower[arg_,m_],n_]:=QPower[arg,m+n]
QPower[Times[x_?CoefficientQ,xs__],n_]:=QPower[x,n]*QPower[Times[xs],n]
QPower[Times[xs__,x_?CoefficientQ],n_]:=QPower[x,n]*QPower[Times[xs],n]


(* ::Text:: *)
(*Hide QPower from displayed equations*)


Format[QPower[arg_,n_]]:=arg^n;


(* ::Subsubsection::Closed:: *)
(*QExpand*)


(* ::Text:: *)
(*Parsing symbolic Spin-Cavity expressions*)


QExpand[expr_]:=expr//.{
	Spin[arg_]:> Spin[arg]["Symbolic"],
	Cavity[arg_]:> Cavity[arg]["Symbolic"]
}


(* ::Subsubsection::Closed:: *)
(*QSimplify*)


Options[QSimplify]:={
	Spin->True,
	Cavity->True,
	"SpinAlgebra"->"PM",
	"SpinHalf"->False,
	"OrderSpin"->False,
	"OrderCavity"->True,
	Rules->{},
	TimeConstraint->1};


QSimplify[expr_,opts:OptionsPattern[]]:= QSimplifyCached[expr,opts]


(* ::Text:: *)
(*Add special recussion rules for commutators*)


QSimplify[Com[op1_,op2_,1],opts:OptionsPattern[]]:= QSimplify[Com[op1,op2],opts]
QSimplify[Com[op1_,op2_,n_?Positive],opts:OptionsPattern[]]:= 
	QSimplify[
		Com[op1,QSimplify[Com[op1,op2],opts],n-1]
	,opts]


QSimplify[ACom[op1_,op2_,1],opts:OptionsPattern[]]:= QSimplify[ACom[op1,op2],opts]
QSimplify[ACom[op1_,op2_,n_?Positive],opts:OptionsPattern[]]:= 
	QSimplify[
		ACom[op1,QSimplify[ACom[op1,op2],opts],n-1]
	,opts]


(* ::Text:: *)
(*Memoized function for QSimplify*)


Clear[QSimplifyCached]


ClearQSimplifyCache[]:=(
	DownValues[QSimplifyCached]=Part[DownValues[QSimplifyCached],{-1}];
	DownValues[QSimplifyRules]=Part[DownValues[QSimplifyRules],{-1}];)


QSimplifyCached[expr_,opts:OptionsPattern[QSimplify]]:=
	QSimplifyCached[expr,opts]=
	Simplify[
	ReplaceRepeated[
			Simplify[QExpand[expr],TimeConstraint->OptionValue[TimeConstraint]],
			QSimplifyRules[opts]
		],
	TimeConstraint->OptionValue[TimeConstraint]]


(* ::Text:: *)
(*Replacement rules for QSimplify*)


Clear[QSimplifyRules]


QSimplifyRules[opts:OptionsPattern[QSimplify]]:=
	QSimplifyRules[opts]=
		Dispatch@Join[
			If[OptionValue["SpinHalf"],$QSimplifySpinHalf,{}],
			Which[
				OptionValue["SpinAlgebra"]==="PM", $QSimplifySpinPM,
				OptionValue["SpinAlgebra"]==="XY", $QSimplifySpinXY,
				True,{}],
			If[OptionValue[Spin],$QSimplifySpin,{}],
			If[OptionValue[Cavity],$QSimplifyCavity,{}],
			OptionValue[Rules],
			$QSimplifyLinearAlgebra,
			If[OptionValue["OrderCavity"],$QSimplifyCavityOrdering,{}],
			If[OptionValue["OrderSpin"],$QSimplifySpinOrdering,{}]
		]


(* ::Subsubsection::Closed:: *)
(*Linear Algebra Rules*)


$QSimplifyLinearAlgebra={
(* Times *)
Times[Plus[a_,b__],c_]:>Expand[Times[Plus[a,b],c]],
(* Dot *)
Dot[a_,n_?CoefficientQ]:> n*Dot[a],
Dot[n_?CoefficientQ,b_]:> n*Dot[b],
Dot[a_,Plus[b_,c__]]:> Dot[a,b]+Dot[a,Plus[c]],
Dot[Plus[a_,b__],c_]:> Dot[a,c]+Dot[Plus[b],c],
Dot[a_,Times[b_?CoefficientQ,c__]]:> b*Dot[a,Times[c]],
Dot[a_,Times[c__,b_?CoefficientQ]]:> b*Dot[a,Times[c]],
Dot[Times[a_?CoefficientQ,b__],c_]:> a*Dot[Times[b],c],
Dot[Times[b__,a_?CoefficientQ],c_]:> a*Dot[Times[b],c],
Dot[a_,a_]:> QPower[a,2],
Dot[QPower[a_,n_],a_]:> QPower[a,n+1],
Dot[a_,QPower[a_,n_]]:> QPower[a,n+1],
Dot[QPower[a_,m_],QPower[a_,n_]]:> QPower[a,n+m],
(* CircleTimes *)
KroneckerProduct[a_,b__]:> CircleTimes[a,b],
CircleTimes[Plus[a_,b__],c_]:> Plus@@Map[CircleTimes[#,c]&,{a,b}],
CircleTimes[c_,Plus[a_,b__]]:> Plus@@Map[CircleTimes[c,#]&,{a,b}],
CircleTimes[Times[a_?CoefficientQ,b__],c_]:> a*CircleTimes[Times[b],c],
CircleTimes[Times[b__,a_?CoefficientQ],c_]:> a*CircleTimes[Times[b],c],
CircleTimes[c_,Times[a_?CoefficientQ,b__]]:> a*CircleTimes[c,Times[b]],
CircleTimes[c_,Times[b__,a_?CoefficientQ]]:> a*CircleTimes[c,Times[b]],
(* Transpose *)
Transpose[a_?CoefficientQ]:> a,
Transpose[Transpose[a_]]:> a,
Transpose[Plus[a_,b__]]:> Plus@@Map[Transpose,{a,b}],
Transpose[Times[a_,b__]]:> Times@@Map[Transpose,{a,b}],
Transpose[Dot[a_,b__]]:> Dot@@Map[Transpose,Reverse[{a,b}]],
Transpose[CircleTimes[a__]]:>CircleTimes@@Map[Transpose,a],
(* ConjugateTranspose *)
ConjugateTranspose[a_?CoefficientQ]:> Conjugate[a],
ConjugateTranspose[ConjugateTranspose[a_]]:> a,
ConjugateTranspose[a_]:> Transpose[Conjugate[a]],
Conjugate[Transpose[a_]]:> Transpose[Conjugate[a]],
(* Conjugate *)
Conjugate[Plus[a_,b__]]:>Plus@@Map[Conjugate,{a,b}],
Conjugate[Times[a_,b__]]:>Times@@Map[Conjugate,{a,b}],
Conjugate[Dot[a_,b__]]:>Dot@@Map[Conjugate,{a,b}],
Conjugate[CircleTimes[a__]]:>CircleTimes@@Map[Conjugate,{a}],
(* Com *)
Com[a_,a_]:> 0,
Com[a_?CoefficientQ,b_]:> 0,
Com[a_,b_?CoefficientQ]:> 0,
Com[a_,b_,0]:> b,
Com[a_,b_,1]:> Com[a,b],
Com[Plus[a_,b__],c_]:> Plus@@Map[Com[#,c]&,{a,b}],
Com[c_,Plus[a_,b__]]:> Plus@@Map[Com[c,#]&,{a,b}],
Com[Times[a_?CoefficientQ,b__],c_]:> a*Com[Times[b],c],
Com[Times[b__,a_?CoefficientQ],c_]:> a*Com[Times[b],c],
Com[c_,Times[a_?CoefficientQ,b__]]:>a*Com[Times[b],c],
Com[c_,Times[b__,a_?CoefficientQ]]:>a*Com[Times[b],c],
Com[Dot[a_,b__],c_]:> Dot[a,Com[Dot[b],c]]+Dot[Com[a,c],Dot[b]],
Com[a_,Dot[b_,c__]]:> Dot[Com[a,b],Dot[c]]+Dot[b,Com[a,Dot[c]]],
Com[QPower[a_,n_],b_]:> Dot[a,Com[QPower[a,n-1],b]]+Dot[Com[QPower[a,n-1],b],a],
Com[a_,QPower[b_,n_]]:> Dot[b,Com[a,QPower[b,n-1]]]+Dot[Com[a,QPower[b,n-1]],b],
Com[a_,b_,n_]:> Com[a,Com[a,b],n-1],
Com[CircleTimes[a1_,b1__],CircleTimes[a2_,b2__]]:> 
	CircleTimes[Com[a1,a2],Dot[b1,b2]]
	+CircleTimes[Dot[a1,a2],Com[CircleTimes[b1],CircleTimes[b2]]],
(* ACom *)
ACom[a_,b_]:> a.b+b.a,
ACom[a_,b_,n_]:> ACom[a,ACom[a,b,n-1]]
	};


(* ::Subsubsection::Closed:: *)
(*Spin Rules*)


$QSimplifySpin={

Power[op_Spin,n_]:> QPower[op,n],
MatrixPower[op_Spin,n_]:>QPower[op,n],

(* Identity Operator *)
Spin["I"].Spin[s_]:> Spin[s],
Spin[s_].Spin["I"]:> Spin[s],
QPower[Spin["I"],n_]:> Spin["I"],
Com[Spin["I"],s_]:> 0,
Com[s_,Spin["I"]]:> 0,

(* PM Spin Algebra *)
Com[Spin["Z"],Spin["P"]]:> Spin["P"],
Com[Spin["Z"],Spin["M"]]:> -Spin["M"],
Com[Spin["P"],Spin["Z"]]:> -Spin["P"],
Com[Spin["P"],Spin["M"]]:> 2Spin["Z"],
Com[Spin["M"],Spin["Z"]]:> Spin["M"],
Com[Spin["M"],Spin["P"]]:> -2Spin["Z"],

(* XY Spin Algebra *)
Com[Spin["X"],Spin["Y"]]:> I*Spin["Z"],
Com[Spin["X"],Spin["Z"]]:> -I*Spin["Y"],
Com[Spin["Y"],Spin["X"]]:> -I*Spin["Z"],
Com[Spin["Y"],Spin["Z"]]:> I*Spin["X"],
Com[Spin["Z"],Spin["X"]]:> I*Spin["Y"],
Com[Spin["Z"],Spin["Y"]]:> -I*Spin["X"],

(* ConjugateTranspose *)
ConjugateTranspose[Spin["I"]]:> Spin["I"],
ConjugateTranspose[Spin["X"]]:> Spin["X"],
ConjugateTranspose[Spin["Y"]]:> Spin["Y"],
ConjugateTranspose[Spin["Z"]]:> Spin["Z"],
ConjugateTranspose[Spin["P"]]:> Spin["M"],
ConjugateTranspose[Spin["M"]]:>Spin["P"],

(* Transpose *)
Transpose[Spin["I"]]:> Spin["I"],
Transpose[Spin["X"]]:> Spin["X"],
Transpose[Spin["Y"]]:> -Spin["Y"],
Transpose[Spin["Z"]]:> Spin["Z"],
Transpose[Spin["P"]]:> Spin["M"],
Transpose[Spin["M"]]:>Spin["P"],

(* Conjugate *)
Conjugate[Spin["I"]]:> Spin["I"],
Conjugate[Spin["X"]]:> Spin["X"],
Conjugate[Spin["Y"]]:> -Spin["Y"],
Conjugate[Spin["Z"]]:> Spin["Z"],
Conjugate[Spin["P"]]:> Spin["P"],
Conjugate[Spin["M"]]:>Spin["M"]
};


(* ::Text:: *)
(*Spin Algebra Convention*)


(* X and Y expand to P and M *)
$QSimplifySpinPM={
Spin["X"]:> (Spin["P"]+Spin["M"])/2,
Spin["Y"]:> (-I*Spin["P"]+I*Spin["M"])/2};
(* P and M expand to X and Y *)
$QSimplifySpinXY={
Spin["P"]:> Spin["X"]+I*Spin["Y"],
Spin["M"]:> Spin["X"]-I*Spin["Y"]};


(* ::Text:: *)
(*Additional Spin 1/2 rules*)


$QSimplifySpinHalf={
Spin["Z"].Spin["Z"]:> Spin["I"]/4,
Spin["Z"].Spin["P"]:> Spin["P"]/2,
Spin["Z"].Spin["M"]:> -Spin["M"]/2,
Spin["P"].Spin["Z"]:> -Spin["P"]/2,
Spin["P"].Spin["P"]:> 0,
Spin["P"].Spin["M"]:> Spin["Z"]+Spin["I"]/2,
Spin["M"].Spin["Z"]:> Spin["M"]/2,
Spin["M"].Spin["M"]:> 0,
Spin["M"].Spin["P"]:> -Spin["Z"]+Spin["I"]/2,
QPower[Spin["Z"],n_?Positive]:> If[EvenQ[n],Spin["I"]/2^n,Spin["Z"]/2^(n-1)],
QPower[Spin["P"]+Spin["M"],n_?Positive]:> If[EvenQ[n],Spin["I"],Spin["P"]+Spin["M"]],
QPower[Spin["P"]-Spin["M"],n_?Positive]:> If[EvenQ[n],Spin["I"],Spin["P"]-Spin["M"]],
QPower[Spin["M"]-Spin["P"],n_?Positive]:> If[EvenQ[n],Spin["I"],Spin["M"]-Spin["P"]],
QPower[Spin["P"],n_?Positive]:> If[n===1,Spin["P"],0],
QPower[Spin["M"],n_?Positive]:> If[n===1,Spin["M"],0]
};


(* ::Text:: *)
(*Spin Normal Ordering*)


$QSimplifySpinOrdering={
(* M always goes to the right *)
Spin["M"].Spin[s_]:> 
	Spin[s].Spin["M"]+Com[Spin["M"],Spin[s]],
Spin["M"].QPower[Spin[s_],m_]:> 
	QPower[Spin[s],m].Spin["M"]
	+Com[Spin["M"],QPower[Spin[s],m]],
QPower[Spin["M"],n_].Spin[s_]:> 
	Spin[s].QPower[Spin["M"],n]
	+Com[QPower[Spin["M"],n],Spin[s]],
QPower[Spin["M"],n_].QPower[Spin[s_],m_]:> 
	QPower[Spin[s],m].QPower[Spin["M"],n]
	+Com[QPower[Spin["M"],n],QPower[Spin[s],m]],
(* Z always to the left *)
Spin[s_].Spin["Z"]:> 
	Spin["Z"].Spin[s]+Com[Spin[s],Spin["Z"]],
Spin[s_].QPower[Spin["Z"],n_]:> 
	QPower[Spin["Z"],n].Spin[s]
	+Com[Spin[s],QPower[Spin["Z"],n]],
QPower[Spin[s_],m_].Spin["Z"]:> 
	Spin["Z"].QPower[Spin[s],m]
	+Com[QPower[Spin[s],m],Spin["Z"]],
QPower[Spin[s_],m_].QPower[Spin["Z"],n_]:> 
	QPower[Spin["Z"],n].QPower[Spin[s],m]
	+Com[QPower[Spin[s],m],QPower[Spin["Z"],n]]
};


(* ::Subsubsection::Closed:: *)
(*Cavity Rules*)


$QSimplifyCavity={
Power[Cavity[a_],n_]:> QPower[Cavity[a],n],
MatrixPower[op_Cavity,n_]:> QPower[op,n],
(* Convert c.a to n *)
Cavity["c"].Cavity["a"]:> Cavity["n"],
Cavity["a"].Cavity["c"]:> Cavity["n"]+Cavity["I"],

(* Cavity Identity *)
SymbolicPower[Cavity["I"],n_]:> Cavity["I"],
Com[Cavity["I"],op_]:> 0,
Com[op_,Cavity["I"]]:> 0,
Cavity["I"].Cavity[a_]:> Cavity[a],
Cavity[a_].Cavity["I"]:> Cavity[a],

(* Cavity n-Algrbra *)
Com[QPower[Cavity["a"],n_?IntegerQ],Cavity["n"]]:> n*QPower[Cavity["a"],n],
Com[Cavity["n"],QPower[Cavity["a"],n_?IntegerQ]]:> -n*QPower[Cavity["a",n]],
Com[QPower[Cavity["c"],n_?IntegerQ],Cavity["n"]]:> -n*QPower[Cavity["c"],n],
Com[Cavity["n"],QPower[Cavity["c"],n_?IntegerQ]]:> n*QPower[Cavity["c"],n],

(* Cavity Algebra *)
Com[Cavity["a"],Cavity["c"]]:> Cavity["I"],
Com[Cavity["c"],Cavity["a"]]:> -Cavity["I"],
Com[Cavity["a"],Cavity["n"]]:> Cavity["a"],
Com[Cavity["n"],Cavity["a"]]:> -Cavity["a"],
Com[Cavity["c"],Cavity["n"]]:> -Cavity["c"],
Com[Cavity["n"],Cavity["c"]]:> Cavity["c"],

(* ConjugateTranspose *)
ConjugateTranspose[Cavity["I"]]:> Cavity["I"],
ConjugateTranspose[Cavity["n"]]:> Cavity["n"],
ConjugateTranspose[Cavity["a"]]:> Cavity["c"],
ConjugateTranspose[Cavity["c"]]:> Cavity["a"],

(* Transpose *)
Transpose[Cavity["I"]]:> Cavity["I"],
Transpose[Cavity["n"]]:> Cavity["n"],
Transpose[Cavity["a"]]:> Cavity["c"],
Transpose[Cavity["c"]]:> Cavity["a"],

(* Conjugate *)
Conjugate[Cavity["I"]]:> Cavity["I"],
Conjugate[Cavity["n"]]:> Cavity["n"],
Conjugate[Cavity["a"]]:> Cavity["a"],
Conjugate[Cavity["c"]]:> Cavity["c"]
};


(* ::Text:: *)
(*Normal Ordering of operators*)


$QSimplifyCavityOrdering={
(* c before a *)
Cavity["a"].Cavity["c"]:> 
	Cavity["c"].Cavity["a"]
	+Com[Cavity["a"],Cavity["c"]],
Cavity["a"].QPower[Cavity["c"],m_]:> 
	QPower[Cavity["c"],m].Cavity["a"]
	+Com[Cavity["a"],QPower[Cavity["c"],m]],
QPower[Cavity["a"],n_].Cavity["c"]:> 
	Cavity["c"].QPower[Cavity["a"],n]
	+Com[QPower[Cavity["a"],n],Cavity["c"]],
QPower[Cavity["a"],n_].QPower[Cavity["c"],m_]:> 
	QPower[Cavity["c"],m].QPower[Cavity["a"],n]
	+Com[QPower[Cavity["a"],n],QPower[Cavity["c"],m]],
(* n before a *)
Cavity["a"].Cavity["n"]:>
	Cavity["n"].Cavity["a"]
	+Com[Cavity["a"],Cavity["n"]],
Cavity["a"].QPower[Cavity["n"],m_]:> 
	QPower[Cavity["n"],m].Cavity["a"]
	+Com[Cavity["a"],QPower[Cavity["n"],m]],
QPower[Cavity["a"],n_].Cavity["n"]:> 
	Cavity["n"].QPower[Cavity["a"],n]
	+Com[QPower[Cavity["a"],n],Cavity["n"]],
QPower[Cavity["a"],n_].QPower[Cavity["n"],m_]:> 
	QPower[Cavity["n"],m].QPower[Cavity["a"],n]
	+Com[QPower[Cavity["a"],n],QPower[Cavity["n"],m]],
(* n before c *)
Cavity["c"].Cavity["n"]:> 
	Cavity["n"].Cavity["c"]
	+Com[Cavity["c"],Cavity["n"]],
Cavity["c"].QPower[Cavity["n"],m_]:> 
	QPower[Cavity["n"],m].Cavity["c"]
	+Com[Cavity["c"],QPower[Cavity["n"],m]],
QPower[Cavity["c"],n_].Cavity["n"]:> 
	Cavity["n"].QPower[Cavity["c"],n]
	+Com[QPower[Cavity["c"],n],Cavity["n"]],
QPower[Cavity["c"],n_].QPower[Cavity["n"],m_]:> 
	QPower[Cavity["n"],m].QPower[Cavity["c"],n]
	+Com[QPower[Cavity["c"],n],QPower[Cavity["n"],m]]
};


(* ::Subsection::Closed:: *)
(*Quantum Gates*)


(* ::Subsubsection:: *)
(*Controlled Gates*)


Options[CGate]={Control->1};


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


CGate[gate_,targ_,ctrl_,opts:OptionsPattern[]]:=
	With[{
		d=If[MatrixQ[gate],
			Length[gate],
			Length[First[gate]]]},
		CGate[d,gate,targ,ctrl,opts]
	]


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
			]*Map[Projector@*Normalize,Last[sys]]
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
			With[{vals=fn[Eigenvalues[MatrixTranspose[op,{da,db,da,db},{1,4,2,3}]]]},
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


(* ::Section:: *)
(*Unit Testing*)


(* ::Section::Closed:: *)
(*End Package*)


Protect[Spin,Cavity,QState,KetForm,VecForm,Ket,Bra,KetBra];
Protect[QPower,QExpand,QSimplify,ClearQSimplifyCache];
Protect[CGate];
Protect[EntropyH,EntropyS,RelativeEntropyS,MutualInformationS];
Protect[Purity,PNorm,Fidelity,EntangledQ,Concurrence,EntanglementF];
Protect[RandomUnitary,RandomDensity,RandomHermitian];


EndPackage[];
