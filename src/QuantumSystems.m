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


BeginPackage["QuantumSystems`",{"Predicates`","Tensor`"}];


Needs["DocTools`"]
Needs["QUOptions`"]


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "QuantumSystems.nb"}]];


(* ::Section::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Error Messages*)


(* ::Subsubsection:: *)
(*States and Operators*)


Spin::spin = "Total spin value must be an non-negative integer or half-integer.";
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
(*Implimentation*)


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
			{SpinI[S],SpinX[S],SpinY[S],SpinZ[S],SpinP[S],SpinM[S]}}
	]


SetAttributes[Spin,HoldAllComplete]
Spin[expr_][S_,SparseArray]:=With[{spin=Rationalize[S]},
	If[SpinQ[spin],
		TP[expr,Replace->SpinTPRules[S]],
		Message[Spin::spin]
	]]

Spin[expr_][S_]:=Normal[Spin[expr][S,SparseArray]]


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
			{CavityI[n],CavityA[n],CavityC[n],CavityN[n],CavityN[n]}}
	]


SetAttributes[Cavity,HoldAllComplete]
Cavity[expr_][n_Integer,SparseArray]:=
	If[Positive[n],
		TP[expr,Replace->CavityTPRules[n]],
		Message[Cavity::int]
	]

Cavity[expr_][n_Integer]:=Normal[Cavity[expr][n,SparseArray]]


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
(*Converting Bra-Ket notation to arrays.*)


KetDimensions[Subscript[num_,dim_]]:={dim,1+num};
KetDimensions[num_]:={2,1+num};


VecForm[obj_]:=
	Which[
		NumericQ[obj],obj,
		ListQ[obj],obj,
		MatchQ[obj,Plus[_,__]],
			VecForm[First[obj]]+VecForm[Rest[obj]],
		MatchQ[obj,Times[_,__]],
			Times[First[obj],VecForm[Rest[obj]]],
		MatchQ[obj,Ket[{__}]],
			Partition[CircleTimes@@UnitVector@@@Map[KetDimensions,First@obj],1],
		MatchQ[obj,Ket[__]],
			Partition[CircleTimes@@UnitVector@@@Map[KetDimensions,List@@obj],1],
		MatchQ[obj,Bra[{__}]],
			ConjugateTranspose[VecForm[Ket@@First[obj]]],
		MatchQ[obj,Bra[__]],
			ConjugateTranspose[VecForm[Ket@@obj]],
		MatchQ[obj,KetBra[{__},{__}]],
			CircleTimes[VecForm[Ket@@First[obj]],VecForm[Bra@@Last[obj]]],
		True,
			Message[VecForm::fail]
]

VecForm[a__]:=Map[VecForm,{a}]


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


(* ::Subsection:: *)
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


RelativeEntropyS[A_,B_]:=
	If[AllQ[SquareMatrixQ,{A,B}],
		-EntropyS[A]-Tr[A.MatrixLog[B]],
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


(* ::Subsubsection:: *)
(*Entanglement Measures*)


EntangledQ[op_,{da_Integer,db_Integer},fn_]:=
	Which[
		GeneralVectorQ[op],EntangledQ[Projector[op],{da,db},fn],
		SquareMatrixQ[op],
			With[{vals=fn[Eigenvalues[MatrixTranspose[op,{da,db,da,db},{1,4,2,3}]]]},
			If[AnyQ[Negative,vals],True,
			If[Length[mat]<=6,False,"Indeterminate"]]],
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
Protect[CGate];
Protect[EntropyH,EntropyS,RelativeEntropyS,MutualInformationS];
Protect[Purity,PNorm,Fidelity,EntangledQ,Concurrence,EntanglementF];
Protect[RandomUnitary,RandomDensity,RandomHermitian];


EndPackage[];
