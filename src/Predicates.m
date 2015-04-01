(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Predicates Package*)


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


BeginPackage["Predicates`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];


$PredicatesUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "Predicates.nb"}]];


(* ::Section:: *)
(*Usage Declarations*)


(* ::Subsection::Closed:: *)
(*Fuzzy Logic*)


Unprotect[PossiblyTrueQ,PossiblyFalseQ,PossiblyNonzeroQ];


AssignUsage[PossiblyTrueQ,$PredicatesUsages];
AssignUsage[PossiblyFalseQ,$PredicatesUsages];
AssignUsage[PossiblyNonzeroQ,$PredicatesUsages];


(* ::Subsection::Closed:: *)
(*Numbers and Lists*)


Unprotect[AnyQ,AnyMatchQ,AnyElementQ,AllQ,AllMatchQ,AllElementQ,AnyNonzeroQ,AnyPossiblyNonzeroQ];


AssignUsage[AnyQ,$PredicatesUsages];
AssignUsage[AnyMatchQ,$PredicatesUsages];
AssignUsage[AnyElementQ,$PredicatesUsages];


AssignUsage[AllQ,$PredicatesUsages];
AssignUsage[AllMatchQ,$PredicatesUsages];
AssignUsage[AllElementQ,$PredicatesUsages];


AssignUsage[AnyNonzeroQ,$PredicatesUsages];
AssignUsage[AnyPossiblyNonzeroQ,$PredicatesUsages];


(* ::Subsection::Closed:: *)
(*Symbolic Expressions*)


Unprotect[SymbolQ,ScalarQ,CoefficientSymbolQ,CoefficientQ];


AssignUsage[SymbolQ,$PredicatesUsages];
AssignUsage[ScalarQ,$PredicatesUsages];
AssignUsage[CoefficientSymbolQ,$PredicatesUsages];
AssignUsage[CoefficientQ,$PredicatesUsages];


(* ::Subsection::Closed:: *)
(*Matrices and Vectors*)


Unprotect[NonzeroDimQ,DiagonalMatrixQ,PureStateQ,ColumnVectorQ,RowVectorQ,GeneralVectorQ];


AssignUsage[NonzeroDimQ,$PredicatesUsages];
AssignUsage[DiagonalMatrixQ,$PredicatesUsages];
AssignUsage[PureStateQ,$PredicatesUsages];
AssignUsage[ColumnVectorQ,$PredicatesUsages];
AssignUsage[RowVectorQ,$PredicatesUsages];
AssignUsage[GeneralVectorQ,$PredicatesUsages];


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Fuzzy Logic*)


PossiblyTrueQ[expr_]:=\[Not]TrueQ[\[Not]expr]


PossiblyFalseQ[expr_]:=\[Not]TrueQ[expr]


PossiblyNonzeroQ[expr_]:=PossiblyTrueQ[expr!=0]


(* ::Subsection::Closed:: *)
(*Numbers and Lists*)


AnyQ[cond_, L_] := Fold[Or, False, cond /@ L]
AllQ[cond_, L_] := Fold[And, True, cond /@ L]


AnyMatchQ[cond_, L_] := AnyQ[MatchQ[#,cond]&,L]
AllMatchQ[cond_, L_] := AllQ[MatchQ[#,cond]&,L]


AnyElementQ[cond_,L_]:=AnyQ[cond,Flatten[L]]
AllElementQ[cond_, L_] := AllQ[cond,Flatten[L]]


AnyNonzeroQ[L_]:=AnyElementQ[#!=0&,L]


AnyPossiblyNonzeroQ[expr_]:=AnyElementQ[PossiblyNonzeroQ,expr]


(* ::Subsection::Closed:: *)
(*Symbolic Expressions*)


SymbolQ[a_]:=
	And[
		SameQ[Head[a],Symbol],
		SameQ[DownValues[a],{}]
	];


ScalarQ[a_]:=Or[NumericQ[a],SymbolQ[a],StringQ[a]]


CoefficientSymbolQ[a_]:=
	And[SymbolQ[a],
		SameQ[DownValues[a],{}],
		SameQ[#,{}]||SameQ[#,{Temporary}]||MemberQ[#,NumericFunction]&[Attributes@a]
	]


CoefficientQ[expr_]:=
	Or[ScalarQ[expr],
	With[{head=Head[expr]},
		And[
			MatchQ[expr,_[__]],
			Or[StringQ[head],CoefficientSymbolQ[head]],
			AllQ[ScalarQ[#]||CoefficientQ[#]&, List@@expr]
		]
	]]


(* ::Subsection::Closed:: *)
(*Matrices and Vectors*)


NonzeroDimQ:=\[Not]MemberQ[Dimensions[#],0]&


DiagonalMatrixQ[A_?MatrixQ]:=
	If[SquareMatrixQ[A],
		Module[{n=Length[A],dlist},
			dlist=List/@Plus[Range[0,n-1]*(n+1),1];
			Total[Abs[Delete[Flatten[A],dlist]]]===0],
	False
	];


PureStateQ[A_]:=TrueQ[Tr[ConjugateTranspose[A].A]==1];


ColumnVectorQ[v_]:=MatchQ[Dimensions[v],{_,1}];


RowVectorQ[v_]:=MatchQ[Dimensions[v],{1,_}];


GeneralVectorQ[v_]:=Or[VectorQ[v],ColumnVectorQ[v]];


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


Protect[PossiblyTrueQ,PossiblyFalseQ,PossiblyNonzeroQ];
Protect[AnyQ,AnyMatchQ,AnyElementQ,AllQ,AllMatchQ,AllElementQ,AnyNonzeroQ,AnyPossiblyNonzeroQ];
Protect[SymbolQ,CoefficientQ];
Protect[NonzeroDimQ,DiagonalMatrixQ,PureStateQ,ColumnVectorQ,RowVectorQ,GeneralVectorQ];


EndPackage[];
