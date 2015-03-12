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


(* ::Subsection::Closed:: *)
(*Preamble*)


BeginPackage["Predicates`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["UnitTesting`"];


(* ::Section:: *)
(*Usage Declarations*)


(* ::Subsection::Closed:: *)
(*Fuzzy Logic*)


PossiblyTrueQ::usage="Returns True if the argument is not definitely False.";
PossiblyFalseQ::usage="Returns True if the argument is not definitely True.";


(* ::Subsection:: *)
(*Numbers and Lists*)


AnyQ::usage="Given a predicate and a list, retuns True if and only if that predicate is True for at least one element of the list.";
AllQ::usage="AllQ[predicate,list] retuns True if and only if predicate is True for all elements of list.";
AnyMatchQ::usage="AnyMatchQ[cond,list] retuns True if and only if MatchQ[cond,elt] is True for at least one element of list.";
AllMatchQ::usage="AllMatchQ[cond,list] retuns True if and only if MatchQ[cond,elt] is True for all elements of list.";
AnyElementQ::usage="Returns True if cond matches any element of L.";
AllElementQ::usage="Returns True if cond matches any element of L.";


PossiblyNonzeroQ::usage="Returns True if and only if its argument is not definitely zero.";
AnyNonzeroQ::usage="Returns True if L is a list such that at least one element is definitely not zero.";
AnyPossiblyNonzeroQ::usage="Returns True if expr is a list such that at least one element is not definitely zero.";


RealQ::usage="Returns True if and only if the argument is a real number";
PositiveQ::usage="Returns True if and only if the argument is a positive real number";
NonnegativeQ::usage="Returns True if and only if the argument is a non-negative real number";
PositiveIntegerQ::usage="Returns True if and only if the argument is a positive integer";
NonnegativeIntegerQ::usage="Returns True if and only if the argument is a non-negative integer";


SymbolQ::usage="Returns True if argument is an unassigned symbol.";
CoefficientQ::usage="Returns True if argument is numeric or a symbol or a symbolic function.";
SymbolicFunctionQ::usage="Returns True if argument is a numeric function of where the argument consists of expressions involving symbols or variables."


(* ::Subsection::Closed:: *)
(*Matrices and Vectors*)


NonzeroDimQ::usage="Returns True for a given matrix if and only if that matrix has no axes with length 0.";
PureStateQ::usage="Returns true if an operator correpsonds to a pure quantum state";


DiagonalMatrixQ::usage="Returns true if and only if the input is a diagonal matrix.";


ColumnVectorQ::usage="Returns true if object is a mx1 dimensional array";
RowVectorQ::usage="Returns true if object is a 1xm dimensional array";
GeneralVectorQ::usage="Returns true if object is either a vector, mx1 or dimensional array";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Fuzzy Logic*)


PossiblyTrueQ[expr_]:=\[Not]TrueQ[\[Not]expr]


PossiblyFalseQ[expr_]:=\[Not]TrueQ[expr]


(* ::Subsection::Closed:: *)
(*Numbers and Lists*)


AnyQ[cond_, L_] := Fold[Or, False, cond /@ L]
AllQ[cond_, L_] := Fold[And, True, cond /@ L]


AnyMatchQ[cond_, L_] := AnyQ[MatchQ[#,cond]&,L]
AllMatchQ[cond_, L_] := AllQ[MatchQ[#,cond]&,L]


AnyElementQ[cond_,L_]:=AnyQ[cond,Flatten[L]]
AllElementQ[cond_, L_] := AllQ[cond,Flatten[L]]


AnyNonzeroQ[L_]:=AnyElementQ[#!=0&,L]
PossiblyNonzeroQ[expr_]:=PossiblyTrueQ[expr!=0]


AnyPossiblyNonzeroQ[expr_]:=AnyElementQ[PossiblyNonzeroQ,expr]

RealQ[n_]:=TrueQ[Im[n]==0];

PositiveQ[n_]:=Positive[n];


PositiveIntegerQ[n_]:=PositiveQ[n]\[And]IntegerQ[n];

NonnegativeQ[n_]:=TrueQ[RealQ[n]&&n>=0];


NonnegativeIntegerQ[n_]:=NonnegativeQ[n]\[And]IntegerQ[n];


IntegerListQ[input_]:=ListQ[input]&&Not[MemberQ[IntegerQ/@input,False]];


(* ::Subsection:: *)
(*Symbolic*)


SymbolQ[a_]:=Head[a]===Symbol;


SymbolicFunctionQ[arg_]:=
	And[
		MemberQ[Attributes@Evaluate[Head@arg],NumericFunction],
		AllElementQ[
			NumericQ[#]||SymbolQ[#]&,
			Apply[List,List@@arg,Infinity]
		]
	]


CoefficientQ[a_]:=Or[
	NumericQ[a]||SymbolQ[a],
	SymbolicFunctionQ[a]
	]


(* ::Subsection:: *)
(*Matrices and Vectors*)


NonzeroDimQ:=\[Not]MemberQ[Dimensions[#],0]&


DiagonalMatrixQ[A_?MatrixQ]:=
	If[SquareMatrixQ,
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
(*Backward Compatibility*)


(* ::Subsection:: *)
(*Version Check*)


Begin["`Private`"];

(* TODO: check Polyfill works! *)

SetAttributes[Polyfill, HoldRest];

Polyfill[goodVersion_, expr_] := If[$VersionNumber < goodVersion,
	ReleaseHold[HoldComplete[expr]]
];


End[];


(* ::Subsection:: *)
(*Usage Strings*)


Predicates`Private`Polyfill[10,
	NormalMatrixQ::usage="Returns True if the object is a normal matrix";
	SquareMatrixQ::usage="Returns True if and only if the argument is a square matrix";
	PositiveSemidefiniteMatrixQ::usage="Returns True if and only if the chopped eigenvalues of the argument are non-negative.";
];


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


Predicates`Private`Polyfill[10,
	NormalMatrixQ[M_]:=M.ConjugateTranspose[M]===ConjugateTranspose[M].M;
	SquareMatrixQ[M_]:=TrueQ[MatrixQ[M]&&Dimensions[M][[1]]==Dimensions[M][[2]]];
	PositiveSemidefiniteMatrixQ[M_?SquareMatrixQ]:=With[{evals=Eigenvalues[M]},Not[MemberQ[NonNegative[evals],False]]&&Not[Norm[evals]==0]];
];


End[];


(* ::Section:: *)
(*Unit Testing*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Fuzzy Logic*)


Module[{maybe}, TestCase["Predicates:PossiblyTrueQ", PossiblyTrueQ[maybe]]];
TestCase["Predicates:PossiblyTrueQ", PossiblyTrueQ[True]];
TestCase["Predicates:PossiblyTrueQ", !PossiblyTrueQ[0 == 2]];


Module[{maybe}, TestCase["Predicates:PossiblyFalseQ", PossiblyFalseQ[maybe]]];
TestCase["Predicates:PossiblyFalseQ", !PossiblyFalseQ[True]];
TestCase["Predicates:PossiblyFalseQ", PossiblyFalseQ[0 == 2]];


(* ::Subsection:: *)
(*Numbers and Lists*)


TestCase["Predicates:AnyQ", AnyQ[# >= 2 &, {0, 1, 5}]];
TestCase["Predicates:AllQ", AnyQ[# >= 2 &, {2, 4, 5}]];


TestCase["Predicates:AnyMatchQ", AnyMatchQ[_Integer,{0, 1.2, 5.5}]];
TestCase["Predicates:AllMatchQ", AllMatchQ[_Integer,{2, 4, 5}]];


TestCase["Predicates:AllElementQ", AllElementQ[# >= 2 &, {3, {{{{4}}}}, 5}]];
TestCase["Predicates:AnyElementQ", AnyElementQ[# >= 2 &, {{{{4}, 0}}, 1}]]


TestCase["Predicates:AnyNonzeroQ", AnyNonzeroQ[{0, 1, 5}]];


Module[{maybe}, TestCase["Predicates:AnyPossiblyNonzeroQ", AnyPossiblyNonzeroQ[{0, maybe, 0}]]]


TestCase["Predicates:RealQ", RealQ[1]];
TestCase["Predicates:RealQ", !RealQ[I]];


TestCase["Predicates:PositiveIntegerQ", PositiveIntegerQ[1]];
TestCase["Predicates:PositiveIntegerQ", !PositiveIntegerQ[1.5]];


TestCase["Predicates:NonnegativeQ", NonnegativeQ[1]];


TestCase["Predicates:NonnegativeIntegerQ", NonnegativeIntegerQ[0]];


TestCase["Predicates:IntegerListQ", IntegerListQ[{0, 1}]];
Module[{cow}, TestCase["Predicates:IntegerListQ", !IntegerListQ[cow]]];
TestCase["Predicates:IntegerListQ", !IntegerListQ[{0, "snow"}]];


TestCase["Predicates:SymbolQ", !SymbolQ[12]];


Module[{aSymbol}, TestCase["Predicates:SymbolQ", SymbolQ[aSymbol]]];


(* ::Subsection::Closed:: *)
(*Matrices and Lists*)


TestCase["Predicates:NonzeroDimQ", True];


TestCase["Predicates:DiagonalMatrixQ", True];


TestCase["Predicates:PureStateQ", True];


TestCase["Predicates:ColumnVectorQ", True];


TestCase["Predicates:RowVectorQ", True];


TestCase["Predicates:GeneralVectorQ", True];


(* ::Subsection:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


EndPackage[];
