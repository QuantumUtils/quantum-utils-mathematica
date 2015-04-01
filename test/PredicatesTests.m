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


BeginPackage["PredicatesTests`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];
Needs["Predicates`"];


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
(*Fuzzy Logic*)


TestCase[$RegisteredTests,"Predicates:PossiblyTrueQ", 
	And[
		Module[{maybe},PossiblyTrueQ[maybe]],
		PossiblyTrueQ[True],
		Not@PossiblyTrueQ[0 == 2]
	]];


TestCase[$RegisteredTests,"Predicates:PossiblyFalseQ", 
	And[
		Module[{maybe}, PossiblyFalseQ[maybe]],
		Not@PossiblyFalseQ[True],
		PossiblyFalseQ[0 == 2]
	]]


TestCase[$RegisteredTests,"Predicates:PossiblyNonzeroQ", 
	And[
		Module[{maybe}, PossiblyNonzeroQ[maybe]],
		Not@PossiblyNonzeroQ[0]
	]]


(* ::Subsection::Closed:: *)
(*Numbers and Lists*)


TestCase[$RegisteredTests,"Predicates:AnyQ", AnyQ[# >= 2 &, {0, 1, 5}]];


TestCase[$RegisteredTests,"Predicates:AllElementQ", AllElementQ[# >= 2 &, {3, {{{{4}}}}, 5}]];


TestCase[$RegisteredTests,"Predicates:AnyMatchQ", AnyMatchQ[_Integer,{0, 1.2, 5.5}]];


TestCase[$RegisteredTests,"Predicates:AllQ", AnyQ[# >= 2 &, {2, 4, 5}]];


TestCase[$RegisteredTests,"Predicates:AnyElementQ", AnyElementQ[# >= 2 &, {{{{4}, 0}}, 1}]]


TestCase[$RegisteredTests,"Predicates:AllMatchQ", AllMatchQ[_Integer,{2, 4, 5}]];


TestCase[$RegisteredTests,"Predicates:AnyNonzeroQ", AnyNonzeroQ[{0, 1, 5}]];


TestCase[$RegisteredTests,"Predicates:AnyPossiblyNonzeroQ", Module[{maybe}, AnyPossiblyNonzeroQ[{0, maybe, 0}]]]


(* ::Subsection::Closed:: *)
(*Symbolic Expressions*)


TestCase[$RegisteredTests,"Predicates:SymbolQ", 
	And[
		Not@SymbolQ[12],
		Module[{arg},SymbolQ[arg]],
		Module[{f},f[x_]:=x; Not@SymbolQ[f]],
		With[{x=10},Not@SymbolQ[x]]
	]];


TestCase[$RegisteredTests,"Predicates:CoefficientQ",
	Module[{f,g,x,y},
	And[
		CoefficientQ[Sin[3*x]],
		CoefficientQ[g[x,y]],
		CoefficientQ[f[g[x]]],
		CoefficientQ[x],
		CoefficientQ[Sin["x"]],
		SetAttributes[g,Protected];Not@CoefficientQ[g[x]],
		Not@CoefficientQ[KroneckerProduct[x,y]],
		Not@CoefficientQ[Dot[1,2]]
	]]];


(* ::Subsection::Closed:: *)
(*Matrices and Lists*)


TestCase[$RegisteredTests,"Predicates:NonzeroDimQ",
	And[
		Not@NonzeroDimQ[{{{},{}}}],
		NonzeroDimQ[{1,2,3}]
	]];


TestCase[$RegisteredTests,"Predicates:DiagonalMatrixQ", 
	And[
		DiagonalMatrixQ@DiagonalMatrix[{1,2,3}],
		Not@DiagonalMatrixQ[{{1,2},{3,4}}]
	]];


TestCase[$RegisteredTests,"Predicates:PureStateQ", 
	And[
		PureStateQ[{{1,1},{1,1}}/2],
		Not@PureStateQ[{{1,0},{0,3}}/4]
	]];


TestCase[$RegisteredTests,"Predicates:ColumnVectorQ",
	And[
		ColumnVectorQ[{{0},{1}}],
		Not@ColumnVectorQ[{0,0,1}],
		Not@ColumnVectorQ[{{0,1}}]
	]];


TestCase[$RegisteredTests,"Predicates:RowVectorQ",
	And[
		Not@RowVectorQ[{{0},{1}}],
		Not@RowVectorQ[{0,0,1}],
		RowVectorQ[{{0,1}}]
	]];


TestCase[$RegisteredTests,"Predicates:GeneralVectorQ",
	And[
		GeneralVectorQ[{{0},{1}}],
		GeneralVectorQ[{0,0,1}],
		Not@GeneralVectorQ[{{0,1}}]
	]];


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


EndPackage[];
