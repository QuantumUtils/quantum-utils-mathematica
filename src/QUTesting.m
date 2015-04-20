(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Unit Testing*)


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


BeginPackage["QUTesting`",{"QUDoc`"}];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];


$QUTestingUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "QUTesting.nb"}]];


(* ::Section::Closed:: *)
(*Usage Declarations*)


AssignUsage[{TestResults,RunAllTests},$QUTestingUsages];


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Test Results*)


(* ::Text:: *)
(*Unit tests for a package "name.m" are stored in the /tests/ folder under under the name "nameTests.m"*)


(* ::Text:: *)
(*The results of the unit tests are stored in the variable nameTests`UnitTests`$TestResults, which is set to delayed evaluation so that the tests will not be run until the variable is called.*)


(* ::Text:: *)
(*To call the test results of an individual package use TestResults["name"].  For example: TestResults["Tensor"]*)


SetAttributes[TestResults,HoldAll];


TestResults[package_]:=
	With[{name=ToString[package]},
		Needs[name<>"Tests`",FileNameJoin[{$QUTestingPath,name<>"Tests.m"}]];
		ToExpression[name<>"Tests`UnitTests`$TestResults"]
	]


TestResults[]:=Join@@Map[TestResults,$UnitTestManifest]


(* ::Text:: *)
(*Registered unit test packages*)


$UnitTestManifest={
	"Predicates",
	"Tensor",
	"QuantumSystems",
	"QuantumChannel",
	"LindbladSolver",
	"M2M",
	"QSim"
	};


(* ::Subsection:: *)
(*Run All Tests*)


(* ::Text:: *)
(*This function checks how many unit tests passed, failed, or returned errors.*)


RunAllTests[] := (
	PrintTemporary["Running unit tests..."];
	Block[{results,n,pass,fail,error},	
		results = TestResults[];
		n = Length[results];
		pass=Length@Select[results,MatchQ[#,_->"T"]&];
		fail=Length@Select[results,MatchQ[#,_->"F"]&];
		error=Length@Select[results,MatchQ[#,_->"E"]&];
		Print[ToString[pass]<>" of "<>ToString[n]<> " unit tests passed."];
		If[fail>0,Print[ToString[fail]<>" of "<>ToString[n]<> " unit tests failed."];
		If[error>0,Print[ToString[error]<>" of "<>ToString[n]<> " unit tests returned errors."]];
		]
	])


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section:: *)
(*End Package*)


EndPackage[];
