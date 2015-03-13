(* ::Package:: *)

#!/usr/local/bin/MathematicaScript -script

Needs["UnitTesting`"];

Needs["Predicates`"];
Needs["Tensor`"];
Needs["LindbladSolver`"]

Print["Running tests..."];
results = RunAllTests[];

Print[
	If[AnyQ[# != "T" &, results[[All, 2]]], "Some unit tests failed!", "All tests passed."]
];



