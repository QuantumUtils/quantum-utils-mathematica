#!/usr/local/bin/MathematicaScript -script

Needs["UnitTesting`"];

Needs["Predicates`"];

Print["Running tests..."];
results = RunAllTests[];

Print[
	If[AnyQ[# != "T" &, results[[All, 2]]], "Some unit tests failed!", "All tests passed."]
];