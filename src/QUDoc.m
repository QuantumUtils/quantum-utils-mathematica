(* ::Package:: *)

BeginPackage["QUDoc`"];
Needs["QUDevTools`"];

(* Only open index.nb if Get (<<) was used to run this file. *)
(* Stack will be {Needs, Get, ...} if Needs is used (and remember *)
(* the package won't be reloaded by Needs if it has already been *) 
(* loaded). Stack will be {BeginPackage, Needs, Get, ...} if *)
(* BeginPackage[..,{..,"QUDoc`",...}] is used. *)
$stack=Stack[];
Print[$stack];
If[First@$stack===Get,
	NotebookOpen[FileNameJoin[{$QUDocumentationPath, "index.nb"}]];
];

EndPackage[];
