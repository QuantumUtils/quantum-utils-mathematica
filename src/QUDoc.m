(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Documentation*)


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


BeginPackage["QUDoc`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];


(* ::Text:: *)
(*The following command makes running Get["QUDoc`"] or <<QUDoc` directly open the index.nb file, while running Needs["QUDoc`"] loads the package without opening any documentation unless the QUDoc[] function is run.*)


$stack=Stack[];
If[SameQ[First[$stack],Get],
	NotebookOpen[FileNameJoin[{$QUDocumentationPath, "index.nb"}]];
];


(* ::Section::Closed:: *)
(*Usage Declarations*)


(* ::Subsection::Closed:: *)
(*Document Function*)


Unprotect[QUDoc];


QUDoc::usage =
"QUDoc[] opens the QuantumUtils documentation notebook index.
QUDoc[''name''] opens the documentation notebook for the QuantumUtils package name.m.
QUDoc[''name`''] opens the documentation notebook for the QuantumUtils package name.m.
QUDoc[func] opens the documentation notebook containing the documentation for the QuantumUtils function 'func'.";


(* ::Subsection::Closed:: *)
(*Error Messages*)


QUDoc::file = "Documentation notebook `1` does not exist.";


(* ::Section::Closed:: *)
(*Implimentation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Document Function*)


QUDocPath[str_]:=FileNameJoin[{$QUDocumentationPath, "api-doc",str<>".nb"}];


SetAttributes[QUDoc,HoldAll];

QUDoc[]:=(NotebookOpen[FileNameJoin[{$QUDocumentationPath, "index.nb"}]];)

QUDoc[name_String]:=With[{
	str=StringReplace[name,"`"->""]},
	If[MemberQ[$DocumentNotebooks,str],
		NotebookOpen@QUDocPath[str];,
		Message[QUDoc::file,str<>".nb"]
	]]

QUDoc[sym_Symbol]:=
	With[{str=ToString[sym]},
		NotebookLocate[{QUDocPath[str/.$FunctionManifest],str<>"::usage"}];
	]


(* ::Subsection::Closed:: *)
(*Manifests*)


(* ::Text:: *)
(*These manifest variables are memoized so that they only load notebooks once (as this is a slow operation).*)


(* ::Text:: *)
(*Get a list of document notebooks stored in /docs/api-docs/*)


$DocumentNotebooks:=
$DocumentNotebooks=
	Block[{dir=Directory[],files},
		SetDirectory[FileNameJoin[{$QUDocumentationPath, "api-doc"}]];
		files=FileNames["*.nb"];
		SetDirectory[dir];
		StringReplace[files,".nb"->""]
	];


(* ::Text:: *)
(*Load a list of all functions in the documentation notebooks*)


$FunctionManifest:=
$FunctionManifest=
	Dispatch[Join@@(
		ReplaceAll[
			List@@LoadUsages[QUDocPath[#]],
			{Rule[a_,_]:>Rule[a,#]}
		]&/@$DocumentNotebooks)];


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


Protect[QUDoc];


EndPackage[];
