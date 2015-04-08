(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*PyLink*)


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


BeginPackage["PyLink`",{"QUDoc`"}];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];


$PyLinkUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "PyLink.nb"}]];


(* ::Section:: *)
(*Usage Declarations*)


AssignUsage[{
	CheckPyVersion, PyVersion, PyCall
}, $PyLinkUsages];


(* ::Section:: *)
(*Implimentation*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Version Check*)


CheckPyVersion[] := (
	If[Run["python --version"] == 0,
		Get["!python -c \"import sys; print('PyVersion[{0.major}, {0.minor}, {0.micro}]'.format(sys.version_info))\""],
		$Failed
	]
)


(* ::Text:: *)
(*FIXME: bug comparing things like 2.7.0 with 2.7.*)


PyVersion /: PyVersion[seq1__] < PyVersion[seq2__] := If[
	First@{seq1} == First@{seq2},
	PyVersion @@ Rest @ {seq1} < PyVersion @@ Rest @ {seq2},
	First@{seq1} < First@{seq2}
];
PyVersion /: PyVersion[] < PyVersion[seq__] := First @ {seq} > 0;
PyVersion /: a_PyVersion == b_PyVersion := List @@ a == List @@ b;
PyVersion /: a_PyVersion > b_PyVersion := b < a;
PyVersion /: a_PyVersion <= b_PyVersion := (a < b) \[Or] a == b;
PyVersion /: a_PyVersion >= b_PyVersion := (a > b) \[Or] a == b;


(* ::Subsection:: *)
(*AST Transformations*)


PyCall[fn_] := PyCall[fn, {}, {}];


End[];


(* ::Section:: *)
(*End Package*)


EndPackage[];
