(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Options Handling Framework*)


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


BeginPackage["QUOptions`"];


(* ::Section:: *)
(*Options Management*)


(* ::Subsection:: *)
(*Usage Strings*)


QuantumUtilsOptions::usage = "QuantumUtilsOptions[] returns a list of options, formatted as Rules, that configure the QuantumUtils` package.";
$QUDocumentationPath::usage = "$QUDocumentationPath returns the path to documentation and usage string notebooks for QuantumUtils`.";


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


QuantumUtilsOptions[] := QuantumUtilsOptions[] = Module[{opts},
	opts = QuantumUtilsOptions /. Options[$FrontEnd];
	If[
		opts === QuantumUtilsOptions,
		{},
		opts
	]
]


$QUDocumentationPath = DocumentationPath /. QuantumUtilsOptions[];


End[];


EndPackage[];
