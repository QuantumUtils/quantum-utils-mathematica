(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*M2M Package*)


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


BeginPackage["M2M`",{"QUDoc`"}];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];


$M2MUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "M2M.nb"}]];


(* ::Section:: *)
(*Known Limitations*)


(* ::Text:: *)
(*Adds lots of extraneous parens, since the different structure of Mathematica and MATLAB syntax trees makes the order of operations ambiguous without. A smarter transformation would deal with this problem.*)


(* ::Text:: *)
(*Assumes that QuantumUtils for MATLAB is on the path.*)


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection:: *)
(*Utility Functions*)


Unprotect[
	LatinLowerCaseQ, LatinUpperCaseQ, LatinLetterQ,
	UnderscoreQ, ValidMATLABIdentifierCharacterQ,
	ValidMATLABIdentifierQ
];


AssignUsage[{
	LatinLowerCaseQ, LatinUpperCaseQ, LatinLetterQ,
	UnderscoreQ, ValidMATLABIdentifierQ,
	ValidMATLABIdentifierQ
}, $M2MUsages];


(* ::Subsection:: *)
(*Expression Converters*)


Unprotect[
	ExpressionToMATLAB
];


AssignUsage[
	ExpressionToMATLAB
, $M2MUsages];


(* ::Subsection:: *)
(*Function Converters*)


Unprotect[
	SimpleFunctionToMATLAB, StatementToMATLAB, FunctionToMATLAB, ExportFunctionToMATLAB
];


AssignUsage[{
	SimpleFunctionToMATLAB, StatementToMATLAB, FunctionToMATLAB, ExportFunctionToMATLAB
}, $M2MUsages];


(* ::Section:: *)
(*Utility Functions*)


(* ::Subsection:: *)
(*Predicates*)


(* ::Subsubsection:: *)
(*Implementations*)


Begin["`Private`"];


LatinLowerCaseQ[x_]:=MemberQ[CharacterRange["a","z"],x];
LatinUpperCaseQ[x_]:=MemberQ[CharacterRange["A","Z"],x];


LatinLetterQ[x_]:=LatinLowerCaseQ[x]\[Or]LatinUpperCaseQ[x];


UnderscoreQ[x_]:=x=="_";


ValidMATLABIdentiferCharacterQ[ch_]:=Or@@Map[#[ch]&,{LatinLetterQ,UnderscoreQ,DigitQ}]


ValidMATLABIdentifierQ[str_]:=(And@@Map[ValidMATLABIdentiferCharacterQ,Characters@str])&&LatinLetterQ[(Characters@str)[[1]]]&&(Length[Characters@str]<=63);


End[];


(* ::Subsection:: *)
(*String Handling*)


(* ::Subsubsection:: *)
(*Implementations*)


Begin["`Private`"];


JoinStrings[{strs__},sep_]:=StringJoin@@Flatten[Map[{#,sep}&, {strs}]][[;;-2]]


End[];


(* ::Subsection:: *)
(*Internal Use Utility Functions*)


Begin["`Private`"];


CheckSymbol[sym_] := If[\[Not]ValidMATLABIdentifierQ[SymbolName[sym]], Message[ExpressionToMATLAB::invsym, SymbolName[sym]]];


End[];


(* ::Section:: *)
(*Expression Conversions*)


(* ::Text:: *)
(*The core of the M2M package is the function ExpressionToMATLAB, which we progressively extend with new functionality. Here, we define the usage statement and error messages for that function.*)


(* ::Subsection:: *)
(*Numeric Literals*)


Begin["`Private`"];


(* ::Text:: *)
(*We now define how to convert numeric literals of Real and Integer heads to MATLAB expressions: by directly converting them to strings.*)


ExpressionToMATLAB[x_Real] := ToString[CForm[x]]
ExpressionToMATLAB[x_Integer] := ToString[x]


(* ::Text:: *)
(*Next, we extend this definition to Complex and Rational heads by recursion.*)


ExpressionToMATLAB[Complex[re_,im_]]:="("<>ExpressionToMATLAB[re] <> " + " <>ExpressionToMATLAB[im]<>"*i)";
ExpressionToMATLAB[Rational[nu_,de_]]:="("<>ExpressionToMATLAB[nu] <> "/" <> ExpressionToMATLAB[de]<>")";


End[];


(* ::Subsection:: *)
(*Symbols*)


Begin["`Private`"];


(* ::Text:: *)
(*We assume that free symbols in Mathematica expressions are intended to be represented by variables in corresponding MATLAB code. Hence, we restrict conversions of symbols to those symbols that could concievably be used as MATLAB identifiers.*)


ExpressionToMATLAB[x_Symbol] := (
	If[\[Not]ValidMATLABIdentifierQ[SymbolName[x]], Message[ExpressionToMATLAB::invsym, SymbolName[x]]];
	SymbolName[x]
);


(* ::Text:: *)
(*Some Mathematica symbols require special treatment, however, and so we override the generic definition for Symbol heads in these particular cases.*)


ExpressionToMATLAB[E]="exp(1)";
ExpressionToMATLAB[\[Pi]]="pi";


End[];


(* ::Subsection:: *)
(*Argument Lists*)


(* ::Subsubsection:: *)
(*Usage Statements and Error Messages*)


MATLABArgsList::usage = "Converts a list of Mathematica expressions into a string representing list of MATLAB arguments.";


(* ::Subsubsection:: *)
(*Implementations*)


Begin["`Private`"];


(* ::Text:: *)
(*In converting to MATLAB code, we shall commonly need to form lists of arguments, and so we define a function that calls JoinStrings appropriately.*)


MATLABArgsList[{args__},sep_,suppressParens_:False] :=
	Module[{inner},
		inner = JoinStrings[ExpressionToMATLAB/@{args}, sep];
		If[suppressParens, inner,
			"(" <> inner <> ")"
		]
	];


End[];


(* ::Subsection:: *)
(*Infix Operators*)


(* ::Subsubsection:: *)
(*Usage Statements and Error Messages*)


DefineMATLABInfixOperator::usage = "Extends ExpressionToMATLAB so as to recognize a given Mathematica head as corresponding to a string represention of a MATLAB operator.";


(* ::Subsubsection:: *)
(*Implementations*)


Begin["`Private`"];


(* ::Text:: *)
(*We are now prepared to define infix operators between two or more expressions.*)


DefineMATLABInfixOperator[head_,str_]:=(
	ExpressionToMATLAB[HoldPattern[head[xs__]]]:=MATLABArgsList[{xs},str];
);


(* ::Text:: *)
(*Using this new function, we can quickly define a few useful infix operators:*)


DefineMATLABInfixOperator[Plus, "+"];
DefineMATLABInfixOperator[Times, ".*"];
DefineMATLABInfixOperator[Dot, "*"];
DefineMATLABInfixOperator[Power, ".^"];
DefineMATLABInfixOperator[MatrixPower, "^"];

DefineMATLABInfixOperator[Equal, "=="];
DefineMATLABInfixOperator[Unequal, "~="];
DefineMATLABInfixOperator[Less, "<"];
DefineMATLABInfixOperator[Greater, ">"];
DefineMATLABInfixOperator[LessEqual, "<="];
DefineMATLABInfixOperator[GreaterEqual, ">="];

DefineMATLABInfixOperator[And, "&&"];
DefineMATLABInfixOperator[Or, "||"];


(* ::Text:: *)
(*Note that we have exercised caution about the discinction between MATLAB's dotted infix operators and their undotted forms.*)


(* ::Text:: *)
(*As a special case of the Power function, we wish to map natural exponentials onto MATLAB's exp function.*)


ExpressionToMATLAB[Power[E,expr_]] := "exp(" <> ExpressionToMATLAB[expr] <> ")";


(* ::Text:: *)
(*Also note that division must be handled as a special case of the Times head, since Mathematica does not separate divison from multiplying by the reciprocal. This is not ideal for MATLAB code, however, as we must worry about efficiency and numerical stability. An added difficulty is that the pattern for division is fragile and must be protected by the HoldPattern head.*)


ExpressionToMATLAB[HoldPattern[x_ / y_]]:= "(" <> ExpressionToMATLAB[x] <> "./" <>ExpressionToMATLAB[y] <> ")"


End[];


(* ::Subsection:: *)
(*Functions*)


(* ::Subsubsection:: *)
(*Usage Statements and Error Messages*)


DefineMATLABFunction::usage = "Extends ExpressionToMATLAB to recognize a Mathematica function as having an equivalent MATLAB function.";


(* ::Subsubsection:: *)
(*Implementations*)


Begin["`Private`"];


(* ::Text:: *)
(*Using the same trick as for infix operators, we now define a function that extends ExpressionToMATLAB to recognize a Mathematica head as corresponding exactly to a MATLAB function call. This is only appropriate if the argument lists are expected to be identical.*)


DefineMATLABFunction[head_,str_]:=(
	ExpressionToMATLAB[HoldPattern[head[args__]]]:=str<>MATLABArgsList[{args}, ","]
);


(* ::Text:: *)
(*We can now define a few functions as having equivalents in the MATLAB world.*)


DefineMATLABFunction[Sin,"sin"];
DefineMATLABFunction[Cos,"cos"];
DefineMATLABFunction[Tan,"tan"];
DefineMATLABFunction[Csc,"csc"];
DefineMATLABFunction[Sec,"sec"];
DefineMATLABFunction[Cot,"cot"];
DefineMATLABFunction[Sqrt,"sqrt"];
DefineMATLABFunction[MatrixExp,"expm"];
DefineMATLABFunction[Abs,"abs"];
DefineMATLABFunction[Tr,"trace"];


(* ::Text:: *)
(*Some of the functions we wish to convert only have equivalents that we provide with QuantumUtils for MATLAB.*)


DefineMATLABFunction[If, "iif"];


ExpressionToMATLAB[HoldPattern[QuantumUtils`Private`VecImpl[ColStackingVectorizationConvention,\[Rho]_Symbol]]] := ExpressionToMATLAB[\[Rho]]<>"(:)";


End[];


(* ::Subsection:: *)
(*Vectors and Matrices*)


Begin["`Private`"];


(* ::Text:: *)
(*Defining a transformation for vectors is relatively straightforward.*)


ExpressionToMATLAB[vec_?VectorQ] := "[" <> MATLABArgsList[vec, " ", True] <> "]";


(* ::Text:: *)
(*More complicated is the transformation for matrices, where we must worry about treating columns and rows differently.*)


ExpressionToMATLAB[array_?MatrixQ] := Module[{marr, rowfn, colfn},
	marr=Map[ExpressionToMATLAB,array,{2}];
	rowfn[{row__}]:=JoinStrings[{row}, " "];
	colfn[{col__}]:=JoinStrings[{col}, "; "];

	"[" <> colfn[Map[rowfn, marr]] <> "]"
];


(* ::Text:: *)
(*We also define a mapping for the ConjugateTranspose operation:*)


ExpressionToMATLAB[HoldPattern[ConjugateTranspose[M_]]] := "(" <> ExpressionToMATLAB[M] <> ")'";


End[];


(* ::Subsection:: *)
(*Held Expressions*)


Begin["`Private`"];


ExpressionToMATLAB::holdunfinished = "Using ExpressionToMATLAB with Hold is currently buggy! Please check the output carefully.";
ExpressionToMATLAB[Hold[head_[args__]]] := (
	Message[ExpressionToMATLAB::holdunfinished];
	StringReplace[
		Quiet[ExpressionToMATLAB[head[$x]], ExpressionToMATLAB::invsym],
		"$x" -> MATLABArgsList[{args}, ", ", False]
	]
);


End[];


(* ::Section:: *)
(*Statement and Function Conversions*)


(* ::Subsection:: *)
(*Simple Functions*)


(* ::Text:: *)
(*At times, we will want to convert entire Mathematica functions into MATLAB functions. We start by defining how to convert functions that consist of single expressions.*)


(* ::Subsubsection:: *)
(*Implementations*)


Begin["`Private`"];


SimpleFunctionToMATLAB[funcname_, {args__}, func_]:=
Module[{header,argsList,footer,exprStr},
	CheckSymbol/@{args};
	(* TODO: Check that funcname is a valid identifier. *)

	exprStr="Y = "<>ExpressionToMATLAB[func[args]] <> ";";
	argsList = MATLABArgsList[{args}, ", "];
	header=
		"% " <> funcname <> " TODO: add documentation here.\n" <>
		"function Y = " <> funcname <> argsList <>"\n"<>
		"% WARNING: This file was automatically generated by the M2M package for Mathematica.\n" <>
		"%          Do not edit the following lines directly." ;
	(* TODO: Add metadata from the current notebook. *)
	footer = "end";
	header <> "\n" <> exprStr <> "\n"<>footer
];


End[];


(* ::Subsection:: *)
(*Statement Conversions*)


(* ::Text:: *)
(*In order to convert more complicated functions, we must operate statement-by-statement. This requires being more careful with what is and is not evaluated. As before, we shall work by repeatedly extending a single function, in this case StatementToMATLAB, in order to recognize more features of the MATLAB language.*)


(* ::Subsubsection:: *)
(*Attributes*)


SetAttributes[StatementToMATLAB, HoldAll];


(* ::Subsubsection:: *)
(*Implementations*)


Begin["`Private`"];


(* ::Text:: *)
(*We strip off calls to Module, as they are unnecessary in MATLAB.*)


StatementToMATLAB[HoldPattern[Module[{vars__},body_]]]:=(
	(* We can ignore the list of vars, owing to MATLAB dynamically allocating variables. *)
	StatementToMATLAB[body]
);


(* ::Text:: *)
(*Next, we reduce the problem of converting compound expressions (those involving a semicolon) to converting each statement at a time.*)


StatementToMATLAB[HoldPattern[CompoundExpression[firstStatement_, rest__]]] := (
	StatementToMATLAB[firstStatement] <> "\n" <> StatementToMATLAB[CompoundExpression[rest]]
);


StatementToMATLAB[HoldPattern[CompoundExpression[singleStatement_]]] := (
	StatementToMATLAB[singleStatement]
);


(* ::Text:: *)
(*When we get down to a single statement, we will sometimes want to interpret this as returning a value, and so we turn lone statements that do not match any other pattern into assignments, using the variable $M2MReturnSymbol to track what we are naming the return value.*)


$M2MReturnSymbol = retval;


StatementToMATLAB[else_] := (
	StatementToMATLAB[Set[Evaluate[$M2MReturnSymbol], else]]
);


(* ::Text:: *)
(*We now can define what it means to set a variable in MATLAB. Note that we do not convert DelayedSet, as there is no equivalent in MATLAB.*)


StatementToMATLAB[HoldPattern[Set[var_, expr_]]] := SymbolName[var] <> " = " <> ExpressionToMATLAB[expr] <> ";";


(* ::Text:: *)
(*Though we can represent conditional expressions using the "iif" MATLAB function, as we did above, it is useful to treat statements differently by converting calls to If into "if-else" blocks in MATLAB.*)


StatementToMATLAB[HoldPattern[If[cond_,ift_]]]:=(
	"if " <> ExpressionToMATLAB[cond] <> "\n" <>
	"\t" <> StatementToMATLAB[ift] <> "\n" <>
	"end" 
);


StatementToMATLAB[HoldPattern[If[cond_,ift_,iff_]]]:=(
	"if " <> ExpressionToMATLAB[cond] <> "\n" <>
	"\t" <> StatementToMATLAB[ift] <> "\n" <>
	"else" <> "\n" <>
	"\t"<>StatementToMATLAB[iff] <>"\n"<>
	"end"
);


(* ::Text:: *)
(*TODO: fix indenting in the statements above.*)


End[];


(* ::Subsection:: *)
(*Compound Functions*)


(* ::Text:: *)
(*TODO: change signature to match FunctionToMATLAB.*)


(* ::Subsubsection:: *)
(*Usage Strings and Attributes*)


SetAttributes[ExportFunctionToMATLAB, HoldAll];
SetAttributes[FunctionToMATLAB, HoldAll];


(* ::Subsubsection:: *)
(*Implementations*)


Begin["`Private`"];


ExportFunctionToMATLAB[directory_, fname_, {args__}, body_] := 	
	Export[dir <> fn <> ".m", FunctionToMATLAB[fname, {args}, body], "Text"];


FunctionToMATLAB[fname_, {args__}, body_] := Module[{
	dir = ReleaseHold[directory],
	fn = ReleaseHold[fname],
	as = ReleaseHold[{args}],
	argsList, header, footer,
	contents
},
	CheckSymbol/@{args};
	
	argsList = MATLABArgsList[{args}, ", "];
	header = (
		"% " <> fn <> " TODO: add documentation here.\n" <>
		"function " <> SymbolName[$M2MReturnSymbol] <> " = " <> fn <> argsList <> "\n" <>
		"% WARNING: This file was automatically generated by the M2M package for Mathematica.\n" <>
		"%          Do not edit the following lines directly."
	);
	footer = "end";

	contents = header <> "\n" <> StatementToMATLAB[body] <> "\n" <> footer;
	contents

	(* TODO: Add metadata from the current notebook. *)
];


End[];


(* ::Section:: *)
(*Epilouge*)


EndPackage[];
