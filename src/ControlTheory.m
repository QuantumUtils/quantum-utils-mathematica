(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*ControlTheory*)


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


BeginPackage["ControlTheory`",{"Tensor`"}];


Needs["DocTools`"];
Needs["QUOptions`"];
Needs["UnitTesting`"];


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "ControlTheory.nb"}]];


(* ::Section:: *)
(*Usage Declarations*)


(* ::Subsection:: *)
(*Lie Algebra Generation*)


Unprotect[GenerateLieAlgebra,G,LieOperation,VSField,LinearIndependenceFun,
StoppingDepth,StoppingDimension,AssumeHamiltonians,GeneratorSymbols,
NumericalLieAlgebra];


AssignUsage[GenerateLieAlgebra,$Usages];


AssignUsage[G,$Usages];


AssignUsage[LieOperation,$Usages];
AssignUsage[VSField,$Usages];
AssignUsage[LinearIndependenceFun,$Usages];
AssignUsage[StoppingDepth,$Usages];
AssignUsage[StoppingDimension,$Usages];
AssignUsage[AssumeHamiltonians,$Usages];
AssignUsage[GeneratorSymbols,$Usages];
AssignUsage[NumericalLieAlgebra,$Usages];


(* ::Subsubsection:: *)
(*Options*)


Options[GenerateLieAlgebra]={
	LieOperation->Com,
	Orthogonalize->True,
	NumericalLieAlgebra->False,
	VSField->Reals,
	LinearIndependenceFun->Automatic,
	StoppingDepth->\[Infinity],
	StoppingDimension->Automatic,
	AssumeHamiltonians->True,
	Threshold->10^-10,
	GeneratorSymbols->Automatic
};


(* ::Subsubsection::Closed:: *)
(*Errors*)


GenerateLieAlgebra::moreargs = "At least two input generators required. Remember input should be a Sequence, and not a List.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Lie Algebra Generation*)


GenerateLieAlgebra[generators__,OptionsPattern[]]:=Module[
	{
		gens,labs,
		com,
		num,dim,
		nextElement,nextPair,depth,
		\[Phi],
		parentSpot,expandOnce,expandAll,
		currentMax,currentDepth,
		meetsCriteria,
		freeBasis,
		i,j,x,
		stoppingDepth,stoppingDimension,stopSearching,
		field,orth,linIndep,
		vectorizor,devectorizor,ip,M,numGens,
		currentElement,noNewElems,noNewElemsAllDepth,
		symbols
	},

	(* Various constants *)
	gens={generators};
	If[Length@gens<2,Message[GenerateLieAlgebra::moreargs];Abort[]];
	If[OptionValue@AssumeHamiltonians,gens=I*gens];
	If[OptionValue@NumericalLieAlgebra,gens=N@gens];
	com[args___]:=OptionValue[LieOperation][args];
	dim=Length[First@gens];
	num=Length[gens];
	stoppingDimension=OptionValue@StoppingDimension;
	If[stoppingDimension===Automatic,stoppingDimension=dim^2-1];
	stoppingDepth=OptionValue@StoppingDepth;
	field=OptionValue@VSField;
	orth=OptionValue@Orthogonalize;
	linIndep=OptionValue@LinearIndependenceFun;
	If[field==Reals,
		ip[v1_,v2_]:=v1.v2;,
		ip[v1_,v2_]:=Conjugate[v1].v2;
	];
	If[linIndep===Automatic,
		(* only works if orth=True*)
		linIndep=((Norm[#2]>OptionValue[Threshold]) && Not[Abs[Norm[ip[#1,#2]]-Norm[#2]]<OptionValue[Threshold]])&
	];

	(* List of Symbols to represent the generators. *)
	symbols=OptionValue[GeneratorSymbols];
	If[symbols===Automatic,
		symbols=Array[G,num];
	];

	(* Indeces and bounds *)
	i=1;
	j=2;
	currentDepth=2;
	currentMax=num;
	noNewElems=True;
	noNewElemsAllDepth=False;

	(* freeBasis will be a list of basis elements (up to some depth) for the free Lie algebra (i.e. where matrix linear dependence is ignored). *)
	(* The first num elements of freeBasis will be of the form \[Phi][n]. *)
	(* The rest will be of the form {\[Phi][n],\[Phi][m]} where n and m are references to indices earlier in the list. *)
	(* This is a nice storage format because it is both space efficient, and convenient for the P Hall criteria. *)
	freeBasis=Table[\[Phi][n],{n,num}];

	(* Helper functions to expand expressions like \[Phi][56] into expressions like {\[Phi][32],\[Phi][15]}. expandAll will do *)
	(* this until no longer possible, so that you end up with integers no bigger than the number of generators. *)
	expandOnce[expr_]:=expr/.{\[Phi][n_?(#>num&)]:>freeBasis[[n]]};
	expandAll[expr_]:=expr//.{\[Phi][n_?(#>num&)]:>freeBasis[[n]]};

	(* Functions to compute the depth of a given element, and the index of the left parent *)
	parentSpot[\[Phi][n_]]:=0;
	parentSpot[{\[Phi][n_],\[Phi][m_]}]:=n;
	Table[depth[\[Phi][n]]=1;,{n,num}];
	depth[{\[Phi][n_],\[Phi][m_]}]:=(depth[{\[Phi][n],\[Phi][m]}]=depth@expandOnce[\[Phi][n]]+depth@expandOnce[\[Phi][m]]);

	(* The P Hall criteria, with an addition enforcing that the current Lie operation have the current depth. *)
	(* This condition is added for bookkeeping to make it easier to ensure we work through an entire depth before moving to the next one. *)
	meetsCriteria:=(depth[{\[Phi][i],\[Phi][j]}]==currentDepth)&&((j<=num)||(parentSpot[expandOnce[\[Phi][j]]]<=i));

	(* Loop i and j between 1 and currentMax, enforcing i<j *)
	(* When i hits the end, we have reached the end of the current depth, and start fresh. *)
	(* It is possible to skip some pairs based on depth logic, but calling meetsCriteria is fast enough that this is probably not worth the headache. *)
	nextPair:=(
		If[++j>currentMax,
			If[++i>=currentMax,
				currentMax=Length[freeBasis];
				currentDepth++;
				(* Keep track of whether a new lin indep elem was added during a given depth for stopping criterion. *)
				If[noNewElems===True,noNewElemsAllDepth=True;];
				noNewElems=True;
				i=1;
			];
			j=i+1;
		];
	);

	(* Keep picking the nextPair until it meetsCriteria; append to the list *)
	nextElement:=(
		While[Not[meetsCriteria],nextPair;];
		With[{next={\[Phi][i],\[Phi][j]}},AppendTo[freeBasis,next];nextPair;next]
	);

	(* One can prove that separating into reals and imaginaries lets you compute R-linear-independance in C. *)
	If[field===Reals,
		vectorizor[A_]:=Flatten[{Re@A,Im@A}];
		devectorizor[v_]:=Partition[v[[1;;dim^2]]+I*v[[dim^2+1;;-1]],dim];,
		vectorizor[A_]:=Flatten[A];
		devectorizor[v_]:=Partition[v,dim];
	];

	(* Make a matrix which spans the output Lie algebra. *)
	M=vectorizor/@gens;
	If[orth,M=Select[Orthogonalize[M],Norm[#]>$MachineEpsilon&]];

	(* Condition to stop looking for more basis elements. *)
	stopSearching:= noNewElemsAllDepth || (currentDepth>stoppingDepth) || (Length[M]>=stoppingDimension);

	com[\[Phi][n_]]:=gens[[n]];
	If[orth,
		com[{\[Phi][n_],\[Phi][m_]}]:=(com[{\[Phi][n],\[Phi][m]}]=
			With[{A=OptionValue[LieOperation][com@expandOnce@\[Phi][n],com@expandOnce@\[Phi][m]]},
				With[{norm=Sqrt[Tr[A\[ConjugateTranspose].A]]},If[norm>OptionValue@Threshold,A/norm,0*A]]
			]
		);,
		com[{\[Phi][n_],\[Phi][m_]}]:=(com[{\[Phi][n],\[Phi][m]}]=OptionValue[LieOperation][com@expandOnce@\[Phi][n],com@expandOnce@\[Phi][m]]);
	];

	(* Main loop. *)
	While[Not[stopSearching],
		currentElement=nextElement;

		(* Check for linear independence. *)
		If[linIndep[M,vectorizor[com[currentElement]]],
			noNewElems=False;
			If[orth,
				(* Append the normalized projection onto the space orthogonal to M. *)
				AppendTo[M,With[{v=vectorizor[com[currentElement]]},Normalize[v-Total[ip[M,v].M]]]],
				(* Append the whole thing. *)
				AppendTo[M,vectorizor[com[currentElement]]]
			];
		];
	];

	numGens=Length@M;
	If[OptionValue@AssumeHamiltonians,M=-I*M;];
	com[{\[Phi][n_],\[Phi][m_]}]:=(com[{\[Phi][n],\[Phi][m]}]=OptionValue[LieOperation][com@expandOnce@\[Phi][n],com@expandOnce@\[Phi][m]]);

	(* Output. *)
	{
		numGens,
		With[{reps=Thread[freeBasis[[1;;num]]->symbols[[1;;num]]]},expandAll[freeBasis[[1;;numGens]]]/.reps],
		devectorizor/@M
	}
]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section:: *)
(*Unit Testing*)


(* ::Section::Closed:: *)
(*End Package*)


Protect[GenerateLieAlgebra,G,LieOperation,VSField,LinearIndependenceFun,
StoppingDepth,StoppingDimension,AssumeHamiltonians,GeneratorSymbols,
NumericalLieAlgebra];


EndPackage[];
