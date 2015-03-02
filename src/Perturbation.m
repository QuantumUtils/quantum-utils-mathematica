(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Perturbation*)


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


BeginPackage["Perturbation`"];


Needs["DocTools`"];
Needs["QUOptions`"];
Needs["UnitTesting`"];
Needs["Tensor`"];


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "Perturbation.nb"}]];


(* ::Section:: *)
(*Usage Declarations*)


(* ::Subsection:: *)
(*Magnus Expansion*)


Unprotect[MagnusExpansionTerm,MagnusExpansion,MagnusGenerator,MagnusConvergenceTest];
ClearAll[MagnusExpansionTerm,MagnusGenerator];


AssignUsage[MagnusExpansionTerm,$Usages];
AssignUsage[MagnusExpansion,$Usages];
AssignUsage[MagnusGenerator,$Usages];
AssignUsage[MagnusConvergenceTest,$Usages];


(* ::Subsection:: *)
(*Average Hamiltonian*)


Unprotect[AverageHamiltonianTerm,AverageHamiltonian];


AssignUsage[AverageHamiltonianTerm,$Usages];
AssignUsage[AverageHamiltonian,$Usages];


(* ::Subsection:: *)
(*Matrix Perturbations*)


Unprotect[FirstOrderEigenvector,SecondOrderEigenvalue];


AssignUsage[FirstOrderEigenvector,$Usages];
AssignUsage[SecondOrderEigenvalue,$Usages];


(* ::Subsection:: *)
(*Zassenhaus Expansion*)


Unprotect[ZassenhausExpansion,ZaussenhausGenerator];
ClearAll[ZassenhausGenerator,ZassenhausTerm];


AssignUsage[ZassenhausGenerator,$Usages];
AssignUsage[ZassenhausExpansion,$Usages];


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Magnus Expansion*)


(* ::Subsection:: *)
(*Average Hamiltonian*)


(* ::Subsection::Closed:: *)
(*Matrix Perturbations*)


FirstOrderEigenvector[A_,B_,\[Lambda]_:All,output_:"sum"]:=
	Block[
		{
			IP,
			dim,inds,deg,M,k,
			eigs,cureig,
			v,eigspacedirections,
			vals,vecs,valshc,vecshc,valOrder,
			pertvecs1,pertvals1
		},
		IP[v1_,v2_]:=Simplify[Conjugate[v1].v2];
		dim=Length[A];
		
		(* First we get the unperturbed eigensystem of both A and A\[HermitianConjugate]*)
		{vals,vecs}=Eigensystem[A]//Simplify;
		If[A===Simplify[A\[HermitianConjugate]],
			{valshc,vecshc}={vals,vecs},
			{valshc,vecshc}=Eigensystem[FullSimplify[A\[HermitianConjugate]]]//Simplify;
			valOrder=Flatten@DeleteDuplicates@Table[Select[Table[m,{m,dim}],FullSimplify[Conjugate[valshc[[n]]]-vals[[#]]]==0&],{n,dim}];
			valshc=Permute[valshc,valOrder];
			vecshc=Permute[vecshc,valOrder];
		];

		(* Next we determine exactly which eigenvalues we're supposed to perturb *)
		eigs=
			Which[
				\[Lambda]===All,DeleteDuplicates[vals],
				ListQ[\[Lambda]],\[Lambda],
				True,{\[Lambda]}
			];

		(* Initialize the output *)
		pertvecs1={};

		(* Finally we loop thrugh eigs and calculate the perturbation on each one *)
		(* This algorithm can be found, eg, on pg 15 of Perturbation Methods by Hinch*)
		For[k=1,k<=Length[eigs],k++,
			cureig=eigs[[k]];

			(* Get the indeces of the eigenvalues *)
			inds=Select[Table[m,{m,dim}],(vals[[#]]==cureig)&];

			(* deg is the ammount of degeneracy*)
			deg=Length[inds];
			If[deg==0,Print["Eigenvalue "<>ToString[cureig]<>" not found"];Abort[];];

			(* M is the degeneracy matrix; it's eigenvalues are the first order perturbations. If deg=1, M is 1x1*)
			M=Table[IP[vecshc[[i]],B.vecs[[j]]]/IP[vecshc[[i]],vecs[[i]]],{i,deg},{j,deg}];
			{pertvals1,v}=Eigensystem[M];
			eigspacedirections=Table[Plus@@(DiagonalMatrix[v[[m]]].vecs[[inds]]),{m,deg}];

			(* We need the first order perturbed vectors in order to get the 2nd order value*)
			pertvecs1=Append[pertvecs1,
				Sum[
					If[MemberQ[inds,i],0,
						(IP[vecshc[[i]],B.#]vecs[[i]])/((cureig-vals[[i]])IP[vecshc[[i]],vecs[[i]]])
					],
					{i,dim}
				]&/@eigspacedirections
			];
		];

		(* Return all perturbations *)
		pertvecs1

	]


SecondOrderEigenvalue[A_,B_,\[Lambda]_:All,output_:"sum"]:=
	Block[
		{
			IP,
			dim,inds,deg,M,k,
			eigs,cureig,
			v,eigspacedirections,
			vals,vecs,valshc,vecshc,valOrder,
			pertvecs1,pertvals1,pertvals2
		},
		IP[v1_,v2_]:=Simplify[Conjugate[v1].v2];
		dim=Length[A];
		
		(* First we get the unperturbed eigensystem of both A and A\[HermitianConjugate]*)
		{vals,vecs}=Eigensystem[A]//Simplify;
		If[A===Simplify[A\[HermitianConjugate]],
			{valshc,vecshc}={vals,vecs},
			{valshc,vecshc}=Eigensystem[A\[HermitianConjugate]]//Simplify;
			(*valOrder=Flatten@DeleteDuplicates@Table[Select[Table[m,{m,dim}],Simplify[Conjugate[valshc[[n]]]-vals[[#]]]==0&],{n,dim}];*)
			(*valOrder=FindPermutation[FullSimplify[Conjugate[valshc]],vals];*)
			valOrder=InversePermutation[Total/@Transpose[Map[(#==0)/.{True->0,False->1}&,Outer[Conjugate[#1].#2&,vecs,vecshc,1]//Simplify,{2}]*Table[n,{n,dim}]]];
			valshc=Permute[valshc,valOrder];
			vecshc=Permute[vecshc,valOrder];
		];

		(* Next we determine exactly which eigenvalues we're supposed to perturb *)
		eigs=
			Which[
				\[Lambda]===All,DeleteDuplicates[vals],
				ListQ[\[Lambda]],\[Lambda],
				True,{\[Lambda]}
			];

		(* Finally we loop thrugh eigs and calculate the perturbation on each one *)
		(* This algorithm can be found, eg, on pg 15 of Perturbation Methods by Hinch*)
		For[k=1,k<=Length[eigs],k++,
			cureig=eigs[[k]];

			(* Get the indeces of the eigenvalues *)
			inds=Select[Table[m,{m,dim}],(vals[[#]]==cureig)&];

			(* deg is the ammount of degeneracy*)
			deg=Length[inds];
			If[deg==0,Print["Eigenvalue "<>ToString[cureig]<>" not found"];Abort[];];

			(* M is the degeneracy matrix; it's eigenvalues are the first order perturbations. If deg=1, M is 1x1*)
			M=Table[IP[vecshc[[i]],B.vecs[[j]]]/IP[vecshc[[i]],vecs[[i]]],{i,deg},{j,deg}];
			{pertvals1,v}=Eigensystem[M];
			eigspacedirections=Table[Plus@@(DiagonalMatrix[v[[m]]].vecs[[inds]]),{m,deg}];

			(* We need the first order perturbed vectors in order to get the 2nd order value*)
			pertvecs1=Sum[
				If[MemberQ[inds,i],0,
					(IP[vecshc[[i]],B.#]vecs[[i]])/((cureig-vals[[i]])IP[vecshc[[i]],vecs[[i]]])
				],
				{i,dim}
			]&/@eigspacedirections;

			(* Finally what we're after*)
			pertvals2=Table[
				With[
					{e=Plus@@(DiagonalMatrix[Conjugate[v[[i]]]].vecshc[[inds]])},
					(IP[e,B.pertvecs1[[i]]]-pertvals1[[i]]IP[e,pertvecs1[[i]]])/IP[e,eigspacedirections[[i]]]
				],
				{i,deg}
				]//Simplify;
			eigs[[k]]=cureig+pertvals1+pertvals2;
		];

		(* Return all perturbations *)
		Flatten@eigs
	]


(* ::Subsection:: *)
(*Zassenhaus Expansion*)


(* ::Text:: *)
(*The generator form is taken from "Efficient Computation of the Zassenhaus formula" F. Casas, A. Murua, M. Nadinic, Computer Physics Communications 183 (2012) 2386-2391.*)


ZassenhausGenerator[X_,Y_,1,k_]:=
	ZassenhausGenerator[X,Y,1,k]=
		Sum[(-1)^k/(j!(k-j)!) NestCom[Y,NestCom[X,Y,j],k-j],{j,1,k}]
ZassenhausGenerator[X_,Y_,n_,k_]:=
	ZassenhausGenerator[X,Y,n,k]=
		Sum[(-1)^j/j! NestCom[ZassenhausTerm[X,Y,n],ZassenhausGenerator[X,Y,n-1,k-n*j],j],{j,0,Floor[k/n]-1}]

ZassenhausTerm[X_,Y_,n_]:=
	ZassenhausTerm[X,Y,n]=
		1/n ZassenhausGenerator[X,Y,If[n>=5,Floor[(n-1)/2],1],n-1]

ZassenhausExpansion[\[Lambda]_:1,X_,Y_,1]:=
	ZassenhausExpansion[\[Lambda],X,Y,1]=
		MatrixExp[\[Lambda] X].MatrixExp[\[Lambda] Y];

ZassenhausExpansion[\[Lambda]_:1,X_,Y_,n_]:=
	ZassenhausExpansion[\[Lambda],X,Y,n]=
		ZassenhausExpansion[\[Lambda],X,Y,n-1].MatrixExp[\[Lambda]^n*ZassenhausTerm[X,Y,n]];


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section:: *)
(*Unit Testing*)


(* ::Section::Closed:: *)
(*End Package*)


Unprotect[MagnusExpansionTerm,MagnusExpansion,MagnusConvergenceTest];
Unprotect[AverageHamiltonianTerm,AverageHamiltonian];
Unprotect[FirstOrderEigenvector,SecondOrderEigenvalue];
Unprotect[ZassenhausExpansion];


EndPackage[];
