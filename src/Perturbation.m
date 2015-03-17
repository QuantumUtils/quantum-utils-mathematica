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


Needs["QUDevTools`"];
Needs["UnitTesting`"];
Needs["Tensor`"];


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "Perturbation.nb"}]];


(* ::Section:: *)
(*Usage Declarations*)


(* ::Subsection:: *)
(*Magnus Expansion*)


Unprotect[MagnusExpansionTerm,MagnusExpansion,MagnusConvergenceTest,ClearMagnusCache];


AssignUsage[MagnusExpansionTerm,$Usages];
AssignUsage[MagnusExpansion,$Usages];
AssignUsage[MagnusConvergenceTest,$Usages];
AssignUsage[ClearMagnusCache,$Usages];


(* ::Subsection::Closed:: *)
(*Average Hamiltonian*)


Unprotect[AverageHamiltonianTerm,AverageHamiltonian];


AssignUsage[AverageHamiltonianTerm,$Usages];
AssignUsage[AverageHamiltonian,$Usages];


(* ::Subsection::Closed:: *)
(*Matrix Perturbations*)


Unprotect[FirstOrderEigenvector,SecondOrderEigenvalue];


AssignUsage[FirstOrderEigenvector,$Usages];
AssignUsage[SecondOrderEigenvalue,$Usages];


(* ::Subsection:: *)
(*Zassenhaus Expansion*)


Unprotect[ZassenhausTerm,ZassenhausSeries,ZassenhausExpansion,ClearZassenhausCache];


AssignUsage[ZassenhausTerm,$Usages];
AssignUsage[ZassenhausSeries,$Usages];
AssignUsage[ZassenhausExpansion,$Usages];
AssignUsage[ClearZassenhausCache,$Usages];


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Magnus Expansion*)


(* ::Text:: *)
(*Clear memoized values*)


Clear[MagnusGenerator,MagnusExpansionTermCached]


ClearMagnusCache[]:=(
	DownValues[MagnusGenerator]=Part[DownValues[MagnusGenerator],{-1}];
	DownValues[MagnusExpansionTermCached]=Part[DownValues[MagnusExpansionTermCached],{-1}];)


(* ::Text:: *)
(*Set Options*)


Options[MagnusExpansionTerm]={Simplify->Identity,Chop->False,NIntegrate->False};
Options[MagnusGenerator]=Options[MagnusExpansionTerm];
Options[MagnusExpansionTermCached]=Options[MagnusExpansionTerm];
Options[MagnusExpansion]=Options[MagnusExpansionTerm];
Options[MagnusConvergenceTest]={NIntegrate->False};


(* ::Text:: *)
(*Generator term for magnus expansion*)


MagnusGenerator[{A_,t0_,tf_},{n_,j_},opt:OptionsPattern[]]:=
	MagnusGenerator[{A,t0,tf},{n,j},opt]=
		Block[{fn=OptionValue[Simplify]},
			If[fn===True,fn=Simplify];
			If[OptionValue[Chop],fn=Composition[fn,Chop]];
			fn@Which[
				j===1,
					Com[MagnusExpansionTerm[{A,t0,tf},n-1,opt],A[tf]],
				j===n-1,
					Com[MagnusExpansionTerm[{A,t0,tf},1,opt],A[tf],n-1],
				True,	
					Total@Map[
						Com[
							MagnusExpansionTerm[{A,t0,tf},#,opt],
							MagnusGenerator[{A,t0,tf},{n-#,j-1},opt]
						]&,Range[n-j]
						]
				]
			];


(* ::Text:: *)
(*Memoized Magnus expansion term*)


MagnusExpansionTermCached[{A_,t0_,tf_},k_,opt:OptionsPattern[]]:=
	MagnusExpansionTermCached[{A,t0,tf},k,opt]=
		Block[{fn=OptionValue[Simplify]},
			If[fn===True,fn=Simplify];
			If[OptionValue[Chop],fn=Composition[fn,Chop]];
			fn@If[k===1,
				If[OptionValue[NIntegrate],
					NIntegrate[A[t],{t,t0,tf}],
					Integrate[A[t],{t,t0,tf}]
				],
				Total@Map[(BernoulliB[#]/#!)*
					If[OptionValue[NIntegrate],
						NIntegrate[MagnusGenerator[{A,t0,t1},{k,#},opt],{t1,t0,tf}],
						Integrate[MagnusGenerator[{A,t0,t1},{k,#},opt],{t1,t0,tf}]
					]&,
					Range[k-1]]
			]
		];




(* ::Text:: *)
(*Calling Magnus Expansion Term and sum of terms*)


MagnusExpansionTerm[{A_,t0_,tf_},k_,opt:OptionsPattern[]]:=
	MagnusExpansionTermCached[{A,t0,tf},k,opt]

MagnusExpansionTerm[{A_,tf_},k_,opt:OptionsPattern[]]:=MagnusExpansionTerm[{A,0,tf},k,opt]


MagnusExpansion[{A_,t0_,tf_},order_,opt:OptionsPattern[]]:=Map[MagnusExpansionTerm[{A,t0,tf},#,opt]&,Range[order]];
MagnusExpansion[{A_,tf_},order_,opt:OptionsPattern[]]:=MagnusExpansion[{A,0,tf},order,opt]


MagnusConvergenceTest[{A_,t0_,tf_},opts:OptionsPattern[]]:=
	If[OptionValue[NIntegrate],
		NIntegrate[Norm[A[t],"Frobenius"],{t,t0,tf}],
		Integrate[Norm[A[t],"Frobenius"],{t,t0,tf}]
	]

MagnusConvergenceTest[{A_,tf_},opts:OptionsPattern[]]:=MagnusConvergenceTest[{A,0,tf},opts]


(* ::Subsection:: *)
(*Average Hamiltonian*)


Options[AverageHamiltonianTerm]=Options[MagnusGenerator];
Options[AverageHamiltonian]=Options[MagnusGenerator];


AverageHamiltonianTerm[{H_,t0_,tf_},k_,opt:OptionsPattern[]]:=
	(-I)^k*MagnusExpansionTerm[{H,t0,tf},k+1,opt]/(tf-t0)

AverageHamiltonianTerm[{H_,tf_},k_,opt:OptionsPattern[]]:=AverageHamiltonianTerm[{H,0,tf},k,opt]


AverageHamiltonian[{H_,t0_,tf_},order_,opt:OptionsPattern[]]:=
	Total[
		Array[(-I)^(#-1)/(tf-t0)&,order+1]*MagnusExpansion[{H,t0,tf},order+1,opt]
	]
AverageHamiltonian[{H_,tf_},order_,opt:OptionsPattern[]]:=AverageHamiltonian[{H,0,tf},order,opt]


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


(* ::Subsubsection:: *)
(*Generator Function*)


(* ::Text:: *)
(*Clear memoized values*)


Clear[ZassenhausGenerator,ZassenhausTermCached];


ClearZassenhausCache[]:=(
	DownValues[ZassenhausGenerator]=Part[DownValues[ZassenhausGenerator],{-1}];
	DownValues[ZassenhausTermCached]=Part[DownValues[ZassenhausTermCached],{-1}];)


ZassenhausGenerator[X_,Y_,n_,k_]:=
	ZassenhausGenerator[X,Y,n,k]=
	If[n===1,
		Total@Map[
			(-1)^k/(#!(k-#)!) Com[Y,Com[X,Y,#],k-#]&,
			Range[k]
		],
		Total@Map[(-1)^#/(#!) 
			Com[
				ZassenhausTermCached[X,Y,n],
				ZassenhausGenerator[X,Y,n-1,k-n*#],#
			]&,
			Range[0,Floor[k/n]-1]
		]
	];


ZassenhausTermCached[X_,Y_,n_]:=
	ZassenhausTermCached[X,Y,n]=
		Which[
			n===0, X,
			n===1, Y,
			IntegerQ[n], (1/n)*ZassenhausGenerator[X,Y,If[n>=5,Floor[(n-1)/2],1],n-1]
		]


(* ::Subsubsection:: *)
(*Expansion Terms*)


ZassenhausTerm[X_,Y_,n_]:=ZassenhausTermCached[X,Y,n]


ZassenhausSeries[X_,Y_,n_]:=ZassenhausTerm[X,Y,#]&/@Range[0,n]


(* ::Subsubsection:: *)
(*MatrixExp Expansion*)


(* ::Text:: *)
(*Cached function*)


Clear[ZassenhausExpansionCached];


ZassenhausExpansionCached[\[Lambda]_,X_,Y_,n_]:=
	ZassenhausExpansionCached[\[Lambda],X,Y,n]=
		Which[
			n===0,
				MatrixExp[\[Lambda] X],
			n===1,
				ZassenhausExpansionCached[\[Lambda],X,Y,0].MatrixExp[\[Lambda] Y],
			IntegerQ[n],
				ZassenhausExpansionCached[\[Lambda],X,Y,n-1].MatrixExp[(\[Lambda]^n)*ZassenhausTerm[X,Y,n]]
		]


ZassenhausExpansion[\[Lambda]_:1,X_,Y_,n_]:=ZassenhausExpansionCached[\[Lambda],X,Y,n]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section:: *)
(*Unit Testing*)


(* ::Section:: *)
(*End Package*)


(* ::Text:: *)
(*The commented sections represent functions which need unprotection for memoization to work.*)


Protect[MagnusExpansionTerm,MagnusExpansion,MagnusConvergenceTest,ClearMagnusCache];
Protect[AverageHamiltonianTerm,AverageHamiltonian];
Protect[FirstOrderEigenvector,SecondOrderEigenvalue];
Protect[ZassenhausSeries,ZassenhausTerm,ZassenhausExpansion,ClearZassenhausCache];


EndPackage[];
