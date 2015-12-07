(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Tensor Package*)


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


BeginPackage["Tensor`",{"QUDoc`","Predicates`"}];


Needs["QUDevTools`"];


$TensorUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "Tensor.nb"}]];


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection::Closed:: *)
(*Matrices and Operations*)


Unprotect[CircleTimes,BlockMatrix,UnitArray,TensorFactorPermutations,OuterProduct,Projector,Com,ACom,SwapMatrix];


AssignUsage[CircleTimes,$TensorUsages];
AssignUsage[\[DoubleStruckOne]->IdentityMatrixShorthand,$TensorUsages];
AssignUsage[BlockMatrix,$TensorUsages];
AssignUsage[UnitArray,$TensorUsages];
AssignUsage[TensorFactorPermutations,$TensorUsages];
AssignUsage[SwapMatrix,$TensorUsages];
AssignUsage[Com,$TensorUsages];
AssignUsage[ACom,$TensorUsages];
AssignUsage[OuterProduct,$TensorUsages];
AssignUsage[Projector,$TensorUsages];


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Manipulations*)


Unprotect[MatrixToTensor,MatrixTranspose,Swap,Reshuffle,Unravel,Reravel];


AssignUsage[MatrixToTensor,$TensorUsages];
AssignUsage[MatrixTranspose,$TensorUsages];
AssignUsage[Swap,$TensorUsages];
AssignUsage[Reshuffle,$TensorUsages];
AssignUsage[Unravel,$TensorUsages];
AssignUsage[Reravel,$TensorUsages];


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Contractions*)


Unprotect[PartialTr,TensorPairContract,MatrixContract,MatrixPairContract];


AssignUsage[PartialTr,$TensorUsages];
AssignUsage[TensorPairContract,$TensorUsages];
AssignUsage[MatrixContract,$TensorUsages];
AssignUsage[MatrixPairContract,$TensorUsages];


(* ::Subsection::Closed:: *)
(*Matrix Bases*)


Unprotect[Basis,BasisLabels,ExpressInBasis];


AssignUsage[Basis,$TensorUsages];
AssignUsage[BasisLabels,$TensorUsages];
AssignUsage[ExpressInBasis,$TensorUsages];


(* ::Subsection::Closed:: *)
(*Vectorization*)


Unprotect[Vec,Devec,ProductIdentity,BasisMatrix,BasisTransformation];


AssignUsage[Vec,$TensorUsages];
AssignUsage[Devec,$TensorUsages];
AssignUsage[ProductIdentity,$TensorUsages];
AssignUsage[BasisMatrix,$TensorUsages];
AssignUsage[BasisTransformation,$TensorUsages];


(* ::Subsection::Closed:: *)
(*Tensor Product Parser*)


Unprotect[TP];


AssignUsage[TP,$TensorUsages];


(* ::Subsection::Closed:: *)
(*Error Messages*)


(* ::Subsubsection:: *)
(*Matrices and Operations*)


TensorFactorPermutations::input = "Input must be a sequence of elements {op,int}.";


OuterProduct::input = "Inputs must be a vectors, column vectors, or matrices."


(* ::Subsubsection:: *)
(*Tensor Manipulations*)


MatrixToTensor::dims = "Dimensions of tensor must be a list of integers, or a list of two lists of integers.";


Swap::input = "Input operator must be a square matrix, vector, row vector or column vector.";


Unravel::bipartite="System must be composed of bipartite tensors to unravel.";
Unravel::input="Input operator must be a vector or matrix.";
Unravel::dims="Invalid specification of subsystem dimension.";


(* ::Subsubsection:: *)
(*Tensor Contractions*)


PartialTr::input = "Input must be a square matrix, vector, or column vector.";


(* ::Subsubsection:: *)
(*Vectorization*)


BasisMatrix::dims = "Dimensions of input system must be specified.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Matrices and Operations*)


Subscript[\[DoubleStruckOne], 0]:={{}}
Subscript[\[DoubleStruckOne], n_]:=IdentityMatrix[n]


UnitArray[dims_List,ind_List,SparseArray]:=
	If[Length[dims]===Length[ind],
		SparseArray[ind->1,dims]
		,Message[UnitArray::errind]
	]
UnitArray[dims_List,ind_List]:=Normal@UnitArray[dims,ind,SparseArray]


(* ::Text:: *)
(*This first definition must come before setting the Flat attribute or it will lead to infinite recurrsion.*)


SetAttributes[CircleTimes,OneIdentity];
CircleTimes[first_]:=first
SetAttributes[CircleTimes,Flat];


CircleTimes[first_?NumericQ,rest_]:=first*CircleTimes[rest]
CircleTimes[rest_,first_?NumericQ]:=first*CircleTimes[rest]
CircleTimes[first_?VectorQ,last_?VectorQ]:=Flatten[KroneckerProduct[first,last]]
CircleTimes[first_?VectorQ,last_?ArrayQ]:=KroneckerProduct[Partition[first,1],last]
CircleTimes[first_?ArrayQ,last_?VectorQ]:=KroneckerProduct[first,Partition[last,1]]
CircleTimes[first_?ArrayQ,last_?ArrayQ]:=KroneckerProduct[first,last]
CircleTimes[A_->n_Integer]:=CircleTimes@@ConstantArray[A,n]


TensorFactorPermutations[arrayNums__List]:=
	If[AllMatchQ[{_,_Integer},{arrayNums}],
	Plus@@CircleTimes@@@
			Permutations[Join@@ConstantArray@@@{arrayNums}],
	Message[TensorFactorPermutations::input]]


TensorFactorPermutations[arrayNums__Rule]:=TensorFactorPermutations[Sequence@@List@@@{arrayNums}]


(* ::Text:: *)
(*Commutator for matrices *)


Com[A_,B_,0]:=B;
Com[A_,B_,1]:=Com[A,B]
Com[A_?MatrixQ,B_?MatrixQ]:=A.B-B.A
Com[A_?MatrixQ,B_?MatrixQ,n_?IntegerQ]:=Com[A,Com[A,B],n-1]


(* ::Text:: *)
(*Zero Identities*)


Com[A_,A_]:=0
Com[A_,A_,n_?Positive]:=0
Com[A_,B_?NumericQ]:=0
Com[A_,B_?NumericQ,n_?Positive]:=0
Com[A_?NumericQ,B_]:=0
Com[A_?NumericQ,B_,n_?Positive]:=0


(* ::Text:: *)
(*Anti-commutator*)


ACom[A_,B_,0]:=B;
ACom[A_,B_,1]:=ACom[A,B]
ACom[A_?MatrixQ,B_?MatrixQ]:=A.B+B.A
ACom[A_?MatrixQ,B_?MatrixQ,n_?Positive]:=ACom[A,ACom[A,B],n-1]


(* ::Text:: *)
(*Outer product of two vectors or two matrices. For matrices it returns the outer product of the vectorized matrices.*)


OuterProduct[u_,v_]:=KroneckerProduct[OuterProductVec[u],Conjugate[OuterProductVec[v]]];
OuterProduct[u_,v_,SparseArray]:=OuterProductVec[SparseArray@u,SparseArray@v];

OuterProductVec[u_]:=
	Which[
		GeneralVectorQ[u],Flatten[u],
		MatrixQ[u],Flatten@Vec[u],
		True, Message[OuterProduct::input]]


Projector[v_]:=OuterProduct[v,v]
Projector[v_,SparseArray]:=Projector[SparseArray@v];

Projector[v_,vs__]:=Fold[#1+Projector[#2]&,Projector[v],{vs}]
Projector[v_,vs__,SparseArray]:=Fold[#1+Projector[#2,SparseArray]&,Projector[v,SparseArray],{vs}]


BlockMatrix[B__?MatrixQ]:=
	With[{d=Length/@{B}},
	Sum[ArrayPad[{B}[[i]],{Total@Part[d,1;;i-1],Total@Part[d,i+1;;All]}],
		{i,Length[d]}
	]]


SwapMatrix[ds_List,perm_List,SparseArray]:=
	Flatten[
		ArrayReshape[IdentityMatrix[Times@@ds,SparseArray],Join[ds,ds]]
	,{Range[Length[ds]], perm+Length[ds]}]

SwapMatrix[ds_List,perm_List]:= Normal@SwapMatrix[ds,perm,SparseArray]


SwapMatrix[d_Integer,perm_List,SparseArray]:=SwapMatrix[ConstantArray[d,Length[perm]],perm, SparseArray]
SwapMatrix[d_Integer,perm_List]:= Normal@SwapMatrix[d,perm,SparseArray]


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Manipulations*)


(* ::Subsubsection::Closed:: *)
(*Matrix-Tensor Transpose*)


(* ::Text:: *)
(*Note that this function reshapes so that the subsystem order corresponds to the list of left indicies followed by the list of right indicies. If this splitting is not explicitly specified it will attempt to calculate where it places the split, however there is ambiguitity in where this split occurs in the presence of 1 dimensional subsystems.*)


MatrixToTensorDims[mat_,{dims__Integer}]:=
	With[{dmat=Dimensions[mat]},
	Which[
		Times[dims]^2===Times@@dmat,
			{{dims},{dims}},
		Times[dims]===Times@@dmat,
			Last@Select[
				{Part[{dims},1;;#],Part[{dims}#+1;;-1]}&/@Range[Length[{dims}]-1],
				Apply[Times,#,{1}]==dmat&],
		True,
			Message[MatrixToTensor::dims]
	]]

MatrixToTensorDims[mat_,{{dimsL__Integer},{dimsR__Integer}}]:=
	If[Times@@Dimensions[mat]===Times[dimsL,dimsR],
		{{dimsL},{dimsR}},
		Message[MatrixToTensor::dims]]


MatrixToTensor[mat_,dims_]:=
	ArrayReshape[mat,Flatten@MatrixToTensorDims[mat,dims]]


(* ::Text:: *)
(*Function for implementing TensorTranspose on matrices by first reshaping the matrix to a tensor, then reshaping back to a matrix after the transposition.*)
(*Index ordering goes left then right so L1->1,...,Ln->n, R1->n+1,... Rn->2n*)


MatrixTranspose[mat_,dims_,translist_]:=
	With[{tensDims=MatrixToTensorDims[mat,dims]},
	With[{
		dimsT=Permute[Flatten[tensDims],translist],
		nL=Length@First[tensDims]},
		ArrayReshape[
			TensorTranspose[
				MatrixToTensor[mat,dims],
				translist],
			{Times@@Part[dimsT,1;;nL],Times@@Part[dimsT,nL+1;;-1]}
		]
	]]


(* ::Subsubsection::Closed:: *)
(*Reshuffling*)


Options[Reshuffle]={Basis->"Col"};


Reshuffle[m_,opts:OptionsPattern[Reshuffle]]:=
	With[{d=Sqrt[Dimensions[m]]},
		If[MatrixQ[m]&&AllQ[IntegerQ,d],
			Reshuffle[m,Riffle[d,d],Basis->OptionValue[Basis]],
			Message[Reshuffle::dims]
		]]


Reshuffle[m_,{d1_,d2_,d3_,d4_},OptionsPattern[Reshuffle]]:=
	Which[
		OptionValue[Basis]==="Col",
			ColReshuffle[m,{d1,d2,d3,d4}],
		OptionValue[Basis]==="Row",
			RowReshuffle[m,{d1,d2,d3,d4}],
		True,
			Message[Reshuffle::conv]
		]


ColReshuffle[m_,{dL1_,dL2_,dR1_,dR2_}]:=
	Flatten[
		ArrayReshape[m,{dL1,dL2,dR1,dR2}],
		{{4,2},{3,1}}
	]


RowReshuffle[m_,{dL1_,dL2_,dR1_,dR2_}]:=
	Flatten[
		ArrayReshape[m,{dL1,dL2,dR1,dR2}],
		{{1,3},{2,4}}
	]


(* ::Subsubsection::Closed:: *)
(*Subsystem Swap*)


Swap[op_,dims_List,translist_]:=
	Which[
		Or[GeneralVectorQ[op],RowVectorQ[op]],
			VectorSwap[op,dims,translist],
		SquareMatrixQ[op],
			SquareMatrixSwap[op,dims,translist],
		True,Message[Swap::input]
	]

Swap[op_,d_Integer:2,translist_]:=Swap[op,ConstantArray[d,Length[translist]],translist]


(* ::Text:: *)
(*Used to swap subsystems of composite vectors*)


VectorSwap[vec_,dims_,translist_]:=
	With[{tvec=Flatten[TensorTranspose[ArrayReshape[vec,dims],translist]]},
	Which[
		VectorQ[vec],tvec,
		ColumnVectorQ[vec],Partition[vec,1],
		ColumnVectorQ[vec],{vec},
		True,Null]
	]


(* ::Text:: *)
(*Used to swap subsystems of composite square matrices*)


SquareMatrixSwap[mat_,dims_,translist_]:=
	MatrixTranspose[mat,dims,Join[translist,Length[translist]+translist]]


(* ::Subsubsection::Closed:: *)
(*Unravelling*)


(* ::Text:: *)
(*UnravelTF is a variable that is True for unravelling, False for reraveling*)


FlattenIndex[n_,unravelTF_]:=
	If[unravelTF,
		Riffle[Range[n],Range[n]+n],
		Join[Range[1,2n-1,2],Range[2,2n,2]]
	]


UnravelDims[len_,dims_,unravelTF_]:=With[
	{n=Length[dims],d=Times@@dims},
	Which[
		len==d&&IntegerQ[n/2],
			{dims,FlattenIndex[n/2,unravelTF]},
		len==d*d,
			{If[unravelTF,
				Join[dims,dims],
				Riffle[dims,dims]
			],FlattenIndex[n,unravelTF]},
		True,
			Message[Unravel::bipartite]	
		]
	]


UnravelVector[v_,dims_,unravelTF_]:=With[{
	dn=UnravelDims[Length[v],dims,True]},
	If[ColumnVectorQ[v],Partition[#,1],#]&@Flatten[ArrayReshape[v,First[dn]],{Last[dn]}]
	]


UnravelMatrix[m_,sysDimsLR_,unravelTF_]:=
Block[{
	dims=Dimensions[m],dnL,dnR},
	dnL=UnravelDims[First[dims],First[sysDimsLR],unravelTF];
	dnR=UnravelDims[Last[dims],Last[sysDimsLR],unravelTF];
	Flatten[
		ArrayReshape[m,Join[First[dnL],First[dnR]]],
		{Last[dnL],Length[Last[dnL]]+Last[dnR]}]
	];


Options[Unravel]={Unravel->True};


Unravel[op_,sysDims_List,opts:OptionsPattern[Unravel]]:=
	Which[
		GeneralVectorQ[op],
			UnravelVector[op,Flatten[sysDims],OptionValue[Unravel]],
		MatrixQ[op],
			UnravelMatrix[op,Flatten[{sysDims},ArrayDepth[sysDims]-1],OptionValue[Unravel]],
		True,
			Message[Unravel::input]
	]
	
Unravel[op_,sysDim_Integer:2,opts:OptionsPattern[Unravel]]:=
	With[{
		ns=Log[sysDim,#]&/@Dimensions[op]},
	If[AllQ[IntegerQ,ns],
		Which[
			GeneralVectorQ[op],
				UnravelVector[op,ConstantArray[sysDim,First[ns]],OptionValue[Unravel]],
			MatrixQ[op],
				UnravelMatrix[op,ConstantArray[sysDim,#]&/@ns,OptionValue[Unravel]],
			True,
				Message[Unravel::input]
		],
		Message[Unravel::dims]
	]]


Reravel[args__]:=Unravel[args,Unravel->False]


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Contractions*)


(* ::Text:: *)
(*Partial Trace*)


PartialTr[op_,dimList_,trList_]:=
	With[{tensorDims=Join[dimList,dimList],
		contractIndex={#,#+Length[dimList]}&/@trList,
		nSys=Length[dimList]-Length[trList],
		mat=Which[
				GeneralVectorQ[op],Projector[op],
				SquareMatrixQ[op],op,
				True,Message[PartialTr::input]
			]},
	Flatten[
		TensorContract[ArrayReshape[mat,tensorDims],contractIndex],
		{Range[nSys],nSys+Range[nSys]}]
	]


(* ::Text:: *)
(*Tensor Pair Contraction*)


TensorPairContract[ten1_,ten2_,{pairs__List}]:=
	With[{lr=Transpose[{pairs}], d1=ArrayDepth[ten1], d2=ArrayDepth[ten2]},
		Dot[
			Flatten[ten1,Join[List/@Complement[Range[d1],lr[[1]]],{lr[[1]]}]],
			Flatten[ten2,Join[{lr[[2]]},List/@Complement[Range[d2],lr[[2]]]]]]
		]
TensorPairContract[tens1_,tens2_,{}]:=TensorProduct[tens1,tens2]


(* ::Text:: *)
(*Matrix-Tensor Contraction*)


MatrixContractDims[mat_,matTensDims_,contr_List]:=
	With[{
		pos=Length@First@matTensDims,
		finalDims=Delete[Flatten[matTensDims],Partition[contr,1]]},
			{Times@@Part[finalDims,1;;#],Times@@Part[finalDims,#+1;;-1]}&
			[pos-Length[Select[contr,#<= pos&]]]
	]


MatrixContract[mat_,dims_,{pairs___List}]:=
	With[{tensDims=MatrixToTensorDims[mat,dims]},
		ArrayReshape[
			TensorContract[MatrixToTensor[mat,dims],{pairs}],
		MatrixContractDims[mat,tensDims,Join[pairs]]]
	]


(* ::Text:: *)
(*Matrix-Tensor Pair Contraction*)


MatrixPairContract[{mat1_,dims1_},{mat2_,dims2_},{pairs___List}]:= 
	With[{
		tdims1=MatrixToTensorDims[mat1,dims1],
		tdims2=MatrixToTensorDims[mat2,dims2]},
	ArrayReshape[
		TensorPairContract[
			MatrixToTensor[mat1,dims1],
			MatrixToTensor[mat2,dims2],
			{pairs}]
	, Times[
		MatrixContractDims[mat1,tdims1,First/@{pairs}],
		MatrixContractDims[mat2,tdims2,Last/@{pairs}]]]
	]

MatrixPairContract[{mat1_,dims1_},{mat2_,dims2_},{}]:=CircleTimes[mat1,mat2]


(* ::Subsection::Closed:: *)
(*Matrix Bases*)


(* ::Subsubsection::Closed:: *)
(*Named Bases*)


$POBasis=(PauliMatrix/@Range[0,3]);
$PauliBasis=(PauliMatrix/@Range[0,3])/Sqrt[2];
$WeylBasis[d_]:=Flatten[Table[RotateLeft[DiagonalMatrix[Table[Exp[2\[Pi]*I*n*b/d],{n,0,d-1}]],a]/Sqrt[d],{a,0,d-1},{b,0,d-1}],1];


$NamedBasis={"Pauli"->$PauliBasis,"PO"->$POBasis,"Weyl"[d_]:>$WeylBasis[d]};


(*We need to be fancier than MemberQ because of possible dimension arguments*)
NamedBasisMemberQ[basis_]:=(basis/.Thread[RuleDelayed[Keys[$NamedBasis],True]])===True


CheckNamedBasis[basis_]:=
	If[NamedBasisMemberQ[basis],
		basis/.$NamedBasis,
		basis]


$POBasisLabels={"I","X","Y","Z"};
$PauliBasisLabels={"I","X","Y","Z"}/Sqrt[2];
$WeylBasisLabels[d_]:=Flatten[Table["W["<>ToString[a]<>","<>ToString[b]<>"]",{a,0,d-1},{b,0,d-1}],1]


$NamedBasisLabels={"Pauli"->$PauliBasisLabels,"PO"->$POBasisLabels,"Weyl"[d_]:>$WeylBasisLabels[d]};


(*We need to be fancier than MemberQ because of possible dimension arguments*)
NamedBasisLabelsMemberQ[basis_]:=(basis/.Thread[RuleDelayed[Keys[$NamedBasisLabels],True]])===True


CheckNamedBasisLabels[basis_]:=
	If[NamedBasisLabelsMemberQ[basis],
		basis/.$NamedBasisLabels,
		basis]


Basis[basis_,n_Integer]:=
	With[{opBasis=CheckNamedBasis[basis]},
	If[n===1,opBasis,
	Nest[
		Flatten[
			Outer[CircleTimes,opBasis,#1,1]
		,1]&,
	opBasis,n-1]
	]];
Basis[basis_]:=Basis[basis,1];


Options[BasisLabels]:={Join->Automatic}


BasisLabels[labels_,opts:OptionsPattern[]]:=BasisLabels[labels,1,opts];
BasisLabels[labels_,n_Integer,opts:OptionsPattern[]]:=
	If[labels==="Pauli",
		BasisLabels["PO",n,opts]/Power[Sqrt[2],n],
	With[{
		labels1=CheckNamedBasisLabels[labels],
		joinfun=Which[
				#===Automatic, CircleTimes,
				#===True, StringJoin,
				#===False, List,
				True,#]&[OptionValue[Join]]},
		If[n===1,
			labels1,
			joinfun@@@Tuples[labels1,n]]]
	]


(* ::Subsubsection::Closed:: *)
(*Express In Basis*)


Options[ExpressInBasis]:={Basis->"PO",BasisLabels->False,Join->Automatic,Chop->False};


ExpressInBasis[op_,opts:OptionsPattern[]]:=
	With[{basis=OptionValue[Basis],
		labels=OptionValue[BasisLabels],
		joinfun=OptionValue[Join],
		trim=If[OptionValue[Chop]===True,
				Pick[#1,Abs@Sign@#2,1]&,#1&]},
	With[{coeffs=ExpressInBasisCoeffs[op,basis]},
		Which[
		TrueQ[Not@labels],
			coeffs,
		True,
			trim[Transpose[{ExpressInBasisLabels[op,If[TrueQ[labels],basis,labels],joinfun],coeffs}],
				coeffs]
		]
	]]


ExpressInBasisCoeffs[op_,basis_]:=
	With[{opBasis=CheckNamedBasis[basis]},
	With[{
	n=Log[Length@Flatten@First[opBasis],Length[Flatten[op]]]},
		Map[Tr[ConjugateTranspose[#].op]/Tr[ConjugateTranspose[#].#]&,Basis[opBasis,n]]
	]]


ExpressInBasisLabels[op_,basis_,joinfun_]:=
	With[{labels1=CheckNamedBasisLabels[basis]},
	With[{
	n=Log[Length[labels1],Length[Flatten[op]]]},
		BasisLabels[basis,n,Join->joinfun]
	]]


(* ::Subsection::Closed:: *)
(*Vectorization*)


(* ::Subsubsection::Closed:: *)
(*Options and bases*)


Options[Vec]={Basis->"Col"};


(* ::Subsubsection::Closed:: *)
(*Main Functions*)


Vec[m_,opts:OptionsPattern[Vec]]:=BasisImplimentation["Vec",m,opts]
Devec[args__,opts:OptionsPattern[Vec]]:=BasisImplimentation["Devec",args,opts]
ProductIdentity[A_,C_,opts:OptionsPattern[Vec]]:=BasisImplimentation["ProductIdentity",A,C,opts]


BasisImplimentation[fn_String,args__,opts:OptionsPattern[Vec]]:=
	With[{
		basis=OptionValue[Vec,opts,Basis],
		funcs=ToExpression["Tensor`Private`"<>fn<>#]&/@{"Col","Row","Basis"}},
		Which[
			basis==="Col",funcs[[1]][args],
			basis==="Row",funcs[[2]][args],
			NamedBasisMemberQ[basis],funcs[[3]][(basis/.$NamedBasis),args],
			True,funcs[[3]][basis,args]]]


(* ::Subsubsection::Closed:: *)
(*Convention Implementations*)


VecRow[m_]:= Partition[Flatten[m],1]
DevecRow[v_,{dL_,dR_}]:= ArrayReshape[v,{dL,dR}]
DevecRow[v_]:= With[{d=Sqrt[Length[v]]},DevecRow[v,{d,d}]]
ProductIdentityRow[A_,C_]:=CircleTimes[A,Transpose[C]]


VecCol[m_]:= Partition[Flatten[m,{2,1}],1]
DevecCol[v_,{dL_,dR_}]:= Transpose[DevecRow[v,{dL,dR}]]
DevecCol[v_]:= Transpose[DevecRow[v]]
ProductIdentityCol[A_,C_]:=CircleTimes[Transpose[C],A]


VecBasis[basis_,m_]:=BasisTransformation[VecCol[m],Rule["Col",basis]]
DevecBasis[basis_,v_]:=
	DevecCol[
		BasisTransformation[v,Rule[basis,"Col"]]]

DevecBasis[basis_,v_,{dL_,dR_}]:=
	DevecCol[
		BasisTransformation[v,Rule[basis,"Col"]],{dL,dR}]

ProductIdentityBasis[basis_,A_,C_]:=
	BasisTransformation[ProductIdentityCol[A,C],Rule["Col",basis]]


(* ::Subsubsection::Closed:: *)
(*Change of Basis Matrix*)


BasisMatrixCol["Row",{dL_Integer,dR_Integer}]:=SwapMatrix[{dL,dR},{2,1},SparseArray]
BasisMatrixCol["Row",d_Integer]:=BasisMatrixCol[{d,d}]
BasisMatrixCol["Row"]:=Message[BasisMatrix::dims]
BasisMatrixCol["Col",{dL_Integer,dR_Integer}]:=IdentityMatrix[dL*dR,SparseArray];
BasisMatrixCol["Col",d_Integer]:=BasisMatrixCol["Col",{d,d}]
BasisMatrixCol["Col"]:=Message[BasisMatrix::dims]

BasisMatrixCol[basis_List]:=BasisMatrixCol[basis,1]
BasisMatrixCol[basis_,0]:={{1}};
BasisMatrixCol[basis_List,1]:=With[{d=Length[basis]},ArrayReshape[basis,{d,d}]]
BasisMatrixCol[basis_List,n_Integer]:=
	BasisMatrixCol[basis,n]=
		Reravel[
			CircleTimes[SparseArray[BasisMatrixCol[basis,1]]->n]
			,Length[First[basis]]]


BasisMatrix[Rule["Col",basis_],arg___]:=
	Which[
		basis==="Row",BasisMatrixCol["Row",arg],
		ListQ[basis],BasisMatrixCol[basis,arg],
		NamedBasisMemberQ[basis],BasisMatrixCol[basis/.$NamedBasis,arg],
		True,Message[BasisMatrix::input]
	]
BasisMatrix[Rule[basis_,"Col"],arg___]:=ConjugateTranspose@BasisMatrix[Rule["Col",basis],arg]


BasisMatrix[Rule[basis1_List,basis2_List],arg___]:=
	BasisMatrix[Rule["Col",basis2],arg].BasisMatrix[Rule[basis1,"Col"],arg]


(* ::Subsubsection::Closed:: *)
(*Basis Transformations*)


BasisMatrixColList["Row",op_]:=
	With[{dims=Sqrt[Dimensions[op]]},
		If[AllQ[IntegerQ,dims],
			BasisMatrixCol["Row",#]&/@dims,
			Message[BasisMatrix::dims]		
		]]


BasisMatrixColList[basis_List,op_]:=
	With[{ns=Log[Length[First[basis]],#]&/@Sqrt[Dimensions[op]]},
	If[
		AllQ[IntegerQ,ns],
		BasisMatrixCol[basis,#]&/@ns,
		Message[Vec::basis]
	]]


BasisMatrixColList[basis_,op_]:=
	If[NamedBasisMemberQ[basis],
		BasisMatrixColList[basis/.$NamedBasis,op],
		Message[BasisMatrix::keys]
	]


BasisTransformation[op_,transMat_?MatrixQ]:=BasisTransformation[op,{transMat}]
BasisTransformation[op_,{transMat__?MatrixQ}]:=
	Which[
		GeneralVectorQ[op],
			First[{transMat}].op,
		MatrixQ[op],
			First[{transMat}].op.ConjugateTranspose[Last[{transMat}]],
		RowVectorQ[op],
			op.ConjugateTranspose[Part[{transMat},2]],
		True,
			Message[BasisTransformation::input]
	]


BasisTransformation[op_,Rule[basis_,basis_]]:=op

BasisTransformation[op_,Rule["Col",basis_]]:=
	BasisTransformation[op,BasisMatrixColList[basis,op]]

BasisTransformation[op_,Rule[basis_,"Col"]]:=
	BasisTransformation[op,ConjugateTranspose/@BasisMatrixColList[basis,op]]

BasisTransformation[op_,Rule[basis1_,basis2_]]:=
	BasisTransformation[
		BasisTransformation[op,Rule[basis1,"Col"]],
		Rule["Col",basis2]
	]


(* ::Subsection::Closed:: *)
(*Tensor Product Parser*)


(* ::Subsubsection::Closed:: *)
(*Options*)


(* ::Text:: *)
(*Options for TP: Method allows for specification of the function used to combine operator elements. The default is KroneckerProduct. *)
(*Rules is the list of assignments for strings to operators, everything not in the list of rules is assumed to be a scalar coefficient. The string "i" is reserved for the complex number I, so don't use this in the rules. *)


$defaultTPRules={
	"X"->PauliMatrix[1],
	"Y"->PauliMatrix[2],
	"Z"->PauliMatrix[3],
	"I"->PauliMatrix[0],
	"P"->{{0,1},{0,0}},
	"M"->{{0,0},{1,0}},
	"U"->{{1,0},{0,0}},
	"D"->{{0,0},{0,1}},
	"H"->{{1,1},{1,-1}}/Sqrt[2],
	"S"->{{1,0},{0,I}},
	"T"->{{1,0},{0,Exp[I*Pi/4]}}
};


Options[TP]={Method->CircleTimes,Replace->$defaultTPRules};


(* ::Subsubsection:: *)
(*Function*)


(* ::Text:: *)
(*The following function parses the operators from a substring*)


TPOps[subStr_,opts:OptionsPattern[TP]]:=
	Block[{
		rules=OptionValue[Replace],
		method=OptionValue[Method],ops},
		(* Extract matching ops from input string *)
		ops=StringTake[subStr,#]&/@Sort[StringPosition[subStr,Keys[rules]],#1[[1]]<#2[[1]]&]/.rules;
		Which[
			Length[ops]==0,1,
			Length[ops]==1,First[ops],
			True,method@@ops
		]]


(* ::Text:: *)
(*The following function parses the special case of complex and/or negative coefficients from a substring*)


TPCoeffs[subStr_,opts:OptionsPattern[TP]]:=
	With[{rules=OptionValue[Replace]}, 
	Times@@ToExpression[
		(* Replace i with I, and -, with -1 in extracted list *)
		StringReplace[
			(* Extract list of coefficients and addition/subtraction operators *)
			Characters[
				Fold[StringDrop[#1,#2]&,
					subStr,
					(* Extract matching ops in reverse order for Fold *)
					Sort[StringPosition[subStr,Keys[rules]],#1[[1]]>#2[[1]]&]
				]
			],
		{"-"->"-1","i"->"I"}]]]


SetAttributes[TPStringParser,HoldFirst]


(* ::Text:: *)
(*The follwoing function parses an input string and splits an input string into substrings for further parsing*)


TPStringParser[str_]:=
	DeleteCases[
		StringSplit[
			(* Trim whitespace and replace - with +- for string splitting *)
			StringReplace[
				(* Convert input to string if it isn't already *)
				If[StringQ[str],str,ToString@Unevaluated[str]],
			{Whitespace->"","-"->"+-"}],
		{"+"}](* Split into substrings at + locations *)
	,""](* Remove trivial substrings *)


SetAttributes[TP,HoldFirst]


TP[str_,opts:OptionsPattern[TP]]:=Total[
	Map[TPCoeffs[#,opts]*TPOps[#,opts]&,TPStringParser[Unevaluated[str]]]]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


Protect[CircleTimes,BlockMatrix,UnitArray,TensorFactorPermutations,OuterProduct,Projector,Com,ACom,SwapMatrix];
Protect[Basis,BasisLabels,ExpressInBasis];
Protect[Vec,Devec,ProductIdentity,BasisMatrix,BasisTransformation,Rules];
Protect[MatrixToTensor,MatrixTranspose,Swap,Reshuffle,Unravel,Reravel];
Protect[PartialTr,TensorPairContract,MatrixContract,MatrixPairContract];
Protect[TP];


EndPackage[];
