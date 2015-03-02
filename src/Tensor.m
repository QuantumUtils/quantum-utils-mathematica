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


BeginPackage["Tensor`",{"Predicates`"}];


Needs["DocTools`"];
Needs["QUOptions`"];
Needs["UnitTesting`"];


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "Tensor.nb"}]];


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection::Closed:: *)
(*Matrices and Operations*)


Unprotect[CircleTimes,BlockMatrix,UnitArray,ArrayPermutations,OuterProduct,Projector,Com,ACom,SwapMatrix];


AssignUsage[CircleTimes,$Usages];
AssignUsage[BlockMatrix,$Usages];
AssignUsage[UnitArray,$Usages];
AssignUsage[ArrayPermutations,$Usages];
AssignUsage[SwapMatrix,$Usages];
AssignUsage[Com,$Usages];
AssignUsage[ACom,$Usages];
AssignUsage[OuterProduct,$Usages];
AssignUsage[Projector,$Usages];


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Manipulations*)


Unprotect[MatrixToTensor,MatrixTranspose,Swap,Reshuffle,Unravel,Reravel];


AssignUsage[MatrixToTensor,$Usages];
AssignUsage[MatrixTranspose,$Usages];
AssignUsage[Swap,$Usages];
AssignUsage[Reshuffle,$Usages];
AssignUsage[Unravel,$Usages];
AssignUsage[Reravel,$Usages];


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Contractions*)


Unprotect[PartialTr,TensorPairContract,MatrixContract,MatrixPairContract];


AssignUsage[PartialTr,$Usages];
AssignUsage[TensorPairContract,$Usages];
AssignUsage[MatrixContract,$Usages];
AssignUsage[MatrixPairContract,$Usages];


(* ::Subsection::Closed:: *)
(*Matrix Bases*)


Unprotect[Basis,BasisLabels,ExpressInBasis];


AssignUsage[Basis,$Usages];
AssignUsage[BasisLabels,$Usages];
AssignUsage[ExpressInBasis,$Usages];


(* ::Subsection::Closed:: *)
(*Vectorization*)


Unprotect[Vec,Devec,ProductIdentity,BasisMatrix,BasisTransformation];


AssignUsage[Vec,$Usages];
AssignUsage[Devec,$Usages];
AssignUsage[ProductIdentity,$Usages];
AssignUsage[BasisMatrix,$Usages];
AssignUsage[BasisTransformation,$Usages];


(* ::Subsection::Closed:: *)
(*Tensor Product Parser*)


Unprotect[TP];


AssignUsage[TP,$Usages];


(* ::Subsection::Closed:: *)
(*Error Messages*)


(* ::Subsubsection:: *)
(*Matrices and Operations*)


ArrayPermutations::input = "Input must be a sequence of elements {op,int}.";


(* ::Subsubsection:: *)
(*Tensor Manipulations*)


MatrixToTensor::dims = "Dimensions of tensor must be a list of integers, or a list of two lists of integers.";


Swap::input = "Input operator must be a square matrix, vector, row vector or column vector.";


Unravel::bipartite="System must be composed of bipartite tensors to unravel.";
Unravel::input="Input operator must be a vector or matrix.";
Unravel::dims="Invalid specification of subsystem dimension.";


(* ::Subsubsection:: *)
(*Vectorization*)


BasisMatrix::dims = "Dimensions of input system must be specified.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Matrices and Operations*)


Subscript[\[DoubleStruckOne], 0]:={{}}
Subscript[\[DoubleStruckOne], n_]:=IdentityMatrix[n]


UnitArray[dims_List,ind_List,SparseArray]:=
	If[Length[dims]===Length[ind],
		SparseArray[ind->1,dims]
		,Message[UnitArray::errind]
	]
UnitArray[dims_List,ind_List]:=Normal@UnitArray[dims,ind,SparseArray]


CircleTimes[first_?ArrayQ]:=first
CircleTimes[first_?VectorQ,last_?VectorQ]:=Flatten[KroneckerProduct[first,last]]
CircleTimes[first_?VectorQ,last_?ArrayQ]:=KroneckerProduct[Partition[first,1],last]
CircleTimes[first_?ArrayQ,last_?VectorQ]:=KroneckerProduct[first,Partition[last,1]]
CircleTimes[first_?ArrayQ,last_?ArrayQ]:=KroneckerProduct[first,last]
CircleTimes[first_?ArrayQ,rest__?ArrayQ]:=CircleTimes[CircleTimes[first,First[{rest}]],Sequence@@Rest[{rest}]]


CircleTimes[A_?ArrayQ,n_Integer]:=CircleTimes@@ConstantArray[A,n];


ArrayPermutations[arrayNums__]:=
	If[AllMatchQ[{_,_Integer},{arrayNums}],
	Plus@@CircleTimes@@@
			Permutations[Join@@ConstantArray@@@{arrayNums}],
	Message[ArrayPermutations::input]]


Com[A_,B_,n_]:=
	Block[{Com},
		Com[A,B,0]:=B;
		Com[A,B,1]:=A.B-B.A;
		Com[A,B,m_?Positive]:=Com[A,B,m]=Com[A,Com[A,B,m-1]];
		Com[A,B,n]
	];
Com[A_,B_]:=Com[A,B,1]


ACom[A_,B_,n_]:=
	Block[{ACom},
		ACom[A,B,0]:=B;
		ACom[A,B,1]:=A.B+B.A;
		ACom[A,B,m_?Positive]:=ACom[A,B,m]=ACom[A,ACom[A,B,m-1]];
		ACom[A,B,n]
	];
ACom[A_,B_]:=ACom[A,B,1]


OuterProduct[u_,v_]:=KroneckerProduct[Flatten[u],Conjugate[Flatten[v]]];


Projector[v_]:=OuterProduct[v,v];


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
(*Note that this function reshapes so that the subsystem order corresponds to the list of left indicies followed by the list of right indicies.*)


MatrixToTensorDims[mat_,dims_]:=
	With[
		{dmat=Times@@Dimensions[mat],
		dtens=Times@@Flatten[dims]},
	Which[
		And[AllQ[ListQ,dims],dmat===dtens],
			Flatten[dims],
		And[AllQ[IntegerQ,dims],dmat===dtens],
			dims,
		And[AllQ[IntegerQ,dims],dmat===dtens^2],
			Join[dims,dims],
		True,Message[MatrixToTensor::dims]
	]]


MatrixToTensor[mat_,dims_]:=
	ArrayReshape[mat,MatrixToTensorDims[mat,dims]]


(* ::Text:: *)
(*Utility function to determine where the a list of tensors dimensions should be partitioned for flattening to a matrix.*)


MatrixSplitPosition[mat_,matToTensDims_]:=
		First@Flatten@Position[Rest@FoldList[Times,1,matToTensDims],First[Dimensions[mat]]]


TransposedMatrixDims[mat_,matToTensDims_,translist_]:=
	With[{
		pos=MatrixSplitPosition[mat,matToTensDims],
		dims1=Permute[matToTensDims,translist]},
		{Times@@Part[dims1,1;;pos],Times@@Part[dims1,pos+1;;All]}
	]


(* ::Text:: *)
(*Function for implementing TensorTranspose on matrices by first reshaping the matrix to a tensor, then reshaping back to a matrix after the transposition.*)
(*Index ordering goes left then right so L1->1,...,Ln->n, R1->n+1,... Rn->2n*)


MatrixTranspose[mat_,dims_,translist_]:=
	With[{tdims=MatrixToTensorDims[mat,dims]},
	ArrayReshape[
		TensorTranspose[
			MatrixToTensor[mat,dims],
			translist],
		TransposedMatrixDims[mat,tdims,translist]
	]]


Options[Reshuffle]={Basis->"Col"};


Reshuffle[m_,dims_,OptionsPattern[Reshuffle]]:=
	Which[
		OptionValue[Basis]==="Col",
			ColReshuffle[m,dims],
		OptionValue[Basis]==="Row",
			RowReshuffle[m,dims],
		True,
			Message[Reshuffle::conv]
		]


Reshuffle[m_,opts:OptionsPattern[Reshuffle]]:=
	With[{d=Sqrt[Dimensions[m]]},
		If[MatrixQ[m]&&AllQ[IntegerQ,d],
			Reshuffle[m,Riffle[d,d],opts],
			Message[Reshuffle::dims]
		]]


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


PartialTr[mat_,dimList_,trList_]:=
	With[{tensorDims=Join[dimList,dimList],
		contractIndex={#,#+Length[dimList]}&/@trList,
		nSys=Length[dimList]-Length[trList]},
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
	With[{pos=MatrixSplitPosition[mat,matTensDims],
		finalDims=Delete[matTensDims,Partition[contr,1]]},
		{Times@@Part[finalDims,1;;#],
		Times@@Part[finalDims,#+1;;All]}&@(pos-Length[Select[contr,#<= pos&]])
	]



MatrixContract[mat_,dims_,{pairs___List}]:=
	With[{tdims=MatrixToTensorDims[mat,dims]},
		ArrayReshape[
			TensorContract[MatrixToTensor[mat,dims],{pairs}],
		MatrixContractDims[mat,tdims,Join[pairs]]]
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
		MatrixContractDims[mat1,tdims1,Last/@{pairs}]]]
	]

MatrixPairContract[{mat1_,dims1_},{mat2_,dims2_},{}]:=CircleTimes[mat1,mat2]


(* ::Subsection::Closed:: *)
(*Matrix Bases*)


(* ::Subsubsection:: *)
(*Named Bases*)


$POBasis=(PauliMatrix/@Range[0,3]);
$PauliBasis=(PauliMatrix/@Range[0,3])/Sqrt[2];


$NamedBasis={"Pauli"->$PauliBasis,"PO"->$POBasis};


CheckNamedBasis[basis_]:=
	If[MemberQ[Keys[$NamedBasis],basis],
		basis/.$NamedBasis,
		basis]


$POBasisLabels={"I","X","Y","Z"};
$PauliBasisLabels={"I","X","Y","Z"}/Sqrt[2];


$NamedBasisLabels={"Pauli"->$PauliBasisLabels,"PO"->$POBasisLabels};


CheckNamedBasisLabels[basis_]:=
	If[MemberQ[Keys[$NamedBasisLabels],basis],
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


BasisLabels[labels_]:=BasisLabels[labels,1];
BasisLabels[labels_,n_Integer]:=
	With[{labels1=CheckNamedBasisLabels[labels]},
		If[n===1,
			labels1,
	CircleTimes@@@Tuples[labels1,n]]]


(* ::Subsubsection:: *)
(*Express In Basis*)


Options[ExpressInBasis]:={Basis->"PO",BasisLabels->False};


ExpressInBasis[op_,opts:OptionsPattern[]]:=
	With[{basis=OptionValue[Basis],
		labels=OptionValue[BasisLabels]},
	With[{coeffs=ExpressInBasisCoeffs[op,basis]},
		Which[
		TrueQ[Not@labels],
			coeffs,
		TrueQ[labels],
			Transpose[{ExpressInBasisLabels[op,basis],coeffs}],
		True,
			Transpose[{ExpressInBasisLabels[op,labels],coeffs}]
		]
	]]


ExpressInBasisCoeffs[op_,basis_]:=
	With[{opBasis=CheckNamedBasis[basis]},
	With[{
	n=Log[Length[opBasis],Length[Flatten[op]]]},
		Map[Tr[ConjugateTranspose[#].op]&,Basis[opBasis,n]]
	]]


ExpressInBasisLabels[op_,basis_]:=
	With[{labels1=CheckNamedBasisLabels[basis]},
	With[{
	n=Log[Length[labels1],Length[Flatten[op]]]},
		BasisLabels[basis,n]
	]]


(* ::Subsection::Closed:: *)
(*Vectorization*)


(* ::Subsubsection:: *)
(*Options and bases*)


Options[Vec]={Basis->"Col"};


(* ::Subsubsection:: *)
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
			MemberQ[Keys[$NamedBasis],basis],funcs[[3]][(basis/.$NamedBasis),args],
			True,funcs[[3]][basis,args]]]


(* ::Subsubsection:: *)
(*Convention Implementations*)


VecRow[m_]:= Flatten[{m},{{2,3},{1}}]
DevecRow[v_,{dL_,dR_}]:= ArrayReshape[v,{dL,dR}]
DevecRow[v_]:= With[{d=Sqrt[Length[v]]},DevecRow[v,{d,d}]]
ProductIdentityRow[A_,C_]:=KroneckerProduct[A,Transpose[C]]


VecCol[m_]:= Flatten[{m},{{3,2},{1}}]
DevecCol[v_,{dL_,dR_}]:= Transpose[DevecRow[v,{dL,dR}]]
DevecCol[v_]:= Transpose[DevecRow[v]]
ProductIdentityCol[A_,C_]:=KroneckerProduct[Transpose[C],A]


VecBasis[basis_,m_]:=BasisTransformation[VecCol[m],Rule["Col",basis]]
DevecBasis[basis_,v_]:=
	DevecCol[
		BasisTransformation[v,Rule[basis,"Col"]]]

DevecBasis[basis_,v_,{dL_,dR_}]:=
	DevecCol[
		BasisTransformation[v,Rule[basis,"Col"]],{dL,dR}]

ProductIdentityBasis[basis_,A_,C_]:=
	BasisTransformation[ProductIdentityCol[A,C],Rule["Col",basis]]


(* ::Subsubsection:: *)
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
			CircleTimes[SparseArray@BasisMatrixCol[basis,1],n]
			,Length[First[basis]]]


BasisMatrix[Rule["Col",basis_],arg___]:=
	Which[
		basis==="Row",BasisMatrixCol["Row",arg],
		ListQ[basis],BasisMatrixCol[basis,arg],
		MemberQ[Keys[$NamedBasis],basis],BasisMatrixCol[basis/.$NamedBasis,arg],
		True,Message[BasisMatrix::input]
	]
BasisMatrix[Rule[basis_,"Col"],arg___]:=ConjugateTranspose@BasisMatrix[Rule["Col",basis],arg]


BasisMatrix[Rule[basis1_List,basis2_List],arg___]:=
	BasisMatrix[Rule["Col",basis2],arg].BasisMatrix[Rule[basis1,"Col"],arg]


(* ::Subsubsection:: *)
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
	If[MemberQ[Keys[$NamedBasis],basis],
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
		BasisTransformation[op,Rule[basis1,"Col"],
		Rule["Col",basis2]]]


(* ::Subsection::Closed:: *)
(*Tensor Product Parser*)


(* ::Subsubsection:: *)
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
(*The following function extracts the operators from a string defined by the Keys of the Association, or list of rules "rules".*)


(* Backwards Compatability*)
If[$VersionNumber<10,
	SetAttributes[Keys,Listable];
	Keys[Rule[expr_,_]]:=expr,
	Null
];


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


Protect[CircleTimes,BlockMatrix,UnitArray,ArrayPermutations,OuterProduct,Projector,Com,ACom,SwapMatrix];
Protect[Basis,BasisLabels,ExpressInBasis];
Protect[Vec,Devec,ProductIdentity,BasisMatrix,BasisTransformation,Rules];
Protect[MatrixToTensor,MatrixTranspose,Swap,Reshuffle,Unravel,Reravel];
Protect[PartialTr,TensorPairContract,MatrixContract,MatrixPairContract];
Protect[TP];


EndPackage[];
