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


(* ::Subsection:: *)
(*Preamble*)


BeginPackage["Tensor`",{"Predicates`"}];


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection::Closed:: *)
(*Matrices and Operations*)


Unprotect[CircleTimes,BlockMatrix,UnitArray,ArrayPermutations,OuterProduct,Projector,Com,ACom,SwapMatrix];


CircleTimes::usage = "CircleTimes[vec1,...,vecn] returns the vector formed by taking the tensor product of all vectors.
CircleTimes[op1,..,opn] returns the Array from taking the tensor product of arrays op1 through opn.
CircleTimes[op, n] returns CircleTimes[op1,...,opn].";


BlockMatrix::usage = "Returns the diagonal block matrix of the input matrices.";


Com::usage = "Shorthand for the commutator of two matrices.";
ACom::usage= "Shorthand for the anti-commutator of two matrices.";


OuterProduct::usage="OuterProduct[vec1,vec2] returns the matrix vec1\[CircleTimes]vec2\[HermitianConjugate] by taking the outer product of two vectors." ;
Projector::usage="Projector[v] returns the rank-1 projection operator vec1\[CircleTimes]vec1\[HermitianConjugate].";


UnitArray::usage = "
UnitArray[{d1,...,dn},{i1,...,in}] returns the unit rank-n array with dimensions {d1,...,dn} with a 1 at index {i1,...,in} and zeros elsewhere.
UnitArray[{d1,...,dn},{i1,...,in}, SparseArray] returns the unit array as a SparseArray. ";


ArrayPermutations::usage = "
ArrayPermutations[{A1,n1},{A2,n2},...,{Ak,nk}] returns a sum of all permutations of CircleTimes of n1 copies of A1, n2  copies of A2 etc. ";


SwapMatrix::usage= 
"SwapMatrix[{d1,...,dn},{i1,...,in}] returns the permutation matrix for mapping a subsystem k with dimension dk, to subsystem ik.
SwapMatrix[d,{i1,...,in}] returns the permutation matrix for mapping a subsystem k to subsystem ik where all subsystems have dimension d.
SwapMatrix[ds,{i1,...,in},SparseArray] returns the permutation matrix as a SparseArray.";


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Manipulations*)


Unprotect[MatrixToTensor,MatrixTranspose,Swap,BipartiteTranspose,Reshuffle,Unravel,Reravel];


MatrixToTensor::usage = 
"MatrixToTensor[mat,{{dL1,..dLn},{dR1,...,dRk}}] transforms the matrix mat into a a rank-(n+k) tensor if Dimesensions[mat] = {dL1*...*dLn, dR1*...*dRk}.
MatrixToTensor[mat,{d1,...,dn}] transforms the matrix mat into a a rank-n tensor if Times@@Dimesensions[mat] = d1*...*dn.
MatrixToTensor[mat,{d1,..dn}] transforms the matrix mat into a a rank-2n tensor if Dimesensions[mat] = {d1*...*dn, d1*...*dn}.";


MatrixTranspose::usage = 
"MatrixTranspose[mat,dims,perm] yields the matrix resulting from TensorTranspose[MatrixToTensor[mat,dims],perm].";


Swap::usage = 
"Swap[op,{d1,...,dn},perm] yields the matrix or vector resulting from permuting subystems of dimensions dj as listed by perm.
Swap[op,d,perm] yields the matrix or vector resulting from permuting subystems of dimensions d as listed by perm.
If d isn't specified the default is d=2.";


Reshuffle::usage = "
Reshuffle[mat,{dl1,dl2,dr1,dr2d}] yields the column reshuffled matrix of the bipartite matrix 'mat' with dimensions {dl1*dl2, dr1*dr2}.
Reshuffle[mat] yields the column reshuffled matrix of the bipartite matrix 'mat' with dimensions {dl*dl, dr*dr}.
Reshuffle[mat,{dl1,dl2,dr1,dr2d}, Basis->''Row''] yields the column reshuffled matrix of the bipartite matrix 'mat' with dimensions {dl1*dl2, dr1*dr2}.
Reshuffle[mat, Basis->''Row''] yields the column reshuffled matrix of the bipartite matrix 'mat' with dimensions {dl*dl, dr*dr}.";


Unravel::usage = 
"Unravel[op,{d1,..,dn}] yields the unraveled form of 'op' where op has n subsystems each of dimension {dj^2,dj^2}.
Unravel[op,d] yields the unravelled form of op where op has subsystems of dimension {d^2,d^2}.
If d is not specified then it defaults to d=2.";


Reravel::usage = 
"Reravel[op,{d1,..,dn}] yields the reraveled form of 'op' where op has n subsystems each of dimension {dj^2,dj^2}.
Reravel[op,d] yields the reravelled form of op where op has subsystems of dimension {d^2,d^2}.
If d is not specified then it defaults to d=2.";


(* ::Subsection::Closed:: *)
(*Matrix-Tensor Contractions*)


Unprotect[PartialTr,TensorPairContract,MatrixContract,MatrixPairContract];


PartialTr::usage =
"PartialTrace[mat, {d1,d2,...,dn},{i1,..,ik}] yields the matrix resulting from the partial trace over subsystems i1,..,ik of the mat, where subsystem j has dimensions dj.";


TensorPairContract::usage = 
"TensorPairContract[tenor1_,tenor2_,{{s11,s12},{s21,s22},...}] yields the tensor resulting from contracting index si1 of tensor1 with index si2 of tensor 2.
If the contract list is the empty list {} it yields TensorProduct[tensor1,tensor2]";


MatrixContract::usage = 
"MatrixContract[mat,dims,{{s11,s12},{s21,s22},...}] yields the contraction of mat in the pairs {si1, si2} of slots by reshaping mat to a tensor MatrixToTensor[mat,dims].
It functions as TensorContract[MatrixToTensor[mat,dims],{{s11,s12},{s21,s22},...}] and reshapes the ouput tensor to the appropriate dimensional matrix.";


MatrixPairContract::usage = 
"MatrixPairContract[{mat1_,dims1},{mat2,dims2},{{s11,s12},{s21,s22},...}] yields the matrix resulting from contracting index si1 of tensor1 with index si2 of tensor2, wheretensorj = MatrixToTensor[matj,dimsj].
If the contract list is the empty list {} it yields KroneckerProduct[mat1,mat2]";




(* ::Subsection::Closed:: *)
(*Matrix Bases*)


Unprotect[Basis,BasisLabels,ExpressInBasis];


Basis::usage = 
"Basis[basis,n] returns the matrix basis for n subsystems with subsytem basis 'basis'.
'basis' may either be a list of matrices {b1,...,bn} or a string for named bases.
Supported named bases are:
  ''Pauli'' for the normalized Pauli basis,
  ''PO'' for the product operator basis (unnormalized Pauli basis)";


BasisLabels::usage = 
"BasisLabels[basis,n] returns the basis labels for n subsystems with subsytem basis 'basis'.
'basis' may either be a list of string labels {s1,...,sn} or a string for named bases.
Supported named bases are:
  ''Pauli'' for the normalized Pauli basis,
  ''PO'' for the product operator basis (unnormalized Pauli basis)";


ExpressInBasis::usage = 
"ExpressInBasis[op,opts] expresses operator op in terms of basis elements for a basis.
Options are:
  Basis->basis, with default basis value ''PO'' (See Basis for more details)
  BasisLabels->labels, with default value False. If 'labels' is True it will use the labels for a named basis, 'labels' may also be a list of string labels (See BasisLabels for more details).";


(* ::Subsection::Closed:: *)
(*Vectorization*)


Unprotect[Vec,Devec,ProductIdentity,BasisMatrix,BasisTransformation];


Vec::usage = "Vec[A] vectorizes a matrix A using the currently selected vectorization convention.";
Devec::usage = "Devec[v] de-vectorizes a vector v using the currently selected vectorization convention";
ProductIdentity::usage = "ProductIdentity[A,C] returns a matrix M such that M.Vec[B]=Vec[A.B.C] for matrices A,B and C.";


BasisMatrix::usage = 
"BasisMatrix[basis1->basis2, arg] returns the change of basis unitary matrix for transforming a vectorized matrix in basis 1 to basis 2.
'basis1', 'basis2' may be either ''Col'', ''Row'', ''Pauli'' or a list of matrices.
If basis1 or basis2 are ''Pauli'' or lists of matrices 'arg' is optional. If arg is an integer it specifies the number of subsystems with the given basis.
If both 'basis1' and 'basis2' are ''Col'' or ''Row'' 'arg' is manditory and specifies the dimension of the vectorized matrix (either {dL,dR} or d for square matrices).";

BasisTransformation::usage =
"BasisTransformation[op, basis1->basis2] transforms an operator in vectorization basis1 to an operator in vectorization basis2.
op may be a matrix (which acts on vectorized matrices), or a vector (vectorized matrix).
basis1 and basis2 may be either ''Col'', ''Row'', ''Pauli'', or a list of matrices.";


(* ::Subsection::Closed:: *)
(*Tensor Parser*)


Unprotect[TP];


TP::usage="TP[str,opts] converts an input string into the tensor product of matrices. 
TP has options Replace and Method. Allowed operators are + -, i is used for the imaginary number, and any integers or symbols not specified in the Replace option are treated as scalars.

Method is the method used to combine operators, the default method is CircleTimes.
Replace is a list of operator replacement rules for the input string, the default Replace is 
{''I''->PauliMatrix[0], ''X''->PauliMatrix[1],''Y''->PauliMatrix[2],''Z''->PauliMatrix[3},
''P''->{{0,1},{0,0}}, ''M''->{{0,0},{1,0}}, ''U''->{{1,0},{0,0}}, ''D''->{{0,0},{0,1}},
''H''->{{1,1},{1,-1}}/Sqrt[2], ''S''->{{1,0},{0,I}}, ''T''->{{1,0},{0,Exp[I*Pi/4]}}}.

For example XX+iYY -> CircleTimes[X,X]+I*CircleTimes[Y,Y].";


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


Clear[Com]
Com[A_,B_]:=A.B-B.A
Com[A_,B_,0]:=B
Com[A_,B_,1]:=Com[A,B]
Com[A_,B_,n_Integer]:=Com[A,B,n]=Com[A,Com[A,B,n-1]]


Clear[ACom]
ACom[A_,B_]:=A.B+B.A
ACom[A_,B_,0]:=B
ACom[A_,B_,1]:=ACom[A,B]
ACom[A_,B_,n_Integer]:=ACom[A,B,n]=ACom[A,ACom[A,B,n-1]]


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


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*Tensor Parser*)


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


(* ::Section:: *)
(*End Package*)


Protect[CircleTimes,BlockMatrix,UnitArray,ArrayPermutations,OuterProduct,Projector,Com,ACom,SwapMatrix];
Protect[Basis,BasisLabels,ExpressInBasis];
Protect[Vec,Devec,ProductIdentity,BasisMatrix,BasisTransformation,Rules];
Protect[MatrixToTensor,MatrixTranspose,Swap,BipartiteTranspose,Reshuffle,Unravel,Reravel];
Protect[PartialTr,TensorPairContract,MatrixContract,MatrixPairContract];
Protect[TP];


EndPackage[];
