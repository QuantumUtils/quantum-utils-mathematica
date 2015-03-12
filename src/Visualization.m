(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*Visualization Package*)


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
(*Preample*)


BeginPackage["Visualization`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["UnitTesting`"];
Needs["QUDevTools`"];
Needs["Predicates`"];
Needs["Tensor`"]


$Usages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "Visualization.nb"}]];


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection:: *)
(*Matrices*)


Unprotect[ComplexMatrixPlot,BlockForm,MatrixListForm];


AssignUsage[ComplexMatrixPlot,$Usages];
AssignUsage[BlockForm,$Usages];
AssignUsage[MatrixListForm,$Usages];


(* ::Subsection:: *)
(*Bloch Plots*)


Unprotect[BlochPlot,BlochPlot2D,ListBlochPlot,ListBlochPlot2D];


AssignUsage[BlochPlot,$Usages];
AssignUsage[BlochPlot2D,$Usages];
AssignUsage[ListBlochPlot,$Usages];
AssignUsage[ListBlochPlot2D,$Usages];


(* ::Subsection:: *)
(*Eigensystems*)


Unprotect[EigensystemForm];


AssignUsage[EigensystemForm,$Usages];


(* ::Subsection:: *)
(*Special Plotting Functions*)


Unprotect[FourierListPlot];


AssignUsage[FourierListPlot,$Usages];


(* ::Subsection:: *)
(*Error Messages*)


BlochPlot::input = "Input must be a state vector or density matrix.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Matrices*)


ComplexMatrixPlot[A_,opts:OptionsPattern[DiscretePlot3D]]:=Module[{fun,color},
	fun[x_,y_]:=(2UnitStep[Im[A[[x,y]]]]-1)Abs[A[[x,y]]];
	(* We only use half of the Hue range otherwise 0 and \[Pi] would both be red *)
	color[x_,y_,z_]:=Hue[Abs[Arg[A[[x,y]]]]/\[Pi]/2];
	DiscretePlot3D[
		fun[x,y],
		{x,First@Dimensions@A},
		{y,Last@Dimensions@A},
		(* Allow user options to override anything *)
		opts,
		PlotTheme->"Detailed",
		PlotStyle->Opacity[0.7],
		(* ExtentSize gives it the bar graph look *)
		ExtentSize->Full,
		ColorFunctionScaling->False,
		ColorFunction->color,
		(* Note that the use of Ticks in BarLegend seems to work, although isn't documented*)
		PlotLegends->BarLegend[{Hue[# /\[Pi]/2]&,{0,\[Pi]}},Ticks->Range[0,\[Pi],\[Pi]/4]],
		AxesLabel->{"Row","Column"},
		Ticks->{Range[First@Dimensions@A],Range[Last@Dimensions@A],Automatic},
		Boxed->False
	]
]


BlockForm[mat_,n_]:=
	With[{div={False, Append[Table[False,{n - 1}], Dashed], False}},
		{{Grid[mat, Dividers -> {div, div}, ItemSize -> Full]}} // MatrixForm
	]
BlockForm[mat_]:=BlockForm[mat,First@Last@FactorInteger[Length[mat],2]]


MatrixListForm[mats_]:=Row[Riffle[MatrixForm/@mats,","]]


(* ::Subsection::Closed:: *)
(*Bloch Plots*)


(* ::Subsubsection::Closed:: *)
(*Bloch Sphere Display Components*)


With[{r=1},
$BlochSphereXY=ParametricPlot3D[{r Cos[\[Theta]],r Sin[\[Theta]],0},{\[Theta],0,2 \[Pi]},PlotStyle->Dashed,Boxed->False,Axes->False];
$BlochSphereYZ=ParametricPlot3D[{0,r Cos[\[Theta]],r Sin[\[Theta]]},{\[Theta],0,2 \[Pi]},PlotStyle->Dashed,Boxed->False,Axes->False];
$BlochSphereXZ=ParametricPlot3D[{r Cos[\[Theta]],0,r Sin[\[Theta]]},{\[Theta],0,2 \[Pi]},PlotStyle->Dashed,Boxed->False,Axes->False];

$BlochSphere=Show[
Graphics3D[{
Opacity[.3],
	Sphere[{0,0,0},r],
Opacity[.5],Black,Thick,
	Line[{{0,r,0},{0,-r,0}}],Line[{{0,0,r},{0,0,-r}}],Line[{{r,0,0},{-r,0,0}}],
Opacity[1],Black,Thick,
Text[Style["|+Z\[RightAngleBracket]",FontSize->14,FontWeight->Bold,FontColor->GrayLevel[.3]],{0,0,1.15 r}],
Text[Style["|-Z\[RightAngleBracket]",FontSize->14,FontWeight->Bold,FontColor->GrayLevel[.3]],{0,0,-1.15 r}],
Text[Style["|+X\[RightAngleBracket]",FontSize->14,FontWeight->Bold,FontColor->GrayLevel[.3]],{1.15 r,0,0}],
Text[Style["|-X\[RightAngleBracket]",FontSize->14,FontWeight->Bold,FontColor->GrayLevel[.3]],{-1.15 r,0,0}],
Text[Style["|+Y\[RightAngleBracket]",FontSize->14,FontWeight->Bold,FontColor->GrayLevel[.3]],{0,1.15 r,0}],
Text[Style["|-Y\[RightAngleBracket]",FontSize->14,FontWeight->Bold,FontColor->GrayLevel[.3]],{0,-1.15 r,0}]}],
$BlochSphereXY,$BlochSphereXZ,$BlochSphereYZ,Boxed->False];

$BlochCircleXZ=Graphics[{
Circle[{0,0},1],
Line[{{1,0},{-1,0}}],
Line[{{0,1},{0,-1}}],
Text[Style["|+X\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{1.15,0}],
	Text[Style["|-X\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{-1.15,0}],Text[Style["|+Z\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{0,1.15}],
	Text[Style["|-Z\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{0,-1.15 }]}];

$BlochCircleXY=Graphics[{
Circle[{0,0},1],
Line[{{1,0},{-1,0}}],
Line[{{0,1},{0,-1}}],
Text[Style["|+X\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{1.15,0}],
	Text[Style["|-X\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{-1.15,0}],Text[Style["|+Y\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{0,1.15}],
	Text[Style["|-Y\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{0,-1.15 }]}];

$BlochCircleYZ=Graphics[{
Circle[{0,0},1],
Line[{{1,0},{-1,0}}],
Line[{{0,1},{0,-1}}],
Text[Style["|+Y\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{1.15,0}],
	Text[Style["|-Y\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{-1.15,0}],
Text[Style["|+Z\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{0,1.15}],
	Text[Style["|-Z\[RightAngleBracket]",FontSize->12,FontWeight->Bold],{0,-1.15 }]}];

];


(* ::Subsubsection::Closed:: *)
(*Bloch Sphere Plotting*)


BlochCoordinates[state_]:=
	With[{
		mat=Which[
			GeneralVectorQ[state],Projector[state],
			SquareMatrixQ[state], state,
			True, Message[BlochPlot::input]]},
	Map[Tr[ConjugateTranspose[PauliMatrix[#]].mat]&,Range[3]]
	]


BlochPlot[state_,opts:OptionsPattern[Graphics3D]]:= 
	With[{p=BlochCoordinates[state]},
	Show[$BlochSphere,
	Graphics3D[{
		{Green,Thickness[0.01],Line[{{0,0,0},p}]},
		{Green,PointSize[0.02], Point[p]}
	},opts]
]];


ListBlochPlot[\[Rho]list_,opt:OptionsPattern[Graphics3D]]:=
	With[{plotdat=Map[BlochCoordinates,\[Rho]list],d=Length[\[Rho]list]},
		Show[
			$BlochSphere,
			Graphics3D[{
				Table[
					{Blend[{Yellow,Red},1-j/d],PointSize@If[1<j<d,0.01,0.03], 
					Point[plotdat[[j]]]}
					,{j,d}
				]
			}],opt
		]
	]


(* ::Subsubsection::Closed:: *)
(*Bloch Sphere 2D Projection*)


BlochPlot2D[state_]:=
	With[{p=BlochCoordinates[state]},
	GraphicsRow[{
		Show[$BlochCircleXZ,Graphics[
			{Red,Thickness[0.01],Line[{{0,0},Part[p,{1,3}]}]},
			{Red,PointSize[0.03],Point[Part[p,{1,3}]]}]],
		Show[$BlochCircleYZ,Graphics[
			{Red,Thickness[0.01],Line[{{0,0},Part[p,{2,3}]}]},
			{Red,PointSize[0.03],Point[Part[p,{2,3}]]}]],
		Show[$BlochCircleXY,Graphics[
			{Red,Thickness[0.01],Line[{{0,0},Part[p,{1,2}]}]},
			{Red,PointSize[0.03],Point[Part[p,{1,2}]]}]]
			},ImageSize->Large]]


ListBlochPlot2D[\[Rho]list_]:=
With[{
	ps=Map[BlochCoordinates,\[Rho]list],
	d=Length[\[Rho]list]},
GraphicsRow[{
	Show[$BlochCircleXZ,
		Graphics[{
			Map[{Blend[{Red,Blue},1-#/d],PointSize[0.02],Point[Part[ps,#,{1,3}]]}&,
			Range[d]]
		}]],
	Show[$BlochCircleYZ,
		Graphics[{
			Map[{Blend[{Red,Blue},1-#/d],PointSize[0.02],Point[Part[ps,#,{2,3}]]}&,
			Range[d]]
		}]],
	Show[$BlochCircleXY,
		Graphics[{
			Map[{Blend[{Red,Blue},1-#/d],PointSize[0.02],Point[Part[ps,#,{1,2}]]}&,
			Range[d]]
		}]]
},ImageSize->Large]
];


(* ::Subsection::Closed:: *)
(*Eigensystems*)


EigensystemForm[sys_]:=Grid[
	({#[[1]],"\[DownArrow]",MatrixForm[#[[2]]]}&/@(sys\[Transpose]))\[Transpose],
	ItemSize->Full
];


(* ::Subsection::Closed:: *)
(*Special Plotting Functions*)


FourierListPlot[data_,{mint_,maxt_},function_,opt:OptionsPattern[ListPlot]]:=
	Module[{n,n2,dt,fdata,minf,maxf},
		(*Take the discrete FT of each input list*)
		fdata=(Fourier/@If[VectorQ[data],{data},data]);
		(* Now that we've sorted out whether data is a list or list of lists... *)
		n=Length[fdata[[1]]];
		n2=Floor[n/2];
		dt=(maxt-mint)/(n-1);
		(* Apply each input function to each fourier domain list *)
		fdata=Flatten[Outer[#2[#1]&,fdata,If[ListQ[function],function,{function}],1],1];
		(* Add x cooordinates to the lists *)
		fdata=Map[({Range[0,1/dt,1/((n-1)dt)],#}\[Transpose])&,fdata,1];
		(* determine the plot bounds *)
		If[OptionValue[DataRange]===Automatic,
			minf=0;maxf=1/(2dt);,
			minf=OptionValue[DataRange][[1]];maxf=OptionValue[DataRange][[2]];
		];
		(* select only those data lying in the frequency bounds*)
		fdata=Function[{list},Select[list,minf<=#[[1]]<=maxf&]]/@fdata;
		(* usual list plot *)
		ListPlot[fdata,DataRange->Automatic,opt]
]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section::Closed:: *)
(*End Package*)


Protect[ComplexMatrixPlot,BlockForm,MatrixListForm];
Protect[BlochPlot,BlochPlot2D,ListBlochPlot,ListBlochPlot2D];
Protect[EigensystemForm];
Protect[FourierListPlot];


EndPackage[];
