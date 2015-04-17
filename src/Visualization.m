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


(* ::Subsection::Closed:: *)
(*Preamble*)


BeginPackage["Visualization`",{"QUDoc`"}];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];
Needs["Predicates`"];
Needs["Tensor`"];
Needs["QuantumChannel`"];


$VisualizationUsages = LoadUsages[FileNameJoin[{$QUDocumentationPath, "api-doc", "Visualization.nb"}]];


(* ::Section:: *)
(*Usage Declaration*)


(* ::Subsection::Closed:: *)
(*Matrices*)


Unprotect[ComplexMatrixPlot,BlockForm,MatrixListForm, HintonPlot,ChannelHintonPlot];


AssignUsage[ComplexMatrixPlot,$VisualizationUsages];
AssignUsage[BlockForm,$VisualizationUsages];
AssignUsage[MatrixListForm,$VisualizationUsages];
AssignUsage[{Gap, HintonPlot, ChannelHintonPlot}, $VisualizationUsages];


(* ::Subsection::Closed:: *)
(*Bloch Plots*)


Unprotect[BlochPlot,BlochPlot2D,ListBlochPlot2D,BlochPlotColors,BlochPlotEndPoints,BlochPlotJoined,BlochPlotLabels];


AssignUsage[BlochPlot,$VisualizationUsages];
AssignUsage[BlochPlot2D,$VisualizationUsages];
AssignUsage[ListBlochPlot2D,$VisualizationUsages];
AssignUsage[BlochPlotColors,$VisualizationUsages];
AssignUsage[BlochPlotJoined,$VisualizationUsages];
AssignUsage[BlochPlotEndPoints,$VisualizationUsages];
AssignUsage[BlochPlotLabels,$VisualizationUsages];


(* ::Subsection::Closed:: *)
(*Eigensystems*)


Unprotect[EigensystemForm];


AssignUsage[EigensystemForm,$VisualizationUsages];


(* ::Subsection::Closed:: *)
(*Special Plotting Functions*)


Unprotect[FourierListPlot];


AssignUsage[FourierListPlot,$VisualizationUsages];


(* ::Subsection::Closed:: *)
(*Error Messages*)


BlochPlot::input = "Unable to parse input state(s); see Documentation. Constructed density lists have dimensions `1`.";
BlochPlot::color = "BlochPlotColor option value not understood.";


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


Options[HintonPlot] = {
	AxesLabel -> None,
	"Gap" -> 0.05,
	"Colors" -> {Black,Gray,White},
	AxesStyle -> {}
};


HintonPlot[dat_,OptionsPattern[]] := 
	With[{
		data=Reverse[dat\[Transpose],{2}],
		colors=OptionValue["Colors"]},
	With[{
		n=Dimensions[data][[1]],
		m=Dimensions[data][[2]],
		normdata=(1-OptionValue["Gap"])data/Max[Abs[data]],
		graydata=Map[colors[[#]]&,Sign[data]+2,{2}]},
    Module[{plot},
        plot = Graphics[
            {colors[[2]],Rectangle[{1/4,1/4},{n+3/4,m+3/4}]}~Join~
            Table[
                {
                    graydata[[i,j]],
                    Tooltip[Rectangle[
                        {i-normdata[[i,j]]/2,j-normdata[[i,j]]/2},
                        {i+normdata[[i,j]]/2,j+normdata[[i,j]]/2}
                    ], data[[i,j]]]
                },
                {i,n}, {j,m}
            ]
        ];

        If[OptionValue[AxesLabel]=!=None,
            plot[[1]]=Join[
                plot[[1]],
               {Gray,Table[
                    Style[Text[OptionValue[AxesLabel][[1,i]],{i,m+3/4},{0,-2}],OptionValue[AxesStyle]],
                    {i,n}
                ]},
                {Gray,Table[
                    Style[Text[Reverse[OptionValue[AxesLabel][[2]]][[j]],{0,j},{1,0}],OptionValue[AxesStyle]],
                    {j,m}
                ]}
            ];
        ];
        plot
    ]
]];


(* Not exposed by intention. *)
PauliLabels[nq_] := Table[
    StringJoin@@(Reverse[IntegerDigits[i,4,nq]]/.{
        0->"I",1->"X",2->"Y",3->"Z"
    }),
    {i,0,4^nq-1}
];


ChannelHintonPlot[chan_,opts:OptionsPattern[HintonPlot]]:=With[{
		mtx=First@Super[chan,Basis->"Pauli"],
		nqIn=Log2 @ InputDim@ chan,
		nqOut=Log2 @ OutputDim @ chan
	},
	HintonPlot[mtx,
		If[AnyMatchQ[AxesLabel->_,{opts}],
			{opts},
			Append[{opts},AxesLabel->PauliLabels/@{nqOut,nqIn}]]]
]


(* ::Subsection::Closed:: *)
(*Bloch Plots*)


(* ::Subsubsection::Closed:: *)
(*Bloch Sphere Display Components*)


With[{r=1},
$BlochSphereXY=ParametricPlot3D[0.99*{r Cos[\[Theta]],r Sin[\[Theta]],0},{\[Theta],0,2 \[Pi]},PlotStyle->Dashed,Boxed->False,Axes->False];
$BlochSphereYZ=ParametricPlot3D[0.99*{0,r Cos[\[Theta]],r Sin[\[Theta]]},{\[Theta],0,2 \[Pi]},PlotStyle->Dashed,Boxed->False,Axes->False];
$BlochSphereXZ=ParametricPlot3D[0.99*{r Cos[\[Theta]],0,r Sin[\[Theta]]},{\[Theta],0,2 \[Pi]},PlotStyle->Dashed,Boxed->False,Axes->False];

$BlochSphere=Show[
Graphics3D[{
Opacity[.2],
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
(*Private Helper Functions*)


(*BlochCoordinates[state_]:=Table[Re@Tr[PauliMatrix[n].state],{n,3}]
BlochCoordinates = Compile[{{state,_Complex,2}},{
	Re[state[[1,2]]+state[[2,1]]],
	-Im[state[[1,2]]]+Im[state[[2,1]]],
	Re[state[[1,1]]-state[[2,2]]]
}];*)
BlochCoordinates = Function[{state},{
	Re[state[[1,2]]+state[[2,1]]],
	-Im[state[[1,2]]]+Im[state[[2,1]]],
	Re[state[[1,1]]-state[[2,2]]]
}];
SetAttributes[BlochCoordinates,Listable];


BlochLegend[labels_,joined_,colorPairs_]:=LineLegend[
	Directive[Thickness[0.01],Sequence@@If[joined,{},{Dotted}],#]&/@colorPairs[[All,1]],
	labels
]


ChooseBlochColorPairs[colorOption_]:=
	Which[
		colorOption===Automatic,
			ChooseBlochColorPairs[{RGBColor[0.742077, 0.0624857, 0.00605783],RGBColor[0, 0.501961, 0],RGBColor[1, 0.0175937, 0.505959],RGBColor[1, 0.904479, 0.279423],RGBColor[0.915, 0.3325, 0.2125],RGBColor[0.28026441037696703`, 0.715, 0.4292089322474965],RGBColor[0.6705882352941176, 0.8784313725490196, 0.9372549019607843],RGBColor[0.09019607843137255, 0.33725490196078434`, 0.49411764705882355`],RGBColor[0.975067, 1, 0.906294],RGBColor[0.5333333333333333, 0.23529411764705882`, 0.3058823529411765],RGBColor[0.823529, 0.663981, 0.00828565]}],
		ListQ[colorOption]&&MatchQ[colorOption,{__?ColorQ}],
			Map[{#,Lighter[#]}&,colorOption],
		ListQ[colorOption]&&ArrayDepth[colorOption]===2&&MatchQ[Flatten[colorOption],{__?ColorQ}],
			colorOption,
		True,
			Message[BlochPlot::color];
			ChooseBlochColorPairs[Automatic]
	]


(* ::Subsubsection::Closed:: *)
(*Bloch Sphere Plotting*)


InheritOptions[BlochPlot, {Graphics3D, ParametricPlot3D, Interpolation}, {
	ViewAngle->0.34,
	BlochPlotLabels->Off,
	BlochPlotJoined->False,
	BlochPlotColors->Automatic,
	BlochPlotEndPoints->True
}];


Unprotect@BlochPlot;
BlochPlot[state_,opts:OptionsPattern[]]:=
	Module[{statesList,listOfDensity},

		statesList=state;
		listOfDensity[array_]:=And[ArrayDepth[array]===3,Dimensions[array][[-2;;]]==={2,2}];

		(*Turn single pure state or density matrix into list of one density matrix*)
		If[VectorQ[statesList],statesList=Projector[statesList]];
		If[SquareMatrixQ[statesList],statesList={statesList}];
		(*If the depth is still 2 it must be a list of pure states *)
		If[ArrayDepth[statesList]===2,statesList=Map[Projector,statesList]];
		(*Turn a list of density matrices into a list thereof*)
		If[listOfDensity[statesList],statesList={statesList}];
		(*If the depth is still 3 it must be a list of lists of pure states; make into densities*)
		If[Apply[And,Map[MatrixQ,statesList]],statesList=Map[Projector,statesList,{2}]];	

		(*If all went right we should have a list of lists of 2x2 density matrices.*)
		If[Not[And@@(Map[listOfDensity,statesList])],Message[BlochPlot::input,Dimensions/@statesList]];
		If[OptionValue[BlochPlotJoined],
			ParametricBlochPlot[statesList,opts],
			ListBlochPlot[statesList,opts]
		]
	]


ListBlochPlot[statesList_,opt:OptionsPattern[BlochPlot]]:=
	Module[{plotdat,d,numlists,colorPairs,ep,fig},
		colorPairs=ChooseBlochColorPairs[OptionValue[BlochPlotColors]];
		numlists=Length@statesList;
		ep=OptionValue@BlochPlotEndPoints;
		plotdat=Table[Map[BlochCoordinates,statesList[[n]]],{n,numlists}];

		d=Table[Length[statesList[[n]]],{n,numlists}];
		colorPairs=colorPairs[[Mod[Range[numlists]-1,Length@colorPairs]+1]];

		fig=Show[
			$BlochSphere,
			Table[
				Graphics3D[{
					Table[
						{
							Blend[colorPairs[[n]],1-j/d[[n]]],
							PointSize@If[Or[1<j<d[[n]],Not[ep]],0.015,0.035], 
							Point[plotdat[[n,j]]]
						},
						{j,d[[n]]}
					],
					Sequence@@If[d[[n]]===1,{colorPairs[[n,1]],Thickness[0.005],Line[{{0,0,0},plotdat[[n,1]]}]},{}]
				},Boxed->False],
				{n,numlists}
			],
			ViewAngle->OptionValue[ViewAngle],
			FilterOptions[Graphics3D,opt]
		];
		If[OptionValue[BlochPlotLabels]===Off,
			fig,
			Legended[fig,BlochLegend[OptionValue[BlochPlotLabels],False,colorPairs]]
		]
	]
	


ParametricBlochPlot[statesList_,opt:OptionsPattern[BlochPlot]]:=
	Module[{fig,plotdat,fx,fy,fz,d,numlists,colorPairs,range},
		colorPairs=ChooseBlochColorPairs[OptionValue[BlochPlotColors]];
		numlists=Length@statesList;
		d=Table[Length[statesList[[n]]],{n,numlists}];
		colorPairs=colorPairs[[Mod[Range[numlists]-1,Length@colorPairs]+1]];

		plotdat=Table[Map[BlochCoordinates,statesList[[n]]],{n,numlists}];
		Table[
			If[d[[n]]>1,
				range=Range[0,1,1/(d[[n]]-1)];
				fx[n]=Interpolation[{range,plotdat[[n,All,1]]}\[Transpose],FilterOptions[Interpolation,opt]];
				fy[n]=Interpolation[{range,plotdat[[n,All,2]]}\[Transpose],FilterOptions[Interpolation,opt]];
				fz[n]=Interpolation[{range,plotdat[[n,All,3]]}\[Transpose],FilterOptions[Interpolation,opt]];,
				fx[n]=Null &;
				fy[n]=Null &;
				fz[n]=Null &;
			],
			{n,numlists}
		];
		fig=Show[
			$BlochSphere,
			Table[Sequence@@{
				ParametricPlot3D[
					{fx[n][u],fy[n][u],fz[n][u]}, {u,0,1},
					Boxed->False,
					Evaluate@FilterOptions[ParametricPlot3D,opt],
					PlotStyle->Thickness[0.01],
					ColorFunction->(Blend[colorPairs[[n]],#4]&)
				],
				Graphics3D[{PointSize[0.035],
					If[OptionValue[BlochPlotEndPoints],Sequence@@{
						colorPairs[[n,1]],Point[plotdat[[n,1]]],
						colorPairs[[n,2]],Point[plotdat[[n,-1]]]
					}],
					Sequence@@If[d[[n]]===1,{colorPairs[[n,1]],Thickness[0.005],Line[{{0,0,0},plotdat[[n,1]]}]},{}]
				}]},
				{n,numlists}
			],
			ViewAngle->OptionValue[ViewAngle],
			FilterOptions[Graphics3D,opt]
		];
		If[OptionValue[BlochPlotLabels]===Off,
			fig,
			Legended[fig,BlochLegend[OptionValue[BlochPlotLabels],True,colorPairs]]
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
Protect[BlochPlot,BlochPlot2D,ListBlochPlot2D,BlochPlotColors,BlochPlotEndPoints,BlochPlotJoined,BlochPlotLabels,HintonPlot,ChannelHintonPlot];
Protect[EigensystemForm];
Protect[FourierListPlot];


EndPackage[];
