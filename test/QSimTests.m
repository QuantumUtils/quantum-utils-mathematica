(* ::Package:: *)

(* ::Title:: *)
(*QuantumUtils for Mathematica*)
(*QSim Unit Tests*)


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


BeginPackage["QSimTests`"];


(* ::Text:: *)
(*The following packages are needed, but their contexts should not be loaded globally.*)


Needs["QUDevTools`"];
Needs["QSim`"];
Needs["QuantumChannel`"];


(* ::Section::Closed:: *)
(*Results*)


Begin["`UnitTests`"];


$Threshold = 10^-5;


$RegisteredTests={};
$TestResults := RunTest[$RegisteredTests];


End[];


(* ::Section:: *)
(*Unit Tests*)


Begin["`UnitTests`"];


(* ::Subsection::Closed:: *)
(*Predicates*)


(* ::Text:: *)
(*No tests here for now since all of the predicates are tested implicitly in the tests of PulseSim.*)


(* ::Subsection:: *)
(*Helper Functions*)


(* ::Subsection::Closed:: *)
(*Single Pulse*)


(* ::Subsubsection:: *)
(*Drift Pulse*)


TestCase[$RegisteredTests,"PulseSim:DriftPulse",
	Norm[
		Last@Unitaries@PulseSim[{{0,1},{1,0}},2]
		-{{Cos[2],-I Sin[2]},{-I Sin[2],Cos[2]}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:DriftPulseTimeDep",
	Norm[
		Last@Unitaries@PulseSim[{{0,Cos[#]},{Cos[#],0}}&,5]
		-{{0.5742394822354157` +0.` I,0.` +0.8186873744244522` I},{0.` +0.818687374424452` I,0.5742394822354161` +0.` I}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:DriftPulseAnalytic",
	Module[{a,b,t},
		Last@Unitaries@PulseSim[{{0,a},{a,0}},t,NumericEvaluation->False]
		== {{Cos[a t],-I*Sin[a t]},{-I*Sin[a t],Cos[a t]}}
	]
]


TestCase[$RegisteredTests,"PulseSim:DriftPulseWithStates",
	Norm[
		Last@States@PulseSim[{{0,1},{1,0}},2,InitialState->{{.7,0},{0,.3}}]
		-{{0.36927127582727765` +0.` I,0.` -0.15136049906158555` I},{0.` +0.1513604990615856` I,0.6307287241727222` +0.` I}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:DriftPulseLindblad",
	Norm[
		Last@States@PulseSim[LindbladForm[{{0,1},{1,0}},{{{1,0},{0,-1}}}],2,InitialState->{{.7,0},{0,.3}}]
		-{{0.46937544631719` +0.` I,0.` -0.00990597594838295` I},{0.` +0.009905975948382967` I,0.5306245536828097` +0.` I}}
	] < $Threshold
]


(* ::Subsubsection:: *)
(*Shaped Pulse*)


TestCase[$RegisteredTests,"PulseSim:ShapedPulse",
	Norm[
		Last@Unitaries@PulseSim[{{1,0},{0,-1}},{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}}]
		-{{0.9777278846389216` -0.1505870528557579` I,0.12353764730343048` -0.07816759437541426` I},{-0.12353764730342981`- 0.07816759437541462` I,0.9777278846389224` +0.1505870528557493` I}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:ShapedPulseTimeDep",
	Norm[
		Last@Unitaries@PulseSim[{{0,Cos[#]},{Cos[#],0}}&,{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}}]
		-{{0.8400832121177894` +0.0297745991183988` I,-0.39781488092880346`+ 0.367582630814888` I},{0.3978148809288031` +0.36758263081488907` I,0.8400832121177911` -0.02977459911839913` I}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:ShapedPulseWithStates",
	Norm[
		Last@States@PulseSim[{{1,0},{0,-1}},{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}},InitialState->{{.7,0},{0,.3}}]
		-{{0.69145131075531` -1.431146867680866`*^-17 I,-0.043606069962172984`+ 0.03801192276805836` I},{-0.04360606996217298`- 0.03801192276805836` I,0.3085486892446788` +0.` I}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:ShapedPulseLindblad",
	Norm[
		Last@States@PulseSim[LindbladForm[{{0,1},{1,0}},{{{1,0},{0,-1}}}],{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}},InitialState->{{.7,0},{0,.3}}]
		-{{0.4999999999481136` -7.71010187685563`*^-16 I,-1.1788512412532763`*^-11+ 1.1788321797058875`*^-11 I},{-1.178745933839757`*^-11- 1.1787315074464443`*^-11 I,0.5000000000518892` -7.621125555523322`*^-16 I}}
	] < $Threshold
]


(* ::Subsubsection:: *)
(*Unitary Pulse*)


TestCase[$RegisteredTests,"PulseSim:UnitaryPulse",
	Module[{a,b,c,d},
		Norm[
			Last@Unitaries@PulseSim[{{1,0},{0,-1}},{{{a,b},{c,d}},1}]
			- {{a,b},{c,d}}
		] < $Threshold
	]
]


TestCase[$RegisteredTests,"PulseSim:UnitaryPulseWithStates",
	Module[{a,b,c,d},
		Norm[
			Last@States@PulseSim[{{1,0},{0,-1}},{{{a,b},{c,d}},1},InitialState->{{.7,0},{0,.3}}]
			-{{0.7` a Conjugate[a]+0.3` b Conjugate[b],0.7` a Conjugate[c]+0.3` b Conjugate[d]},{0.7` c Conjugate[a]+0.3` d Conjugate[b],0.7` c Conjugate[c]+0.3` d Conjugate[d]}}
		] < $Threshold
	]
]


(* ::Subsubsection:: *)
(*Channel Pulse*)


TestCase[$RegisteredTests,"PulseSim:ChannelPulse",
	Last@Superoperators@PulseSim[{{1,0},{0,-1}},{Choi[IdentityMatrix[4]],1}]
	=== Super@Choi[IdentityMatrix[4]]
]


TestCase[$RegisteredTests,"PulseSim:ChannelPulseWithStates",
	Norm[
		Last@States@PulseSim[{{1,0},{0,-1}},{Choi[IdentityMatrix[4]],1},InitialState->{{.7,0},{0,.3}}]
		-{{1.`,0.`},{0.`,1.`}}
	] < $Threshold
]


(* ::Subsubsection:: *)
(*Polling Interval*)


TestCase[$RegisteredTests,"PulseSim:PollingInterval",
	Length@Unitaries@PulseSim[{{0,1},{1,0}},2,PollingInterval->0.1]
	=== 21
]


(* ::Subsubsection:: *)
(*Step Size*)


TestCase[$RegisteredTests,"PulseSim:StepSize",
	Norm[
		Last@Unitaries@PulseSim[{{0,Cos[#]},{Cos[#],0}}&,{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}}]
		-Last@Unitaries@PulseSim[{{0,Cos[#]},{Cos[#],0}}&,{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}},StepSize->0.01]
	] < 10^-3
]


(* ::Subsubsection:: *)
(*Force Superoperator*)


TestCase[$RegisteredTests,"PulseSim:ForceSuperoperator",
	Norm[
		First@Last@Superoperators@PulseSim[{{0,1},{1,0}},2,ForceSuperoperator->True]
		-{{0.173178189568194` +0.` I,0.` +0.37840124765396416` I,0.` -0.37840124765396416` I,0.8268218104318061` +0.` I},{0.` +0.37840124765396405` I,0.173178189568194` +0.` I,0.826821810431806` +0.` I,0.` -0.3784012476539641` I},{0.` -0.37840124765396405` I,0.826821810431806` +0.` I,0.173178189568194` +0.` I,0.` +0.3784012476539641` I},{0.8268218104318058` +0.` I,0.` -0.378401247653964` I,0.` +0.378401247653964` I,0.17317818956819397` +0.` I}}
	] < $Threshold
]


(* ::Subsubsection:: *)
(*Observables*)


TestCase[$RegisteredTests,"PulseSim:Observables",
	Norm@Flatten[
		Observables[PulseSim[{{0,1},{1,0}},1,Observables->{{{1,0},{0,-1}}},PollingInterval->.1,InitialState->{{.7,0},{0,.3}}],TimeVector->True]
		-{{{0.`,0.39999999999999997`},{0.1`,0.3920266311364967`},{0.2`,0.3684243976011541`},{0.3`,0.3301342459638715`},{0.4`,0.2786826837388663`},{0.5`,0.21612092234725588`},{0.6`,0.14494310179066938`},{0.7`,0.06798685716009634`},{0.8`,-0.011679808920515677`},{0.9`,-0.090880837877235`},{1.`,-0.16645873461885724`}}}
	] < $Threshold
]


(* ::Subsubsection:: *)
(*Functions*)


TestCase[$RegisteredTests,"PulseSim:Functions",
	Norm@Flatten[
		Functions[PulseSim[{{0,1},{1,0}},1,Functions->{Total[Abs[Flatten[#]]]&},PollingInterval->.1],TimeVector->True]
		-{{{0.`,2.`},{0.1`,2.189675163849708`},{0.2`,2.357471817272606`},{0.3`,2.5017133915738916`},{0.4`,2.620958672623072`},{0.5`,2.7140162009891524`},{0.6`,2.7799561766094287`},{0.7`,2.81811974904436`},{0.8`,2.8281256004933777`},{0.9`,2.8098737557962976`},{1.`,2.7635465813520748`}}}
	] < $Threshold
]


(* ::Subsection::Closed:: *)
(*Pulse Sequence*)


TestCase[$RegisteredTests,"PulseSim:PulseSequence",
	Norm[
		Last@States@PulseSim[{{0,1},{1,0}},{
			{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}},
			{{{0,1},{1,0}},1},
			2,
			{Choi[DiagonalMatrix[{.1,.2,.8,.1}]],1},
			1,
			{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}},
			{Choi[DiagonalMatrix[{.1,.3,.8,.1}]],1},
			{{{0,1},{1,0}},1},
			2
		},InitialState->{{.7,0},{0,.3}}]
		-{{0.21328632515588192` -7.960309225208996`*^-18 I,-2.231833193990433`*^-18+ 0.027377296961943964` I},{2.231833193990433`*^-18- 0.027377296961943964` I,0.16599526671828432` -1.1815538400926602`*^-17 I}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:PulseSequenceTimeDep",
	Norm[
		Last@States@PulseSim[{{0,Cos[#]},{Cos[#],0}}&,{
			{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}},
			{{{0,1},{1,0}},1},
			2,
			{Choi[DiagonalMatrix[{.1,.2,.8,.1}]],1},
			1,
			{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}},
			{Choi[DiagonalMatrix[{.1,.3,.8,.1}]],1},
			{{{0,1},{1,0}},1},
			2
		},InitialState->{{.7,0},{0,.3}}]
		-{{0.19362915131935904` +8.655442582853384`*^-18 I,4.0357821396869856`*^-18- 0.031159266734885976` I},{-4.035782139686985`*^-18+ 0.03115926673488599` I,0.17783775701308532` +6.6101240057425336`*^-18 I}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:PulseSequenceLindblad",
	Norm[
		Last@States@PulseSim[LindbladForm[{{0,1},{1,0}},{{{1,0},{0,-1}}}],{
			{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}},
			{{{0,1},{1,0}},1},
			2,
			{Choi[DiagonalMatrix[{.1,.2,.8,.1}]],1},
			1,
			{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,1},{1,0}},{{0,I},{-I,0}}}},
			{Choi[DiagonalMatrix[{.1,.3,.8,.1}]],1},
			{{{0,1},{1,0}},1},
			2
		},InitialState->{{.7,0},{0,.3}}]
		-{{0.206484207635503` -6.276578586380288`*^-16 I,1.1247658762996243`*^-17+ 0.0037147409808631167` I},{-1.1247658762996234`*^-17- 0.0037147409808631136` I,0.18351579237203836` -5.581130643869315`*^-16 I}}
	] < $Threshold
]


(* ::Subsection::Closed:: *)
(*Over a Distribution*)


TestCase[$RegisteredTests,"PulseSim:OverDist",
	Norm[
		Observables@PulseSim[{{b,a},{a,b}},{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,b},{b,0}},{{0,I},{-I,0}}}},{{.5,.5},{{a->1,b->1},{a->2,b->3}}},InitialState->{{.7,0},{0,.3}},Observables->{{{a,0},{b,0}}}]
		-{{1.0499999999999998`,0.7748695726561399`}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:OverDistTimeDep",
	Norm[
		Observables@PulseSim[{{Cos[b #],a},{a,Sin[b #]}}&,{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,b},{b,0}},{{0,I},{-I,0}}}},{{.5,.5},{{a->1,b->1},{a->2,b->3}}},InitialState->{{.7,0},{0,.3}},Observables->{{{a,0},{b,0}}}]
		-{{1.0499999999999998`,0.8758200899757376`}}
	] < $Threshold
]


TestCase[$RegisteredTests,"PulseSim:OverDistLindblad",
	Norm[
		Observables@PulseSim[LindbladForm[{{b,a},{a,b}},{{{a,0},{0,-b}}}],{{{1,2,3},{4,5,6},{7,8,9},{10,11,12}},{{{0,b},{b,0}},{{0,I},{-I,0}}}},{{.5,.5},{{a->1,b->1},{a->2,b->3}}},InitialState->{{.7,0},{0,.3}},Observables->{{{a,0},{b,0}}}]
		-{{1.0499999999999998`,0.7499999999681967`}}
	] < $Threshold
]


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End Package*)


EndPackage[];
