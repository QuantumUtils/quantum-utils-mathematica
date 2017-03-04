(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Mathematica 10.0' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       158,          7]
NotebookDataLength[     27483,        813]
NotebookOptionsPosition[     24335,        706]
NotebookOutlinePosition[     25129,        735]
CellTagsIndexPosition[     24962,        728]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{

Cell[CellGroupData[{
Cell["Using QUDevTools", "Chapter",
 CellChangeTimes->{{3.633439785011228*^9, 3.633439797185196*^9}, {
  3.633441077794402*^9, 3.633441081824923*^9}, {3.63511187471688*^9, 
  3.635111877828617*^9}}],

Cell[CellGroupData[{

Cell["Requirements", "Section",
 CellChangeTimes->{{3.633440236609514*^9, 3.63344023862528*^9}, {
  3.633442825476911*^9, 3.633442825564836*^9}}],

Cell[BoxData[
 RowBox[{"Needs", "[", "\"\<QUDevTools`\>\"", "]"}]], "Input",
 CellChangeTimes->{{3.633441087771078*^9, 3.6334410928941*^9}, {
  3.6976415017888823`*^9, 3.697641523618401*^9}}]
}, Open  ]],

Cell[CellGroupData[{

Cell["Intro", "Section",
 CellChangeTimes->{{3.633441104241781*^9, 3.6334411051412086`*^9}}],

Cell["\<\
The DocTools package exists to make creating documentation for this library \
less of a pain, and also to help standardize some things.\
\>", "Text",
 CellChangeTimes->{{3.6334411159602833`*^9, 3.633441160519436*^9}, {
  3.6976410863353043`*^9, 3.69764108660446*^9}}]
}, Open  ]],

Cell[CellGroupData[{

Cell["Links", "Section",
 CellChangeTimes->{{3.633439803464673*^9, 3.633439816454121*^9}, {
  3.633441098217311*^9, 3.633441098756905*^9}, {3.633442816267712*^9, 
  3.633442816339408*^9}, {3.633714481366995*^9, 3.633714481459066*^9}}],

Cell[CellGroupData[{

Cell["Notebook Link", "Subsection",
 CellChangeTimes->{{3.6334428066987047`*^9, 3.633442811152179*^9}}],

Cell[TextData[{
 "Links can be created with the ",
 StyleBox["NotebookLink[notebookFile,name,description]", "Input"],
 " function as seen in the examples below. This function outputs a new cell. \
Once the cell is generated, it can be modified as any other cell. Delete the \
code that generated the cell when you are done."
}], "Text",
 CellChangeTimes->{{3.633441177013389*^9, 3.6334412396240797`*^9}, {
  3.633441313038054*^9, 3.633441324815538*^9}}],

Cell[TextData[{
 "The builtin Hyperlink function seems to be smart about relative paths. From \
trial and error, it appears to look in",
 StyleBox[" NotebookDirectory[]", "Input"],
 " first, and then ",
 StyleBox["Directory[]", "Input"],
 ", which is convenient for us."
}], "Text",
 CellChangeTimes->{{3.6334412652219477`*^9, 3.63344129091085*^9}, {
  3.6334415225186443`*^9, 3.633441557198667*^9}}],

Cell[BoxData[
 RowBox[{"NotebookLink", "[", 
  RowBox[{
  "\"\<../api-doc/GRAPE.nb\>\"", ",", "\"\<GRAPE\>\"", ",", 
   "\"\<An implementation of GRadient Ascent Pulse Engineering with many \
features, including tools for discrete distortion operators.\>\""}], 
  "]"}]], "Input",
 CellChangeTimes->{{3.633441244299698*^9, 3.633441258902042*^9}, {
  3.633441308773676*^9, 3.633441349125222*^9}, {3.633459983842565*^9, 
  3.633459984751299*^9}, {3.633714091410062*^9, 3.6337140926263742`*^9}, {
  3.633714228253684*^9, 3.633714228847991*^9}, {3.633714472880781*^9, 
  3.633714473082502*^9}}],

Cell[BoxData[GridBox[{
   {
    StyleBox[
     TagBox[
      ButtonBox[
       PaneSelectorBox[{False->"\<\"GRAPE\"\>", True->
        StyleBox["\<\"GRAPE\"\>", "HyperlinkActive"]}, Dynamic[
         CurrentValue["MouseOver"]],
        BaseStyle->{"Hyperlink"},
        FrameMargins->0,
        ImageSize->Automatic],
       BaseStyle->"Hyperlink",
       ButtonData:>{"api-doc/GRAPE.nb", None},
       ButtonNote->"api-doc/GRAPE.nb"],
      Annotation[#, "api-doc/GRAPE.nb", "Hyperlink"]& ], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{
      RowBox[{
      "An", " ", "implementation", " ", "of", " ", "GRadient", " ", "Ascent", 
       " ", "Pulse", " ", "Engineering", " ", "with", " ", "many", " ", 
       "features"}], ",", " ", 
      RowBox[{
      "including", " ", "tools", " ", "for", " ", "discrete", " ", 
       "distortion", " ", 
       RowBox[{"operators", "."}]}]}], "Text"]}
  },
  GridBoxAlignment->{"Columns" -> {{Left}}},
  GridBoxDividers->{"Columns" -> {{False}}, "Rows" -> {{False}}},
  GridBoxItemSize->{"Columns" -> {
      Scaled[0.1], {
       Scaled[0.7]}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, 
    "RowsIndexed" -> {}}]], "Text",
 GeneratedCell->True,
 CellAutoOverwrite->True,
 CellChangeTimes->{3.633714234561171*^9}],

Cell[BoxData[
 RowBox[{"NotebookLink", "[", 
  RowBox[{
  "\"\<../api-doc/QSim.nb\>\"", ",", "\"\<QSim\>\"", ",", 
   "\"\<A general purpose quantum system time-evolution simulator whose \
primary objective is ease of use.\>\""}], "]"}]], "Input",
 CellChangeTimes->{{3.633441373310739*^9, 3.63344139385056*^9}, {
  3.633441429065579*^9, 3.633441430942194*^9}, {3.6337141189846067`*^9, 
  3.6337141206547213`*^9}, {3.633714230577765*^9, 3.633714231108385*^9}, {
  3.6337144756886463`*^9, 3.6337144759554663`*^9}}],

Cell[BoxData[GridBox[{
   {
    StyleBox[
     TagBox[
      ButtonBox[
       PaneSelectorBox[{False->"\<\"QSim\"\>", True->
        StyleBox["\<\"QSim\"\>", "HyperlinkActive"]}, Dynamic[
         CurrentValue["MouseOver"]],
        BaseStyle->{"Hyperlink"},
        FrameMargins->0,
        ImageSize->Automatic],
       BaseStyle->"Hyperlink",
       ButtonData:>{"api-doc/QSim.nb", None},
       ButtonNote->"api-doc/QSim.nb"],
      Annotation[#, "api-doc/QSim.nb", "Hyperlink"]& ], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{
      RowBox[{
      "A", " ", "general", " ", "purpose", " ", "quantum", " ", "system", " ",
        "time"}], "-", 
      RowBox[{
      "evolution", " ", "simulator", " ", "whose", " ", "primary", " ", 
       "objective", " ", "is", " ", "ease", " ", "of", " ", 
       RowBox[{"use", "."}]}]}], "Text"]}
  },
  GridBoxAlignment->{"Columns" -> {{Left}}},
  GridBoxDividers->{"Columns" -> {{False}}, "Rows" -> {{False}}},
  GridBoxItemSize->{"Columns" -> {
      Scaled[0.1], {
       Scaled[0.7]}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, 
    "RowsIndexed" -> {}}]], "Text",
 GeneratedCell->True,
 CellAutoOverwrite->True,
 CellChangeTimes->{3.633714252630775*^9}]
}, Closed]]
}, Open  ]],

Cell[CellGroupData[{

Cell["Tables", "Section",
 CellChangeTimes->{{3.633441654515319*^9, 3.633441655716835*^9}}],

Cell[CellGroupData[{

Cell["Descriptive Table", "Subsection",
 CellChangeTimes->{{3.633442225176917*^9, 3.633442233536725*^9}}],

Cell[TextData[{
 "Because the ",
 StyleBox["Mathematica",
  FontSlant->"Italic"],
 " GUI menu item Insert>Table/Matrix doesn\[CloseCurlyQuote]t support tables \
with left-justified text, DocTools implements an alternative, ",
 StyleBox["DescriptiveTable[headers,content]", "Input"],
 "."
}], "Text",
 CellChangeTimes->{{3.633441661205536*^9, 3.633441719925829*^9}}],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{"?", "DescriptiveTable"}]], "Input",
 CellChangeTimes->{{3.633442373518284*^9, 3.633442376221279*^9}}],

Cell[BoxData[
 StyleBox["\<\"DescriptiveTable[headers,content,OptionsPattern[GridBox]] \
generates a new cell containing a table. The first row of the table contains \
the header strings provided in headers in bold font. The rest of the table is \
filled in with content.\"\>", "MSG"]], "Print", "PrintUsage",
 CellChangeTimes->{3.633442376587818*^9},
 CellTags->"Info3633424376-4088463"]
}, Open  ]],

Cell[CellGroupData[{

Cell["Examples", "Subsubsection",
 CellChangeTimes->{{3.633442382203384*^9, 3.6334423832169123`*^9}}],

Cell["Below is a simple example.", "Text",
 CellChangeTimes->{{3.633441798985717*^9, 3.633441803106983*^9}}],

Cell[BoxData[
 RowBox[{"DescriptiveTable", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
    "\"\<Header 1\>\"", ",", "\"\<Header 2\>\"", ",", "\"\<Header 3\>\""}], 
    "}"}], ",", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{"\"\<a\>\"", ",", "\"\<b\>\"", ",", "\"\<c\>\""}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{"\"\<d\>\"", ",", "\"\<e\>\"", ",", "\"\<f\>\""}], "}"}]}], 
    "}"}]}], "]"}]], "Input",
 CellChangeTimes->{{3.633441736825877*^9, 3.63344178387381*^9}}],

Cell[BoxData[GridBox[{
   {
    StyleBox[
     RowBox[{"Header", " ", "1"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "2"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "3"}], "Text",
     FontWeight->Bold]},
   {"a", "b", "c"},
   {"d", "e", "f"}
  },
  GridBoxAlignment->{"Columns" -> {{Left}}},
  GridBoxDividers->{"Columns" -> {{False}}, "Rows" -> {{True}}}]], "Text",
 GeneratedCell->True,
 CellAutoOverwrite->True,
 CellChangeTimes->{3.6334417844284782`*^9}],

Cell[TextData[{
 "Note that DescriptiveTable uses low-level tools to create tables, and so \
for maximal generality, the ",
 StyleBox["content", "Input"],
 " input is expected to define its own display properties. As an example, \
notice how the following fails:"
}], "Text",
 CellChangeTimes->{{3.633441810644368*^9, 3.6334418996116467`*^9}}],

Cell[BoxData[
 RowBox[{"DescriptiveTable", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
    "\"\<Header 1\>\"", ",", "\"\<Header 2\>\"", ",", "\"\<Header 3\>\""}], 
    "}"}], ",", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"Sin", "[", "x", "]"}], ",", "\"\<b\>\"", ",", "\"\<c\>\""}], 
      "}"}], ",", 
     RowBox[{"{", 
      RowBox[{"\"\<d\>\"", ",", "\"\<e\>\"", ",", "\"\<f\>\""}], "}"}]}], 
    "}"}]}], "]"}]], "Input",
 CellChangeTimes->{{3.6334419069962883`*^9, 3.633441920427349*^9}}],

Cell[BoxData[GridBox[{
   {
    StyleBox[
     RowBox[{"Header", " ", "1"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "2"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "3"}], "Text",
     FontWeight->Bold]},
   {
    Sin[$CellContext`x], "b", "c"},
   {"d", "e", "f"}
  },
  GridBoxAlignment->{"Columns" -> {{Left}}},
  GridBoxDividers->{"Columns" -> {{False}}, "Rows" -> {{True}}}]], "Text",
 GeneratedCell->True,
 CellAutoOverwrite->True,
 CellChangeTimes->{3.633441920590241*^9}],

Cell["\<\
Sin[x] is expected to have a representation in term of Boxes, which it does \
not. We can fix this as follows:\
\>", "Text",
 CellChangeTimes->{{3.6334419596775208`*^9, 3.633441984923134*^9}}],

Cell[BoxData[
 RowBox[{"DescriptiveTable", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
    "\"\<Header 1\>\"", ",", "\"\<Header 2\>\"", ",", "\"\<Header 3\>\""}], 
    "}"}], ",", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"ToBoxes", "[", 
        RowBox[{"Sin", "[", "x", "]"}], "]"}], ",", "\"\<b\>\"", ",", 
       "\"\<c\>\""}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{"\"\<d\>\"", ",", "\"\<e\>\"", ",", "\"\<f\>\""}], "}"}]}], 
    "}"}]}], "]"}]], "Input",
 CellChangeTimes->{{3.633441993674436*^9, 3.633441997368866*^9}}],

Cell[BoxData[GridBox[{
   {
    StyleBox[
     RowBox[{"Header", " ", "1"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "2"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "3"}], "Text",
     FontWeight->Bold]},
   {
    RowBox[{"Sin", "[", "x", "]"}], "b", "c"},
   {"d", "e", "f"}
  },
  GridBoxAlignment->{"Columns" -> {{Left}}},
  GridBoxDividers->{"Columns" -> {{False}}, "Rows" -> {{True}}}]], "Text",
 GeneratedCell->True,
 CellAutoOverwrite->True,
 CellChangeTimes->{3.63344199817524*^9}],

Cell["We can also change the display style:", "Text",
 CellChangeTimes->{{3.633442011242036*^9, 3.633442019802047*^9}}],

Cell[BoxData[
 RowBox[{"DescriptiveTable", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
    "\"\<Header 1\>\"", ",", "\"\<Header 2\>\"", ",", "\"\<Header 3\>\""}], 
    "}"}], ",", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"StyleBox", "[", 
        RowBox[{
         RowBox[{"ToBoxes", "[", 
          RowBox[{"Sin", "[", "x", "]"}], "]"}], ",", "\"\<Text\>\""}], "]"}],
        ",", "\"\<b\>\"", ",", 
       RowBox[{"StyleBox", "[", 
        RowBox[{"\"\<text style\>\"", ",", "\"\<Text\>\"", ",", "Italic"}], 
        "]"}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{"\"\<d\>\"", ",", "\"\<e\>\"", ",", "\"\<f\>\""}], "}"}]}], 
    "}"}]}], "]"}]], "Input",
 CellChangeTimes->{{3.6334420343287287`*^9, 3.633442103952721*^9}}],

Cell[BoxData[GridBox[{
   {
    StyleBox[
     RowBox[{"Header", " ", "1"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "2"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "3"}], "Text",
     FontWeight->Bold]},
   {
    StyleBox[
     RowBox[{"Sin", "[", "x", "]"}], "Text"], "b", 
    StyleBox[
     RowBox[{"text", " ", "style"}], "Text",
     FontSlant->Italic]},
   {"d", "e", "f"}
  },
  GridBoxAlignment->{"Columns" -> {{Left}}},
  GridBoxDividers->{"Columns" -> {{False}}, "Rows" -> {{True}}}]], "Text",
 GeneratedCell->True,
 CellAutoOverwrite->True,
 CellChangeTimes->{3.633442104293007*^9}],

Cell[TextData[{
 "Finally, note that any options passed to ",
 StyleBox["DescriptiveTable", "Input"],
 " will be forwarded to ",
 StyleBox["BoxData", "Input"],
 ". The ",
 StyleBox["Scaled", "Input"],
 " function comes in handy. We can use this, for example to force column \
sizes:"
}], "Text",
 CellChangeTimes->{{3.633442133825021*^9, 3.633442196713496*^9}, {
  3.633442333126203*^9, 3.633442340495822*^9}}],

Cell[BoxData[
 RowBox[{"DescriptiveTable", "[", "\[IndentingNewLine]", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
    "\"\<Header 1\>\"", ",", "\"\<Header 2\>\"", ",", "\"\<Header 3\>\""}], 
    "}"}], ",", "\[IndentingNewLine]", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"StyleBox", "[", 
        RowBox[{
         RowBox[{"ToBoxes", "[", 
          RowBox[{"Sin", "[", "x", "]"}], "]"}], ",", "\"\<Text\>\""}], "]"}],
        ",", "\"\<b\>\"", ",", 
       RowBox[{"StyleBox", "[", 
        RowBox[{"\"\<text style\>\"", ",", "\"\<Text\>\"", ",", "Italic"}], 
        "]"}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{"\"\<d\>\"", ",", "\"\<e\>\"", ",", "\"\<f\>\""}], "}"}]}], 
    "}"}], ",", "\[IndentingNewLine]", 
   RowBox[{"ColumnWidths", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{"20", ",", "Automatic", ",", 
      RowBox[{"Scaled", "[", "0.4", "]"}]}], "}"}]}]}], "\[IndentingNewLine]",
   "]"}]], "Input",
 CellChangeTimes->{{3.633442256950156*^9, 3.633442278752667*^9}, {
  3.633442311223393*^9, 3.633442324025093*^9}}],

Cell[BoxData[GridBox[{
   {
    StyleBox[
     RowBox[{"Header", " ", "1"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "2"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "3"}], "Text",
     FontWeight->Bold]},
   {
    StyleBox[
     RowBox[{"Sin", "[", "x", "]"}], "Text"], "b", 
    StyleBox[
     RowBox[{"text", " ", "style"}], "Text",
     FontSlant->Italic]},
   {"d", "e", "f"}
  },
  GridBoxAlignment->{"Columns" -> {{Left}}},
  GridBoxDividers->{"Columns" -> {{False}}, "Rows" -> {{True}}},
  GridBoxItemSize->{"Columns" -> {20, Automatic, {
       Scaled[0.4]}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, 
    "RowsIndexed" -> {}}]], "Text",
 GeneratedCell->True,
 CellAutoOverwrite->True,
 CellChangeTimes->{3.63344232459762*^9}]
}, Closed]]
}, Closed]],

Cell[CellGroupData[{

Cell["Descriptive Table with Place Holders", "Subsection",
 CellChangeTimes->{{3.63344224527881*^9, 3.6334422523910313`*^9}}],

Cell[TextData[{
 "It is sometimes useful to some of the formatting of a table by hand. ",
 StyleBox["DescriptiveFillInTable", "Input"],
 " puts placeholders in the table."
}], "Text",
 CellChangeTimes->{{3.6334424019035997`*^9, 3.633442413576338*^9}, {
  3.63344247510393*^9, 3.6334425037509527`*^9}}],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{"?", "DescriptiveFillInTable"}]], "Input",
 CellChangeTimes->{{3.633442510176855*^9, 3.633442513268634*^9}}],

Cell[BoxData[
 StyleBox["\<\"DescriptiveFillInTable[headers,rows,OptionsPattern[GridBox]] \
invokes DescriptiveTable to generate a new cell containing a table. The first \
row of the table contains the header strings provided in headers in bold \
font. The rest of the table has the given number of rows, with each table \
element a placeholder.\"\>", "MSG"]], "Print", "PrintUsage",
 CellChangeTimes->{3.633442525993555*^9},
 CellTags->"Info3633424525-4792925"]
}, Open  ]],

Cell[CellGroupData[{

Cell["Example", "Subsubsection",
 CellChangeTimes->{{3.6334425294680157`*^9, 3.63344253111756*^9}}],

Cell[BoxData[
 RowBox[{"DescriptiveFillInTable", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
    "\"\<Header 1\>\"", ",", "\"\<Header 2\>\"", ",", "\"\<Header 3\>\"", 
     ",", "\"\<Last Header\>\""}], "}"}], ",", "4", ",", 
   RowBox[{"ColumnWidths", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{"Automatic", ",", "Automatic", ",", "Automatic", ",", 
      RowBox[{"Scaled", "[", "0.4", "]"}]}], "}"}]}]}], "]"}]], "Input",
 CellChangeTimes->{{3.633442535266415*^9, 3.6334425893741693`*^9}}],

Cell[BoxData[GridBox[{
   {
    StyleBox[
     RowBox[{"Header", " ", "1"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "2"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Header", " ", "3"}], "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Last", " ", "Header"}], "Text",
     FontWeight->Bold]},
   {"\[Placeholder]", "\[Placeholder]", "\[Placeholder]", "\[Placeholder]"},
   {"\[Placeholder]", "\[Placeholder]", "\[Placeholder]", "\[Placeholder]"},
   {"\[Placeholder]", "\[Placeholder]", "\[Placeholder]", "\[Placeholder]"},
   {"\[Placeholder]", "\[Placeholder]", "\[Placeholder]", "\[Placeholder]"}
  },
  GridBoxAlignment->{"Columns" -> {{Left}}},
  GridBoxDividers->{"Columns" -> {{False}}, "Rows" -> {{True}}},
  GridBoxItemSize->{"Columns" -> {Automatic, Automatic, Automatic, {
       Scaled[0.4]}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, 
    "RowsIndexed" -> {}}]], "Text",
 GeneratedCell->True,
 CellAutoOverwrite->True,
 CellChangeTimes->{3.6334425906072197`*^9}]
}, Closed]]
}, Closed]],

Cell[CellGroupData[{

Cell["Function Options", "Subsection",
 CellChangeTimes->{{3.63344261295022*^9, 3.6334426149751*^9}}],

Cell[TextData[{
 StyleBox["DisplayOptions", "Input"],
 " uses ",
 StyleBox["DescriptiveTable", "Input"],
 " to display all of the options, default values, and option usage strings of \
a given function."
}], "Text",
 CellChangeTimes->{{3.633442631891361*^9, 3.6334426605337276`*^9}}],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{"?", "DisplayOptions"}]], "Input",
 CellChangeTimes->{{3.633442667954946*^9, 3.6334426713355913`*^9}}],

Cell[BoxData[
 StyleBox["\<\"DisplayOptions[TargetFunction] prints a human readable cell \
describing the options of a given function. Assumes all options have usage \
text.\"\>", "MSG"]], "Print", "PrintUsage",
 CellChangeTimes->{3.633442671797982*^9},
 CellTags->"Info3633424671-4792925"]
}, Open  ]],

Cell[CellGroupData[{

Cell["Example", "Subsubsection",
 CellChangeTimes->{{3.633442679788275*^9, 3.633442680852071*^9}}],

Cell[BoxData[
 RowBox[{"DisplayOptions", "[", "FindMaximum", "]"}]], "Input",
 CellChangeTimes->{{3.633442682788767*^9, 3.633442702050301*^9}, {
  3.633442753067842*^9, 3.633442782769556*^9}}],

Cell[BoxData[GridBox[{
   {
    StyleBox["Option", "Text",
     FontWeight->Bold], 
    StyleBox[
     RowBox[{"Default", " ", "Value"}], "Text",
     FontWeight->Bold], 
    StyleBox["Description", "Text",
     FontWeight->Bold]},
   {"AccuracyGoal", "Automatic", 
    StyleBox[
     RowBox[{
     "AccuracyGoal", " ", "is", " ", "an", " ", "option", " ", "for", " ", 
      "various", " ", "numerical", " ", "operations", " ", "which", " ", 
      "specifies", " ", "how", " ", "many", " ", "effective", " ", "digits", 
      " ", "of", " ", "accuracy", " ", "should", " ", "be", " ", "sought", 
      " ", "in", " ", "the", " ", "final", " ", 
      RowBox[{"result", ".", " "}]}], "Text"]},
   {"Compiled", "Automatic", 
    StyleBox[
     RowBox[{
     "Compiled", " ", "is", " ", "an", " ", "option", " ", "for", " ", 
      "various", " ", "numerical", " ", "and", " ", "plotting", " ", 
      "functions", " ", "which", " ", "specifies", " ", "whether", " ", "the",
       " ", "expressions", " ", "they", " ", "work", " ", "with", " ", 
      "should", " ", "automatically", " ", "be", " ", 
      RowBox[{"compiled", ".", " "}]}], "Text"]},
   {"EvaluationMonitor", "None", 
    StyleBox[
     RowBox[{
     "EvaluationMonitor", " ", "is", " ", "an", " ", "option", " ", "for", 
      " ", "various", " ", "numerical", " ", "computation", " ", "and", " ", 
      "plotting", " ", "functions", " ", "that", " ", "gives", " ", "an", " ",
       "expression", " ", "to", " ", "evaluate", " ", "whenever", " ", 
      "functions", " ", "derived", " ", "from", " ", "the", " ", "input", " ",
       "are", " ", "evaluated", " ", 
      RowBox[{"numerically", ".", " "}]}], "Text"]},
   {"Gradient", "Automatic", 
    StyleBox[
     RowBox[{
     "Gradient", " ", "is", " ", "an", " ", "option", " ", "for", " ", 
      "FindMinimum", " ", "and", " ", "related", " ", "functions", " ", 
      "that", " ", "specifies", " ", "the", " ", "gradient", " ", "vector", 
      " ", "to", " ", "assume", " ", "for", " ", "the", " ", "function", " ", 
      "being", " ", 
      RowBox[{"extremized", "."}]}], "Text"]},
   {"MaxIterations", "Automatic", 
    StyleBox[
     RowBox[{
      RowBox[{
      "MaxIterations", " ", "is", " ", "an", " ", "option", " ", "that", " ", 
       "specifies", " ", "the", " ", "maximum", " ", "number", " ", "of", " ",
        "iterations", " ", "that", " ", "should", " ", "be", " ", "tried", 
       " ", "in", " ", "various", " ", "built"}], "-", 
      RowBox[{"in", " ", "functions", " ", "and", " ", 
       RowBox[{"algorithms", "."}]}]}], "Text"]},
   {"Method", "Automatic", 
    StyleBox[
     RowBox[{
      RowBox[{
      "Method", " ", "is", " ", "an", " ", "option", " ", "for", " ", 
       "various", " ", "algorithm"}], "-", 
      RowBox[{
      "intensive", " ", "functions", " ", "that", " ", "specifies", " ", 
       "what", " ", "internal", " ", "methods", " ", "they", " ", "should", 
       " ", 
       RowBox[{"use", "."}]}]}], "Text"]},
   {"PrecisionGoal", "Automatic", 
    StyleBox[
     RowBox[{
     "PrecisionGoal", " ", "is", " ", "an", " ", "option", " ", "for", " ", 
      "various", " ", "numerical", " ", "operations", " ", "which", " ", 
      "specifies", " ", "how", " ", "many", " ", "effective", " ", "digits", 
      " ", "of", " ", "precision", " ", "should", " ", "be", " ", "sought", 
      " ", "in", " ", "the", " ", "final", " ", 
      RowBox[{"result", ".", " "}]}], "Text"]},
   {"StepMonitor", "None", 
    StyleBox[
     RowBox[{
     "StepMonitor", " ", "is", " ", "an", " ", "option", " ", "for", " ", 
      "iterative", " ", "numerical", " ", "computation", " ", "functions", 
      " ", "that", " ", "gives", " ", "an", " ", "expression", " ", "to", " ",
       "evaluate", " ", "whenever", " ", "a", " ", "step", " ", "is", " ", 
      "taken", " ", "by", " ", "the", " ", "numerical", " ", "method", " ", 
      RowBox[{"used", ".", " "}]}], "Text"]},
   {"WorkingPrecision", "MachinePrecision", 
    StyleBox[
     RowBox[{
     "WorkingPrecision", " ", "is", " ", "an", " ", "option", " ", "for", " ",
       "various", " ", "numerical", " ", "operations", " ", "that", " ", 
      "specifies", " ", "how", " ", "many", " ", "digits", " ", "of", " ", 
      "precision", " ", "should", " ", "be", " ", "maintained", " ", "in", 
      " ", "internal", " ", 
      RowBox[{"computations", ".", " "}]}], "Text"]}
  },
  GridBoxAlignment->{"Columns" -> {{Left}}},
  GridBoxDividers->{"Columns" -> {{False}}, "Rows" -> {{True}}},
  GridBoxItemSize->{"Columns" -> {Automatic, Automatic, {
       Scaled[0.5]}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, 
    "RowsIndexed" -> {}}]], "Text",
 GeneratedCell->True,
 CellAutoOverwrite->True,
 CellChangeTimes->{3.633442783135995*^9}]
}, Closed]]
}, Closed]]
}, Open  ]]
}, Open  ]]
},
WindowSize->{1280, 1000},
WindowMargins->{{Automatic, 0}, {Automatic, 28}},
FrontEndVersion->"11.0 for Linux x86 (64-bit) (September 21, 2016)",
StyleDefinitions->"Default.nb"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{
 "Info3633424376-4088463"->{
  Cell[7208, 211, 388, 6, 60, "Print",
   CellTags->"Info3633424376-4088463"]},
 "Info3633424525-4792925"->{
  Cell[16160, 502, 462, 7, 60, "Print",
   CellTags->"Info3633424525-4792925"]},
 "Info3633424671-4792925"->{
  Cell[18889, 580, 290, 5, 42, "Print",
   CellTags->"Info3633424671-4792925"]}
 }
*)
(*CellTagsIndex
CellTagsIndex->{
 {"Info3633424376-4088463", 24639, 716},
 {"Info3633424525-4792925", 24748, 719},
 {"Info3633424671-4792925", 24858, 722}
 }
*)
(*NotebookFileOutline
Notebook[{
Cell[CellGroupData[{
Cell[580, 22, 198, 3, 66, "Chapter"],
Cell[CellGroupData[{
Cell[803, 29, 145, 2, 65, "Section"],
Cell[951, 33, 191, 3, 32, "Input"]
}, Open  ]],
Cell[CellGroupData[{
Cell[1179, 41, 92, 1, 65, "Section"],
Cell[1274, 44, 277, 5, 33, "Text"]
}, Open  ]],
Cell[CellGroupData[{
Cell[1588, 54, 234, 3, 65, "Section"],
Cell[CellGroupData[{
Cell[1847, 61, 103, 1, 44, "Subsection"],
Cell[1953, 64, 453, 8, 55, "Text"],
Cell[2409, 74, 400, 9, 55, "Text"],
Cell[2812, 85, 590, 11, 57, "Input"],
Cell[3405, 98, 1273, 35, 57, "Text"],
Cell[4681, 135, 513, 9, 56, "Input"],
Cell[5197, 146, 1220, 34, 36, "Text"]
}, Closed]]
}, Open  ]],
Cell[CellGroupData[{
Cell[6466, 186, 91, 1, 65, "Section"],
Cell[CellGroupData[{
Cell[6582, 191, 105, 1, 45, "Subsection"],
Cell[6690, 194, 365, 9, 55, "Text"],
Cell[CellGroupData[{
Cell[7080, 207, 125, 2, 32, "Input"],
Cell[7208, 211, 388, 6, 60, "Print",
 CellTags->"Info3633424376-4088463"]
}, Open  ]],
Cell[CellGroupData[{
Cell[7633, 222, 101, 1, 35, "Subsubsection"],
Cell[7737, 225, 108, 1, 33, "Text"],
Cell[7848, 228, 499, 14, 32, "Input"],
Cell[8350, 244, 529, 18, 77, "Text"],
Cell[8882, 264, 343, 7, 55, "Text"],
Cell[9228, 273, 536, 16, 32, "Input"],
Cell[9767, 291, 548, 19, 81, "Text"],
Cell[10318, 312, 202, 4, 33, "Text"],
Cell[10523, 318, 575, 17, 32, "Input"],
Cell[11101, 337, 558, 19, 78, "Text"],
Cell[11662, 358, 119, 1, 33, "Text"],
Cell[11784, 361, 774, 22, 55, "Input"],
Cell[12561, 385, 663, 23, 80, "Text"],
Cell[13227, 410, 410, 11, 55, "Text"],
Cell[13640, 423, 1076, 28, 121, "Input"],
Cell[14719, 453, 804, 26, 80, "Text"]
}, Closed]]
}, Closed]],
Cell[CellGroupData[{
Cell[15572, 485, 125, 1, 37, "Subsection"],
Cell[15700, 488, 301, 6, 33, "Text"],
Cell[CellGroupData[{
Cell[16026, 498, 131, 2, 32, "Input"],
Cell[16160, 502, 462, 7, 60, "Print",
 CellTags->"Info3633424525-4792925"]
}, Open  ]],
Cell[CellGroupData[{
Cell[16659, 514, 99, 1, 35, "Subsubsection"],
Cell[16761, 517, 494, 11, 55, "Input"],
Cell[17258, 530, 1042, 26, 115, "Text"]
}, Closed]]
}, Closed]],
Cell[CellGroupData[{
Cell[18349, 562, 101, 1, 37, "Subsection"],
Cell[18453, 565, 283, 7, 33, "Text"],
Cell[CellGroupData[{
Cell[18761, 576, 125, 2, 32, "Input"],
Cell[18889, 580, 290, 5, 42, "Print",
 CellTags->"Info3633424671-4792925"]
}, Open  ]],
Cell[CellGroupData[{
Cell[19216, 590, 98, 1, 35, "Subsubsection"],
Cell[19317, 593, 192, 3, 32, "Input"],
Cell[19512, 598, 4771, 102, 539, "Text"]
}, Closed]]
}, Closed]]
}, Open  ]]
}, Open  ]]
}
]
*)

