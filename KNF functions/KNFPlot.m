(* ::Package:: *)

(* ::Text:: *)
(*This notebook and the associated Mathematica package generate user-defined y vs x plots of species occurring in the KNF model. It relies upon the existence of a KNF data set generated by KNFGenerateDataSet.m. It and the associated Mathematica package are*)
(**)
(*Copyright (C), 2012,*)
(*David M. Morgan, Ph.D.*)
(*dmorgan@stfx.ca*)
(*St. Francis Xavier University, Angitonish, NS, Canada*)
(*Under the terms of the G.N.U. General Public License Version 3:00 or later.*)
(*http://www.gnu.org/licenses/gpl.html*)


KNFPlot[
YString_, XString_, KNFDataSet_, ColourArgument_
] := Module[
{
YLIST,
XLIST,
XYLIST,
OUTPUT
},
YLIST = Which[
YString == "PO2", #[[1]] & /@KNFDataSet,
YString == "Y", #[[2]] & /@KNFDataSet,
YString == "O2", #[[3]] & /@KNFDataSet,
YString == "T4RO20", #[[4]] & /@KNFDataSet,
YString == "T3RO21", #[[5]] & /@KNFDataSet,
YString == "T2RO22", #[[6]] & /@KNFDataSet,
YString == "T1RO23", #[[7]] & /@KNFDataSet,
YString == "T0RO24", #[[8]] & /@KNFDataSet,
YString == "fT4RO20", #[[4]] / (#[[4]] + #[[5]] + #[[6]] + #[[7]] + #[[8]]) & /@KNFDataSet,
YString == "fT3RO21", #[[5]] / (#[[4]] + #[[5]] + #[[6]] + #[[7]] + #[[8]]) & /@KNFDataSet,
YString == "fT2RO22", #[[6]] / (#[[4]] + #[[5]] + #[[6]] + #[[7]] + #[[8]]) & /@KNFDataSet,
YString == "fT1RO23", #[[7]] / (#[[4]] + #[[5]] + #[[6]] + #[[7]] + #[[8]]) & /@KNFDataSet,
YString == "fT0RO24", #[[8]] / (#[[4]] + #[[5]] + #[[6]] + #[[7]] + #[[8]]) & /@KNFDataSet
];
XLIST=Which[
XString=="PO2",#[[1]]&/@KNFDataSet,
XString=="Y",#[[2]]&/@KNFDataSet,
XString=="O2",#[[3]]&/@KNFDataSet,
XString=="T4RO20",#[[4]]&/@KNFDataSet,
XString=="T3RO21",#[[5]]&/@KNFDataSet,
XString=="T2RO22",#[[6]]&/@KNFDataSet,
XString=="T1RO23",#[[7]]&/@KNFDataSet,
XString=="T0RO24",#[[8]]&/@KNFDataSet,
XString=="fT4RO20",#[[4]]/(#[[4]]+#[[5]]+#[[6]]+#[[7]]+#[[8]])&/@KNFDataSet,
XString=="fT3RO21",#[[5]]/(#[[4]]+#[[5]]+#[[6]]+#[[7]]+#[[8]])&/@KNFDataSet,
XString=="fT2RO22",#[[6]]/(#[[4]]+#[[5]]+#[[6]]+#[[7]]+#[[8]])&/@KNFDataSet,
XString=="fT1RO23",#[[7]]/(#[[4]]+#[[5]]+#[[6]]+#[[7]]+#[[8]])&/@KNFDataSet,
XString=="fT0RO24",#[[8]]/(#[[4]]+#[[5]]+#[[6]]+#[[7]]+#[[8]])&/@KNFDataSet
];
XYLIST = Transpose[
{XLIST,YLIST}
];
OUTPUT = ListPlot[
XYLIST,
PlotRange -> All,
PlotStyle -> ColourArgument
]
]

"Function KNFPlot

Usage:  

KNFPlot[\[IndentingNewLine]YString_, XString_, KNFDataSet_, ColourArgument_\[IndentingNewLine]]

in which each of 
	YString and
	XString
may take on any of the following values:
	PO2, Y, O2
	T4RO20, T3RO21, T2RO22, T1RO23, T0RO24
	fT4RO20, fT3RO21, fT2RO22, fT1RO23, fT0RO24

	KNFDataSet\t\tis the name of a dataset generated by
\t\t\t\t\t  KNFGenerateDataSet
	ColourArgument\tis the desired colour for the plot."

