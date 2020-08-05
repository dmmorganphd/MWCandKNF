(* ::Package:: *)

(* ::Text:: *)
(*This notebook and the associated Mathematica package generate an MWC data set of numPoints length using user-specified allosteric and dissociation constants for the T and R states, between user-specified minimum and maximum partial pressures of oxygen. It relies on MWCSpeciesvH.m.*)


MWCGenerateDataSetvH[
{ll_, kdt_, kdr_, minPO2_ ,maxPO2_, totalHb_, numPoints_}
] := Module[
{
iterator, SOLUTIONSET
},
SOLUTIONSET = Table[
MWCSpeciesvH[
{ll, kdt, kdr, iterator, totalHb}
],
{iterator, minPO2, maxPO2, (maxPO2 - minPO2)/(-1 + numPoints)}
];
SOLUTIONSET
]

"Function MWCGenerateDatasetvH

Usage:

MWCGenerateDatasetvH[\[IndentingNewLine]{ll_, kdt_, kdr_, minPO2_ ,maxPO2_, totalHb_, numPoints_}\[IndentingNewLine]]

in which:

	ll\t\t allosteric constant
	kdt\t\tdissociation constant for the T state
	kdr\t\tdissociation constant for the R state
	minPO2\t minimum partial pressure of oxygen (torr)
	maxPO2\t maximum partial pressure of oxygen (torr)
	numPoints  number of points for which to solve

Returns:

A list of length numPoints, each element of which is a list having 
the composition:

{\[IndentingNewLine]PO2, FF, YY, TTState, RRState, T4O20, T4O21, T4O22, T4O23, T4O24, R4O20, R4O21, R4O22, R4O23, R4O24
}

in which:

	PO2\t\tpartial pressure of oxygen above solution (torr)
	FF\t\t free oxygen in solution (molar)
	Y\t\t  fraction of available sites which are occupied by O2
	TState\t fraction of all Hb molecules in the T state
	RState\t fraction of all Hb molecules in the R state
	T4O2x\t  absolute concentration of Hb molecules in the T state
	\t\t   with x O2 bound
	R4O2x\t  absolute concentration of Hb molecules in the R state
	\t\t   with x O2 bound"
