(* ::Package:: *)

(* ::Text:: *)
(*This notebook computes solutions for all species defined in the MWC model of allostery*)
(**)
(*J Mol Biol. 1965 May;12:88-118.*)
(**)
(*in terms of user-specified allosteric and dissociation constants for the T and R states, using user-specified oxygen partial pressure and total hemoglobin concentrations.*)
(**)
(*Copyright (C), 2012,*)
(*David M. Morgan, Ph.D.*)
(*dmorgan@stfx.ca*)
(*St. Francis Xavier University, Angitonish, NS, Canada*)
(*Under the terms of the G.N.U. General Public License Version 3:00 or later.*)
(*http://www.gnu.org/licenses/gpl.html*)
(**)
(*In what follows oxygen concentration is specified as a partial pressure in torr. Dissolved oxygen is directly computed as consequence of Henry's law. I am using the value of Henry's constant for oxygen, kH(O2) = 1.3 X 10^-3 mol / (L atm) as found on p. 446, Atkins and Jones, Chemistry, 3rd Edition, Freeman, 1997. *)


MWCSpeciesvH[
{ll_, kdt_, kdr_, PO2_, totalHb_}
] := Module[
{
FF,
SYSTEM,
FUNCS,
SOLUTION,
OUTPUT,
T4O20, T4O21, T4O22, T4O23, T4O24, R4O20, R4O21, R4O22, R4O23, R4O24,
RRState,
TTState,
OCCUPYSITES,
TOTALSITES,
YY,
TOTALO2,
iterator,
kHO2
},
kHO2 = 0.0013 / 760 (* M / torr *); 
SYSTEM = {
T4O20 == ll R4O20,
T4O21 == T4O20 4 FF / kdt,
T4O22 == T4O21 (3/2) FF / kdt,
T4O23 == T4O22 (2/3) FF / kdt,
T4O24 == T4O23 (1/4) FF / kdt,
R4O21==R4O20 4 FF/kdr,
R4O22==R4O21 (3/2) FF/kdr,
R4O23==R4O22 (2/3) FF/kdr,
R4O24==R4O23 (1/4) FF/kdr,
totalHb == T4O20 + T4O21 + T4O22 + T4O23 + T4O24 + R4O20 + R4O21 + R4O22 + R4O23 + R4O24
} /.FF -> kHO2 PO2;
FUNCS= {
T4O20, T4O21, T4O22, T4O23, T4O24, R4O20, R4O21, R4O22, R4O23, R4O24
};
SOLUTION = Solve[
SYSTEM,
FUNCS
];
TTState = Sum[
FUNCS[[iterator]], {iterator, 1, 5}
]/. SOLUTION[[1]];
RRState = Sum[
FUNCS[[iterator]], {iterator, 6, 10}
]/. SOLUTION[[1]];
OCCUPYSITES = T4O21 + 2 T4O22 + 3 T4O23 + 4 T4O24 + R4O21 + 2 R4O22 + 3 R4O23 + 4 R4O24/. SOLUTION[[1]];
TOTALSITES = 4 totalHb;
YY = OCCUPYSITES / TOTALSITES;
TOTALO2 = kHO2 PO2+ OCCUPYSITES;
OUTPUT = {
PO2, kHO2 PO2, YY, TTState, RRState, T4O20, T4O21, T4O22, T4O23, T4O24, R4O20, R4O21, R4O22, R4O23, R4O24} /. SOLUTION[[1]]
]

"Function MWCSpeciesvH

Usage:

MWCSpeciesvH[\[IndentingNewLine]{ll_, kdt_, kdr_, PO2_, totalHb_}\[IndentingNewLine]] 

in which: 
	ll\t\t allosteric constant
	kdt\t\tdissociation constant for O2 binding to the T state
	kdr\t\tdissociation constant for O2 binding to the R state
	PO2\t\tO2 partial pressure of oxygen above solution (torr)
	totalHb\ttotal concentration of Hb (molar)

The output is a list with elements:

{\[IndentingNewLine]PO2, FF, YY, TTState, RRState, T4O20, T4O21, T4O22, T4O23, T4O24, R4O20, R4O21, R4O22, R4O23, R4O24
}"


(* ::Text:: *)
(*Example.*)


(* ::Input:: *)
(*ll = 9054;*)
(*kdt = .00001/.014;*)
(*kdr = .00001;*)
(*PO2 = 10;*)
(*totalHb = .0001;*)
(**)
(*MWCSpeciesvH[*)
(*{ll, kdt, kdr, PO2, totalHb}*)
(*] *)
