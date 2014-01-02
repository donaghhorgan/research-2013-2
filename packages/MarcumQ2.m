(* ::Package:: *)

(* ::Title:: *)
(*Marcum Q function*)


(* ::Text:: *)
(*Mathematica implementation of the series form of the Marcum Q function. This method is faster than the built in one for large parameters.*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Copyright (C) 2012 Donagh Horgan.*)
(*Email: donaghh@rennes.ucc.ie.*)
(**)
(*This program is free software : you can redistribute it and/or modify*)
(*it under the terms of the GNU General Public License as published by*)
(*the Free Software Foundation, either version 3 of the License, or*)
(*(at your option) any later version.*)
(**)
(*This program is distributed in the hope that it will be useful,*)
(*but WITHOUT ANY WARRANTY; without even the implied warranty of*)
(*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See *)
(*COPYING for more details.*)
(**)
(*You should have received a copy of the GNU General Public License*)
(*along with this program. If not, see http://www.gnu.org/licenses.*)


(* ::Subsection:: *)
(*Version information*)


(* ::Text:: *)
(*04/07/2013*)
(*1.02*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.02: Minor bug fixes.*)
(*Version 1.01: Added Tolerance option.*)
(*Version 1.0: Basic implementation of the series form of the Marcum Q function.*)


(* ::Section::Closed:: *)
(*Public*)


BeginPackage["MarcumQ2`"];


(* ::Subsection::Closed:: *)
(*MarcumQ2*)


MarcumQ2;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*MarcumQ2*)


Options[MarcumQ2] = {Tolerance -> 10^(-10)};
MarcumQ2::usage="MarcumQ2[m, a, z] calculates the value of the Marcum Q function, \!\(\*SubscriptBox[\(Q\), \(m\)]\)(a, b).";
MarcumQ2[m_?NumericQ, a_?NumericQ, b_?NumericQ, OptionsPattern[]] := Module[{f, n, tol = OptionValue[Tolerance]},
	f[n0_] := 1 - GammaRegularized[n0, (a^2) / 2];
	n = Round[(a^2) / 2];
	If[
		f[n] > tol,
			While[f[n] > tol, n++],
			While[f[n] < tol, n--]
	];
	Exp[-((a^2) / 2)] GammaRegularized[m, (b^2) / 2] + Total[Table[(((((a^2) / 2)^k) Exp[-((a^2) / 2)]) / k!) GammaRegularized[k + m, (b^2) / 2], {k, 1, n}]]
]
MarcumQ2[m_?NumericQ, a_?NumericQ, \[Infinity], OptionsPattern[]] := 0
MarcumQ2[m_?NumericQ, a_?NumericQ, 0, b_?NumericQ, OptionsPattern[]] := 1 - MarcumQ2[m, a, b]
MarcumQ2[m_?NumericQ, a_?NumericQ, 0, \[Infinity], OptionsPattern[]] := 1
MarcumQ2[m_?NumericQ, a0_?NumericQ, a1_?NumericQ, b_?NumericQ, OptionsPattern[]] := Undefined


End[];


EndPackage[];
