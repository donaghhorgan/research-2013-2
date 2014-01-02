(* ::Package:: *)

(* ::Title:: *)
(*Error function approximations*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica approximations for the error function and complementary error function.*)
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


(* ::Subsection::Closed:: *)
(*Version information*)


(* ::Text:: *)
(*21/04/2013*)
(*1.0*)


(* ::Subsection::Closed:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.01: Restored LopezBenitezParameters function (it's needed in the Nakagami package).*)
(*Version 1.1: Merged numerical and symbolic function definitions - no need for both. Small bug fixes, removed LopezBenitezParameters public definition. Updated bibliographical note.*)
(*Version 1.0: First working version.*)


(* ::Subsection::Closed:: *)
(*Note*)


(* ::Text:: *)
(*This package implements some approximations for the error function and its complement as defined in [1].*)
(**)
(*[1] M. Lo\:0301pez-Beni\:0301tez and F. Casadevall, "Versatile, Accurate, and Analytically Tractable Approximation for the Gaussian Q-Function," Communications, IEEE Transactions on, vol. 59, no. 4, pp. 917 \[Dash]922, Apr. 2011.*)
(**)


(* ::Section:: *)
(*Public*)


BeginPackage["ErfApprox`"];


(* ::Subsection::Closed:: *)
(*Erfc*)


ErfcApprox;


(* ::Subsection::Closed:: *)
(*Erf*)


ErfApprox;


(* ::Subsection::Closed:: *)
(*Diagnostics*)


ErfApproxError;


ErfcApproxError;


(* ::Subsection::Closed:: *)
(*Lopez-Benitez parameters*)


LopezBenitezParameters;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


<<Help`;


(* ::Subsection::Closed:: *)
(*Erfc*)


Options[ErfcApprox] = {Method -> "Lopez-Benitez"};
ErfcApprox::usage = "ErfcApprox[x] calculates the approximate value of the complementary error function at x.\n\n"<>MethodHelp[ErfcApprox, {"\"Chiani\"", "\"Lopez-Benitez\"", "\"Loskot\"", "\"Karagiannidis\""}];
ErfcApprox[x_,OptionsPattern[]] := Module[{method = OptionValue[Method], a, b, c, d, e, f, parameters},
	If[x < 0,
		2 - ErfcApprox[-x, Method -> method],
		Which[
			method == "Chiani",
				(a Exp[-x^2] + b Exp[-c x^2])/.{a -> 1 / 6, b -> 1 / 2, c -> 4 / 3},
			method == "Lopez-Benitez",
				(2 Exp[2 a (x^2) + Sqrt[2] b x + c])/.Table[ToExpression[(ToString/@{a, b, c})[[i]] <> " \[Rule] " <> ToString[LopezBenitezParameters[x][[i]]]], {i, 3}],
			method == "Loskot",
				(2 (a * Exp[-2b x^2] + c * Exp[-2 d x^2] + e Exp[-2 f x^2]))/.{a -> 0.168, b -> 0.876, c -> 0.144, d -> 0.525, e -> 0.002, f -> 0.603},
			method == "Karagiannidis",
				((1 - Exp[-a x]) Exp[-x^2] / (b Sqrt[Pi] x))/.{a -> 1.98, b -> 1.135},
			True,
				Return[Undefined]
		]
	]
]


(* ::Subsection::Closed:: *)
(*Erf*)


Options[ErfApprox] = Options[ErfcApprox];
ErfApprox::usage = StringReplace[ErfcApprox::usage, {"ErfcApprox" -> "ErfApprox", "complementary error function" -> "error function"}];
ErfApprox[x_,OptionsPattern[]] := 1 - ErfcApprox[x, Method -> OptionValue[Method]]


(* ::Subsection::Closed:: *)
(*Diagnostics*)


Options[ErfcApproxError] = Options[ErfcApprox];
ErfcApproxError::usage = "ErfcApproxError[x] calculates the relative error between the complementary error function and its approximation.\n\n"<>MethodHelp[ErfcApprox, {"Method\[Rule]\"Chiani\"", "Method\[Rule]\"Lopez-Benitez\"", "Method\[Rule]\"Loskot\"", "Method\[Rule]\"Karagiannidis\"}]"}];
ErfcApproxError[x_,OptionsPattern[]] := (Erfc[x] - ErfcApprox[x, Method -> OptionValue[Method]]) / Erfc[x];


Options[ErfApproxError] = Options[ErfApprox];
ErfApproxError::usage = StringReplace[ErfcApproxError::usage, {"ErfcApproxError" -> "ErfApproxError", "complementary error function" -> "error function"}];
ErfApproxError[x_,OptionsPattern[]] := (Erf[x] - ErfApprox[x, Method -> OptionValue[Method]]) / Erf[x];


(* ::Subsection::Closed:: *)
(*Lopez-Benitez parameters*)


LopezBenitezParameters::usage = "LopezBenitezParameters[x] generates the min-MARE parameters for use in Lopez-Benitez's approximation for the error function.";
LopezBenitezParameters[x_]:=If[x < 0,
	LopezBenitezParameters[-x],
	Which[
		0 <= x < 2,
			{-0.3976, -0.7418, -0.7019},
		2 <= x < 4,
			{-0.4369, -0.6511, -0.7358},
		4 <= x < 6,
			{-0.4577, -0.5695, -0.7864},
		6 <= x < 8,
			{-0.4698, -0.5026, -0.8444},
		8 <= x < 10,
			{-0.4774, -0.4484, -0.9049},
		10 <= x,
			{-0.4920, -0.2887, -1.1893}
	]
]


End[];


EndPackage[];
