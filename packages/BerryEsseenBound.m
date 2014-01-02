(* ::Package:: *)

(* ::Title:: *)
(*Berry-Esseen bound on the convergence of the central limit theorem*)


(* ::Text:: *)
(*A function for calculating the error resulting from the use of the central limit theorem to approximate sum of random variables, using the method developed by Korolev and Shevstova [1].*)
(**)
(*[1] V. Korolev and I. Shevtsova, "An improvement of the Berry\[Dash]Esseen inequality with applications to Poisson and mixed Poisson random sums," Scandinavian Actuarial Journal, no. 2, pp. 81\[Dash]105, 2012.*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Copyright (C) 2013 Donagh Horgan.*)
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
(*05/06/2013*)
(*1.01*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.01: Changed to NExpectation for speed.*)
(*Version 1.0: Basic implementation.*)


(* ::Section:: *)
(*Public*)


BeginPackage["BerryEsseenBound`"];


(* ::Subsection::Closed:: *)
(*BerryEsseenBound*)


BerryEsseenBound;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*BerryEsseenBound*)


BerryEsseenBound::usage="BerryEsseenBound[n, dist] calculates the value of the Berry-Esseen bound on the convergence of the sum of n random variables of the specified distribution to a normal distribution.";
BerryEsseenBound[n_, dist_] := Module[{C1 = 0.33477, C2 = 0.429, \[Beta], x},
	\[Beta] = NExpectation[Abs[(x - Mean[dist]) / StandardDeviation[dist]]^3, x \[Distributed] dist];

	C1 (\[Beta] + C2) / Sqrt[n]
]


End[];


EndPackage[];
