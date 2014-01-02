(* ::Package:: *)

(* ::Title:: *)
(*Repeated integral of the complementary error function*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica code to compute the repeated integral of the complementary error function.*)
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
(*27/04/2013*)
(*1.0*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.0: First working version.*)


(* ::Section:: *)
(*Public*)


BeginPackage["IErfc`"]; 


IErfc;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


IErfc::usage="IErfc[n, z] computes the \!\(\*SuperscriptBox[\(n\), \(th\)]\) repeated integral of the complementary error function.";
IErfc[-1, z_] := (2 / Sqrt[\[Pi]]) Exp[-z^2];
IErfc[0, z_] := Erfc[z];
IErfc[n_?IntegerQ, z_] := IErfc[n, z] = - (z / n) IErfc[n - 1, z] + (1 / (2 n)) IErfc[n - 2, z]


End[];


EndPackage[];
