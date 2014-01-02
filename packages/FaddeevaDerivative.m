(* ::Package:: *)

(* ::Title:: *)
(*Faddeeva derivative*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica code to compute arbitrary order derivatives  of the Faddeeva function.*)
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
(*21/04/2012*)
(*1.0*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.0: Moved function from Extras package.*)


(* ::Section:: *)
(*Public*)


BeginPackage["FaddeevaDerivative`"]; 


FaddeevaDerivative;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


FaddeevaDerivative::usage="FaddeevaDerivative[k, z] computes \!\(\*SuperscriptBox[\(w\), \((k)\)]\)(z), the \!\(\*SuperscriptBox[\(k\), \(th\)]\) order derivative of the Faddeeva function, w(z).";
FaddeevaDerivative[0, z_] := Exp[-z^2] Erfc[-I z];
FaddeevaDerivative[1, z_] := -2 z FaddeevaDerivative[0, z] + (2 I)/Sqrt[\[Pi]];
FaddeevaDerivative[k_?IntegerQ, z_] := FaddeevaDerivative[k, z] = -2 z FaddeevaDerivative[k - 1, z] - 2 (k - 1) FaddeevaDerivative[k - 2, z] // Simplify;


End[];


EndPackage[];
