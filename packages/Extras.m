(* ::Package:: *)

(* ::Title:: *)
(*Extra functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions to support other packages.*)
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
(*1.07*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.07: Changed ProcessMethod function and moved FaddeevaDerivative to its own package.*)
(*Version 1,06: Removed ProcessSNR (obsolete) and updated ProcessDiversityType.*)
(*Version 1.05: Added ProcessChannelType function.*)
(*Version 1.04: Added ProcessMethod function.*)
(*Version 1.03: Fixed a bug in ProcessSNR.*)
(*Version 1.02: Moved FaddeevaDerivative function from the Nakagami package.*)
(*Version 1.01: Added SEC support, and removed SSC support.*)
(*Version 1.0: First working version.*)


(* ::Section:: *)
(*Public*)


BeginPackage["Extras`"]; 


ProcessMethod;


ProcessChannelType;


ProcessDiversityType;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


ProcessMethod::usage="ProcessMethod[method] processes the specified method and returns a shorthand result.";
ProcessMethod[method_]:=Which[
	TrueQ[StringLength[method] >= 5] && StringTake[method, 5] == "Exact",
		"Exact",
	TrueQ[StringLength[method] >= 11] && StringTake[method, 11] == "Approximate" && StringFreeQ[method, "LowSNR"],
		"Approximate",
	TrueQ[StringLength[method] >= 11] && StringTake[method, 11] == "Approximate" && !StringFreeQ[method, "LowSNR"],
		"ApproximateLowSNR",
	True,
		method
]


ProcessChannelType::usage="ProcessChannelType[channelType] processes the specified channel type into a standard format."
ProcessChannelType[channelType_]:=If[ListQ[channelType], {channelType[[1]], channelType[[2]]}, {channelType, Null}, Undefined]


ProcessDiversityType::usage="ProcessDiversityType[diversityType] processes the specified diversity type into a standard format.";
ProcessDiversityType[diversityType_]:=If[ListQ[diversityType], {StringReplace[diversityType[[1]], "None"->"ND"], diversityType[[2]]}, If[diversityType=="None"||diversityType=="ND", {"ND", 1}, {Undefined, Undefined}], Undefined]


End[];


EndPackage[];
