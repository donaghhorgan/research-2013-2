(* ::Package:: *)

(* ::Title:: *)
(*Rayleigh channel functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions for cooperative energy detection in Rayleigh channels.*)
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
(*21/04/2013*)
(*1.2*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.2: Complete rewrite, but should be relatively invisible.*)
(*Version 1.12: Updated help functions and cleaned up code.*)
(*Version 1.11: Moved database logging functions to the Network package.*)
(*Version 1.1: Introduced RelevantOptions function and changed function definitions, so that child options are inherited from parents.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


BeginPackage["Rayleigh`"];


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


SignalToNoiseRatioPDFRayleigh;


(* ::Subsection::Closed:: *)
(*Detection probability*)


ProbabilityOfDetectionRayleigh;


(* ::Subsection::Closed:: *)
(*Sample complexity*)


SampleComplexityRayleigh;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


<<Help`;
<<Nakagami`;


(* ::Subsection::Closed:: *)
(*PDF of the signal to noise ratio*)


Options[SignalToNoiseRatioPDFRayleigh] = Options[SignalToNoiseRatioPDFNakagami];
SignalToNoiseRatioPDFRayleigh::usage = StringReplace[SignalToNoiseRatioPDFNakagami::usage, {"Nakagami-m" -> "Rayleigh", "Nakagami" -> "Rayleigh", ", m" -> ""}];
SignalToNoiseRatioPDFRayleigh[\[Gamma]_,x_,OptionsPattern[]] := SignalToNoiseRatioPDFNakagami[\[Gamma], 1, x, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]]


(* ::Subsection::Closed:: *)
(*Detection probability*)


Options[ProbabilityOfDetectionRayleigh] = Options[ProbabilityOfDetectionNakagami];
ProbabilityOfDetectionRayleigh::usage = StringReplace[ProbabilityOfDetectionNakagami::usage, {"Nakagami-m" -> "Rayleigh", "Nakagami" -> "Rayleigh", ", m" -> ""}];
ProbabilityOfDetectionRayleigh[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]] := ProbabilityOfDetectionNakagami[M, \[Gamma], \[Lambda], 1, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]]


(* ::Subsection::Closed:: *)
(*Sample complexity*)


Options[SampleComplexityRayleigh] = Options[SampleComplexityNakagami];
SampleComplexityRayleigh::usage = StringReplace[SampleComplexityNakagami::usage, {"Nakagami-m" -> "Rayleigh", "Nakagami" -> "Rayleigh", ", m" -> ""}];
SampleComplexityRayleigh[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,OptionsPattern[]] := SampleComplexityNakagami[\[Gamma], Pf, Pd, 1, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]]


End[];


EndPackage[];
