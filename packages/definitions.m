(* ::Package:: *)

(* ::Title:: *)
(*Definitions*)


(* ::Text:: *)
(*Generic initialisation file for loading Mathematica function definitions.*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Copyright (C) 2012-2013 Donagh Horgan.*)
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
(*29/04/2013*)
(*1.02*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.02: Now loads all packages, in parallel.*)
(*Version 1.01: Grouped similar functionality into individual packages.*)
(*Version 1.0: First working version, bug fixes to follow.*)


(* ::Subsection:: *)
(*Load packages*)


packagesDirectory = Directory[];
DistributeDefinitions[packagesDirectory];


Get/@DeleteCases[FileNames["*.m", {packagesDirectory}], ToFileName[packagesDirectory, "definitions.m"]];
ParallelEvaluate[Get/@DeleteCases[FileNames["*.m", {packagesDirectory}], ToFileName[packagesDirectory, "definitions.m"]]];
