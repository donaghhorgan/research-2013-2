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


(* ::Subsection::Closed:: *)
(*Version information*)


(* ::Text:: *)
(*08/06/2013*)
(*1.01*)


(* ::Subsection::Closed:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.01: Custom version for this project.*)
(*Version 1.0: First working version.*)


(* ::Subsection:: *)
(*Load packages*)


(* ::Subsubsection:: *)
(*Non-local packages*)


packagesDirectory = Evaluate[ToFileName["./packages/"]];


AppendTo[$Path, packagesDirectory];
DistributeDefinitions[packagesDirectory];
ParallelEvaluate[AppendTo[$Path, packagesDirectory]];


Get/@DeleteCases[FileNames["*.m", {packagesDirectory}], ToFileName[packagesDirectory, "definitions.m"]];
ParallelEvaluate[Get/@DeleteCases[FileNames["*.m", {packagesDirectory}], ToFileName[packagesDirectory, "definitions.m"]]];


(* ::Subsubsection:: *)
(*Local packages*)


workingDirectory = NotebookDirectory[];
AppendTo[$Path, workingDirectory];
DistributeDefinitions[workingDirectory];
ParallelEvaluate[AppendTo[$Path, workingDirectory]];


Get["plots.m"];
ParallelEvaluate[Get["plots.m"]];


(* ::Subsection:: *)
(*EPS export directory*)


exportDirectory = ToFileName["./"];
