(* ::Package:: *)

(* ::Title:: *)
(*Help functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica code to automatically generate help for functions.*)
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
(*Version 1.0: Moved functions from Network package (now EnergyDetectors package).*)


(* ::Section:: *)
(*Public*)


BeginPackage["Help`"]; 


DefaultHelp;


DiversityTypeHelp;


ChannelTypeHelp;


MethodHelp;


ToleranceHelp;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


Needs["AWGN`"];
Needs["DBLogging`"];
Needs["EnergyDetectors`"];
Needs["ErfApprox`"];
Needs["Extras`"];
Needs["FaddeevaDerivative`"];
Needs["IErfc`"];
Needs["InverseMarcumQ`"];
Needs["Nakagami`"];
Needs["QFunction`"];
Needs["Rayleigh`"];
Needs["Rice`"];
Needs["SignalGenerators`"];
Needs["SQLite`"];


DefaultHelp[fName_,optionSpec_] := Module[{help, n, options, defaultOption},
	If[ListQ[optionSpec], options = optionSpec, options = {optionSpec}];
	help = "By default, ";
	For[n = 1, n <= Length[options], n++,
		Which[
			1 < n < Length[options],
				help = help <> ", ",
			n == Length[options] && n != 1,
				help = help <> " and "
		];
		defaultOption = options[[n]]/.Options[fName];
		If[StringQ[defaultOption],
			defaultOption = "\"" <> ToString[defaultOption] <> "\"",
			If[NumericQ[defaultOption] && !IntegerQ[defaultOption],
				defaultOption = ToString[defaultOption, InputForm],
				defaultOption = ToString[defaultOption]
			]
		];
		help = help <> ToString[options[[n]]] <> "\[Rule]" <> defaultOption;
	];
	help = help <> "."
]


DiversityTypeHelp[fName_] := "The following diversity reception schemes may be specified:

DiversityType\[Rule]\"None\"
DiversityType\[Rule]{\"MRC\", n}
DiversityType\[Rule]{\"EGC\", n}
DiversityType\[Rule]{\"SLC\", n}

where n is the number of diversity branches. " <> DefaultHelp[fName, DiversityType];


ChannelTypeHelp[fName_] := "The following channel types may be specified:

ChannelType\[Rule]\"AWGN\"
ChannelType\[Rule]\"Rayleigh\"
ChannelType\[Rule]{\"Nakagami\", m}
ChannelType\[Rule]{\"Rice\", K}

" <> DefaultHelp[fName, ChannelType];


MethodHelp[fName_, methods_] := Module[{help, n, m},
	help = "The following methods may be specified:\n\n";

	For[n = 1, n <= Length[methods], n++,
		help = help <> "Method\[Rule]" <> ToString[methods[[n]]] <> "\n"
	];

	help = help <> "\n" <> Evaluate[DefaultHelp[fName, Method]]
];


ToleranceHelp[fName_] := "The calculation tolerance may be specified using the Tolerance option. " <> DefaultHelp[fName, Tolerance];


TimingHelp[fName_] := "Function timing options may be specified using the Timed option. " <> DefaultHelp[fName, {Timed, MaxIterations, MaxTime}] <> " If Timed\[Rule]True, then a {Pd, time} list of values will be returned. The Timed option is incompatible with database lookup/caching."


DatabaseHelp[fName_] := "Database lookup/caching may be enabled with the DatabaseLookup and DatabaseCaching options. For database caching, both the DatabaseLookup and DatabaseCaching options must be set to True. " <> DefaultHelp[fName, {DatabaseLookup, DatabaseCaching}];


DecisionBitsHelp[fName_] := "Test statistic compression may be specified using the DecisionBits option. " <> DefaultHelp[fName, DecisionBits];


CorrelationHelp[fName_] := "Additionally, the average correlation between nodes may be specified with the CorrelationCoefficient option. " <> DefaultHelp[fName, CorrelationCoefficient];


End[];


EndPackage[];
