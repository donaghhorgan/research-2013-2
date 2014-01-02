(* ::Package:: *)

(* ::Title:: *)
(*Rice channel functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions for cooperative energy detection in Rice channels.*)
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
(*1.43*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.43: New functions for calculating approximation errors and more efficient calculation of truncation points.*)
(*Version 1.42: Updated ApproximationErrorRiceLowSNR and EGCPDFApproximationErrorRice. AddedSignalToNoiseRatioDistributionRiceEGC function.*)
(*Version 1.41: Rewrote ApproximateLowSNR method and added test statistic PDF.*)
(*Version 1.40: Complete rewrite, but should be relatively invisible. Removed SC / SEC / SLS diversity types (incomplete and unused), and other small updates and clean up.*)
(*Version 1.33: New code for AnnamalaiLimit to speed up calculations.*)
(*Version 1.32: Amalgamated algorithms and methods, removed LowSNR option completely.*)
(*Version 1.31: Created NumericalLowSNR method instead of using LowSNR option.*)
(*Version 1.30: Major updates to SampleComplexityRice, added diversity support, exact and approximate methods.*)
(*Version 1.25: Minor bug fixes for limit functions.*)
(*Version 1.24: Added error bound functions and renamed SmallK method to IntegerKN.*)
(*Version 1.23: Added SEC and limited SC (numerical methods only) support.*)
(*Version 1.22: Finished MRC and SLC implementations.*)
(*Version 1.21: Finished SLS implementations.*)
(*Version 1.2: Moved timing functions to ProbabilityOfDetectionRice and added TruncationPointRice function for public access to truncation points. More minor bug fixes.*)
(*Version 1.1: Major clean up of code, added approximations and numerical methods for no diversity Rice channels.*)
(*Version 1.02: Moved database logging functions to the Network package.*)
(*Version 1.01: Added sample complexity function.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


BeginPackage["Rice`"]; 


(* ::Subsection::Closed:: *)
(*SignalToNoiseRatioPDFRice*)


(* ::Subsubsection::Closed:: *)
(*SignalToNoiseRatioPDFRice*)


SignalToNoiseRatioPDFRice;


(* ::Subsubsection::Closed:: *)
(*HuBeaulieuParameters*)


HuBeaulieuParameters;


(* ::Subsection::Closed:: *)
(*TestStatisticPDFRice*)


TestStatisticPDFRice;


(* ::Subsection::Closed:: *)
(*ProbabilityOfDetectionRice*)


ProbabilityOfDetectionRice;


TruncationPointRice;


(* ::Subsection::Closed:: *)
(*ApproximationErrorRice*)


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorRice*)


ApproximationErrorRice;


(* ::Subsection::Closed:: *)
(*TruncationErrorRice*)


TruncationErrorRice;


(* ::Subsection::Closed:: *)
(*SampleComplexityRice*)


SampleComplexityRice;


(* ::Subsection::Closed:: *)
(*ReceiverSensitivityRice*)


ReceiverSensitivityRice;


(* ::Subsection::Closed:: *)
(*DiversityGainRice*)


DiversityGainRice;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


<<Help`;
<<EnergyDetectors`;
<<AWGN`;
<<ErfApprox`;
<<QFunction`;
<<InverseMarcumQ`;
<<Extras`;
<<IErfc`;


(* ::Subsection::Closed:: *)
(*Help generation*)


GenerateMethodHelp[fName_, methodName_] := ToString[fName] <> "[M, \[Gamma], \[Lambda], K] calculates the probability of detection for a single energy detector operating in a Rice fading channel using the " <> methodName <> " method.
" <> ToString[fName] <> "[M, \[Gamma], \[Lambda], K, n] calculates the probability of detection for energy detection with diversity reception in a Rice fading channel using the " <> methodName <> " method."<>"\n\n"<>DiversityTypeHelp[fName];


GenerateTruncationHelp[fName_,methodName_] := ToString[fName] <> "[M, \[Gamma], \[Lambda], K] calculates the truncation point for use in the " <> methodName <> " method for a single energy detector operating on a Rice channel.
" <> ToString[fName] <> "[M, \[Gamma], \[Lambda], K, n] calculates the truncation point for use in the " <> methodName <> " method for energy detection with diversity reception in a Rice channel."<>"\n\n"<>DiversityTypeHelp[fName]<>"\n\n"<>ToleranceHelp[fName];


(* ::Subsection::Closed:: *)
(*SignalToNoiseRatioPDFRice*)


(* ::Subsubsection::Closed:: *)
(*SignalToNoiseRatioPDFRice*)


Options[SignalToNoiseRatioPDFRice] = {Method -> "Exact", DiversityType -> OptionValue[ProbabilityOfDetection, DiversityType]};
SignalToNoiseRatioPDFRice::usage = "SignalToNoiseRatioPDFRice[\[Gamma], K, x] calculates the probability density function of the instantaneous signal to noise ratio for an energy detector operating in a Rice fading channel.\n\n"<>DiversityTypeHelp[SignalToNoiseRatioPDFRice]<>"\n\n"<>MethodHelp[SignalToNoiseRatioPDFRice, {"\"Exact\"", "\"Approximate\""}];
SignalToNoiseRatioPDFRice[\[Gamma]_, K_, x_, OptionsPattern[]] := Module[{diversityType, n, method = OptionValue[Method]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[
		method == "Exact" && diversityType == "EGC",
			PDF[SignalToNoiseRatioDistributionRiceEGC[\[Gamma], K, n], x],
			PDF[SignalToNoiseRatioDistributionRice[\[Gamma], K, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]], x]
	]
]


(* ::Subsubsection::Closed:: *)
(*SignalToNoiseRatioDistributionRice*)


Options[SignalToNoiseRatioDistributionRice] = {Method -> OptionValue[SignalToNoiseRatioPDFRice, Method], DiversityType -> OptionValue[SignalToNoiseRatioPDFRice, DiversityType]};
SignalToNoiseRatioDistributionRice::usage = "SignalToNoiseRatioDistributionRice[\[Gamma], K] returns the distribution of the signal to noise ratio for specified diversity type.\n\n"<>DiversityTypeHelp[SignalToNoiseRatioDistributionRice];
SignalToNoiseRatioDistributionRice[\[Gamma]_, K_, OptionsPattern[]] := Module[{r, diversityType, n, method = OptionValue[Method], a, b, x},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		method == "Exact" || method == "ExactNumerical",
			Which[
				diversityType == "ND",
					TransformedDistribution[x / ((2 (K + 1)) / \[Gamma]), x \[Distributed] NoncentralChiSquareDistribution[2, 2 K]],
				diversityType == "MRC" || diversityType == "SLC",
					TransformedDistribution[x / ((2 (K + 1)) / \[Gamma]), x \[Distributed] NoncentralChiSquareDistribution[2 n, 2 K n]],
				diversityType == "EGC",
					r = Table[Unique[], {n}];
					TransformedDistribution[x / ((2 (K + 1)) / \[Gamma]), x \[Distributed] TransformedDistribution[Sum[Sqrt[r[[i]]], {i, n}]^2 / n, Table[r[[i]] \[Distributed] NoncentralChiSquareDistribution[2, 2 K], {i, n}]]],
				True,
					Undefined
			],
		method == "Approximate" || method == "ApproximateNumerical" || method == "ApproximateNumericalLowSNR",
			Which[
				diversityType == "ND",
					TransformedDistribution[x / ((2 (K + 1)) / \[Gamma]), x \[Distributed] NoncentralChiSquareDistribution[2, 2 K]],
				diversityType == "MRC" || diversityType == "SLC",
					TransformedDistribution[x / ((2 (K + 1)) / \[Gamma]), x \[Distributed] NoncentralChiSquareDistribution[2 n, 2 K n]],
				diversityType == "EGC",
					{a, b} = HuBeaulieuParameters[K, n];
					TransformedDistribution[x / ((2 (K + 1)) / (b \[Gamma])), x \[Distributed] NoncentralChiSquareDistribution[2 n, 2 K n / a]],
				True,
					Undefined
			],
		method == "ApproximateAsymptotic",
			Which[
				diversityType == "ND",
					NormalDistribution[\[Gamma], Sqrt[2K + 1] (\[Gamma] / (K + 1))],
				diversityType == "MRC" || diversityType == "SLC",
					NormalDistribution[n \[Gamma], Sqrt[(1 + 2K) n] (\[Gamma] / (K + 1))],
				diversityType == "EGC",
					Undefined,
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*SignalToNoiseRatioDistributionRiceEGC*)


Options[SignalToNoiseRatioDistributionRiceEGC] = {NumberOfPoints -> 10^6}
SignalToNoiseRatioDistributionRiceEGC[\[Gamma]_, K_, n_, OptionsPattern[]] := SignalToNoiseRatioDistributionRiceEGC[\[Gamma], K, n, NumberOfPoints -> OptionValue[NumberOfPoints]] = Module[{\[ScriptCapitalD], numberOfPoints = OptionValue[NumberOfPoints]},
	\[ScriptCapitalD] = SignalToNoiseRatioDistributionRice[\[Gamma], K, DiversityType -> {"EGC", n}, Method -> "Exact"];
	SmoothKernelDistribution[RandomVariate[\[ScriptCapitalD], numberOfPoints]]
]


(* ::Subsubsection::Closed:: *)
(*HuBeaulieuParameters*)


HuBeaulieuParameters[K_, n_] := Module[{data, a, b, K0, tol = 10^(-6)},
	data = {{{0.5194, 0.4746}, {0.5411, 0.4770}, {0.5507, 0.4772}, {0.5589, 0.4783}, {0.5634, 0.4786}, {0.5675, 0.4791}, {0.5699, 0.4793}},{{0.4395, 0.4131}, {0.4518, 0.4150}, {0.4580, 0.4157}, {0.4621, 0.4165}, {0.4646, 0.4168}, {0.4668, 0.4172}, {0.4680, 0.4173}},{{0.3657, 0.3509}, {0.3726, 0.3524}, {0.3761, 0.3530}, {0.3782, 0.3534}, {0.3796, 0.3537}, {0.3806, 0.3539}, {0.3815, 0.3541}},{{0.2992, 0.2914}, {0.3027, 0.2922}, {0.3048, 0.2928}, {0.3058, 0.2931}, {0.3066, 0.2933}, {0.3073, 0.2935}, {0.3076, 0.2936}}};
	K0 = 10^({1, 3, 5, 7}/10);
	a = Interpolation[Flatten[Table[{{K0[[x]], y + 1}, 2 (K0[[x]] + 1) data[[x, y]][[1]]^2}, {x, Dimensions[data][[1]]}, {y, Dimensions[data][[2]]}], 1], InterpolationOrder -> 1];
	b = Interpolation[Flatten[Table[{{K0[[x]], y + 1}, 2 (K0[[x]] + 1) data[[x, y]][[2]]^2}, {x, Dimensions[data][[1]]}, {y, Dimensions[data][[2]]}], 1], InterpolationOrder -> 1];
	Quiet[Rationalize[{a[K, n], b[K, n]}, tol]]
]


(* ::Subsection::Closed:: *)
(*TestStatisticPDFRice*)


(* ::Subsubsection::Closed:: *)
(*TestStatisticPDFRice*)


Options[TestStatisticPDFRice] = {Method -> OptionValue[ProbabilityOfDetection, Method], DiversityType -> OptionValue[ProbabilityOfDetection, DiversityType]};
TestStatisticPDFRice::usage="TestStatisticPDFRice[M, \[Gamma], \[Lambda], K] computes the probability density function of the test statistic for an energy detector with the specified diversity type operating on an occupied Rice channel.\n\n"<>DiversityTypeHelp[TestStatisticPDFRice]<>"\n\n"<>MethodHelp[TestStatisticPDFRice, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
TestStatisticPDFRice[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, K_?NumericQ, OptionsPattern[]] := PDF[TestStatisticDistributionRice[M, \[Gamma], K, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]], \[Lambda]]


(* ::Subsubsection::Closed:: *)
(*TestStatisticDistributionRice*)


Options[TestStatisticDistributionRice] = {Method -> OptionValue[ProbabilityOfDetection, Method], DiversityType -> OptionValue[ProbabilityOfDetection, DiversityType], NumberOfPoints -> 10^5};
TestStatisticDistributionRice::usage="TestStatisticDistributionRice[M, \[Gamma], K] computes the distribution of the test statistic for an energy detector with the specified diversity type operating on an occupied Rice channel.\n\n"<>DiversityTypeHelp[TestStatisticDistributionRice]<>"\n\n"<>MethodHelp[TestStatisticDistributionRice, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
TestStatisticDistributionRice[M_?NumericQ, \[Gamma]_?NumericQ, K_?NumericQ, OptionsPattern[]] := TestStatisticDistributionRice[M, \[Gamma], K, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method], NumberOfPoints -> OptionValue[NumberOfPoints]] = Module[{method = ProcessMethod[OptionValue[Method]], x, numberOfPoints = OptionValue[NumberOfPoints], \[ScriptCapitalD]},
	\[ScriptCapitalD] = SignalToNoiseRatioDistributionRice[\[Gamma], K, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]];
	SmoothKernelDistribution[RandomVariate[ParameterMixtureDistribution[Evaluate[TestStatisticDistributionAWGN[M, x, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]]], x \[Distributed] \[ScriptCapitalD]], numberOfPoints], MaxRecursion -> 3]
]


(* ::Subsection::Closed:: *)
(*ProbabilityOfDetectionRice*)


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionRice*)


Options[ProbabilityOfDetectionRice] = {Method -> OptionValue[ProbabilityOfDetection, Method], DiversityType -> OptionValue[ProbabilityOfDetection, DiversityType], Limit -> OptionValue[ProbabilityOfDetection, Limit](*,Timed -> OptionValue[ProbabilityOfDetection,Timed],MaxTime -> OptionValue[ProbabilityOfDetection,MaxTime],MaxIterations -> OptionValue[ProbabilityOfDetection,MaxIterations]*)};
ProbabilityOfDetectionRice::usage="ProbabilityOfDetectionRice[M, \[Gamma], \[Lambda], K] calculates the probability of detection for an energy detector with the specified diversity type operating on a Rice fading channel.\n\n"<>MethodHelp[ProbabilityOfDetectionRice, {"\"ExactNumerical\"", "\"ExactAnnamalai\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateLowSNR\"", "\"ApproximateAsymptotic\""}]<>"\n\n"<>DiversityTypeHelp[ProbabilityOfDetectionRice];
ProbabilityOfDetectionRice[M_, \[Gamma]_, \[Lambda]_, K_, OptionsPattern[]] := Module[{method = OptionValue[Method], limit = OptionValue[Limit] (*f, totaltime = 0, iterations = 0, time, result*)},
	If[SameQ[limit, Undefined],
		limit = TruncationPointRice[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]];
	];

	Which[
		(* Catch extreme values - they can cause errors *)
		\[Lambda] <= 0 || M == \[Infinity] || \[Gamma] == \[Infinity],
			1,
		\[Lambda] == \[Infinity],
			0,
		method == "ApproximateAsymptotic",
			ProbabilityOfDetectionRiceApproximateAsymptotic[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateLowSNR",
			ProbabilityOfDetectionRiceApproximateLowSNR[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Limit -> limit],
		method == "ApproximateNumerical",
			ProbabilityOfDetectionRiceApproximateNumerical[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateNumericalLowSNR",
			ProbabilityOfDetectionRiceApproximateNumericalLowSNR[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateSimple",
			ProbabilityOfDetectionRiceApproximateSimple[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType]],
		method == "ExactAnnamalai",
			ProbabilityOfDetectionRiceExactAnnamalai[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Limit -> limit],
		method == "ExactNumerical",
			ProbabilityOfDetectionRiceExactNumerical[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType]],
		method == "ExactSun",
			ProbabilityOfDetectionRiceExactSun[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Limit -> limit],
		True,
			Undefined
	]
]

(* OLD CODE *)
(*If[OptionValue[Timed],
		(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
		While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
			ClearSystemCache[];
			{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
			totaltime += time;
			iterations++;
		];
		{result,totaltime/iterations},
		f
	]*)


Options[TruncationPointRice] = {Method -> OptionValue[TruncationPoint, Method], DiversityType -> OptionValue[TruncationPoint, DiversityType], Tolerance -> OptionValue[TruncationPoint, Tolerance]};
TruncationPointRice::usage = "TruncationPointRice[M, \[Gamma], \[Lambda], K] calculates the truncation point for use in the specified method for calculating the probability of detection for an energy detector with the specified diversity type operating on a Rice fading channel.\n\n" <> MethodHelp[TruncationPointRice, {"\"ExactNumerical\"", "\"ExactAnnamalai\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateLowSNR\"", "\"ApproximateAsymptotic\""}] <> "\n\n" <> DiversityTypeHelp[TruncationPointRice];
TruncationPointRice[M_, \[Gamma]_, \[Lambda]_, K_, OptionsPattern[]] := Module[{method = OptionValue[Method]},
	Which[
		(* Catch extreme values - they can cause errors *)
		\[Lambda] <= 0 || M == \[Infinity] || \[Gamma] == \[Infinity] || \[Lambda] == \[Infinity],
			Undefined,
		method == "ApproximateLowSNR",
			TruncationPointRiceApproximateLowSNR[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Tolerance -> OptionValue[Tolerance]],
		method == "ExactAnnamalai",
			TruncationPointRiceExactAnnamalai[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Tolerance -> OptionValue[Tolerance]],
		method == "ExactSun",
			TruncationPointRiceExactSun[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Tolerance -> OptionValue[Tolerance]],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionRiceApproximateAsymptotic*)


Options[ProbabilityOfDetectionRiceApproximateAsymptotic] = {DiversityType -> OptionValue[ProbabilityOfDetectionRice, DiversityType]};
ProbabilityOfDetectionRiceApproximateAsymptotic::usage = GenerateMethodHelp[ProbabilityOfDetectionRiceApproximateAsymptotic, "Asymptotic"];
ProbabilityOfDetectionRiceApproximateAsymptotic[M_, \[Gamma]_, \[Lambda]_, K_, OptionsPattern[]] := Module[{diversityType, n, a, b},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND",
			Q[(\[Lambda] - M (1 + \[Gamma])) / Sqrt[2M (1 + M/2 (\[Gamma]/(K + 1))^2 (2K + 1))]],
		diversityType == "MRC",
			Q[(\[Lambda] - M (1 + n \[Gamma])) / Sqrt[2M (1 + (M n)/2 (\[Gamma]/(K + 1))^2 (2K + 1))]],
		diversityType == "EGC",
			{a, b} = HuBeaulieuParameters[K, n];
			Q[(\[Lambda] - M (1 + n b \[Gamma] ((K/a + 1)/(K + 1)))) / Sqrt[2M (1 + (M n)/2 ((b \[Gamma])/(K + 1))^2 ((2K)/a + 1))]],
		diversityType == "SLC",
			Q[(\[Lambda] - M n (1 + \[Gamma])) / Sqrt[2M n (1 + M/2 (\[Gamma]/(K + 1))^2 (2K + 1))]],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionRiceApproximateLowSNR*)


Options[ProbabilityOfDetectionRiceApproximateLowSNR] = {DiversityType -> OptionValue[ProbabilityOfDetectionRice, DiversityType], Limit -> Undefined};
ProbabilityOfDetectionRiceApproximateLowSNR::usage = GenerateMethodHelp[ProbabilityOfDetectionRiceApproximateLowSNR, "LowSNR"];
ProbabilityOfDetectionRiceApproximateLowSNR[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, K_?NumericQ, OptionsPattern[]] := Module[{diversityType, n, G, limit = OptionValue[Limit], a, b},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[SameQ[limit, Undefined], limit = TruncationPointRice[M, \[Gamma], \[Lambda], K, Method -> "ApproximateLowSNR", DiversityType -> OptionValue[DiversityType]]];

	G[k_, a_, b_, c_] := N[(Exp[(c^2) / (2 b^2) - a c / b] / 2) (Sqrt[2]c / b)^(k) IErfc[k, N[(c/b - a) / Sqrt[2], 100]], 100];

	Which[
		diversityType == "ND",
			If[K==0,
				ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + G[0, (\[Lambda] - M) / Sqrt[2 M], Sqrt[M / 2], 1 / \[Gamma]],
				ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[Exp[-K] ((K^l) / l!) Total[Table[G[p, (\[Lambda] - M) / Sqrt[2 M], Sqrt[M / 2], (K + 1) / \[Gamma]], {p, 0, l}]], {l, 0, limit}]]
			],
		diversityType == "MRC",
			If[K==0,
				ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[G[p, (\[Lambda] - M) / Sqrt[2 M], Sqrt[M / 2], 1 / \[Gamma]], {p, 0, n - 1}]],
				ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[Exp[-K n] (((K n)^l) / l!) Total[Table[G[p, (\[Lambda] - M) / Sqrt[2 M], Sqrt[M / 2], (K + 1) / \[Gamma]], {p, 0, n + l - 1}]], {l, 0, limit}]]
			],
		diversityType == "EGC",
			{a, b} = HuBeaulieuParameters[K, n];
			If[K==0,
				ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[G[p, (\[Lambda] - M) / Sqrt[2 M], Sqrt[M / 2], 1 / (b \[Gamma])], {p, 0, n - 1}]],
				ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[((Exp[-K n / a] (((K n / a)^l) / l!))) Total[Table[G[p, (\[Lambda] - M) / Sqrt[2 M], Sqrt[M / 2], (K + 1) / (b \[Gamma])], {p, 0, n + l - 1}]], {l, 0, limit}]]
			],
		diversityType == "SLC",
			If[K==0,
				ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[G[p, (\[Lambda] - M n) / Sqrt[2 M n], Sqrt[M / (2 n)], 1 / \[Gamma]], {p, 0, n - 1}]],
				ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[Exp[-K n] (((K n)^l) / l!) Total[Table[G[p, (\[Lambda] - M n) / Sqrt[2 M n], Sqrt[M / (2 n)], (K + 1) / \[Gamma]], {p, 0, n + l - 1}]], {l, 0, limit}]]
			],
		True,
			Undefined
	]//N
]


Options[TruncationPointRiceApproximateLowSNR] = {DiversityType -> OptionValue[TruncationPointRice, DiversityType], Tolerance -> OptionValue[TruncationPointRice, Tolerance]};
TruncationPointRiceApproximateLowSNR::usage = GenerateTruncationHelp[TruncationPointRiceApproximateLowSNR, "LowSNR"];
TruncationPointRiceApproximateLowSNR[M_, \[Gamma]_, \[Lambda]_, K_, OptionsPattern[]] := Module[{diversityType, n, j0, f, \[Epsilon]tr = OptionValue[Tolerance], \[Epsilon], Pf, a, b},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[K == 0, Return[0]];

	Pf = ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"];
	\[Epsilon] = \[Epsilon]tr / (1 - Pf);

	Which[
		diversityType == "ND",
			K - 1 - InverseQ[1 - \[Epsilon]]/Sqrt[2] (Sqrt[2 K + ((InverseQ[1 - \[Epsilon]]/Sqrt[2])^2) ] - InverseQ[1 - \[Epsilon]]/Sqrt[2]) // Ceiling,
		diversityType == "MRC",
			K n - 1 - InverseQ[1 - \[Epsilon]]/Sqrt[2] (Sqrt[2 K n + ((InverseQ[1 - \[Epsilon]]/Sqrt[2])^2) ] - InverseQ[1 - \[Epsilon]]/Sqrt[2]) // Ceiling,
		diversityType == "EGC",
			{a, b} = HuBeaulieuParameters[K, n];
			((K n)/a - 1 - InverseQ[1 - \[Epsilon]]/Sqrt[2] (Sqrt[(2 K n)/a + ((InverseQ[1 - \[Epsilon]]/Sqrt[2])^2) ] - InverseQ[1 - \[Epsilon]]/Sqrt[2])) // Ceiling,
		diversityType == "SLC",
			K n - 1 - InverseQ[1 - \[Epsilon]]/Sqrt[2] (Sqrt[2 K n + ((InverseQ[1 - \[Epsilon]]/Sqrt[2])^2) ] - InverseQ[1 - \[Epsilon]]/Sqrt[2]) // Ceiling,
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionRiceApproximateNumerical*)


Options[ProbabilityOfDetectionRiceApproximateNumerical] = {DiversityType -> OptionValue[ProbabilityOfDetectionRice, DiversityType]};
ProbabilityOfDetectionRiceApproximateNumerical::usage = GenerateMethodHelp[ProbabilityOfDetectionRiceApproximateNumerical, "ApproximateNumerical"];
ProbabilityOfDetectionRiceApproximateNumerical[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, K_?NumericQ, OptionsPattern[]] := NIntegrate[ProbabilityOfDetectionAWGN[M, x, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "ApproximateNumerical"] SignalToNoiseRatioPDFRice[\[Gamma], K, x, DiversityType -> OptionValue[DiversityType], Method -> "ApproximateNumerical"], {x, 0, \[Infinity]}]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionRiceApproximateNumericalLowSNR*)


Options[ProbabilityOfDetectionRiceApproximateNumericalLowSNR] = {DiversityType -> OptionValue[ProbabilityOfDetectionRice, DiversityType]};
ProbabilityOfDetectionRiceApproximateNumericalLowSNR::usage = GenerateMethodHelp[ProbabilityOfDetectionRiceApproximateNumericalLowSNR, "ApproximateNumericalLowSNR"];
ProbabilityOfDetectionRiceApproximateNumericalLowSNR[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, K_?NumericQ, OptionsPattern[]] := NIntegrate[ProbabilityOfDetectionAWGN[M, x, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "ApproximateNumericalLowSNR"] SignalToNoiseRatioPDFRice[\[Gamma], K, x, DiversityType -> OptionValue[DiversityType], Method -> "ApproximateNumericalLowSNR"], {x, 0, \[Infinity]}]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionRiceApproximateSimple*)


Options[ProbabilityOfDetectionRiceApproximateSimple] = {DiversityType -> OptionValue[ProbabilityOfDetectionRice, DiversityType]};
ProbabilityOfDetectionRiceApproximateSimple::usage = GenerateMethodHelp[ProbabilityOfDetectionRiceApproximateSimple, "Simple"];
ProbabilityOfDetectionRiceApproximateSimple[M_, \[Gamma]_, \[Lambda]_, K_, OptionsPattern[]] := Module[{diversityType, n, g},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];
	
	(*g[a_, b_, c_] := ProbabilityOfFalseAlarmAWGN[M, \[Lambda], n, Method -> "Approximate", DiversityType -> diversityType] + (1 - ProbabilityOfFalseAlarmAWGN[M, \[Lambda], n, Method -> "Approximate", DiversityType -> diversityType]) Exp[-K] Total[Table[K^k / k! GammaRegularized[k + 1, - a c / b], {k, 0, limit}]];*)
	g[a_, b_, c_] := ProbabilityOfFalseAlarmAWGN[M, \[Lambda], n, Method -> "Approximate", DiversityType -> diversityType] + (1 - ProbabilityOfFalseAlarmAWGN[M, \[Lambda], n, Method -> "Approximate", DiversityType -> diversityType]) MarcumQ[1, Sqrt[2K], Sqrt[-2 a c / b]];

	Which[
		diversityType == "ND",
			g[(\[Lambda] - M) / (2 Sqrt[M]), - Sqrt[M] / 2, (K + 1) / \[Gamma]],
		diversityType == "MRC",
			g[(\[Lambda] - M) / (2 Sqrt[M]), - Sqrt[M] / 2, (K + 1) / \[Gamma]],
		diversityType == "EGC",
			Undefined,
		diversityType == "SLC",
			Undefined,
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionRiceExactAnnamalai*)


Options[ProbabilityOfDetectionRiceExactAnnamalai] = {DiversityType -> OptionValue[ProbabilityOfDetectionRice, DiversityType], Limit -> Undefined};
ProbabilityOfDetectionRiceExactAnnamalai::usage = GenerateMethodHelp[ProbabilityOfDetectionRiceExactAnnamalai, "Annamalai"];
ProbabilityOfDetectionRiceExactAnnamalai[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, K_?NumericQ, OptionsPattern[]] := Module[{limit = OptionValue[Limit], diversityType, n, \[CapitalOmega]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[SameQ[limit, Undefined], limit = TruncationPointRice[M, \[Gamma], \[Lambda], K, Method -> "ExactAnnamalai", DiversityType -> OptionValue[DiversityType]]];

	\[CapitalOmega] = N[(K + 1) / (K + 1 + (M / 2) \[Gamma]), 20];

	Which[
		diversityType == "ND",
			If[K == 0,
				1 - \[CapitalOmega] Total[Table[(1 - GammaRegularized[k + M / 2, \[Lambda] / 2]) (1 - \[CapitalOmega])^(k) , {k, 0, limit}]],
				1 - Exp[- K (1 - \[CapitalOmega])] \[CapitalOmega] Total[Table[(1 - GammaRegularized[k + M / 2, \[Lambda] / 2]) (1 - \[CapitalOmega])^(k) Total[Table[Gamma[k + 1] / ((i!)^2 (k - i)!) (K \[CapitalOmega])^(i), {i, 0, k}]], {k, 0, limit}]]
			],
		diversityType == "MRC",
			If[K == 0,
				1 - (\[CapitalOmega]^n) Total[Table[(1 - GammaRegularized[k + M / 2, \[Lambda] / 2]) (1 - \[CapitalOmega])^(k) Gamma[k + n] / (k! Gamma[n]), {k, 0, limit}]],
				1 - Exp[- K n (1 - \[CapitalOmega])] (\[CapitalOmega]^n) Total[Table[(1 - GammaRegularized[k + M / 2, \[Lambda] / 2]) (1 - \[CapitalOmega])^(k) Total[Table[Gamma[k + n] / (i! (k - i)! Gamma[n + i]) (K n \[CapitalOmega])^(i), {i, 0, k}]], {k, 0, limit}]]
			],
		diversityType == "SLC",
			If[K == 0,
				1 - (\[CapitalOmega]^n) Total[Table[(1 - GammaRegularized[k + M n / 2, \[Lambda] / 2]) (1 - \[CapitalOmega])^(k) Gamma[k + n] / (k! Gamma[n]), {k, 0, limit}]],
				1 - Exp[- K n (1 - \[CapitalOmega])] (\[CapitalOmega]^n) Total[Table[(1 - GammaRegularized[k + M n / 2, \[Lambda] / 2]) (1 - \[CapitalOmega])^(k) Total[Table[Gamma[k + n] / (i! (k - i)! Gamma[n + i]) (K n \[CapitalOmega])^(i), {i, 0, k}]], {k, 0, limit}]]
			],
		True,
			Undefined
	]//N
]


Options[TruncationPointRiceExactAnnamalai] = {DiversityType -> OptionValue[TruncationPointRice, DiversityType], Tolerance -> OptionValue[TruncationPointRice, Tolerance]};
TruncationPointRiceExactAnnamalai::usage = GenerateTruncationHelp[TruncationPointRiceExactAnnamalai, "Annamalai"];
TruncationPointRiceExactAnnamalai[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, K_?NumericQ, OptionsPattern[]] := Module[{tol = OptionValue[Tolerance], diversityType, n, f, j, j0, j1, c, \[CapitalOmega]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	\[CapitalOmega] = N[(K + 1) / (K + 1 + M \[Gamma]), 20];

	Which[
		diversityType == "ND",
			j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol] // Round;
			f[j_] := (1 - GammaRegularized[(M / 2) + j + 1, \[Lambda] / 2]) - tol,
		diversityType == "MRC",
			c = If[K == 0, 1, Max[1, Exp[-K n (1 - \[CapitalOmega])] \[CapitalOmega]^n NSum[(1 - \[CapitalOmega])^k (Gamma[k + n] / (i! (k - i)! Gamma[n + i])) (K n \[CapitalOmega])^i, {k, 0, \[Infinity]}, {i, 0, k}, Method -> "AlternatingSigns", VerifyConvergence -> False]]];
			j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol] // Round;
			f[j_] := c (1 - GammaRegularized[(M / 2) + j + 1, \[Lambda] / 2]) - tol,
		diversityType == "SLC",
			c = If[K == 0, 1, Max[1, Exp[-K n (1 - \[CapitalOmega])] \[CapitalOmega]^n NSum[(1 - \[CapitalOmega])^k (Gamma[k + n] / (i! (k - i)! Gamma[n + i])) (K n \[CapitalOmega])^i, {k, 0, \[Infinity]}, {i, 0, k}, Method -> "AlternatingSigns", VerifyConvergence -> False]]];
			j0 = (\[Lambda] / 2) - (M n / 2) - Sqrt[M n / 2] InverseQ[1 - tol] // Round;
			f[j_] := c (1 - GammaRegularized[(M / 2) n + j + 1, \[Lambda] / 2]) - tol,
		True,
			Return[Undefined]
	];

	j1 = If[f[0] <= 0,
		0,
		j/.FindRoot[f[j], {j, 0, Max[100, 2j0]}, Method->"Brent"]
	]//Round;

	If[f[j1] < 0,
		j1,
		If[f[j1 + 1] < 0,
			j1 + 1,
			Print["Error: root does not meet tolerance requirements."];
			Abort[];
		]
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionRiceExactNumerical*)


Options[ProbabilityOfDetectionRiceExactNumerical] = {DiversityType -> OptionValue[ProbabilityOfDetectionRice, DiversityType]};
ProbabilityOfDetectionRiceExactNumerical::usage = GenerateMethodHelp[ProbabilityOfDetectionRiceExactNumerical, "Numerical"];
ProbabilityOfDetectionRiceExactNumerical[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, K_?NumericQ, OptionsPattern[]] := 1 - CDF[TestStatisticDistributionRice[M, \[Gamma], K, DiversityType -> OptionValue[DiversityType], Method -> "Exact"], \[Lambda]]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionRiceExactSun*)


Options[ProbabilityOfDetectionRiceExactSun] = {DiversityType -> OptionValue[ProbabilityOfDetectionRice, DiversityType], Limit -> Undefined};
ProbabilityOfDetectionRiceExactSun::usage = GenerateMethodHelp[ProbabilityOfDetectionRiceExactSun, "Sun"];
ProbabilityOfDetectionRiceExactSun[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, K_?NumericQ, OptionsPattern[]] := Module[{limit = OptionValue[Limit], diversityType, n, \[CapitalOmega]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[SameQ[limit, Undefined], limit = TruncationPointRice[M, \[Gamma], \[Lambda], K, Method -> "ExactSun", DiversityType -> OptionValue[DiversityType]]];

	\[CapitalOmega] = (K + 1) / (K + 1 + M \[Gamma]);

	Which[
		diversityType == "ND",
			1 - Exp[-K - \[Lambda] / 2] \[CapitalOmega] Total[Table[(\[Lambda] / 2)^k / k! Total[Table[(1 - \[CapitalOmega])^l Hypergeometric1F1[l + 1, 1, K \[CapitalOmega]], {l, 0, k - M / 2}]], {k, M / 2, M / 2 + limit}]],
		diversityType == "MRC",
			Undefined,
		diversityType == "SLC",
			Undefined,
		True,
			Undefined
	]//N
]


Options[TruncationPointRiceExactSun] = {DiversityType -> OptionValue[TruncationPointRice, DiversityType], Tolerance -> OptionValue[TruncationPointRice, Tolerance]};
TruncationPointRiceExactSun::usage = GenerateTruncationHelp[TruncationPointRiceExactSun, "Sun"];
TruncationPointRiceExactSun[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, K_?NumericQ, OptionsPattern[]] := Module[{tol = OptionValue[Tolerance], diversityType, n, f, j, j0, j1},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND",
			j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol] // Round;
			f[j_] := (1 - GammaRegularized[(M / 2) + j + 1, \[Lambda] / 2]) - tol,
		diversityType == "MRC",
			Undefined,
		diversityType == "SLC",
			Undefined,
		True,
			Return[Undefined]
	];

	j1 = If[f[0] <= 0,
		0,
		j/.FindRoot[f[j], {j, 0, Max[100, 2j0]}, Method->"Brent"]
	]//Round;

	If[f[j1] < 0,
		j1,
		If[f[j1 + 1] < 0,
			j1 + 1,
			Print["Error: root does not meet tolerance requirements."];
			Abort[];
		]
	]
]


(* ::Subsection::Closed:: *)
(*ApproximationErrorRice*)


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorRice*)


Options[ApproximationErrorRice] = {Method -> OptionValue[ApproximationError, Method], DiversityType -> OptionValue[ApproximationError, DiversityType]};
ApproximationErrorRice::usage="ApproximationErrorRice[M, \[Gamma], \[Lambda], K] calculates the specified approximation error for Rice channels.\n\n"<>DiversityTypeHelp[ApproximationErrorRice]<>"\n\n"<>MethodHelp[ApproximationErrorRice, {"\"ApproximateAsymptotic\"", "\"ApproximateLowSNR\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateSimple\"", "\"Asymptotic\"", "\"CentralLimitTheorem\"", "\"EGCPDF\"", "\"LowSNR\"", "\"Simple\""}];
ApproximationErrorRice[M_, \[Gamma]_, \[Lambda]_, K_, OptionsPattern[]] := Module[{method = OptionValue[Method], diversityType, n},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		(* Catch extreme values *)
		\[Lambda] == \[Infinity],
			0,
		method == "ApproximateAsymptotic" || method == "ApproximateLowSNR" || method == "ApproximateNumerical" || method == "ApproximateNumericalLowSNR" || method == "ApproximateSimple",
			ApproximationErrorRiceTotal[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Method -> method],
		method == "Asymptotic",
			ApproximationErrorRiceAsymptotic[K, DiversityType -> OptionValue[DiversityType]],
		method == "CentralLimitTheorem",
			ApproximationErrorAWGN[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method], ChannelType -> "AWGN"],
		method == "EGCPDF",
			ApproximationErrorRiceEGCPDF[\[Gamma], K, n],
		method == "LowSNR" || method == "LowSNRLowSNR" || method == "LowSNRMaximum" || method == "LowSNRHighSNR",
			ApproximationErrorRiceLowSNR[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Method -> method],
		method == "Simple",
			Undefined,
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorRiceAsymptotic*)


Options[ApproximationErrorRiceAsymptotic] = {DiversityType -> OptionValue[ProbabilityOfDetectionRice, DiversityType]};
ApproximationErrorRiceAsymptotic[K_, OptionsPattern[]] := Module[{diversityType, n, \[Epsilon]\[Infinity], a, b},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];
	
	\[Epsilon]\[Infinity][k_, \[CurlyTheta]_] := 1/2 - MarcumQ[k / 2, Sqrt[k \[CurlyTheta]], Sqrt[k (1 + \[CurlyTheta])]];

	Which[
		diversityType == "ND",
			Max[Q[(1 + K) / Sqrt[1 + 2K]], \[Epsilon]\[Infinity][2, K]],
		diversityType == "MRC" || diversityType == "SLC",
			Max[Q[n (1 + K) / Sqrt[n (1 + 2K)]], \[Epsilon]\[Infinity][2n, K]],
		diversityType == "EGC",
			{a, b} = HuBeaulieuParameters[K, n];
			Max[Q[n (1 + K / a) / Sqrt[n (1 + 2K / a)]], \[Epsilon]\[Infinity][2n, K / a]],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorRiceEGCPDF*)


Options[ApproximationErrorRiceEGCPDF] = {NumberOfPoints -> 10^6};
ApproximationErrorRiceEGCPDF[\[Gamma]_, K_, n_, OptionsPattern[]] := ApproximationErrorRiceEGCPDF[\[Gamma], K, n, NumberOfPoints -> OptionValue[NumberOfPoints]] = Module[{g, \[Epsilon], \[ScriptCapitalD]0, \[ScriptCapitalD]1},
	\[ScriptCapitalD]0 = SignalToNoiseRatioDistributionRiceEGC[\[Gamma], K, n, NumberOfPoints -> OptionValue[NumberOfPoints]];
	\[ScriptCapitalD]1 = SignalToNoiseRatioDistributionRice[\[Gamma], K, DiversityType -> {"EGC", n}, Method -> "Approximate"];

	g[\[Epsilon]_?NumericQ] := If[\[Epsilon] < 0, 0, Abs[CDF[\[ScriptCapitalD]0, \[Epsilon]] - CDF[\[ScriptCapitalD]1, \[Epsilon]]]];

	FindMaxValue[g[\[Epsilon]], {\[Epsilon], 0.0001}, Method -> "PrincipalAxis", WorkingPrecision -> 20]
]


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorRiceLowSNR*)


Options[ApproximationErrorRiceLowSNR] = {DiversityType -> OptionValue[ApproximationErrorRice, DiversityType], Method -> "Minimum"};
ApproximationErrorRiceLowSNR[M_, \[Gamma]_, \[Lambda]_, K_, OptionsPattern[]] := Module[{diversityType, n, a, b, method = OptionValue[Method]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[method == "Minimum" || method == "LowSNR",
		Return[Min[Flatten[{1 / Sqrt[2 \[Pi] E], Table[ApproximationErrorRiceLowSNR[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType], Method -> methods], {methods, {"LowSNRMaximum", "LowSNRLowSNR"}}]}]]]
	];

	Which[
		diversityType == "ND",
			Which[
				method == "LowSNRMaximum",
					(1 / \[Pi]) Sqrt[(K + 1) / (2 M)],
				method == "LowSNRLowSNR",
					\[Gamma] / Sqrt[2 \[Pi] E],
				method == "LowSNRHighSNR",
					Undefined,
				True,
					Undefined
			],
		diversityType == "MRC",
			Which[
				method == "LowSNRMaximum",
					(1 / \[Pi]) Sqrt[(K + 1) n / (2 M)],
				method == "LowSNRLowSNR",
					n \[Gamma] / Sqrt[2 \[Pi] E],
				method == "LowSNRHighSNR",
					Undefined,
				True,
					Undefined
			],
		diversityType == "EGC",
			{a, b} = HuBeaulieuParameters[K, n];
			Which[
				method == "LowSNRMaximum",
					(1 / \[Pi]) Sqrt[(K / a + 1) n / (2 M)],
				method == "LowSNRLowSNR",
					(K / a + 1) / (K + 1)  n b \[Gamma] / Sqrt[2 \[Pi] E],
				method == "LowSNRHighSNR",
					Undefined,
				True,
					Undefined
			],
		diversityType == "SLC",
			Which[
				method == "LowSNRMaximum",
					(1 / \[Pi]) Sqrt[(K + 1) / (2 M)],
				method == "LowSNRLowSNR",
					\[Gamma] / Sqrt[2 \[Pi] E],
				method == "LowSNRHighSNR",
					Undefined,
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsubsection:: *)
(*ApproximationErrorRiceSimple*)


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorRiceTotal*)


Options[ApproximationErrorRiceTotal] = {Method -> OptionValue[ApproximationErrorRice, Method], DiversityType -> OptionValue[ApproximationErrorRice, DiversityType]};
ApproximationErrorRiceTotal[M_, \[Gamma]_, \[Lambda]_, K_, OptionsPattern[]] := Module[{method = OptionValue[Method], diversityType, n},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		method == "ApproximateAsymptotic",
			ApproximationErrorRiceLowSNR[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType]] + ApproximationErrorRiceAsymptotic[K, DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateNumerical",
			Which[
				diversityType == "ND" || diversityType == "MRC" || diversityType == "SLC",
					ApproximationError[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "CentralLimitTheorem", ChannelType -> "AWGN"],
				diversityType == "EGC",
					ApproximationError[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "CentralLimitTheorem", ChannelType -> "AWGN"] + ApproximationErrorRiceEGCPDF[\[Gamma], K, n],
				True,
					Undefined
			],
		method == "ApproximateLowSNR" || method == "ApproximateNumericalLowSNR",
			Which[
				diversityType == "ND" || diversityType == "MRC" || diversityType == "SLC",
					ApproximationError[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "CentralLimitTheorem", ChannelType -> "AWGN"] + ApproximationErrorRiceLowSNR[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType]],
				diversityType == "EGC",
					ApproximationError[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "CentralLimitTheorem", ChannelType -> "AWGN"] + ApproximationErrorRiceLowSNR[M, \[Gamma], \[Lambda], K, DiversityType -> OptionValue[DiversityType]] + ApproximationErrorRiceEGCPDF[\[Gamma], K, n, NumberOfPoints->10^5],
				True,
					Undefined
			],
		method == "ApproximateSimple",
			Which[
				diversityType == "ND" || diversityType == "MRC" || diversityType == "SLC",
					Undefined,
				diversityType == "EGC",
					Undefined,
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsection::Closed:: *)
(*TruncationErrorRice*)


Options[TruncationErrorRice] = {DiversityType -> OptionValue[TruncationPointRice, DiversityType]};
TruncationErrorRice[K_, Pf_, T_, OptionsPattern[]] := Module[{diversityType, n, a, b},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND",
			(1 - GammaRegularized[T + 1, K]) (1 - Pf),
		diversityType == "MRC",
			(1 - GammaRegularized[T + 1, K n]) (1 - Pf),
		diversityType == "EGC",
			{a, b} = HuBeaulieuParameters[K, n];
			((1 - GammaRegularized[T + 1, K n / a]) (1 - Pf)),
		diversityType == "SLC",
			(1 - GammaRegularized[T + 1, K n]) (1 - Pf),
		True,
			Undefined
	]
]


(* ::Subsection::Closed:: *)
(*SampleComplexityRice*)


Options[SampleComplexityRice] = {DiversityType -> OptionValue[SampleComplexity, DiversityType], Method -> OptionValue[SampleComplexity, Method], Tolerance -> OptionValue[SampleComplexity, Tolerance]};
SampleComplexityRice::usage="SampleComplexityRice[\[Gamma], Pf, Pd, K, n] calculates the number of samples required to meet the specified decision probabilities in a Rice channel.\n\n"<>MethodHelp[SampleComplexityRice, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateAsymptotic\""}]<>"\n\n"<>DiversityTypeHelp[SampleComplexityRice];
SampleComplexityRice[\[Gamma]_?NumericQ,Pf_?NumericQ,Pd_?NumericQ,K_?NumericQ,OptionsPattern[]] := Module[{a, b, diversityType, n, method = OptionValue[Method], f, M, tolerance = Log[10, 1/OptionValue[Tolerance]]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Quiet[Which[
		method == "ExactNumerical",
			f[x_?NumericQ] := ProbabilityOfDetection[x, \[Gamma], \[Lambda][x, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType -> {"Rice", K}, Method -> "ExactNumerical"];
			M/.FindRoot[f[M] == Pd, {M, Max[1, SampleComplexity[\[Gamma], Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> {"Rice", K}, Method -> "ApproximateNumerical"]], 1, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateNumerical",
			f[x_?NumericQ] := ProbabilityOfDetection[x, \[Gamma], \[Lambda][x, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType ->{"Rice", K}, Method -> "ApproximateNumerical"];
			M/.FindRoot[f[M] == Pd, {M, Max[1, SampleComplexity[\[Gamma], Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> "AWGN", Method -> "ApproximateNumerical"]], 1, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateNumericalLowSNR",
			f[x_?NumericQ] := ProbabilityOfDetection[x, \[Gamma], \[Lambda][x, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType ->{"Rice", K}, Method -> "ApproximateNumericalLowSNR"];
			M/.FindRoot[f[M] == Pd, {M, Max[1, SampleComplexity[\[Gamma], Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> "AWGN", Method -> "ApproximateNumericalLowSNR"]], 1, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateSmallPf",
			Which[
				diversityType == "ND",
					2 (2 (K + 1) InverseQ[Pf] / (\[Gamma] InverseMarcumQ[1, Sqrt[2K], (Pd - Pf) / (1 - Pf)]^2))^2,
				diversityType == "MRC",
					Undefined,
				diversityType == "SLC",
					Undefined,
				True,
					Undefined
			],
		method == "ApproximateAsymptotic",
			Which[
				diversityType == "ND",
					2 ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / ((K + 1)^2 / (2K + 1))] InverseQ[Pd]) / (\[Gamma] (1 - (InverseQ[Pd])^2 / ((K + 1)^2 / (2K + 1)))))^2,
				diversityType == "MRC",
					(2 / n^2) ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (n ((K + 1)^2 / (2K + 1)))] InverseQ[Pd]) / (\[Gamma] (1 - (InverseQ[Pd])^2 / (n ((K + 1)^2 / (2K + 1))))))^2,
				diversityType == "EGC",
					{a, b} = HuBeaulieuParameters[K, n];
					2 ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (n ((K / a + 1)^2 / (2K + 1)))] InverseQ[Pd]) / (b ((K / a + 1) / (K + 1)) \[Gamma] (1 - (InverseQ[Pd])^2 / (n ((K / a + 1)^2 / (2K + 1))))))^2,
				diversityType == "SLC",
					(2 / n) ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (n ((K + 1)^2 / (2K + 1)))] InverseQ[Pd]) / (\[Gamma] (1 - (InverseQ[Pd])^2 / (n ((K + 1)^2 / (2K + 1))))))^2,
				True,
					Undefined
			],
		True,
			Undefined
	]]
]


(* ::Subsection::Closed:: *)
(*ReceiverSensitivityRice*)


Options[ReceiverSensitivityRice] = {DiversityType -> OptionValue[ReceiverSensitivity, DiversityType], Method -> OptionValue[ReceiverSensitivity, Method], Tolerance -> OptionValue[ReceiverSensitivity, Tolerance]};
ReceiverSensitivityRice::usage="ReceiverSensitivityRice[M, Pf, Pd, K] calculates the lowest detectable signal to noise ratio for the specified decision probabilities in a Rice channel.\n\n"<>MethodHelp[ReceiverSensitivityRice, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateAsymptotic\""}]<>"\n\n"<>DiversityTypeHelp[ReceiverSensitivityRice];
ReceiverSensitivityRice[M_?NumericQ, Pf_?NumericQ, Pd_?NumericQ, K_?NumericQ, OptionsPattern[]] := Module[{a, b, diversityType, n, method = OptionValue[Method], tolerance = Log[10, 1/OptionValue[Tolerance]], f, \[Gamma], \[Epsilon]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Quiet[Which[
		method == "ExactNumerical",
			f[x_?NumericQ] := ProbabilityOfDetection[M, x, \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType -> {"Rice", m}, Method -> "ExactNumerical"];
			\[Gamma]/.FindRoot[f[\[Gamma]] == Pd, {\[Gamma], Max[0, ReceiverSensitivity[M, Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> {"Rice", m}, Method -> "ApproximateNumerical"]], 0, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateNumerical",
			f[x_?NumericQ] := ProbabilityOfDetection[M, x, \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType -> {"Rice", m}, Method -> "ApproximateNumerical"];
			\[Gamma]/.FindRoot[f[\[Gamma]] == Pd, {\[Gamma], Max[0, ReceiverSensitivity[M, Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> "AWGN", Method -> "ApproximateLowSNR"]], 0, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateNumericalLowSNR",
			f[x_?NumericQ] := ProbabilityOfDetection[M, x, \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType -> {"Rice", m}, Method -> "ApproximateNumericalLowSNR"];
			\[Gamma]/.FindRoot[f[\[Gamma]] == Pd, {\[Gamma], Max[0, ReceiverSensitivity[M, Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> "AWGN", Method -> "ApproximateLowSNR"]], 0, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateAsymptotic",
			Which[
				diversityType == "ND",
					Sqrt[2 / M] ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / ((K + 1)^2 / (2K + 1))] InverseQ[Pd]) / ((1 - (InverseQ[Pd])^2 / ((K + 1)^2 / (2K + 1))))),
				diversityType == "MRC",
					Sqrt[2 / (M n^2)] ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (n ((K + 1)^2 / (2K + 1)))] InverseQ[Pd]) / ((1 - (InverseQ[Pd])^2 / (n ((K + 1)^2 / (2K + 1)))))),
				diversityType == "EGC",
					{a, b} = HuBeaulieuParameters[K, n];
					Sqrt[2 / M] ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (n ((K / a + 1)^2 / (2K + 1)))] InverseQ[Pd]) / (b ((K / a + 1) / (K + 1)) (1 - (InverseQ[Pd])^2 / (n ((K / a + 1)^2 / (2K + 1)))))),
				diversityType == "SLC",
					Sqrt[2 / (M n)] ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (n ((K + 1)^2 / (2K + 1)))] InverseQ[Pd]) / ((1 - (InverseQ[Pd])^2 / (n ((K + 1)^2 / (2K + 1)))))),
				True,
					Undefined
			],
		True,
			Undefined
	]]
]


(* ::Subsection::Closed:: *)
(*DiversityGainRice*)


Options[DiversityGainRice] = {DiversityType -> OptionValue[DiversityGain, DiversityType], Method -> OptionValue[DiversityGain, Method], Tolerance -> OptionValue[DiversityGain, Tolerance]};
DiversityGainRice::usage="DiversityGainRice[M, \[Gamma], Pf, Pd, K] calculates the minimum number of nodes required to meet the specified decision probabilities in a Rice channel.\n\n"<>MethodHelp[DiversityGainRice, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateAsymptotic\""}]<>"\n\n"<>DiversityTypeHelp[DiversityGainRice];
DiversityGainRice[M_?NumericQ, \[Gamma]_?NumericQ, Pf_?NumericQ, Pd_?NumericQ, K_?NumericQ, OptionsPattern[]] := Module[{diversityType, n, n0, method = OptionValue[Method], tolerance = Log[10, 1/OptionValue[Tolerance]], f},
	{diversityType, n0} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		method == "ExactNumerical" || method == "ApproximateNumerical" || method == "ApproximateNumericalLowSNR",
			Which[
				method == "ExactNumerical",
					n0 = DiversityGain[M, \[Gamma], Pf, Pd, DiversityType -> OptionValue[DiversityType], ChannelType -> {"Rice", K}, Method -> "ApproximateNumerical"];
					f[x_?IntegerQ] := ProbabilityOfDetection[M, \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, x}, Method -> method], DiversityType -> {diversityType, x}, ChannelType -> {"Rice", K}, Method -> "ExactNumerical"],
				method == "ApproximateNumerical",
					n0 = DiversityGain[M, \[Gamma], Pf, Pd, DiversityType -> OptionValue[DiversityType], ChannelType -> "AWGN", Method -> "ApproximateLowSNR"];
					f[x_?IntegerQ] := ProbabilityOfDetection[M, \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, x}, Method -> method], DiversityType -> {diversityType, x}, ChannelType -> {"Rice", K}, Method -> "ApproximateNumerical"],
				method == "ApproximateNumericalLowSNR",
					n0 = DiversityGain[M, \[Gamma], Pf, Pd, DiversityType -> OptionValue[DiversityType], ChannelType -> "AWGN", Method -> "ApproximateLowSNR"];
					f[x_?IntegerQ] := ProbabilityOfDetection[M, \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, x}, Method -> method], DiversityType -> {diversityType, x}, ChannelType -> {"Rice", K}, Method -> "ApproximateNumericalLowSNR"]
			];
			Which[
				f[n0] > Pd,
					While[f[n0] > Pd, n0--];
					n0 + 1,
				f[n0] < Pd,
					While[f[n0] < Pd, n0++];
					n0
			],
		method == "ApproximateAsymptotic",
			Which[
				diversityType == "SLC",
					(2 / M) ((InverseQ[Pf]-Sqrt[1+(M \[Gamma]^2)/(2((K + 1)^2 / (2K + 1)))]InverseQ[Pd])/\[Gamma])^2//Ceiling,
				True,
					Undefined
			],
		True,
			Undefined
	]
]


End[];


EndPackage[];
