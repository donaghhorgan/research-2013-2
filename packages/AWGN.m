(* ::Package:: *)

(* ::Title:: *)
(*AWGN channel functions*)


(* ::Subsection:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions for cooperative energy detection in AWGN channels.*)
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
(*01/05/2013*)
(*1.51*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.51: Various clean ups, added ApproximationErrorAWGN function.*)
(*Version 1.5: Complete rewrite - lots of changes, but should be relatively transparent.*)
(*Version 1.40: Added low SNR assumption error bounds.*)
(*Version 1.32: Various bug fixes.*)
(*Version 1.31: Removed LowSNR option in favour of new LowSNR approximate method and amalgamated Method and Algorithm options.*)
(*Version 1.30: Added diversity reception support to the sample complexity function.*)
(*Version 1.24: Added SEC support, and removed SSC support.*)
(*Version 1.23: Moved LowSNRErrorBound to the Nakagami package.*)
(*Version 1.22: Moved help functions to Network package.*)
(*Version 1.21: Added generic help functions for consistent documentation.*)
(*Version 1.2: Recoded ProbabilityOfFalseAlarm, ProbabilityOfDetection and \[Lambda] functions, so they are easier to read.*)
(*Version 1.12: Added EGC support.*)
(*Version 1.11: Added protection for symbols.*)
(*Version 1.1: Added diversity types to functions.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


BeginPackage["AWGN`"];


(* ::Subsection::Closed:: *)
(*TestStatisticPDFAWGN*)


TestStatisticPDFAWGN;


TestStatisticDistributionAWGN;


(* ::Subsection::Closed:: *)
(*ProbabilityOfFalseAlarmAWGN*)


ProbabilityOfFalseAlarmAWGN;


(* ::Subsection::Closed:: *)
(*ProbabilityOfDetectionAWGN*)


ProbabilityOfDetectionAWGN;


(* ::Subsection::Closed:: *)
(*ApproximationErrorAWGN*)


ApproximationErrorAWGN;


(* ::Subsection::Closed:: *)
(*\[Lambda]*)


\[Lambda];


(* ::Subsection::Closed:: *)
(*SampleComplexityAWGN*)


SampleComplexityAWGN;


(* ::Subsection::Closed:: *)
(*ReceiverSensitivityAWGN*)


ReceiverSensitivityAWGN;


(* ::Subsection::Closed:: *)
(*DiversityGainAWGN*)


DiversityGainAWGN;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


<<Help`;
Needs["QFunction`"];
Needs["Extras`"];
Needs["EnergyDetectors`"];
Needs["MarcumQ2`"];
Needs["BerryEsseenBound`"];


(* ::Subsection::Closed:: *)
(*TestStatisticPDFAWGN*)


(* ::Subsubsection::Closed:: *)
(*TestStatisticPDFAWGN*)


Options[TestStatisticPDFAWGN] = {Method -> OptionValue[ProbabilityOfDetection,Method], DiversityType -> OptionValue[ProbabilityOfDetection,DiversityType]};
TestStatisticPDFAWGN::usage="TestStatisticPDFAWGN[M, \[Gamma], \[Lambda]] computes the probability density function of the test statistic for an energy detector with the specified diversity type operating on an occupied AWGN channel.\n\n"<>DiversityTypeHelp[TestStatisticPDFAWGN]<>"\n\n"<>MethodHelp[TestStatisticPDFAWGN, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
TestStatisticPDFAWGN[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]] := PDF[TestStatisticDistributionAWGN[M, \[Gamma], DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]], \[Lambda]]


(* ::Subsubsection::Closed:: *)
(*TestStatisticDistributionAWGN*)


Options[TestStatisticDistributionAWGN] = {Method -> OptionValue[ProbabilityOfDetection,Method], DiversityType -> OptionValue[ProbabilityOfDetection,DiversityType]};
TestStatisticDistributionAWGN::usage="TestStatisticDistributionAWGN[M, \[Gamma]] computes the distribution of the test statistic for an energy detector with the specified diversity type operating on an occupied AWGN channel.\n\n"<>DiversityTypeHelp[TestStatisticDistributionAWGN]<>"\n\n"<>MethodHelp[TestStatisticDistributionAWGN, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
TestStatisticDistributionAWGN[M_,\[Gamma]_,OptionsPattern[]]:=Module[{method = ProcessMethod[OptionValue[Method]], diversityType, n, k, s},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND" || diversityType == "MRC" || diversityType == "EGC",
			k = M;
			s = M \[Gamma],
		diversityType == "SLC",
			k = M n;
			s = M \[Gamma],
		True,
			Return[Undefined]
	];

	Which[
		method == "Exact",
			NoncentralChiSquareDistribution[k, s],
		method == "Approximate",
			NormalDistribution[k + s, Sqrt[2 (k + 2s)]],
		method == "ApproximateLowSNR",
			NormalDistribution[k + s, Sqrt[2 k]],
		True,
			Return[Undefined]
	]
]


(* ::Subsection::Closed:: *)
(*ProbabilityOfFalseAlarmAWGN*)


Options[ProbabilityOfFalseAlarmAWGN]={Method -> OptionValue[ProbabilityOfFalseAlarm,Method], DiversityType -> OptionValue[ProbabilityOfFalseAlarm,DiversityType]};
ProbabilityOfFalseAlarmAWGN::usage="ProbabilityOfFalseAlarmAWGN[M, \[Lambda]] calculates the probability of false alarm for an energy detector with the specified diversity type operating on an AWGN channel.\n\n"<>DiversityTypeHelp[ProbabilityOfFalseAlarmAWGN]<>"\n\n"<>MethodHelp[ProbabilityOfFalseAlarmAWGN, {"\"ExactNumerical\"", "\"ApproximateNumerical\""}];
ProbabilityOfFalseAlarmAWGN[M_,\[Lambda]_,OptionsPattern[]]:=Module[{method = ProcessMethod[OptionValue[Method]], diversityType, n, k},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND" || diversityType == "MRC" || diversityType == "EGC",
			k = M,
		diversityType == "SLC",
			k = M n,
		True,
			Return[Undefined]
	];

	Which[
		method == "Exact",
			1 - CDF[ChiSquareDistribution[k], \[Lambda]],
		method == "Approximate" || method == "ApproximateLowSNR",
			1 - CDF[NormalDistribution[k, Sqrt[2 k]], \[Lambda]],
		True,
			Return[Undefined]
	]
]


(* ::Subsection::Closed:: *)
(*ProbabilityOfDetectionAWGN*)


Options[ProbabilityOfDetectionAWGN]={Method -> OptionValue[ProbabilityOfDetection,Method], DiversityType -> OptionValue[ProbabilityOfDetection,DiversityType]};
ProbabilityOfDetectionAWGN::usage="ProbabilityOfDetectionAWGN[M, \[Gamma], \[Lambda]] calculates the probability of detection for an energy detector with the specified diversity type operating on an AWGN channel.\n\n"<>DiversityTypeHelp[ProbabilityOfDetectionAWGN]<>"\n\n"<>MethodHelp[ProbabilityOfDetectionAWGN, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
ProbabilityOfDetectionAWGN[M_,\[Gamma]_,\[Lambda]_,OptionsPattern[]]:=Module[{method = ProcessMethod[OptionValue[Method]], diversityType, n, k, s, k0, s0, \[Lambda]0},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND" || diversityType == "MRC" || diversityType == "EGC",
			k = M;
			s = M \[Gamma],
		diversityType == "SLC",
			k = M n;
			s = M \[Gamma],
		True,
			Return[Undefined]
	];

	Which[
		method == "Exact",
			(* Use standard functions to get the distribution, but switch out built in MarcumQ function for a better one *)
			(* 1 - CDF[NoncentralChiSquareDistribution[k, s], \[Lambda]] *)
			(1 - CDF[NoncentralChiSquareDistribution[k0, s0], \[Lambda]0])/.{MarcumQ -> MarcumQ2, k0 -> k, s0 -> s, \[Lambda]0 -> \[Lambda]},
		method == "Approximate",
			1 - CDF[NormalDistribution[k + s, Sqrt[2 (k + 2s)]], \[Lambda]],
		method == "ApproximateLowSNR",
			1 - CDF[NormalDistribution[k + s, Sqrt[2 k]], \[Lambda]],
		True,
			Return[Undefined]
	]
]


(* ::Subsection::Closed:: *)
(*ApproximationErrorAWGN*)


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorAWGN*)


Options[ApproximationErrorAWGN] = {Method -> OptionValue[ApproximationError, Method], DiversityType -> OptionValue[ApproximationError, DiversityType]};
ApproximationErrorAWGN::usage="ApproximationErrorAWGN[M, \[Gamma]] calculates the approximation error for the specified channel and diversity types.\n\n"<>ChannelTypeHelp[ApproximationError]<>"\n\n"<>DiversityTypeHelp[ApproximationError];
ApproximationErrorAWGN[M_, \[Gamma]_, \[Lambda]_, OptionsPattern[]] := Module[{method = ProcessMethod[OptionValue[Method]]},
	Which[
		method == "Approximate" || method == "CentralLimitTheorem",
			ApproximationErrorAWGNCentralLimitTheorem[M, \[Gamma], DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateLowSNR",
			ApproximationErrorAWGNCentralLimitTheorem[M, \[Gamma], DiversityType -> OptionValue[DiversityType]] + ApproximationErrorAWGNLowSNR[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType]],
		method == "LowSNR",
			ApproximationErrorAWGNLowSNR[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType]],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorAWGNCentralLimitTheorem*)


Options[ApproximationErrorAWGNCentralLimitTheorem] = {DiversityType -> OptionValue[ProbabilityOfDetectionAWGN, DiversityType], Method -> "Asymptotic"};
ApproximationErrorAWGNCentralLimitTheorem::usage="ApproximationErrorAWGNCentralLimitTheorem[M, \[Gamma]] calculates the upper bound for the approximation error resulting from the use of the central limit theorem to approximate the distribution of the test statistic for an energy detector with the specified diversity type.\n\n"<>DiversityTypeHelp[ProbabilityOfFalseAlarmAWGNCentralLimitTheorem]<>"\n\n"<>MethodHelp[ProbabilityOfFalseAlarmAWGNCentralLimitTheorem, {"\"Asymptotic\"", "\"BerryEsseen\""}];
ApproximationErrorAWGNCentralLimitTheorem[M_, \[Gamma]_, OptionsPattern[]] := Module[{method = ProcessMethod[OptionValue[Method]], diversityType, n, k, s, s0},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND" || diversityType == "MRC" || diversityType == "EGC",
			k = M;
			s = M \[Gamma],
		diversityType == "SLC",
			k = M n;
			s = M \[Gamma],
		True,
			Return[Undefined]
	];

	Which[
		method == "Asymptotic",
			Max[Q[Sqrt[k/2]], 1 / Sqrt[9 \[Pi] k]],
		method == "BerryEsseen",
			BerryEsseenBound[k, NoncentralChiSquareDistribution[1, s0]]/.s0->s,
		True,
			Return[Undefined]
	]
]


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorAWGNLowSNR*)


Options[ApproximationErrorAWGNLowSNR] = {DiversityType -> OptionValue[ProbabilityOfDetectionAWGN,DiversityType]};
ApproximationErrorAWGNLowSNR::usage="ApproximationErrorAWGNLowSNR[M, \[Gamma], \[Lambda]] calculates the upper bound for the low SNR approximation error for an energy detector with the specified diversity type.\n\n"<>DiversityTypeHelp[LowSNRAssumptionErrorAWGN];
ApproximationErrorAWGNLowSNR[M_, \[Gamma]_, \[Lambda]_, OptionsPattern[]] := Module[{diversityType, n, \[Phi]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND" || diversityType == "MRC" || diversityType == "EGC",
			\[Phi] = Abs[(\[Lambda] - M (1 + \[Gamma])) / Sqrt[2 M]];
			(\[Phi] / Sqrt[2 \[Pi]]) \[Gamma] Exp[- \[Phi]^2 / (2 (1 + 2 \[Gamma]))],
		diversityType == "SLC",
			\[Phi] = Abs[(\[Lambda] - M (n + \[Gamma])) / Sqrt[2 M n]];
			(\[Phi] / Sqrt[2 \[Pi]]) (\[Gamma] / n) Exp[- \[Phi]^2 / (2 (1 + 2 \[Gamma] / n))],
		True,
			Return[Undefined]
	]		
]


(* ::Subsection::Closed:: *)
(*\[Lambda]*)


Options[\[Lambda]] = {Method -> OptionValue[ProbabilityOfFalseAlarmAWGN,Method], DiversityType -> OptionValue[ProbabilityOfFalseAlarmAWGN,DiversityType]};
\[Lambda]::usage="\[Lambda][M, Pf] calculates the threshold required to ensure the specified probability of false alarm for an energy detector with the specified diversity type operating on an AWGN channel.\n\n"<>DiversityTypeHelp[\[Lambda]]<>"\n\n"<>MethodHelp[\[Lambda], {"\"ExactNumerical\"", "\"ApproximateNumerical\""}];
\[Lambda][M_,Pf_,OptionsPattern[]]:=Module[{method = ProcessMethod[OptionValue[Method]], diversityType, n, k},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND" || diversityType == "MRC" || diversityType == "EGC",
			k = M,
		diversityType == "SLC",
			k = M n,
		True,
			Return[Undefined]
	];

	Which[
		method == "Exact",
			InverseCDF[ChiSquareDistribution[k], 1 - Pf],
		method == "Approximate" || method == "ApproximateLowSNR",
			InverseCDF[NormalDistribution[k, Sqrt[2 k]], 1 - Pf],
		True,
			Return[Undefined]
	]
]


(* ::Subsection::Closed:: *)
(*SampleComplexityAWGN*)


Options[SampleComplexityAWGN] = {Method -> OptionValue[SampleComplexity,Method], DiversityType -> OptionValue[SampleComplexity,DiversityType], Tolerance -> OptionValue[SampleComplexity, Tolerance]};
SampleComplexityAWGN::usage="SampleComplexityAWGN[\[Gamma], Pf, Pd] calculates the number of samples required to meet the specified probabilities of false alarm and detection for an energy detector with the specified diversity type operating on an AWGN channel.\n\n"<>DiversityTypeHelp[SampleComplexityAWGN]<>"\n\n"<>MethodHelp[SampleComplexityAWGN, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
SampleComplexityAWGN[\[Gamma]_,Pf_,Pd_,OptionsPattern[]] := Module[{method = ProcessMethod[OptionValue[Method]], tolerance = Log[10, 1/OptionValue[Tolerance]], diversityType, n, f, M},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND",
			Which[
				method == "Exact",
					f[M_?NumericQ] := ProbabilityOfDetectionAWGN[M, \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> "Exact"], DiversityType -> {diversityType, n}, Method -> "Exact"];
					Quiet[M/.FindRoot[f[M] == Pd, {M, SampleComplexityAWGN[\[Gamma], Pf, Pd, DiversityType -> {diversityType, n}, Method -> "Approximate"], 1, \[Infinity]}, AccuracyGoal -> tolerance]],
				method == "Approximate",
					2 ((InverseQ[Pf] - Sqrt[1 + 2 \[Gamma]]InverseQ[Pd]) / \[Gamma])^2,
				method == "ApproximateLowSNR",
					2 ((InverseQ[Pf] - InverseQ[Pd]) / \[Gamma])^2
			],
		diversityType == "MRC" || diversityType == "EGC",
			Which[
				method == "Exact",
					f[M_?NumericQ] := ProbabilityOfDetectionAWGN[M, n \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> "Exact"], DiversityType -> {diversityType, n}, Method -> "Exact"];
					Quiet[M/.FindRoot[f[M] == Pd, {M, SampleComplexityAWGN[\[Gamma], Pf, Pd, DiversityType -> {diversityType, n}, Method -> "Approximate"], 1, \[Infinity]}, AccuracyGoal -> tolerance]],
				method == "Approximate",
					(2 / n^2) ((InverseQ[Pf] - Sqrt[1 + 2 n \[Gamma]]InverseQ[Pd]) / \[Gamma])^2,
				method == "ApproximateLowSNR",
					(2 / n^2) ((InverseQ[Pf] - InverseQ[Pd]) / \[Gamma])^2
			],
		diversityType == "SLC",
			Which[
				method == "Exact",
					f[M_?NumericQ] := ProbabilityOfDetectionAWGN[M, n \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> "Exact"], DiversityType -> {diversityType, n}, Method -> "Exact"];
					Quiet[M/.FindRoot[f[M] == Pd, {M, SampleComplexityAWGN[\[Gamma], Pf, Pd, DiversityType -> {diversityType, n}, Method -> "Approximate"], 1, \[Infinity]}, AccuracyGoal -> tolerance]],
				method == "Approximate",
					(2 / n) ((InverseQ[Pf] - Sqrt[1 + 2 \[Gamma]]InverseQ[Pd]) / \[Gamma])^2,
				method == "ApproximateLowSNR",
					(2 / n) ((InverseQ[Pf] - InverseQ[Pd]) / \[Gamma])^2
			],
		True,
			Return[Undefined]
	]
]


(* ::Subsection::Closed:: *)
(*ReceiverSensitivityAWGN*)


Options[ReceiverSensitivityAWGN] = {Method -> OptionValue[ReceiverSensitivity,Method], DiversityType -> OptionValue[ReceiverSensitivity,DiversityType], Tolerance -> OptionValue[ReceiverSensitivity, Tolerance]};
ReceiverSensitivityAWGN::usage="ReceiverSensitivityAWGN[M, Pf, Pd] calculates the number of samples required to meet the specified probabilities of false alarm and detection for an energy detector with the specified diversity type operating on an AWGN channel.\n\n"<>DiversityTypeHelp[ReceiverSensitivityAWGN]<>"\n\n"<>MethodHelp[ReceiverSensitivityAWGN, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
ReceiverSensitivityAWGN[M_,Pf_,Pd_,OptionsPattern[]] := Module[{method = ProcessMethod[OptionValue[Method]], tolerance = Log[10, 1/OptionValue[Tolerance]], diversityType, n, f, \[Gamma]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND",
			Which[
				method == "Exact",
					f[\[Gamma]_?NumericQ] := ProbabilityOfDetectionAWGN[M, \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> "Exact"], DiversityType -> {diversityType, n}, Method -> "Exact"];
					Quiet[\[Gamma]/.FindRoot[f[\[Gamma]] == Pd, {\[Gamma], ReceiverSensitivityAWGN[M, Pf, Pd, DiversityType -> {diversityType, n}, Method -> "ApproximateLowSNR"], 0, \[Infinity]}, AccuracyGoal -> tolerance]],
				method == "ApproximateLowSNR",
					Sqrt[2 / M] (InverseQ[Pf] - InverseQ[Pd])
			],
		diversityType == "MRC" || diversityType == "EGC",
			Which[
				method == "Exact",
					f[\[Gamma]_?NumericQ] := ProbabilityOfDetectionAWGN[M, n \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> "Exact"], DiversityType -> {diversityType, n}, Method -> "Exact"];
					Quiet[\[Gamma]/.FindRoot[f[\[Gamma]] == Pd, {\[Gamma], ReceiverSensitivityAWGN[M, Pf, Pd, DiversityType -> {diversityType, n}, Method -> "ApproximateLowSNR"], 0, \[Infinity]}, AccuracyGoal -> tolerance]],
				method == "ApproximateLowSNR",
					Sqrt[2 / M] ((InverseQ[Pf] - InverseQ[Pd]) / n)
			],
		diversityType == "SLC",
			Which[
				method == "Exact",
					f[\[Gamma]_?NumericQ] := ProbabilityOfDetectionAWGN[M, n \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> "Exact"], DiversityType -> {diversityType, n}, Method -> "Exact"];
					Quiet[\[Gamma]/.FindRoot[f[\[Gamma]] == Pd, {\[Gamma], ReceiverSensitivityAWGN[M, Pf, Pd, DiversityType -> {diversityType, n}, Method -> "ApproximateLowSNR"], 0, \[Infinity]}, AccuracyGoal -> tolerance]],
				method == "ApproximateLowSNR",
					Sqrt[2 / (M n)] (InverseQ[Pf] - InverseQ[Pd])
			],
		True,
			Return[Undefined]
	]
]


(* ::Subsection::Closed:: *)
(*DiversityGainAWGN*)


Options[DiversityGainAWGN] = {Method -> OptionValue[DiversityGain,Method], DiversityType -> OptionValue[DiversityGain,DiversityType], Tolerance -> OptionValue[DiversityGain, Tolerance]};
DiversityGainAWGN::usage="DiversityGainAWGN[M, \[Gamma], Pf, Pd] calculates the number of branches required to meet the specified probabilities of false alarm and detection for an energy detector with the specified diversity type operating on an AWGN channel.\n\n"<>DiversityTypeHelp[DiversityGainAWGN]<>"\n\n"<>MethodHelp[DiversityGainAWGN, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
DiversityGainAWGN[M_,\[Gamma]_,Pf_,Pd_,OptionsPattern[]] := Module[{method = ProcessMethod[OptionValue[Method]], tolerance = Log[10, 1/OptionValue[Tolerance]], diversityType, n, n0, f},
	{diversityType, n0} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		method == "Exact",
			n0 = DiversityGainAWGN[M, \[Gamma], Pf, Pd, DiversityType -> OptionValue[DiversityType], Method -> "ApproximateLowSNR"];
			f[n_?IntegerQ] := ProbabilityOfDetectionAWGN[M, n \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> "Exact"], DiversityType -> {diversityType, n}, Method -> "Exact"];
			Which[
				f[n0] > Pd,
					While[f[n0] > Pd, n0--];
					n0 + 1,
				f[n0] < Pd,
					While[f[n0] < Pd, n0++];
					n0
			],
		method == "ApproximateLowSNR",
			Which[
				diversityType == "MRC" || diversityType == "EGC",
					Sqrt[2 / M] ((InverseQ[Pf] - InverseQ[Pd]) / \[Gamma])//Ceiling,
				diversityType == "SLC",
					(2 / M) ((InverseQ[Pf] - InverseQ[Pd]) / \[Gamma])^2//Ceiling
			],
		True,
			Undefined
	]
]


End[];


EndPackage[];
