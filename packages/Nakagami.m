(* ::Package:: *)

(* ::Title:: *)
(*Nakagami-m channel functions*)


(* ::Subsection::Closed:: *)
(*Copyright notice*)


(* ::Text:: *)
(*Mathematica function definitions for energy detection in Nakagami-m channels.*)
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
(*1.92*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.93: More efficient calculation of truncation points and new ApproximationErrorNakagamiTotal function.*)
(*Version 1.92: Added SignalToNoiseRatioDistributionNakagamiEGC function and rewrote ApproximationErrorNakagamiEGCPDF.*)
(*Version 1.91: Rewrote ApproximateLowSNR method, added error bounds and test statistic PDF. Removed Herath's EGC method (not working).*)
(*Version 1.90: Complete rewrite, removed SLS / SC / SEC code (it was incomplete), and disabled function timing (might re-enable this in the future if I have time). Huge code clean up too, should be relatively invisible.*)
(*Version 1.80: Added tighter low SNR error bound function. Only valid for no diversity, EGC, MRC and SLC cases. May add support for other diversity when PhD is over.*)
(*Version 1.71: Improved the computation time of the TruncationPointNakagamiExactAnnamalai function.*)
(*Version 1.70: Added no diversity, MRC and EGC methods to the LargeSNRProbabilityOfDetectionNakagami function. Added LargeSNR sample complexity algorithms for no diversity, MRC, EGC and SLC.*)
(*Version 1.63: Added asymptotic error bounds to the SampleComplexityNakagami function.*)
(*Version 1.62: Amalgamated algorithms and methods.*)
(*Version 1.61: Created NumericalLowSNR method instead of using LowSNR option.*)
(*Version 1.60: Major updates for SampleComplexityNakagami: added diversity support, exact and approximate methods.*)
(*Version 1.59: Fixed minor bugs in limit functions.*)
(*Version 1.58: Moved timing functions to the main function, and minor bug fixes.*)
(*Version 1.57: Moved FaddeevaDerivative function to the Extras package.*)
(*Version 1.56: Added approximaton error for Nakagami's PDF for EGC diversity.*)
(*Version 1.55: Fixed a bug in the asymptotic error bound function.*)
(*Version 1.54: Added asymptotic error bound functions for EGC and MRC.*)
(*Version 1.53: Added SEC support to IntegerMN, NGaussian and Numerical methods. Herath's method supports SEC with n = 2, i.e. SSC.*)
(*Version 1.52: Added new EGC methods to IntegerMN and Asymptotic algorithms, based on Nakagami's approximation for the sum of iid Nakagami rvs. Dharmawansa's exact PDF-based method is no longer enabled.*)
(*Version 1.51: Improved exact numerical method with smooth kernel distributions and caching.*)
(*Version 1.5: Finished polishing up error bound functions. SLS bound is very loose, but works.*)
(*Version 1.49: Moved error bounds to separate section, added LowSNRAssumptionErrorNakagami from AWGN package.*)
(*Version 1.48: Updated all function help definitions and fixed bug where LargeSNR algorithm was not publicly accessible.*)
(*Version 1.47: Moved ProcessDiversityType and ProcessSNR functions to the Extras package.*)
(*Version 1.46: Finished updating IntegerMN method with FaddeevaDerivative-based algorithms.*)
(*Version 1.45: Updated Asymptotic approximation for no diversity, MRC, SLC and SLS diversity cases.*)
(*Version 1.44: Recoded Numerical, Sun, Herath, Digham and Annamalai's methods to use new ProcessDiversityType and ProcessSNR functions. All methods are much easier to read and understand now.*)
(*Version 1.43: Retitled LargeMN method to Lopez-Benitez method, and recoded for speed. Also added Asymptotic method.*)
(*Version 1.42: Changed IntegerMN method to support faster computation with FaddeevaDerivative function.*)
(*Version 1.41: Added large SNR approximation method for SLC and no diversity types.*)
(*Version 1.4: Added an exact numeric method, minor bug fixes and retitled some methods.*)
(*Version 1.32: Added SC and SSC diversity to the IntegerMN method.*)
(*Version 1.31: Filled out all known diversity types for each exact method.*)
(*Version 1.3: Added MRC, EGC, SLC, SSC, SC and SLS diversity cases to all methods.*)
(*Version 1.2: Split Horgan's approximation into separate IntegerMN and LargeMN functions, so that full functionality can be accessed through the ProbabilityOfDetectionNakagami interface.*)
(*Version 1.11: Moved database logging functions to the Network package.*)
(*Version 1.1: Introduced RelevantOptions function and changed function definitions, so that child options are inherited from parents. The Gaussian approximation method is now called Horgan's approximation to avoid confusion with the numerical Gaussian method.*)
(*Version 1.0: First working version, minor bug fixes to follow.*)


(* ::Section:: *)
(*Public*)


BeginPackage["Nakagami`"]; 


(* ::Subsection::Closed:: *)
(*SignalToNoiseRatioPDFNakagami*)


SignalToNoiseRatioPDFNakagami;


(* ::Subsection::Closed:: *)
(*TestStatisticPDFNakagami*)


TestStatisticPDFNakagami;


(* ::Subsection::Closed:: *)
(*ProbabilityOfDetectionNakagami*)


ProbabilityOfDetectionNakagami;


TruncationPointNakagami;


(* ::Subsection::Closed:: *)
(*ApproximationErrorNakagami*)


ApproximationErrorNakagami;


(* ::Subsection::Closed:: *)
(*SampleComplexityNakagami*)


SampleComplexityNakagami;


(* ::Subsection::Closed:: *)
(*ReceiverSensitivityNakagami*)


ReceiverSensitivityNakagami;


(* ::Subsection::Closed:: *)
(*DiversityGainNakagami*)


DiversityGainNakagami;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


<<Help`;
<<EnergyDetectors`;
<<AWGN`;
Needs["ErfApprox`"];
Needs["QFunction`"];
Needs["Extras`"];
<<IErfc`;


(* ::Subsection::Closed:: *)
(*Help generation*)


GenerateMethodHelp[fName_, methodName_] := ToString[fName] <> "[M, \[Gamma], \[Lambda], m] calculates the probability of detection for a single energy detector operating in a Nakagami-m fading channel using the " <> methodName <> " method." <> "\n\n"<>DiversityTypeHelp[fName];


GenerateTruncationHelp[fName_, methodName_] := ToString[fName] <> "[M, \[Gamma], \[Lambda], m] calculates the truncation point for use in the " <> methodName <> " method for a single energy detector operating on a Nakagami-m channel." <> "\n\n"<>DiversityTypeHelp[fName] <> "\n\n"<>ToleranceHelp[fName];


(* ::Subsection::Closed:: *)
(*SignalToNoiseRatioPDFNakagami*)


(* ::Subsubsection::Closed:: *)
(*SignalToNoiseRatioPDFNakagami*)


Options[SignalToNoiseRatioPDFNakagami] = {Method -> "Exact", DiversityType -> OptionValue[ProbabilityOfDetection, DiversityType]};
SignalToNoiseRatioPDFNakagami::usage = "SignalToNoiseRatioPDFNakagami[\[Gamma], m, x] calculates the probability density function of the instantaneous signal to noise ratio for an energy detector operating in a Nakagami-m fading channel.\n\n"<>DiversityTypeHelp[SignalToNoiseRatioPDFNakagami]<>"\n\n"<>MethodHelp[SignalToNoiseRatioPDFNakagami, {"\"Exact\"", "\"Approximate\""}];
SignalToNoiseRatioPDFNakagami[\[Gamma]_, m_, x_, OptionsPattern[]] := Module[{diversityType, n, method = OptionValue[Method]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[
		method == "Exact" && diversityType == "EGC",
			PDF[SignalToNoiseRatioDistributionNakagamiEGC[\[Gamma], m, n], x],
			PDF[SignalToNoiseRatioDistributionNakagami[\[Gamma], m, DiversityType -> OptionValue[DiversityType], Method -> method], x]
	]
]


(* ::Subsubsection::Closed:: *)
(*SignalToNoiseRatioDistributionNakagami*)


Options[SignalToNoiseRatioDistributionNakagami] = {Method -> OptionValue[SignalToNoiseRatioPDFNakagami, Method], DiversityType -> OptionValue[SignalToNoiseRatioPDFNakagami, DiversityType]};
SignalToNoiseRatioDistributionNakagami::usage = "SignalToNoiseRatioDistributionNakagami[\[Gamma], m] returns the distribution of the signal to noise ratio for specified diversity type.\n\n"<>DiversityTypeHelp[SignalToNoiseRatioDistributionNakagami];
SignalToNoiseRatioDistributionNakagami[\[Gamma]_, m_, OptionsPattern[]] := Module[{r, diversityType, n, method = OptionValue[Method], \[Omega]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	\[Omega] = 1 + (n - 1) Gamma[m + 1 / 2]^2 / (m Gamma[m]^2);

	Which[
		method == "Exact" || method == "ExactNumerical",
			Which[
				diversityType == "ND",
					GammaDistribution[m, \[Gamma] / m],
				diversityType == "MRC" || diversityType == "SLC",
					GammaDistribution[m n, \[Gamma] / m],
				diversityType == "EGC",
					r = Table[Unique[], {n}];
					TransformedDistribution[Sum[Sqrt[r[[i]]], {i, n}]^2 / n, Table[r[[i]] \[Distributed] GammaDistribution[m, \[Gamma] / m], {i, n}]],
				True,
					Undefined
			],
		method == "Approximate" || method == "ApproximateNumerical" || method == "ApproximateNumericalLowSNR",
			Which[
				diversityType == "ND",
					GammaDistribution[m, \[Gamma] / m],
				diversityType == "MRC" || diversityType == "SLC",
					GammaDistribution[m n, \[Gamma] / m],
				diversityType == "EGC",
					GammaDistribution[m n, \[Omega] \[Gamma] / (m n)],
				True,
					Undefined
			],
		method == "ApproximateAsymptotic",
			Which[
				diversityType == "ND",
					NormalDistribution[\[Gamma], \[Gamma] / Sqrt[m]],
				diversityType == "MRC" || diversityType == "SLC",
					NormalDistribution[n \[Gamma], Sqrt[n]\[Gamma] / Sqrt[m]],
				diversityType == "EGC",
					NormalDistribution[n \[Gamma] \[Omega], Sqrt[n]\[Gamma] \[Omega] / Sqrt[m]],
				True,
					Undefined
			],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*SignalToNoiseRatioDistributionNakagamiEGC*)


Options[SignalToNoiseRatioDistributionNakagamiEGC] = {NumberOfPoints -> 10^6};
SignalToNoiseRatioDistributionNakagamiEGC[\[Gamma]_, m_, n_, OptionsPattern[]] := SignalToNoiseRatioDistributionNakagamiEGC[\[Gamma], m, n, OptionValue[NumberOfPoints]] = Module[{\[ScriptCapitalD], dist, numberOfPoints = OptionValue[NumberOfPoints]},
	\[ScriptCapitalD] = SignalToNoiseRatioDistributionNakagami[\[Gamma], m, DiversityType -> {"EGC", n}, Method -> "Exact"];
	SmoothKernelDistribution[RandomVariate[\[ScriptCapitalD], numberOfPoints]]
]


(* ::Subsection::Closed:: *)
(*TestStatisticPDFNakagami*)


(* ::Subsubsection::Closed:: *)
(*TestStatisticPDFNakagami*)


Options[TestStatisticPDFNakagami] = {Method -> OptionValue[ProbabilityOfDetection, Method], DiversityType -> OptionValue[ProbabilityOfDetection, DiversityType]};
TestStatisticPDFNakagami::usage="TestStatisticPDFNakagami[M, \[Gamma], \[Lambda], m] computes the probability density function of the test statistic for an energy detector with the specified diversity type operating on an occupied Nakagami-m channel.\n\n"<>DiversityTypeHelp[TestStatisticPDFNakagami]<>"\n\n"<>MethodHelp[TestStatisticPDFNakagami, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
TestStatisticPDFNakagami[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := PDF[TestStatisticDistributionNakagami[M, \[Gamma], m, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]], \[Lambda]]


(* ::Subsubsection::Closed:: *)
(*TestStatisticDistributionNakagami*)


Options[TestStatisticDistributionNakagami] = {Method -> OptionValue[ProbabilityOfDetection, Method], DiversityType -> OptionValue[ProbabilityOfDetection, DiversityType], NumberOfPoints -> 10^5};
TestStatisticDistributionNakagami::usage="TestStatisticDistributionNakagami[M, \[Gamma]] computes the distribution of the test statistic for an energy detector with the specified diversity type operating on an occupied Nakagami-m channel.\n\n"<>DiversityTypeHelp[TestStatisticDistributionNakagami]<>"\n\n"<>MethodHelp[TestStatisticDistributionNakagami, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\""}];
TestStatisticDistributionNakagami[M_?NumericQ, \[Gamma]_?NumericQ, m_?NumericQ, OptionsPattern[]] := TestStatisticDistributionNakagami[M, \[Gamma], m, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method], NumberOfPoints -> OptionValue[NumberOfPoints]] = Module[{method = ProcessMethod[OptionValue[Method]], x, numberOfPoints = OptionValue[NumberOfPoints], \[ScriptCapitalD]},
	\[ScriptCapitalD] = SignalToNoiseRatioDistributionNakagami[\[Gamma], m, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]];
	SmoothKernelDistribution[RandomVariate[ParameterMixtureDistribution[Evaluate[TestStatisticDistributionAWGN[M, x, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]]], x \[Distributed] \[ScriptCapitalD]], numberOfPoints], MaxRecursion -> 3]
]


(* ::Subsection::Closed:: *)
(*ProbabilityOfDetectionNakagami*)


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagami*)


Options[ProbabilityOfDetectionNakagami] = {Method -> OptionValue[ProbabilityOfDetection, Method], DiversityType -> OptionValue[ProbabilityOfDetection, DiversityType], Limit -> OptionValue[ProbabilityOfDetection, Limit](*, Timed -> OptionValue[ProbabilityOfDetection, Timed], MaxTime -> OptionValue[ProbabilityOfDetection, MaxTime], MaxIterations -> OptionValue[ProbabilityOfDetection, MaxIterations]*)};
ProbabilityOfDetectionNakagami::usage = "ProbabilityOfDetectionNakagami[M, \[Gamma], \[Lambda], m] calculates the probability of detection for an energy detector with the specified diversity type operating on a Nakagami-m fading channel.\n\n"<>MethodHelp[ProbabilityOfDetectionNakagami, {"\"ApproximateAsymptotic\"", "\"ApproximateLopezBenitez\"", "\"ApproximateLowSNR\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateSimple\"", "\"ExactAnnamalai\"", "\"ExactDigham\"", "\"ExactHerath\"", "\"ExactNumerical\"","\"ExactSun\""}]<>"\n\n"<>DiversityTypeHelp[ProbabilityOfDetectionNakagami];
ProbabilityOfDetectionNakagami[M_, \[Gamma]_, \[Lambda]_, m_, OptionsPattern[]] := Module[{method = OptionValue[Method], limit = OptionValue[Limit](*, f, totaltime = 0, iterations = 0, time, result*)},
	If[SameQ[limit, Undefined],
		limit = TruncationPointNakagami[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method]];
	];

	Which[
		(* Catch extreme values - they can cause errors *)
		TrueQ[\[Lambda] <= 0 || M == \[Infinity] || \[Gamma] == \[Infinity]],
			1,
		TrueQ[\[Lambda] == \[Infinity]],
			0,
		method == "ApproximateAsymptotic",
			ProbabilityOfDetectionNakagamiApproximateAsymptotic[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateLopezBenitez",
			ProbabilityOfDetectionNakagamiApproximateLopezBenitez[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateLowSNR",
			ProbabilityOfDetectionNakagamiApproximateLowSNR[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateNumerical",
			ProbabilityOfDetectionNakagamiApproximateNumerical[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateNumericalLowSNR",
			ProbabilityOfDetectionNakagamiApproximateNumericalLowSNR[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]],
		method == "ApproximateSimple",
			ProbabilityOfDetectionNakagamiApproximateSimple[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]],
		method == "ExactAnnamalai",
			ProbabilityOfDetectionNakagamiExactAnnamalai[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Limit -> limit],
		method == "ExactDigham",
			ProbabilityOfDetectionNakagamiExactDigham[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]],
		method == "ExactHerath",
			ProbabilityOfDetectionNakagamiExactHerath[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Limit -> limit],
		method == "ExactNumerical",
			ProbabilityOfDetectionNakagamiExactNumerical[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]],
		method == "ExactSun",
			ProbabilityOfDetectionNakagamiExactSun[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Limit -> limit],
		True,
			Undefined
	]
]

(* OLD CODE *)
(*	If[OptionValue[Timed],
		(* Evaluate result until MaxTime seconds of CPU time have been used or MaxIterations have been performed, whichever comes first *)
		While[totaltime < OptionValue[MaxTime] && iterations < OptionValue[MaxIterations],
			ClearSystemCache[];
			{time, result} = TimeConstrained[Timing[f],OptionValue[MaxTime],{OptionValue[MaxTime],Null}];
			totaltime += time;
			iterations++;
		];
		{result,totaltime/iterations},
		f
	]
*)


Options[TruncationPointNakagami] = {Method -> OptionValue[TruncationPoint, Method], DiversityType -> OptionValue[TruncationPoint, DiversityType], Tolerance -> OptionValue[TruncationPoint, Tolerance]};
TruncationPointNakagami::usage = "TruncationPointNakagami[M, \[Gamma], \[Lambda], m] calculates the truncation point for use in the specified method for calculating the probability of detection for an energy detector with the specified diversity type operating on a Nakagami-m fading channel.\n\n" <> DiversityTypeHelp[TruncationPointNakagami] <> "\n\n" <> MethodHelp[TruncationPointNakagami, {"\"ExactAnnamalai\"", "\"ExactHerath\"", "\"ExactSun\""}];
TruncationPointNakagami[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{method = OptionValue[Method]},
	Which[
		(* Catch extreme values - they can cause errors *)
		\[Lambda] <= 0 || M == \[Infinity] || \[Gamma] == \[Infinity] || \[Lambda] == \[Infinity],
			Undefined,
		method == "ExactAnnamalai",
			TruncationPointNakagamiExactAnnamalai[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Tolerance -> OptionValue[Tolerance]],
		method == "ExactHerath",
			TruncationPointNakagamiExactHerath[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Tolerance -> OptionValue[Tolerance]],
		method == "ExactSun",
			TruncationPointNakagamiExactSun[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Tolerance -> OptionValue[Tolerance]],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagamiApproximateAsymptotic*)


Options[ProbabilityOfDetectionNakagamiApproximateAsymptotic] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami, DiversityType]};
ProbabilityOfDetectionNakagamiApproximateAsymptotic::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiApproximateAsymptotic, "ApproximateAsymptotic"];
ProbabilityOfDetectionNakagamiApproximateAsymptotic[M_, \[Gamma]_, \[Lambda]_, m_, OptionsPattern[]] := Module[{diversityType, n, \[Omega]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	\[Omega] = 1 + (n - 1) Gamma[m + 1 / 2]^2 / (m Gamma[m]^2);

	Which[
		diversityType == "ND",
			Q[(\[Lambda] - M (1 + \[Gamma])) / Sqrt[2M (1 + M \[Gamma]^2 / (2 m))]],
		diversityType == "MRC",
			Q[(\[Lambda] - M (1 + n \[Gamma])) / Sqrt[2M (1 + M \[Gamma]^2 n / (2 m))]],
		diversityType == "EGC",
			Q[(\[Lambda] - M (1 + \[Omega] \[Gamma])) / Sqrt[2M (1 + M \[Gamma]^2 \[Omega]^2 / (2 m n))]],
		diversityType == "SLC",
			Q[(\[Lambda] - M n (1 + \[Gamma])) / Sqrt[2M n (1 + M \[Gamma]^2 / (2 m))]],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagamiApproximateLopezBenitez*)


Options[ProbabilityOfDetectionNakagamiApproximateLopezBenitez] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami,DiversityType]};
ProbabilityOfDetectionNakagamiApproximateLopezBenitez::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiApproximateLopezBenitez, "ApproximateLopezBenitez"];
ProbabilityOfDetectionNakagamiApproximateLopezBenitez[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{diversityType, n, G, a, b, c},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND",
			(1/2 (1+Erf[(m (M (1+\[Gamma])-\[Lambda]))/(Sqrt[2] M Sqrt[m] \[Gamma])])+1/2 E^((4 c M (-m+a M \[Gamma]^2)+b (-b M^2 \[Gamma]^2+2 Sqrt[2] m Sqrt[M] (M (1+\[Gamma])-\[Lambda]))-2 a m (-M (1+\[Gamma])+\[Lambda])^2)/(4 M (-m+a M \[Gamma]^2))) Sqrt[m/(m-a M \[Gamma]^2)] (Erf[(b Sqrt[M^3] \[Gamma]^2+Sqrt[2] m (-M (1+\[Gamma])+\[Lambda]))/(2 M \[Gamma] Sqrt[(m-a M \[Gamma]^2)])]-Erf[(-2 m+\[Gamma] (-2 a M+Sqrt[2] b Sqrt[M]+2 a \[Lambda]))/(2 Sqrt[2] Sqrt[(m-a M \[Gamma]^2)])])+1/2 E^(-((4 c M (m-a M \[Gamma]^2)+b (b M^2 \[Gamma]^2+2 Sqrt[2] m Sqrt[M] (M (1+\[Gamma])-\[Lambda]))+2 a m (-M (1+\[Gamma])+\[Lambda])^2)/(4 M (-m+a M \[Gamma]^2)))) Sqrt[m/(m-a M \[Gamma]^2)] (-2+Erfc[(b Sqrt[M^3] \[Gamma]^2+Sqrt[2] m (M (1+\[Gamma])-\[Lambda]))/(2 M \[Gamma] Sqrt[(m-a M \[Gamma]^2)])]))/.Table[ToExpression[(ToString/@{a, b, c})[[i]] <> " \[Rule] " <> ToString[LopezBenitezParameters[(-M (1+\[Gamma])+\[Lambda])/(2 Sqrt[M])][[i]]]], {i, 3}],
		diversityType == "MRC",
			(-((E^(-((b^2 M n \[Gamma]^2+4 c (m-a M n \[Gamma]^2)+(2 Sqrt[2] b m (M+M n \[Gamma]-\[Lambda]))/Sqrt[M]+(2 a m (M+M n \[Gamma]-\[Lambda])^2)/M)/(4 (-m+a M n \[Gamma]^2)))) m (1+Erf[(b M^(3/2) n \[Gamma]^2+Sqrt[2] m (M+M n \[Gamma]-\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M n \[Gamma]^2)])]))/(2 Sqrt[m (m-a M n \[Gamma]^2)]))+(E^(-((b^2 M n \[Gamma]^2+4 c (m-a M n \[Gamma]^2)-(2 Sqrt[2] b m (M+M n \[Gamma]-\[Lambda]))/Sqrt[M]+(2 a m (M+M n \[Gamma]-\[Lambda])^2)/M)/(4 (-m+a M n \[Gamma]^2)))) m (Erf[(Sqrt[2] b M^(3/2) n \[Gamma]^2-2 m (M+M n \[Gamma]-\[Lambda]))/(2 M n Sqrt[-2 a M+(2 m)/(n \[Gamma]^2)] \[Gamma]^2)]-Erf[(-2 m+\[Gamma] (Sqrt[2] b Sqrt[M]-2 a M+2 a \[Lambda]))/(2 Sqrt[-2 a M+(2 m)/(n \[Gamma]^2)] \[Gamma])]))/(2 Sqrt[m (m-a M n \[Gamma]^2)])+1/2 Erfc[-((m (M+M n \[Gamma]-\[Lambda]))/(Sqrt[2] M Sqrt[m n] \[Gamma]))])/.Table[ToExpression[(ToString/@{a, b, c})[[i]] <> " \[Rule] " <> ToString[LopezBenitezParameters[(-M (1+\[Gamma])+\[Lambda])/(2 Sqrt[M])][[i]]]], {i, 3}],
		diversityType == "SLC",
			(1/2 (1+Erf[(m (M n (1+\[Gamma])-\[Lambda]))/(Sqrt[2] M Sqrt[m n] \[Gamma])])+1/2 E^((4 c M n (-m+a M \[Gamma]^2)+b (-b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))-2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2))) Sqrt[m/(m-a M \[Gamma]^2)] (Erf[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (-M n (1+\[Gamma])+\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]-Erf[(-2 m n+\[Gamma] (-2 a M n+Sqrt[2] b Sqrt[M n]+2 a \[Lambda]))/(2 Sqrt[2] Sqrt[n (m-a M \[Gamma]^2)])])+1/2 E^(-((4 c M n (m-a M \[Gamma]^2)+b (b M^2 n \[Gamma]^2+2 Sqrt[2] m Sqrt[M n] (M n (1+\[Gamma])-\[Lambda]))+2 a m (-M n (1+\[Gamma])+\[Lambda])^2)/(4 M n (-m+a M \[Gamma]^2)))) Sqrt[m/(m-a M \[Gamma]^2)] (-2+Erfc[(b Sqrt[M^3 n] \[Gamma]^2+Sqrt[2] m (M n (1+\[Gamma])-\[Lambda]))/(2 M \[Gamma] Sqrt[n (m-a M \[Gamma]^2)])]))/.Table[ToExpression[(ToString/@{a, b, c})[[i]] <> " \[Rule] " <> ToString[LopezBenitezParameters[(-M (n+\[Gamma])+\[Lambda])/(2 Sqrt[M n])][[i]]]], {i, 3}],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagamiApproximateLowSNR*)


Options[ProbabilityOfDetectionNakagamiApproximateLowSNR] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami, DiversityType]};
ProbabilityOfDetectionNakagamiApproximateLowSNR::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiApproximateLowSNR, "ApproximateLowSNR"];
ProbabilityOfDetectionNakagamiApproximateLowSNR[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{diversityType, n, G, \[Omega]},
	(* This method can only be used when m * n is an integer *)
	If[Abs[m n - Round[m n]] > 10^-6, Return[Undefined]];
	
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	\[Omega] = 1 + (n - 1) Gamma[m + 1 / 2]^2 / (m Gamma[m]^2);

	G[k_, a_, b_, c_] := N[(Exp[(c^2) / (2 b^2) - a c / b] / 2) (Sqrt[2]c / b)^(k) IErfc[k, N[(c/b - a) / Sqrt[2], 100]], 100];
	
	Which[
		diversityType == "ND",
			ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[G[p, (\[Lambda] - M) / Sqrt[2 M], Sqrt[M / 2], m / \[Gamma]], {p, 0, m - 1}]],
		diversityType == "MRC",
			ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[G[p, (\[Lambda] - M) / Sqrt[2 M], Sqrt[M / 2], m / \[Gamma]], {p, 0, m n - 1}]],
		diversityType == "EGC",
			ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[G[p, (\[Lambda] - M) / Sqrt[2 M], Sqrt[M / 2], m  n/ (\[Omega] \[Gamma])], {p, 0, m n - 1}]],
		diversityType == "SLC",
			ProbabilityOfFalseAlarmAWGN[M, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "Approximate"] + Total[Table[G[p, (\[Lambda] - M n) / Sqrt[2 M n], Sqrt[M / (2 n)], m / \[Gamma]], {p, 0, m n - 1}]],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagamiApproximateNumerical*)


Options[ProbabilityOfDetectionNakagamiApproximateNumerical] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami, DiversityType]};
ProbabilityOfDetectionNakagamiApproximateNumerical::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiApproximateNumerical, "ApproximateNumerical"];
ProbabilityOfDetectionNakagamiApproximateNumerical[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := NIntegrate[ProbabilityOfDetectionAWGN[M, x, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "ApproximateNumerical"] SignalToNoiseRatioPDFNakagami[\[Gamma], m, x, DiversityType -> OptionValue[DiversityType], Method -> "ApproximateNumerical"], {x, 0, \[Infinity]}]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagamiApproximateNumericalLowSNR*)


Options[ProbabilityOfDetectionNakagamiApproximateNumericalLowSNR] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami, DiversityType]};
ProbabilityOfDetectionNakagamiApproximateNumericalLowSNR::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiApproximateNumericalLowSNR, "ApproximateNumericalLowSNR"];
ProbabilityOfDetectionNakagamiApproximateNumericalLowSNR[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := NIntegrate[ProbabilityOfDetectionAWGN[M, x, \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "ApproximateNumericalLowSNR"] SignalToNoiseRatioPDFNakagami[\[Gamma], m, x, DiversityType -> OptionValue[DiversityType], Method -> "ApproximateNumericalLowSNR"], {x, 0, \[Infinity]}]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagamiApproximateSimple*)


Options[ProbabilityOfDetectionNakagamiApproximateSimple] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami, DiversityType]};
ProbabilityOfDetectionNakagamiApproximateSimple::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiApproximateSimple, "ApproximateSimplified"];
ProbabilityOfDetectionNakagamiApproximateSimple[M_, \[Gamma]_, \[Lambda]_, m_, OptionsPattern[]] := Module[{diversityType, n, g, \[Omega]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	\[Omega] = 1 + (n - 1) Gamma[m + 1 / 2]^2 / (m Gamma[m]^2);

	g[k_, a_, b_, c_] := ProbabilityOfFalseAlarmAWGN[M, \[Lambda], n, Method -> "Approximate", DiversityType -> diversityType] + (1 - ProbabilityOfFalseAlarmAWGN[M, \[Lambda], n, Method -> "Approximate", DiversityType -> diversityType]) GammaRegularized[k, - a c / b];

	Which[
		diversityType == "ND",
			g[m, (\[Lambda] - M) / (2 Sqrt[M]), - Sqrt[M] / 2, m / \[Gamma]],
		diversityType == "MRC",
			g[m n, (\[Lambda] - M) / (2 Sqrt[M]), - Sqrt[M] / 2, m / \[Gamma]],
		diversityType == "EGC",
			With[{\[Beta] = (Gamma[m + 1]^2 + m (n - 1) Gamma[m + 1 / 2]^2) / (n Gamma[m + 1]^2)},
				g[m n, (\[Lambda] - M) / (2 Sqrt[M]), - Sqrt[M] / 2, m / (\[Beta] \[Gamma])]
			],
		diversityType == "SLC",
			g[m n, (\[Lambda] - M n) / (2 Sqrt[M n]), - Sqrt[M] / (2 Sqrt[n]), m / \[Gamma]],
		True,
			Undefined
	]
]




(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagamiExactAnnamalai*)


Options[ProbabilityOfDetectionNakagamiExactAnnamalai] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami,DiversityType], Limit -> Undefined};
ProbabilityOfDetectionNakagamiExactAnnamalai::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiExactAnnamalai, "ExactAnnamalai"];
ProbabilityOfDetectionNakagamiExactAnnamalai[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{limit = OptionValue[Limit], diversityType, n},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[SameQ[limit, Undefined],
		limit = TruncationPointNakagami[M, \[Gamma], \[Lambda], m, Method -> "ExactAnnamalai", DiversityType -> OptionValue[DiversityType]]
	];

	Which[
		diversityType == "ND",
			1 - ((m / (m + (M / 2) \[Gamma]))^m) (1 - GammaRegularized[M / 2, \[Lambda] / 2]) - Total[Table[(Gamma[m + k] / (Gamma[m] Gamma[k+1])) ((m / (m + (M / 2) \[Gamma]))^m) ((((M / 2) \[Gamma]) / (m + (M / 2) \[Gamma]))^k) (1 - GammaRegularized[M / 2 + k, \[Lambda] / 2]),{k, 1, limit}]//N],
		diversityType == "MRC",
			1 - (2m / (2m + M \[Gamma]))^(m n) (1 - GammaRegularized[M / 2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 \[Gamma]))^(m n) (((M/2 \[Gamma])/(m+M/2 \[Gamma]))^k) (1-GammaRegularized[M / 2 + k, \[Lambda] / 2]),{k, 1, limit}]//N],
		diversityType == "SLC",
			1 - (m / (m + (M / 2) \[Gamma]))^(m n) (1 - GammaRegularized[M n/2, \[Lambda] / 2]) - Total[Table[Gamma[m n+k]/(Gamma[m n]Gamma[k+1]) (m/(m+M/2 \[Gamma]))^(m n) (((M/2 \[Gamma])/(m+M/2 \[Gamma]))^k) (1-GammaRegularized[M n/2+k, \[Lambda] / 2]),{k, 1, limit}]//N],
		True,
			Undefined
	]//N
]


Options[TruncationPointNakagamiExactAnnamalai] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagamiExactAnnamalai, DiversityType], Tolerance -> OptionValue[TruncationPointNakagami, Tolerance]};
TruncationPointNakagamiExactAnnamalai::usage = GenerateTruncationHelp[TruncationPointNakagamiExactAnnamalai, "Annamalai"];
TruncationPointNakagamiExactAnnamalai[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]] := Module[{tol = OptionValue[Tolerance], diversityType, n, f, j, j0, j1},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND" || diversityType == "MRC",
			j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol] // Round;
			f[j_] := 1 - GammaRegularized[(M / 2) + j + 1, \[Lambda] / 2] - tol,
		diversityType == "SLC",
			j0 = (\[Lambda] / 2) - (M n / 2) - Sqrt[M n / 2] InverseQ[1 - tol] // Round;
			f[j_] := 1 - GammaRegularized[(M / 2) n + j + 1, \[Lambda] / 2] - tol,
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
(*ProbabilityOfDetectionNakagamiExactDigham*)


Options[ProbabilityOfDetectionNakagamiExactDigham] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami, DiversityType]};
ProbabilityOfDetectionNakagamiExactDigham::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiExactDigham, "ExactDigham"];
ProbabilityOfDetectionNakagamiExactDigham[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{diversityType, n},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND",
			Module[{A1, \[Beta]},
				A1 = Exp[-\[Lambda] \[Beta] / (2 m)] (\[Beta]^(m - 1) LaguerreL[m - 1, -\[Lambda] (1 - \[Beta]) / 2] + (1 - \[Beta]) Total[Table[\[Beta]^i LaguerreL[i,-\[Lambda] (1 - \[Beta]) / 2], {i, 0, m - 2}]]);
				\[Beta] = (2m) / (2m + M \[Gamma]);
				A1 + \[Beta]^(m) Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^i / i!) Hypergeometric1F1[m, i + 1, \[Lambda] (1 - \[Beta]) / 2], {i, 1, (M / 2) - 1}]]
			],
		diversityType == "SLC",
			Module[{A1, \[Beta]},
				A1 = Exp[-\[Lambda] \[Beta] / (2 m n)] (\[Beta]^(m n - 1) LaguerreL[m n - 1, -\[Lambda] (1 - \[Beta]) / 2] + (1 - \[Beta]) Total[Table[\[Beta]^i LaguerreL[i,-\[Lambda] (1 - \[Beta]) / 2], {i, 0, m n - 2}]]);
				\[Beta] = (2m) / (2m + M \[Gamma]);
				A1 + \[Beta]^(m n) Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^i / i!) Hypergeometric1F1[m n, i + 1, \[Lambda] (1 - \[Beta]) / 2], {i, 1, (M n / 2) - 1}]]
			],
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagamiExactHerath*)


Options[ProbabilityOfDetectionNakagamiExactHerath] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami, DiversityType], Limit -> Undefined};
ProbabilityOfDetectionNakagamiExactHerath::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiExactHerath, "ExactHerath"];
ProbabilityOfDetectionNakagamiExactHerath[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{limit = OptionValue[Limit], diversityType, n},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[SameQ[limit, Undefined],
		limit = TruncationPointNakagami[M, \[Gamma], \[Lambda], m, Method -> "ExactHerath", DiversityType -> OptionValue[DiversityType]]
	];

	Which[
		diversityType == "ND",
			1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))], {j, M / 2, M / 2 + limit}]//N],
		diversityType == "MRC",
			1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M / 2), M / 2 + limit}]//N],
		diversityType == "SLC",
			1 - Exp[-\[Lambda] / 2] (m / ((M / 2) \[Gamma] + m))^(n m) Total[Table[((\[Lambda] / 2)^j / j!) Hypergeometric1F1[m n, j + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))],{j, (M n / 2), M n / 2 + limit}]//N],
		True,
			Undefined
	]
]


Options[TruncationPointNakagamiExactHerath] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagamiExactHerath, DiversityType], Tolerance -> OptionValue[TruncationPointNakagami, Tolerance]};
TruncationPointNakagamiExactHerath::usage = GenerateTruncationHelp[TruncationPointNakagamiExactHerath, "Herath"];
TruncationPointNakagamiExactHerath[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]] := Module[{tol = OptionValue[Tolerance], diversityType, n, f, j, j0, j1, c},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND",
			c = (m / ((M / 2) \[Gamma] + m))^(m) Hypergeometric1F1[m, M / 2 + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))];
			j0 = M;
			f[j_]:= c (1 - GammaRegularized[M / 2 + j, \[Lambda] / 2]) - tol,
		diversityType == "MRC",
			c = (m / ((M / 2) \[Gamma] + m))^(n m) Hypergeometric1F1[m n, M / 2 + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))];
			j0 = M;
			f[j_]:= c (1 - GammaRegularized[M / 2 + j, \[Lambda] / 2]) - tol,
		diversityType == "SLC",
			c = (m / ((M / 2) \[Gamma] + m))^(n m) Hypergeometric1F1[m n, M n / 2 + 1, \[Lambda] (M / 2) \[Gamma] / (2 ((M / 2) \[Gamma] + m))];
			j0 = M n;
			f[j_]:= c (1 - GammaRegularized[M n / 2 + j, \[Lambda] / 2]) - tol,
		True,
			Undefined
	];

	j1 = If[f[0] <= 0,
		0,
		j/.FindRoot[f[j], {j, 1, Max[100, 2j0]}, Method->"Brent"]
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
(*ProbabilityOfDetectionNakagamiExactNumerical*)


Options[ProbabilityOfDetectionNakagamiExactNumerical] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami, DiversityType]};
ProbabilityOfDetectionNakagamiExactNumerical::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiExactNumerical, "ExactNumerical"];
ProbabilityOfDetectionNakagamiExactNumerical[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := 1 - CDF[TestStatisticDistributionNakagami[M, \[Gamma], m, DiversityType -> OptionValue[DiversityType], Method -> "Exact"], \[Lambda]]


(* ::Subsubsection::Closed:: *)
(*ProbabilityOfDetectionNakagamiExactSun*)


Options[ProbabilityOfDetectionNakagamiExactSun] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagami, DiversityType], Limit -> Undefined};
ProbabilityOfDetectionNakagamiExactSun::usage = GenerateMethodHelp[ProbabilityOfDetectionNakagamiExactSun, "ExactSun"];
ProbabilityOfDetectionNakagamiExactSun[M_?NumericQ, \[Gamma]_?NumericQ, \[Lambda]_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{limit = OptionValue[Limit], diversityType, n},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[SameQ[limit, Undefined],
		limit = TruncationPointNakagami[M, \[Gamma], \[Lambda], m, Method -> "ExactSun", DiversityType -> OptionValue[DiversityType]]
	];

	Which[
		diversityType == "ND",
			GammaRegularized[M / 2, \[Lambda] / 2] + Exp[-\[Lambda] / 2] Total[Table[((\[Lambda] / 2)^(j) / j!) (1 - (m / (m + (M / 2) \[Gamma]))^(m) Total[Table[(m + k - 1)! / (Gamma[m] k!) ((M / 2) \[Gamma] / (m + (M / 2) \[Gamma]))^(k),{k, 0, j - M / 2}]]),{j, M / 2, M / 2 + limit}]//N],
		diversityType == "MRC",
			1 - (m / (m + (M / 2) \[Gamma]))^(m n) Total[Table[(1 - GammaRegularized[(M / 2) + j, \[Lambda] / 2]) Pochhammer[m n, j] / j! ((M / 2) \[Gamma] / (m + (M / 2) \[Gamma]))^(j),{j, 0, limit}]//N],
		diversityType == "SLC",
			1 - (m / (m + (M / 2) \[Gamma]))^(m n) Total[Table[(1 - GammaRegularized[(M / 2) n + j, \[Lambda] / 2]) Pochhammer[m n, j] / j! ((M / 2) \[Gamma] / (m + (M / 2) \[Gamma]))^(j),{j, 0, limit}]//N],
		True,
			Undefined
	]
]


Options[TruncationPointNakagamiExactSun] = {DiversityType -> OptionValue[ProbabilityOfDetectionNakagamiExactSun, DiversityType], Tolerance -> OptionValue[TruncationPointNakagami, Tolerance]};
TruncationPointNakagamiExactSun::usage = GenerateTruncationHelp[TruncationPointNakagamiExactSun, "Sun"];
TruncationPointNakagamiExactSun[M_?NumericQ,\[Gamma]_,\[Lambda]_,m_?NumericQ,OptionsPattern[]] := Module[{tol = OptionValue[Tolerance], diversityType, n, f, j, j0, j1},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		diversityType == "ND",
			j0 = (\[Lambda] / 2) - (M / 2) - Sqrt[M / 2] InverseQ[1 - tol] // Round;
			f[j_]:=(1 - GammaRegularized[M / 2 + j - 1, \[Lambda] / 2]) (1 - CDF[NegativeBinomialDistribution[m, (m / (m + (M / 2) \[Gamma]))^(m)], j + 1]) - tol;
			
			j1 = If[f[0] <= 0,
				0,
				j/.FindRoot[f[j], {j, 0, Max[100, 2j0]}, Method->"Brent", AccuracyGoal->Log[10,1/tol]]
			]//Round;

			If[f[j1] < 0,
				j1,
				If[f[j1 + 1] < 0,
					j1 + 1,
					Print["Error: root does not meet tolerance requirements."];
					Abort[];
				]
			],
		diversityType == "MRC",
			TruncationPointNakagamiExactAnnamalai[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Tolerance -> tol],
		diversityType == "SLC",
			TruncationPointNakagamiExactAnnamalai[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Tolerance -> tol],
		True,
			Undefined
	]
]


(* ::Subsection::Closed:: *)
(*ApproximationErrorNakagami*)


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorNakagami*)


Options[ApproximationErrorNakagami] = {Method -> OptionValue[ApproximationError, Method], DiversityType -> OptionValue[ApproximationError, DiversityType]};
ApproximationErrorNakagami::usage="ApproximationErrorNakagami[M, \[Gamma], \[Lambda], m] calculates the specified approximation error for Nakagami-m channels.\n\n"<>DiversityTypeHelp[ApproximationErrorNakagami]<>"\n\n"<>MethodHelp[ApproximationErrorNakagami, {"\"ApproximateAsymptotic\"", "\"ApproximateLowSNR\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateSimple\"", "\"Asymptotic\"", "\"CentralLimitTheorem\"", "\"EGCPDF\"", "\"LowSNR\"", "\"Simple\""}];
ApproximationErrorNakagami[M_, \[Gamma]_, \[Lambda]_, m_, OptionsPattern[]] := Module[{method = OptionValue[Method], diversityType, n},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		(* Catch extreme values *)
		\[Lambda] == \[Infinity],
			0,
		method == "ApproximateAsymptotic" || method == "ApproximateLowSNR" || method == "ApproximateNumerical" || method == "ApproximateNumericalLowSNR" || method == "ApproximateSimple",
			ApproximationErrorNakagamiTotal[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Method -> method],
		method == "Asymptotic",
			ApproximationErrorNakagamiAsymptotic[m, n],
		method == "CentralLimitTheorem",
			ApproximationError[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> OptionValue[Method], ChannelType -> "AWGN"],
		method == "EGCPDF",
			ApproximationErrorNakagamiEGCPDF[\[Gamma], m, n],
		method == "LowSNR" || method == "LowSNRLowSNR" || method == "LowSNRMaximum" || method == "LowSNRHighSNR",
			ApproximationErrorNakagamiLowSNR[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Method -> method],
		method == "Simple",
			Undefined,
		True,
			Undefined
	]
]


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorNakagamiAsymptotic*)


ApproximationErrorNakagamiAsymptotic[m_, n_:1] := Max[Q[Sqrt[m n]], 1 / Sqrt[18 \[Pi] m n]]


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorNakagamiEGCPDF*)


Options[ApproximationErrorNakagamiEGCPDF] = {NumberOfPoints -> 10^6};
ApproximationErrorNakagamiEGCPDF[\[Gamma]_, m_, n_, OptionsPattern[]] := ApproximationErrorNakagamiEGCPDF[\[Gamma], m, n, NumberOfPoints -> OptionValue[NumberOfPoints]] = Module[{g, \[Epsilon], \[ScriptCapitalD]0, \[ScriptCapitalD]1},
	\[ScriptCapitalD]0 = SignalToNoiseRatioDistributionNakagamiEGC[\[Gamma], m, n, NumberOfPoints -> OptionValue[NumberOfPoints]];
	\[ScriptCapitalD]1 = SignalToNoiseRatioDistributionNakagami[\[Gamma], m, DiversityType -> {"EGC", n}, Method -> "Approximate"];

	g[\[Epsilon]_?NumericQ] := If[\[Epsilon] < 0, 0, Abs[CDF[\[ScriptCapitalD]0, \[Epsilon]] - CDF[\[ScriptCapitalD]1, \[Epsilon]]]];

	FindMaxValue[g[\[Epsilon]], {\[Epsilon], 0.0001}, Method -> "PrincipalAxis", WorkingPrecision -> 20]
]


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorNakagamiLowSNR*)


Options[ApproximationErrorNakagamiLowSNR] = {DiversityType -> OptionValue[ApproximationErrorNakagami, DiversityType], Method -> "Minimum"};
ApproximationErrorNakagamiLowSNR[M_, \[Gamma]_, \[Lambda]_, m_, OptionsPattern[]] := Module[{diversityType, n, method = OptionValue[Method], \[Omega]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	If[method == "Minimum" || method == "LowSNR",
		Return[Min[Flatten[{1 / Sqrt[2 \[Pi] E], Table[ApproximationErrorNakagamiLowSNR[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType], Method -> methods], {methods, {"LowSNRMaximum", "LowSNRLowSNR"}}]}]]]
	];

	Which[
		diversityType == "ND",
			Which[
				method == "LowSNRMaximum",
					(1 / \[Pi]) Sqrt[m / (2 M)],
				method == "LowSNRLowSNR",
					\[Gamma] / Sqrt[2 \[Pi] E],
				method == "LowSNRHighSNR",
					Which[
						\[Lambda] >= M,
							(1 / Sqrt[M \[Pi]]) (Gamma[1 + m / 2] / Gamma[m]) (m / \[Gamma])^m ((\[Lambda] - M + 2 Sqrt[M]) / M)^m,
						\[Lambda] < M,
							(1 / Sqrt[M \[Pi]]) (Gamma[1 + m / 2] / Gamma[m]) (m / \[Gamma])^m (2 / Sqrt[M])^m,
						True,
							Undefined
					],
				True,
					Undefined
			],
		diversityType == "MRC",
			Which[
				method == "LowSNRMaximum",
					(1 / \[Pi]) Sqrt[m n / (2 M)],
				method == "LowSNRLowSNR",
					n \[Gamma] / Sqrt[2 \[Pi] E],
				method == "LowSNRHighSNR",
					Undefined,
				True,
					Undefined
			],
		diversityType == "EGC",
			\[Omega] = 1 + (n - 1) Gamma[m + 1 / 2]^2 / (m Gamma[m]^2);
			Which[
				method == "LowSNRMaximum",
					(1 / \[Pi]) Sqrt[m n / (2 M)],
				method == "LowSNRLowSNR",
					\[Omega] \[Gamma] / Sqrt[2 \[Pi] E],
				method == "LowSNRHighSNR",
					Undefined,
				True,
					Undefined
			],
		diversityType == "SLC",
			Which[
				method == "LowSNRMaximum",
					(1 / \[Pi]) Sqrt[m / (2 M)],
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


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorNakagamiSimple*)


Options[ApproximationErrorNakagamiSimple]={DiversityType -> OptionValue[ApproximationErrorNakagami, DiversityType]};
ApproximationErrorNakagamiSimple[M_, \[Gamma]_, \[Lambda]_, m_?NumericQ, n_?NumericQ, OptionsPattern[]] := Module[{diversityType = OptionValue[DiversityType], \[Gamma], \[Gamma]t},
	(* Handle both lists and scalar values for diversityType *)
	{diversityType, \[Gamma]t} = ProcessDiversityType[diversityType];

	(* Convert lists of SNR values to averages or maxima, depending on the specified diversity type *)
	\[Gamma] = ProcessSNR[\[Gamma], diversityType];

	(* Check for invalid combinations of inputs *)
	If[diversityType == "ND" && n > 1, Return[Undefined]];
	If[\[Gamma] == Undefined, Return[Undefined]];

	Which[
		diversityType == "ND",
			With[{k = m, a = (\[Lambda] - M) / (2 Sqrt[M]), b = Sqrt[M] / 2, c = m / \[Gamma]},
				x0[R_] := Module[{x},(a b c + c x^2 + Sqrt[2 a b c^2 x^2 + b^2 c^2 x^2 + c^2 x^4])/(b^2)/.FindRoot[x E^-x^2 == fmaxRHS / R, {x, Sqrt[2], 1 / Sqrt[2], \[Infinity]}]];
				Max[Min[fmaxLHS (1 - GammaRegularized[k, a c/b]), 1/Sqrt[\[Pi]] (a k / c) Exp[-a^2 / (1 + 2 a / b)] (1 / (1 - 2 a b / c))^(k + 1) ((Exp[- (a c / b) (1 - 2 a b / c)] ((a c / b) (1 - 2 a b / c))^(k) / k!) + (1 - (k + 1) / ((a c / b) (1 - 2 a b / c))) (1 - GammaRegularized[k + 1, (a c / b) (1 - 2 a b / c)]))//Re],Min[fmaxRHS Gamma[k, a c/b]/Gamma[k], fmaxRHS Gamma[k, a c/b]/Gamma[k] - (fmaxRHS - fmaxRHS/R1) Gamma[k, x0[R1]]/Gamma[k] - (fmaxRHS/R1 - fmaxRHS/R2) Gamma[k, x0[R2]]/Gamma[k]]]
			],
		diversityType == "MRC" || diversityType == "EGC",
			With[{k = m n, a = (\[Lambda] - M) / (2 Sqrt[M]), b = Sqrt[M] / 2, c = m / \[Gamma]},
				x0[R_] := Module[{x},(a b c + c x^2 + Sqrt[2 a b c^2 x^2 + b^2 c^2 x^2 + c^2 x^4])/(b^2)/.FindRoot[x E^-x^2 == fmaxRHS / R, {x, Sqrt[2], 1 / Sqrt[2], \[Infinity]}]];
				Max[Min[fmaxLHS (1 - GammaRegularized[k, a c/b]), 1/Sqrt[\[Pi]] (a k / c) Exp[-a^2 / (1 + 2 a / b)] (1 / (1 - 2 a b / c))^(k + 1) ((Exp[- (a c / b) (1 - 2 a b / c)] ((a c / b) (1 - 2 a b / c))^(k) / k!) + (1 - (k + 1) / ((a c / b) (1 - 2 a b / c))) (1 - GammaRegularized[k + 1, (a c / b) (1 - 2 a b / c)]))//Re],Min[fmaxRHS Gamma[k, a c/b]/Gamma[k], fmaxRHS Gamma[k, a c/b]/Gamma[k] - (fmaxRHS - fmaxRHS/R1) Gamma[k, x0[R1]]/Gamma[k] - (fmaxRHS/R1 - fmaxRHS/R2) Gamma[k, x0[R2]]/Gamma[k]]]
			],
		diversityType == "SLC",
			With[{k = m n, a = (\[Lambda] - M n) / (2 Sqrt[M n]), b = Sqrt[M] / (2 Sqrt[n]), c = m / \[Gamma]},
				x0[R_] := Module[{x},(a b c n + c x^2 + Sqrt[2 a b c^2 n x^2 + b^2 c^2 n^2 x^2 + c^2 x^4])/(b^2 n)/.FindRoot[x E^-x^2 == fmaxRHS / R, {x, Sqrt[2], 1 / Sqrt[2], \[Infinity]}]];
				Max[Min[fmaxLHS (1 - GammaRegularized[k, a c/b]), 1/Sqrt[\[Pi]] (a k / (c n)) Exp[-a^2 / (1 + 2 a / (n b))] (1 / (1 - 2 a b / c))^(k + 1) ((Exp[- (a c / b) (1 - 2 a b / c)] ((a c / b) (1 - 2 a b / c))^(k) / k!) + (1 - (k + 1) / ((a c / b) (1 - 2 a b / c))) (1 - GammaRegularized[k + 1, (a c / b) (1 - 2 a b / c)]))//Re],Min[fmaxRHS Gamma[k, a c/b]/Gamma[k], fmaxRHS Gamma[k, a c/b]/Gamma[k] - (fmaxRHS - fmaxRHS/R1) Gamma[k, x0[R1]]/Gamma[k] - (fmaxRHS/R1 - fmaxRHS/R2) Gamma[k, x0[R2]]/Gamma[k]]]
			],
		True,
			Undefined
	]

	(*(0.1/\[Gamma])^n*)
	Block[{$MaxExtraPrecision=\[Infinity]},
	With[{k = m n, a = (\[Lambda] - M n) / (2 Sqrt[M n]), b = Sqrt[M] / (2 Sqrt[n]), c = m / \[Gamma]},
		(*1/2 NIntegrate[(y^(k-1) E^-y)/Gamma[k] (Erf[a - b/c y] - Erf[a]),{y,0,(a c)/b}] - 1/2 NIntegrate[(y^(k-1) E^-y)/Gamma[k] (Erf[b/c y - a] - 1),{y,(a c)/b,\[Infinity]}]*)
		(*1/2 NIntegrate[(y^(k-1) E^-y)/Gamma[k] (Erf[a - b/c y] - Erf[a]),{y,0,(a c)/b}] - NIntegrate[(-((((a c)/b)^(-1+k) E^(-((a c)/b)))/Gamma[k]))(y - (a c)/b)(b/(c Sqrt[\[Pi]]))E^-(a-b/c y)^2,{y,(a c)/b,\[Infinity]}]*)
		(*1/2 NIntegrate[(y^(k-1) E^-y)/Gamma[k] ((2 E^-a^2)/Sqrt[\[Pi]] (-((b y)/c))),{y,0,(a c)/b}] - 1/2 NIntegrate[(y^(k-1) E^-y)/Gamma[k] (Erf[b/c y - a] - 1),{y,(a c)/b,\[Infinity]}]*)
		(*N[Total[Table[(D[GammaRegularized[k,x],{x,p}]/.x -> (a c)/b)(-(c/b))^p Gamma[(1+p)/2]/(2 Sqrt[\[Pi]] p!) (-1)^p,{p,1,3}]],30]*)
		(*((a^(k-1) (c/b)^k)/Gamma[k])Total[Table[Binomial[k - 1, p] a^-p Gamma[1+p/2]/(2Sqrt[\[Pi]]),{p,0,k - 1}]]*)
(*Print[{10Log[10,\[Gamma]], (2 a b)/c}//N];*)
		(*b/(c Sqrt[\[Pi]]) ((*(k-Gamma[1+k,(a c)/b]/Gamma[k])*)NIntegrate[((y)^k E^(y((2 a b - c)/c)))/Gamma[k] E^(-a^2-(b^2 0^2)/c^2),{y,0,(a c)/b}] (*NIntegrate[(y^k E^-y)/Gamma[k] E^-(a-b/c (a c)/b)^2,{y,(a c)/b,\[Infinity]}]*))(* + (1/2)NIntegrate[(y^(k-1) E^-y)/Gamma[k] (Erf[b/c y - a] - 1),{y,(a c)/b,\[Infinity]}]*)*)
		(*Min[Re[1/(c Sqrt[\[Pi]] Gamma[k]) b (Gamma[1+k,(a c)/b]+(1-(2 a b)/c)^(-1-k) E^-a^2 (Gamma[1+k]-Gamma[1+k,(a (-2 a b+c))/b]))]//N, ((a^(k-1) (c/b)^k)/Gamma[k])Total[Table[Binomial[k - 1, p] a^-p Gamma[1+p/2]/(2Sqrt[\[Pi]]),{p,0,k - 1}]]//N,1]*)
		(*Min[-ProbabilityOfFalseAlarm[M, \[Lambda], n, DiversityType -> diversityType, Method -> "Approximate"] + ProbabilityOfDetection[M, \[Gamma], \[Lambda], n, ChannelType -> {"Nakagami", m}, DiversityType -> diversityType, Method -> "ApproximateLargeSNR"],1 - ProbabilityOfDetection[M, \[Gamma], \[Lambda], n, ChannelType -> {"Nakagami", m}, DiversityType -> diversityType, Method -> "ApproximateLargeSNR"]]*)
		(*NIntegrate[(y^(k-1) E^-y)/Gamma[k] E^(-a^2+(2 a b y)/c-((b 0)/c)^2)/Sqrt[\[Pi]] (b/c y),{y,0,(a c)/b}] + NIntegrate[(y^(k-1) E^(-((a c)/b)))/Gamma[k] E^(-a^2+(2 a b y)/c-((b y)/c)^2)/Sqrt[\[Pi]] (b/c y),{y,(a c)/b,\[Infinity]}]*)
		(*NIntegrate[(y^(k-1) E^-y)/Gamma[k] E^-a^2/Sqrt[\[Pi]] (b/c y),{y,0,(a c)/b}] - b/(c Sqrt[\[Pi]]) NIntegrate[((-E^(-((a c)/b)) ((a c)/b)^(k-1))/Gamma[k])(y-(a c)/b)E^-(a-b/c y)^2,{y,(a c)/b,\[Infinity]}]*)
		(*Total[Table[-1/Gamma[k] Binomial[k-1,p]((a c)/b)^k a^(-1-p) E^(-((a c)/b)) NIntegrate[(E^-z^2 E^(-((\[Phi] c z)/b)) z^(p+1))/Sqrt[\[Pi]],{z,-a,\[Infinity]}],{p,0,k-1}]]//N*)
		(*N[NIntegrate[GammaRegularized[k,y] E^-(a-b/c y)^2,{y,0,a c/b}] + NIntegrate[GammaRegularized[k,y] E^-(a-b/c y)^2,{y,a c/b,\[Infinity]}] - 1/2 (1+Erf[a]) GammaRegularized[k, a c/b],20]*)
		(*NIntegrate[(1)(b/(c Sqrt[\[Pi]]))E^-(a-b/c y)^2,{y,0,a c/b}] + NIntegrate[(0)(b/(c Sqrt[\[Pi]]))E^-(a-b/c y)^2,{y,a c/b,\[Infinity]}] - 1/2 (1+Erf[a]) GammaRegularized[k, a c/b]*)
		(* LHS upper bounds *)
		(*Min[NIntegrate[GammaRegularized[k, y](b/(c Sqrt[\[Pi]]))E^(-a^2+(2 a b y)/c-(b^2 0^2)/c^2),{y,0,a c/b}] - 1/2 (1+Erf[a]) GammaRegularized[k, a c/b], NIntegrate[GammaRegularized[k, y](b/(c Sqrt[\[Pi]])),{y,0,a c/b}] - 1/2 (1+Erf[a]) GammaRegularized[k, a c/b]]*)
		(*NIntegrate[GammaRegularized[k, y](b/(c Sqrt[\[Pi]]))E^(-a^2+(2 a b y)/c-(b^2 y^2)/c^2),{y,0,a c/b}] - GammaRegularized[k, a c/b](b/(c Sqrt[\[Pi]]))NIntegrate[E^(-a^2+(2 a b y)/c-(b^2 y^2)/c^2),{y,0,a c/b}]*)
		(*Min[NIntegrate[GammaRegularized[k, y](b/(c Sqrt[\[Pi]]))E^(-a^2+(2 a b y)/c-(b^2 0^2)/c^2),{y,0,a c/b}] - 1/2 (Erf[a]) GammaRegularized[k, a c/b], NIntegrate[GammaRegularized[k, y](b/(c Sqrt[\[Pi]])),{y,0,a c/b}] - 1/2 (Erf[a]) GammaRegularized[k, a c/b]]*)
		(*Min[NIntegrate[GammaRegularized[k, 0](b/(c Sqrt[\[Pi]]))E^(-a^2+(2 a b y)/c-(b^2 y^2)/c^2),{y,0,a c/b}] - 1/2 (Erf[a]) GammaRegularized[k, a c/b], ]*)
		(*NIntegrate[Total[Table[(E^-y y^p)/p!,{p,0,k-1}]](b/(c Sqrt[\[Pi]]))E^0,{y,0,a c/b}] - 1/2 (Erf[a]) GammaRegularized[k, a c/b]*)
		(*If[a c/b>k-1,Min[NIntegrate[GammaRegularized[k, 0](b/(c Sqrt[\[Pi]]))E^(-a^2+(2 a b y)/c-(b^2 y^2)/c^2),{y,0,a c/b}] - 1/2 (Erf[a]) GammaRegularized[k, a c/b], NIntegrate[GammaRegularized[k, y](b/(c Sqrt[\[Pi]]))E^(-a^2+(2 a b y)/c-(b^2 0^2)/c^2),{y,0,a c/b}] - 1/2 (Erf[a]) GammaRegularized[k, a c/b]],(a^(k-1) (c/b)^k E^(-((a c)/b)))/Gamma[k] Total[Table[Binomial[k - 1, p] a^-p (Gamma[1+p/2]/(2 Sqrt[\[Pi]])),{p,0,k - 1}]]]*)
		(* RHS upper bounds *)
		(*(a^(k-1) (c/b)^k E^(-((a c)/b)))/Gamma[k] Total[Table[Binomial[k - 1, p] a^-p (Gamma[1+p/2]/(2 Sqrt[\[Pi]])),{p,0,k - 1}]]*)
		(*-(NIntegrate[GammaRegularized[1, y](b/(c Sqrt[\[Pi]]))E^(-a^2+(2 a b y)/c-(b^2 y^2)/c^2),{y,a c/b,\[Infinity]}] - 1/2 GammaRegularized[k, a c/b])*)
		(* Derived bounds *)
		(*If[a c/b>k-1,-(((a c)/b)^k)Total[Table[(-(1/(2a)))^(p+1)/(Gamma[k-p]Gamma[(p+1)/2]) (1-Gamma[p/2+1,a^2]/Gamma[p/2+1]),{p,0,k - 1}]],E^(-((a c)/b)) (((a c)/b)^k)Total[Table[(1/(2a))^(p+1)/(Gamma[k-p]Gamma[(p+1)/2]),{p,0,k - 1}]]]*)
		(*Min[1,If[a c/b>k-1,Min[NIntegrate[GammaRegularized[k, 0](b/(c Sqrt[\[Pi]]))E^(-a^2+(2 a b y)/c-(b^2 y^2)/c^2),{y,0,a c/b}] - 1/2 (Erf[a]) GammaRegularized[k, a c/b], NIntegrate[GammaRegularized[k, y](b/(c Sqrt[\[Pi]]))E^(-a^2+(2 a b y)/c-(b^2 0^2)/c^2),{y,0,a c/b}] - 1/2 (Erf[a]) GammaRegularized[k, a c/b]],E^(-((a c)/b)) (((a c)/b)^k)Total[Table[(1/(2a))^(p+1)/(Gamma[k-p]Gamma[(p+1)/2]),{p,0,k - 1}]]]]*)
		(*If[a c/b>k-1,Min[1/(2 a Sqrt[\[Pi]] Gamma[k]) (-2 a b+c)^-k E^-a^2 ((-2 a b+c)^k (-Gamma[k]+E^a^2 (E^a^2-a Sqrt[\[Pi]] Erf[a]) Gamma[k,(a c)/b])+c^k (Gamma[k]-Gamma[k,(a (-2 a b+c))/b]))//Re, Erf[a]/2 (1-GammaRegularized[k, a c/b])], Min[E^(-((a c)/b)) (((a c)/b)^k)Total[Table[(1/(2a))^(p+1)/(Gamma[k-p]Gamma[(p+1)/2]),{p,0,k - 1}]], 1/2 (1-GammaRegularized[k, a c/b])]]//Abs*)
		(*Max[Min[1/(2 a Sqrt[\[Pi]] Gamma[k]) (-2 a b+c)^-k E^-a^2 (((-2 a b+c)^k) (-Gamma[k]+E^a^2 (E^a^2-a Sqrt[\[Pi]] Erf[a]) Gamma[k,(a c)/b])+(c^k) (Gamma[k]-Gamma[k,(a (-2 a b+c))/b]))//Re,Erf[a]/2 (1-GammaRegularized[k,a c/b])], Min[E^(-((a c)/b)) (((a c)/b)^k)Total[Table[(1/(2a))^(p+1)/(Gamma[k-p]Gamma[(p+1)/2]),{p,0,k - 1}]], 1/2]]*)

		(* New bounds *)
		Max[Min[E^-a^2/(2a Sqrt[\[Pi]]) ((1/(1-(2a b)/c))^k-1)+GammaRegularized[k, (a c)/b](E^a^2/(2a Sqrt[\[Pi]])-1/2),Erf[a]/2 (1-GammaRegularized[k,a c/b])], Min[1/2 (1-E^(c/(2b))^2 Erfc[c/(2b)])GammaRegularized[k, (a c)/b] + GammaRegularized[k-1, (a c)/b] (c/(2b))(c/(2b) E^(c/(2b))^2 Erfc[c/(2b)]-1/Sqrt[\[Pi]]),E^(-((a c)/b)) (((a c)/b)^k)Total[Table[(1/(2a))^(p+1)/(Gamma[k-p]Gamma[(p+1)/2]),{p,0,k - 1}]]]]


		(* Test *)
		(*(-c/(2b))(2/Sqrt[\[Pi]] E^(-a^2-(a c)/b)-c/(2b) E^(-((a c)/b)) (1+Erf[a])) + (-(E^(-a^2-(a c)/b) (-8 a b^2 c Gamma[-1+k] Gamma[k]+2 a b c^2 E^a^2 Sqrt[\[Pi]] Gamma[-1+k] Gamma[k]+2 a b c^2 E^a^2 Sqrt[\[Pi]] Erf[a] Gamma[-1+k] Gamma[k]-8 a b^2 c E^((a c)/(2 b)) Gamma[k] Gamma[-1+k,(a c)/(2 b)]+4 a^2 b c^2 E^((a c)/(2 b)) Gamma[k] Gamma[-1+k,(a c)/(2 b)]+4 a b c^2 E^(a^2+(a c)/(2 b)) Sqrt[\[Pi]] Gamma[k] Gamma[-1+k,(a c)/(2 b)]-a^2 c^3 E^(a^2+(a c)/(2 b)) Sqrt[\[Pi]] Gamma[k] Gamma[-1+k,(a c)/(2 b)]+4 a b c^2 E^(a^2+(a c)/(2 b)) Sqrt[\[Pi]] Erf[a] Gamma[k] Gamma[-1+k,(a c)/(2 b)]-a^2 c^3 E^(a^2+(a c)/(2 b)) Sqrt[\[Pi]] Erf[a] Gamma[k] Gamma[-1+k,(a c)/(2 b)]+8 a b^2 c E^((a c)/(2 b)) Gamma[-1+k] Gamma[k,(a c)/(2 b)]-2 a b c^2 E^(a^2+(a c)/(2 b)) Sqrt[\[Pi]] Gamma[-1+k] Gamma[k,(a c)/(2 b)]-2 a b c^2 E^(a^2+(a c)/(2 b)) Sqrt[\[Pi]] Erf[a] Gamma[-1+k] Gamma[k,(a c)/(2 b)]))/(8 a b^3 Sqrt[\[Pi]] Gamma[-1+k] Gamma[k]))*)
	]//Abs
]
]


(* ::Subsubsection::Closed:: *)
(*ApproximationErrorNakagamiTotal*)


Options[ApproximationErrorNakagamiTotal] = {Method -> OptionValue[ApproximationErrorNakagami, Method], DiversityType -> OptionValue[ApproximationErrorNakagami, DiversityType]};
ApproximationErrorNakagamiTotal[M_, \[Gamma]_, \[Lambda]_, m_, OptionsPattern[]] := Module[{method = OptionValue[Method], diversityType, n},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	Which[
		method == "ApproximateAsymptotic",
			ApproximationErrorNakagamiLowSNR[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]] + ApproximationErrorNakagamiAsymptotic[m, n],
		method == "ApproximateNumerical",
			Which[
				diversityType == "ND" || diversityType == "MRC" || diversityType == "SLC",
					ApproximationError[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "CentralLimitTheorem", ChannelType -> "AWGN"],
				diversityType == "EGC",
					ApproximationError[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "CentralLimitTheorem", ChannelType -> "AWGN"] + ApproximationErrorNakagamiEGCPDF[\[Gamma], m, n],
				True,
					Undefined
			],
		method == "ApproximateLowSNR" || method == "ApproximateNumerical",
			Which[
				diversityType == "ND" || diversityType == "MRC" || diversityType == "SLC",
					ApproximationError[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "CentralLimitTheorem", ChannelType -> "AWGN"] + ApproximationErrorNakagamiLowSNR[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]],
				diversityType == "EGC",
					ApproximationError[M, \[Gamma], \[Lambda], DiversityType -> OptionValue[DiversityType], Method -> "CentralLimitTheorem", ChannelType -> "AWGN"] + ApproximationErrorNakagamiLowSNR[M, \[Gamma], \[Lambda], m, DiversityType -> OptionValue[DiversityType]] + ApproximationErrorNakagamiEGCPDF[\[Gamma], m, n, NumberOfPoints->10^5],
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
(*SampleComplexityNakagami*)


Options[SampleComplexityNakagami] = {DiversityType -> OptionValue[SampleComplexity, DiversityType], Method -> OptionValue[SampleComplexity, Method], Tolerance -> OptionValue[SampleComplexity, Tolerance]};
SampleComplexityNakagami::usage="SampleComplexityNakagami[\[Gamma], Pf, Pd, m] calculates the number of samples required to meet the specified decision probabilities in a Nakagami-m channel.\n\n"<>MethodHelp[SampleComplexityNakagami, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateAsymptotic\""}]<>"\n\n"<>DiversityTypeHelp[SampleComplexityNakagami];
SampleComplexityNakagami[\[Gamma]_?NumericQ, Pf_?NumericQ, Pd_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{diversityType, n, method = OptionValue[Method], tolerance = Log[10, 1/OptionValue[Tolerance]], f, M, \[Epsilon], \[Omega]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	\[Omega] = 1 + (n - 1) Gamma[m + 1 / 2]^2 / (m Gamma[m]^2);

	Quiet[Which[
		method == "ExactNumerical",
			f[x_?NumericQ] := ProbabilityOfDetection[x, \[Gamma], \[Lambda][x, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType -> {"Nakagami", m}, Method -> If[diversityType == "EGC", "ExactNumerical", "ExactAnnamalai"]];
			M/.FindRoot[f[M] == Pd, {M, SampleComplexity[\[Gamma], Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> {"Nakagami", m}, Method -> "ApproximateNumerical"], 1, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateNumerical",
			f[x_?NumericQ] := ProbabilityOfDetection[x, \[Gamma], \[Lambda][x, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType ->{"Nakagami", m}, Method -> "ApproximateNumerical"];
			M/.FindRoot[f[M] == Pd, {M, SampleComplexity[\[Gamma], Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> "AWGN", Method -> "ApproximateNumerical"], 1, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateNumericalLowSNR",
			f[x_?NumericQ] := ProbabilityOfDetection[x, \[Gamma], \[Lambda][x, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType ->{"Nakagami", m}, Method -> "ApproximateNumericalLowSNR"];
			M/.FindRoot[f[M] == Pd, {M, SampleComplexity[\[Gamma], Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> "AWGN", Method -> "ApproximateNumericalLowSNR"], 1, \[Infinity]}, AccuracyGoal -> tolerance],
		(* Keep two names for legacy reasons *)
		method == "ApproximateLargeSNR" || method == "ApproximateSimplified",
			Which[
				diversityType == "ND",
					2 (m InverseQ[Pf] / (\[Gamma] InverseGammaRegularized[m, (Pd - Pf) / (1 - Pf)]))^2,
				diversityType == "MRC",
					2 (m InverseQ[Pf] / (\[Gamma] InverseGammaRegularized[m n, (Pd - Pf) / (1 - Pf)]))^2,
				diversityType == "EGC",
					With[{\[Beta] = (Gamma[m + 1]^2 + m (n - 1) Gamma[m + 1 / 2]^2) / (n Gamma[m + 1]^2)},
						2 (m InverseQ[Pf] / (\[Beta] \[Gamma] InverseGammaRegularized[m n, (Pd - Pf) / (1 - Pf)]))^2
					],
				diversityType == "SLC",
					2 n (m InverseQ[Pf] / (\[Gamma] InverseGammaRegularized[m n, (Pd - Pf) / (1 - Pf)]))^2,
				True,
					Undefined
			],
		method == "ApproximateAsymptotic",
			Which[
				diversityType == "ND",
					2 ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / m] InverseQ[Pd]) / (\[Gamma] (1 - (InverseQ[Pd])^2 / m)))^2,
				diversityType == "MRC",
					(2 / n^2) ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (m n)] InverseQ[Pd]) / (\[Gamma] (1 - (InverseQ[Pd])^2 / (m n))))^2,
				diversityType == "EGC",
					2 ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (m n)] InverseQ[Pd]) / (\[Omega] \[Gamma] (1 - (InverseQ[Pd])^2 / (m n))))^2,
				diversityType == "SLC",
					(2 / n) ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (m n)] InverseQ[Pd]) / (\[Gamma] (1 - (InverseQ[Pd])^2 / (m n))))^2,
				True,
					Undefined
			],
		True,
			Undefined
	]]
]


(* ::Subsection::Closed:: *)
(*ReceiverSensitivityNakagami*)


Options[ReceiverSensitivityNakagami] = {DiversityType -> OptionValue[ReceiverSensitivity, DiversityType], Method -> OptionValue[ReceiverSensitivity, Method], Tolerance -> OptionValue[ReceiverSensitivity, Tolerance]};
ReceiverSensitivityNakagami::usage="ReceiverSensitivityNakagami[M, Pf, Pd, m] calculates the lowest detectable signal to noise ratio for the specified decision probabilities in a Nakagami-m channel.\n\n"<>MethodHelp[ReceiverSensitivityNakagami, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateAsymptotic\""}]<>"\n\n"<>DiversityTypeHelp[ReceiverSensitivityNakagami];
ReceiverSensitivityNakagami[M_?NumericQ, Pf_?NumericQ, Pd_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{diversityType, n, method = OptionValue[Method], tolerance = Log[10, 1/OptionValue[Tolerance]], f, \[Gamma], \[Epsilon], \[Omega]},
	{diversityType, n} = ProcessDiversityType[OptionValue[DiversityType]];

	\[Omega] = 1 + (n - 1) Gamma[m + 1 / 2]^2 / (m Gamma[m]^2);

	Quiet[Which[
		method == "ExactNumerical",
			f[x_?NumericQ] := ProbabilityOfDetection[M, x, \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType -> {"Nakagami", m}, Method -> If[diversityType == "EGC", "ExactNumerical", "ExactAnnamalai"]];
			\[Gamma]/.FindRoot[f[\[Gamma]] == Pd, {\[Gamma], ReceiverSensitivity[M, Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> {"Nakagami", m}, Method -> "ApproximateNumerical"], 0, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateNumerical",
			f[x_?NumericQ] := ProbabilityOfDetection[M, x, \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType -> {"Nakagami", m}, Method -> "ApproximateNumerical"];
			\[Gamma]/.FindRoot[f[\[Gamma]] == Pd, {\[Gamma], ReceiverSensitivity[M, Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> "AWGN", Method -> "ApproximateLowSNR"], 0, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateNumericalLowSNR",
			f[x_?NumericQ] := ProbabilityOfDetection[M, x, \[Lambda][M, Pf, DiversityType -> {diversityType, n}, Method -> method], DiversityType -> {diversityType, n}, ChannelType -> {"Nakagami", m}, Method -> "ApproximateNumericalLowSNR"];
			\[Gamma]/.FindRoot[f[\[Gamma]] == Pd, {\[Gamma], ReceiverSensitivity[M, Pf, Pd, DiversityType -> {diversityType, n}, ChannelType -> "AWGN", Method -> "ApproximateLowSNR"], 0, \[Infinity]}, AccuracyGoal -> tolerance],
		method == "ApproximateAsymptotic",
			Which[
				diversityType == "ND",
					Sqrt[2 / M] ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / m] InverseQ[Pd]) / ((1 - (InverseQ[Pd])^2 / m))),
				diversityType == "MRC",
					Sqrt[2 / (M n^2)] ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (m n)] InverseQ[Pd]) / ( (1 - (InverseQ[Pd])^2 / (m n)))),
				diversityType == "EGC",
					Sqrt[2 / M] ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (m n)] InverseQ[Pd]) / (\[Omega] (1 - (InverseQ[Pd])^2 / (m n)))),
				diversityType == "SLC",
					Sqrt[2 / (M n)] ((InverseQ[Pf] - Sqrt[1 + ((InverseQ[Pf])^2 - (InverseQ[Pd])^2) / (m n)] InverseQ[Pd]) / ( (1 - (InverseQ[Pd])^2 / (m n)))),
				True,
					Undefined
			],
		True,
			Undefined
	]]
]


(* ::Subsection::Closed:: *)
(*DiversityGainNakagami*)


Options[DiversityGainNakagami] = {DiversityType -> OptionValue[DiversityGain, DiversityType], Method -> OptionValue[DiversityGain, Method], Tolerance -> OptionValue[DiversityGain, Tolerance]};
DiversityGainNakagami::usage="DiversityGainNakagami[M, \[Gamma], Pf, Pd, m] calculates the minimum number of nodes required to meet the specified decision probabilities in a Nakagami-m channel.\n\n"<>MethodHelp[DiversityGainNakagami, {"\"ExactNumerical\"", "\"ApproximateNumerical\"", "\"ApproximateNumericalLowSNR\"", "\"ApproximateAsymptotic\""}]<>"\n\n"<>DiversityTypeHelp[DiversityGainNakagami];
DiversityGainNakagami[M_?NumericQ, \[Gamma]_?NumericQ, Pf_?NumericQ, Pd_?NumericQ, m_?NumericQ, OptionsPattern[]] := Module[{diversityType, n, n0, method = OptionValue[Method], tolerance = Log[10, 1/OptionValue[Tolerance]], f, \[Omega]},
	{diversityType, n0} = ProcessDiversityType[OptionValue[DiversityType]];

	\[Omega] = 1 + (n - 1) Gamma[m + 1 / 2]^2 / (m Gamma[m]^2);

	Which[
		method == "ExactNumerical" || method == "ApproximateNumerical" || method == "ApproximateNumericalLowSNR",
			Which[
				method == "ExactNumerical",
					n0 = DiversityGain[M, \[Gamma], Pf, Pd, DiversityType -> OptionValue[DiversityType], ChannelType -> {"Nakagami", m}, Method -> "ApproximateNumerical"];
					f[x_?IntegerQ] := ProbabilityOfDetection[M, \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, x}, Method -> method], DiversityType -> {diversityType, x}, ChannelType -> {"Nakagami", m}, Method -> If[diversityType == "EGC", "ExactNumerical", "ExactAnnamalai"]],
				method == "ApproximateNumerical",
					n0 = DiversityGain[M, \[Gamma], Pf, Pd, DiversityType -> OptionValue[DiversityType], ChannelType -> "AWGN", Method -> "ApproximateLowSNR"];
					f[x_?IntegerQ] := ProbabilityOfDetection[M, \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, x}, Method -> method], DiversityType -> {diversityType, x}, ChannelType -> {"Nakagami", m}, Method -> "ApproximateNumerical"],
				method == "ApproximateNumericalLowSNR",
					n0 = DiversityGain[M, \[Gamma], Pf, Pd, DiversityType -> OptionValue[DiversityType], ChannelType -> "AWGN", Method -> "ApproximateLowSNR"];
					f[x_?IntegerQ] := ProbabilityOfDetection[M, \[Gamma], \[Lambda][M, Pf, DiversityType -> {diversityType, x}, Method -> method], DiversityType -> {diversityType, x}, ChannelType -> {"Nakagami", m}, Method -> "ApproximateNumericalLowSNR"]
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
					(2 / M) ((InverseQ[Pf]-Sqrt[1+(M \[Gamma]^2)/(2m)]InverseQ[Pd])/\[Gamma])^2//Ceiling,
				True,
					Undefined
			],
		True,
			Undefined
	]
]


End[];


EndPackage[];
