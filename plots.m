(* ::Package:: *)

(* ::Title:: *)
(*Plots package*)


(* ::Text:: *)
(*This file contains common plot options and functions for my thesis.*)


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
(*28/02/2013*)
(*1.0*)


(* ::Subsection:: *)
(*Changelog*)


(* ::Text:: *)
(*Version 1.0: First working version.*)


(* ::Section:: *)
(*Packages*)


(* ::Text:: *)
(*Load this quietly - it's marked as obsolete in Mathematica 9.*)


Quiet[<<PlotLegends`];


(* ::Section:: *)
(*Colours*)


ThesisColours = {ThesisBlue, ThesisRed, ThesisGreen, ThesisOrange, ThesisBrown, ThesisNavy, ThesisYellow, ThesisPurple, ThesisTorquoise, ThesisGrey};


ThesisFaintColours = {ThesisFaintBlue, ThesisFaintRed, ThesisFaintGreen, ThesisFaintOrange, ThesisFaintBrown, ThesisFaintNavy, ThesisFaintYellow, ThesisFaintPurple, ThesisFaintTorquoise, ThesisFaintGrey};


ThesisVeryFaintColours = {ThesisVeryFaintBlue, ThesisVeryFaintRed, ThesisVeryFaintGreen, ThesisVeryFaintOrange, ThesisVeryFaintBrown, ThesisVeryFaintNavy, ThesisVeryFaintYellow, ThesisVeryFaintPurple, ThesisVeryFaintTorquoise, ThesisVeryFaintGrey};


(* ::Subsection:: *)
(*Primary colours*)


ThesisBlue = RGBColor[10/255, 154/255, 196/255];
ThesisRed = RGBColor[194/255, 71/255, 4/255];
ThesisGreen = RGBColor[59/255, 178/255, 54/255];


(* ::Subsection:: *)
(*Supplementary colours*)


ThesisOrange = RGBColor[217/255, 111/255, 7/255];
ThesisBrown = RGBColor[119/255, 59/255, 0/255];
ThesisNavy = RGBColor[3/255, 54/255, 68/255];
ThesisPurple = RGBColor[206/255, 31/255, 84/255];
ThesisYellow = RGBColor[245/255, 164/255, 30/255];
ThesisTorquoise = RGBColor[0/255, 173/255, 167/255];
ThesisGrey = RGBColor[62/255, 59/255, 54/255];


(* ::Subsection:: *)
(*Primary colours (faint)*)


ThesisFaintBlue = Opacity[0.6, ThesisBlue];
ThesisFaintGreen = Opacity[0.6, ThesisGreen];
ThesisFaintRed = Opacity[0.8, ThesisRed];


(* ::Subsection:: *)
(*Supplementary colours (faint)*)


ThesisFaintOrange = Opacity[0.6, ThesisOrange];
ThesisFaintBrown = Opacity[0.6, ThesisBrown];
ThesisFaintNavy = Opacity[0.6, ThesisNavy];
ThesisFaintPurple = Opacity[0.6, ThesisPurple];
ThesisFaintYellow = Opacity[0.6, ThesisYellow];
ThesisFaintTorquoise = Opacity[0.6, ThesisTorquoise];
ThesisFaintGrey = Opacity[0.6, ThesisGrey]


(* ::Subsection:: *)
(*Primary colours (very faint)*)


ThesisVeryFaintBlue = Opacity[0.3, ThesisBlue];
ThesisVeryFaintGreen = Opacity[0.3, ThesisGreen];
ThesisVeryFaintRed = Opacity[0.4, ThesisRed];


(* ::Subsection:: *)
(*Supplementary colours (very faint)*)


ThesisVeryFaintOrange = Opacity[0.3, ThesisOrange];
ThesisVeryFaintBrown = Opacity[0.3, ThesisBrown];
ThesisVeryFaintNavy = Opacity[0.3, ThesisNavy];
ThesisVeryFaintPurple = Opacity[0.3, ThesisPurple];
ThesisVeryFaintYellow = Opacity[0.3, ThesisYellow];
ThesisVeryFaintTorquoise = Opacity[0.3, ThesisTorquoise];
ThesisVeryFaintGrey = Opacity[0.3, ThesisGrey]


(* ::Section:: *)
(*Image sizes*)


ThesisLaTeXPageWidth = 412.56497;


IEEETCOM = 1.2 * 252;
IEEETCOMDraft = 1.2 * 469.755;


ThesisImageSize = 1.2 ThesisLaTeXPageWidth / GoldenRatio;
ThesisImageSize2 = 0.49 * 1.2 ThesisLaTeXPageWidth;


DisplayImageSize = 500;


(* ::Section:: *)
(*Plot markers*)


DisplayPlotMarkerSize = 8;


PlotMarkerSize = IEEETCOM ((DisplayPlotMarkerSize + 1) / DisplayImageSize);
PlotMarkerSizeDraft = IEEETCOMDraft ((DisplayPlotMarkerSize + 1) / DisplayImageSize);


Markers[colour_, size_] := {HollowCircleMarker[colour, size], SolidPlusMarker[colour, size], HollowTriangleMarker[colour, size], SolidCrossMarker[colour, size], HollowInvertedTriangleMarker[colour, size], SolidStarMarker[colour, size], SolidTriangleMarker[colour, size], SolidInvertedTriangleMarker[colour, size]};


(* ::Subsection:: *)
(*Empty plot marker*)


NoMarker = Graphics[];


(* ::Subsection:: *)
(*Square markers*)


SquareMarkers = {SolidSquareMarker, HollowSquareMarker};


SolidSquareMarker[colour_, size_] := Graphics[{colour, Rectangle[]}, ImageSize -> size];
HollowSquareMarker[colour_, size_] := Graphics[{colour, Line[{{0,0},{0,1},{1,1},{1,0},{0,0}}]}, ImageSize -> size];


(* ::Subsection:: *)
(*Circle markers*)


CircleMarkers = {SolidCircleMarker, HollowCircleMarker};


SolidCircleMarker[colour_, size_] := Graphics[{colour, Disk[]}, ImageSize -> size];
HollowCircleMarker[colour_, size_] := Graphics[{colour, Circle[]}, ImageSize -> size];


(* ::Subsection:: *)
(*Triangle markers*)


TriangleMarkers = {SolidTriangleMarker, SolidInvertedTriangleMarker, HollowTriangleMarker, HollowInvertedTriangleMarker};


SolidTriangleMarker[colour_, size_] := Graphics[{colour, Polygon[{{0,0},{0.5,1},{1,0}}]}, ImageSize -> size];
SolidInvertedTriangleMarker[colour_, size_] := Graphics[{colour, Polygon[{{0,0},{0.5,-1},{1,0}}]}, ImageSize -> size];
HollowTriangleMarker[colour_, size_] := Graphics[{colour, Line[{{0,0},{0.5,1},{1,0},{0,0}}]}, ImageSize -> size];
HollowInvertedTriangleMarker[colour_, size_] := Graphics[{colour, Line[{{0,0},{0.5,-1},{1,0},{0,0}}]}, ImageSize -> size];


(* ::Subsection:: *)
(*Plus markers*)


PlusMarkers = {SolidPlusMarker};


SolidPlusMarker[colour_, size_] := Graphics[{colour, Line[{{1/2, 0}, {1/2, 1}}], Line[{{0, 1/2}, {1, 1/2}}]}, ImageSize -> size];


(* ::Subsection:: *)
(*Cross markers*)


CrossMarkers = {SolidCrossMarker};


SolidCrossMarker[colour_, size_] := Graphics[{colour, Line[{{1/4 (2 - Sqrt[2]), 1/4 (2 - Sqrt[2])}, {1 - 1/4 (2 - Sqrt[2]), 1 - 1/4 (2 - Sqrt[2])}}], Line[{{1/4 (2 - Sqrt[2]), 1 - 1/4 (2 - Sqrt[2])}, {1 - 1/4 (2 - Sqrt[2]), 1/4 (2 - Sqrt[2])}}]}, ImageSize -> size];


(* ::Subsection:: *)
(*Star markers*)


StarMarkers = {SolidStarMarker};


SolidStarMarker[colour_, size_] := Graphics[{colour, Line[{{1/4 (2 - Sqrt[2]), 1/4 (2 - Sqrt[2])}, {1 - 1/4 (2 - Sqrt[2]), 1 - 1/4 (2 - Sqrt[2])}}], Line[{{1/4 (2 - Sqrt[2]), 1 - 1/4 (2 - Sqrt[2])}, {1 - 1/4 (2 - Sqrt[2]), 1/4 (2 - Sqrt[2])}}], Line[{{1/2, 0}, {1/2, 1}}], Line[{{0, 1/2}, {1, 1/2}}]}, ImageSize -> size];
