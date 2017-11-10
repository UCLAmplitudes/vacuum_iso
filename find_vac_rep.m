(* ::Package:: *)

level=ToExpression@$CommandLine[[2]];
savefile="expansion_prep/expansion_prep_N"<>ToString[level]<>".m";


If[$Notebooks, SetDirectory[NotebookDirectory[]];];
<<vacuum_iso_bubbles.m;
<<vacuum_basis.m;
Get["./anc/Level"<>ToString[level]<>"Diagrams.m"];


nDiagramsList={410,2473,7917,15156,19567,17305,10745};
(*nonZero={410,0,1758,3262,4587,4066,2804};*)

nDiagrams[level]=nDiagramsList[[level+1]];

nonZeroNum[level]=Catenate@Position[(ToExpression["NumerN"<>ToString[level]][#]===0)&/@Range[nDiagrams[level]],False];
nonZero[level]=Last[nonZeroNum[level]];

Print["There are "<>ToString[nDiagrams[level]]<>" N"<>ToString[level]<>" diagrams, of which "<>ToString[nonZero[level]]<>" are nonzero." ]


Print["Finding vacuum representatives for N"<>ToString[level]<>" diagrams..."];
vacReps[level]=Table[findVacuumRep[toVacuum[Join[Thread[-{Range[4]}],ToExpression["DiagramN"<>ToString[level]][i]]],basisnofact],{i,1,nonZero[level]}];
Print["Done"];


factorizedQ[level]=Equal[#,{}]&/@vacReps[level];


vacRepDotsUnord=graphToMult/@((collapsePropagator[#,Range[4]]&/@((Join[Thread[-{Range[4]}],ToExpression["DiagramN"<>ToString[level]][#]])&/@Range[nonZero[level]]))/.{a_Integer :> Sign[a](Abs[a]-4)});


vacRepIsos[level]=If[#=={},{},isomorphismRule[#[[2]],#[[3]]]]&/@vacReps[level];


vacRepDots[level]=If[Length[#]<15,Join[#,ConstantArray[0,15-Length[#]]],Drop[#,15-Length[#]]]&/@MapThread[multIsoPerm,{vacRepDotsUnord,vacRepIsos[level]}];


If[FileExistsQ[savefile],DeleteFile[savefile]]
Save[savefile,{nDiagrams,nonZero,factorizedQ,vacReps,vacRepIsos,vacRepDots}];


Quit[]
