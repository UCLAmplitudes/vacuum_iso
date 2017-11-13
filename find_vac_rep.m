(* ::Package:: *)

level=ToExpression@$CommandLine[[2]];
solution=ToExpression@$CommandLine[[3]];
If[solution!=410&&solution!=752,Print["Please, choose solution with 410 or 720 diagrams"]; Quit[];]
savefile="expansion_prep/expansion_prep_N"<>ToString[level]<>"_"<>ToString[solution]<>".m";


If[$Notebooks, SetDirectory[NotebookDirectory[]];];
<<vacuum_iso_bubbles.m;
<<vacuum_basis.m;
Get["./cutlists/N"<>ToString[level]<>"MCutList_"<>ToString[solution]<>".m"];


diagramslist=ToExpression["N"<>ToString[level]<>"MC"];
ndiagrams=Length@diagramslist;


Print["Finding vacuum representatives for N"<>ToString[level]<>" diagrams..."];
vacreps=Table[findVacuumRep[toVacuum[diagramslist[[i]]],basisnofact],{i,1,ndiagrams}];//AbsoluteTiming//First
Print["Done"];


Print["Finding isomorphisms for N"<>ToString[level]<>" diagrams..."];
vacrepisos=If[#=={},{},isomorphismRule[#[[2]],#[[3]]]]&/@vacreps;//AbsoluteTiming//First
Print["Done"];


Print["Finding unsorted dots for N"<>ToString[level]<>" diagrams..."];
vacrepdotsunsorted=MapThread[findDots,{removeExtLegs/@remove1PRLegs/@diagramslist,If[#=={},{},Part[#,2]]&/@vacreps}];//AbsoluteTiming//First
Print["Done"];


Print["Sorting dots for N"<>ToString[level]<>" diagrams..."];
vacrepdots=MapThread[multIsoPerm[#1,#2,4]&,{vacrepdotsunsorted,vacrepisos}];//AbsoluteTiming//First
Print["Done"];


nDiagrams[level]=ndiagrams;
vacReps[level]=vacreps;
factorizedQ[level]=Equal[#,{}]&/@vacreps;
vacRepIsos[level]=vacrepisos;
vacRepDots[level]=If[Length[#]<15,Join[#,ConstantArray[0,15-Length[#]]],Drop[#,15-Length[#]]]&/@vacrepdots;
If[FileExistsQ[savefile],DeleteFile[savefile]]
Save[savefile,{nDiagrams,vacReps,factorizedQ,vacRepIsos,vacRepDots}];
