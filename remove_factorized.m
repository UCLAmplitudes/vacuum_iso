(* ::Package:: *)

level=ToExpression@$CommandLine[[2]];
diagram=ToExpression@$CommandLine[[3]];
dimension=ToExpression@$CommandLine[[4]];


If[$Notebooks, SetDirectory[NotebookDirectory[]];];
<<vacuum_iso_bubbles.m
Get["./expansion_prep/expansion_prep_N"<>ToString[level]<>".m"];


readfile="vacExpand_N"<>ToString[level]<>"_"<>ToString[diagram]<>"_"<>ToString[dimension]<>".exp";
savefile="vacExpand_nofact_N"<>ToString[level]<>"_"<>ToString[diagram]<>"_"<>ToString[dimension]<>".exp";
parent=Last@vacReps[level][[diagram]];


OpenRead[readfile];
OpenWrite[savefile];
line=ReadLine[readfile];
While[line=!=EndOfFile,
  powers=Cases[List@@ToExpression@StringDrop[line,-1],F[a__]:>a];
  If[Not@Or[hasTadpolesQ@#,hasDanglingSunsetQ@#]&@multToGraph[powers,parent],
  WriteLine[savefile,line];
  ];
  line=ReadLine[readfile]
]
Close[readfile];
Close[savefile];
