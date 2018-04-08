(* ::Package:: *)

nloops=4;


If[$MachineName=="julio-mba", SetDirectory[NotebookDirectory[]]];
<<vacuum_iso_bubbles.m
Get["toplevel_canonical_"<>ToString[nloops]<>".m"]


(* We define associations for each of the top level graphs *)
nprop=Length[Union@@Abs[top[1]]];
ClearAll[family,representatives,classes]
For[i=1,i<=ntop,i++,
  family[i] = Association[{nprop->{top[i]}}]
];
Table[representatives[i]=family[i],{i,ntop}];
Table[classes[i]=List/@family[i],{i,ntop}];


(* We obtain all the daughters down to nloops propagators up to isomophisms, sliding bubbles, and loop level drop *)
If[$Notebooks,ProgressIndicator[Dynamic[i/ntop]]]
If[$Notebooks,ProgressIndicator[Dynamic[j/(nprop-nloops)]]]
For[i=1,i<=ntop,i++,
  For[j=1,j<=(nprop-nloops),j++,
    AppendTo[family[i],(nprop-j)-> Select[#,nLoops[#]==nloops&]&@(collapsePropagator[top[i],#]&/@Subsets[Fold[Union,Abs@top[i]],{j}])];
    AppendTo[representatives[i],(nprop-j)-> Union[family[i][nprop-j],SameTest->isomorphicQuptoBubbles]];
  ]
]


allfamilies=Merge[family/@Range[ntop],Catenate];


(* We merge all four associations taking into account common daughers*)
basiswfact=Union[#,SameTest->isomorphicQuptoTadpoles]&/@Merge[representatives/@Range[ntop],Union[Flatten[#,Depth[#]-4]&@#,SameTest->isomorphicQuptoBubbles]&];
(* The counting matches that of the thesis *)
Length/@basiswfact


If[$Notebooks,ProgressIndicator[Dynamic[x/(nprop-nloops)]]]
ClearAll[basisclasseswfact]
basisclasseswfact=<||>;
For[x=0,x<=(nprop-nloops),x++,
AppendTo[basisclasseswfact, (nprop-x)->Table[Select[allfamilies[nprop-x],isomorphicQuptoBubbles[#,basiswfact[nprop-x][[i]]]&],{i,Length@basiswfact[nprop-x]}]]
]


(* Now we drop graphs with tadpoles and graphs with dangling sunsets (i.e., factorized graphs) *)
basisnofact=(Select[#,(Not[hasTadpolesQ[#]]&&Not[hasDanglingSunsetQ[#]])&]&/@basiswfact);
(* And the counting again matches*)
Length/@basisnofact


If[$Notebooks,ProgressIndicator[Dynamic[x/7]]]
ClearAll[basisclassesnofact]
basisclassesnofact=<||>;
For[x=0,x<= (nprop-nloops),x++,
AppendTo[basisclassesnofact, (nprop-x)->Table[Select[allfamilies[nprop-x],isomorphicQuptoBubbles[#,basisnofact[nprop-x][[i]]]&],{i,Length@basisnofact[nprop-x]}]]
]


autoTable=Map[automorphismRules]/@basisnofact;


isoTables=Association@@Table[(nprop-i)->Table[(isomorphismRules[#[[3]],#[[2]]]&@findVacuumRep[#,Association[(nprop-i)->{basisnofact[nprop-i][[j]]}]])&/@basisclassesnofact[nprop-i][[j]],{j,Length@basisnofact[nprop-i]}],{i,0,(nprop-nloops)}];


savefile="tables/vacuum_basis_"<>ToString[nloops]<>".m";
If[FileExistsQ[savefile],DeleteFile[savefile]]
Save[savefile,{basisnofact,basisclassesnofact,basiswfact,basisclasseswfact,autoTable,isoTables}]


basisfactorized=(Select[#,(hasTadpolesQ[#]||hasDanglingSunsetQ[#])&]&/@basiswfact);
factorized=(Select[#,(hasTadpolesQ[#]||hasDanglingSunsetQ[#])&]&@@@basisclasseswfact);


savefile="tables/factorized_"<>ToString[nloops]<>".m";
If[FileExistsQ[savefile],DeleteFile[savefile]]
Save[savefile,{basisfactorized,factorized}]
