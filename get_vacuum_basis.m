(* ::Package:: *)

If[$MachineName=="julio-mba", SetDirectory[NotebookDirectory[]]];
<<vacuum_iso_bubbles.m
<<toplevel_canonical.m


(* We define associations for each of the top level (12 edge) graphs *)
ClearAll[top,family,representatives,classes]
top=AssociationThread[Range[4],{cube,xcube,tennis,xtennis}];
family[1] = Association[{12->{top[1]}}];
family[2] = Association[{12->{top[2]}}];
family[3] = Association[{12->{top[3]}}];
family[4] = Association[{12->{top[4]}}];
Table[representatives[i]=family[i],{i,1,4}];
Table[classes[i]=List/@family[i],{i,1,4}];


(* We obtain all the daughters down to 5 propagators up to isomophisms, sliding bubbles, and loop level drop *)
If[$Notebooks,ProgressIndicator[Dynamic[i/4]]];
If[$Notebooks,ProgressIndicator[Dynamic[j/7]]];
For[i=1,i<5,i++,
  For[j=1,j<8,j++,
    AppendTo[family[i],(12-j)-> Select[#,nLoops[#]==5&]&@(collapsePropagator[top[i],#]&/@Subsets[Fold[Union,Abs@top[i]],{j}])];
    AppendTo[representatives[i],(12-j)-> Union[family[i][12-j],SameTest->isomorphicQuptoBubbles]];
  ]
]


allfamilies=Merge[family/@Range[4],Catenate];


(* We merge all four associations taking into account common daughers*)
basiswfact=Union[#,SameTest->isomorphicQuptoTadpoles]&/@Merge[representatives/@Range[4],Union[Flatten[#,Depth[#]-4]&@#,SameTest->isomorphicQuptoBubbles]&];
(* The counting matches that of the thesis *)
Length/@basiswfact


ClearAll[basisclasseswfact]
basisclasseswfact=<||>;
For[x=0,x<8,x++,
AppendTo[basisclasseswfact, (12-x)->Table[Select[allfamilies[12-x],isomorphicQuptoBubbles[#,basiswfact[12-x][[i]]]&],{i,Length@basiswfact[12-x]}]]
]


(* Now we drop graphs with tadpoles and graphs with dangling sunsets (i.e., factorized graphs) *)
basisnofact=(Select[#,(Not[hasTadpolesQ[#]]&&Not[hasDanglingSunsetQ[#]])&]&/@basiswfact);
(* And the counting again matches*)
Length/@basisnofact


If[$Notebooks,ProgressIndicator[Dynamic[x/7]]];
ClearAll[basisclassesnofact]
basisclassesnofact=<||>;
For[x=0,x<7,x++,
AppendTo[basisclassesnofact, (12-x)->Table[Select[allfamilies[12-x],isomorphicQuptoBubbles[#,basisnofact[12-x][[i]]]&],{i,Length@basisnofact[12-x]}]]
]


autoTable=Map[automorphismRules]/@basisnofact;


isoTables=Association@@Table[(12-i)->Table[(isomorphismRules[#[[2]],#[[3]]]&@findVacuumRep[#,Association[(12-i)->{basisnofact[12-i][[j]]}]])&/@basisclassesnofact[12-i][[j]],{j,Length@basisnofact[12-i]}],{i,0,7}];


If[FileExistsQ["vacuum_basis.m"],DeleteFile["vacuum_basis.m"]]
Save["vacuum_basis.m",{basisnofact,basisclassesnofact,basiswfact,basisclasseswfact,autoTable,isoTables}]


basisfactorized=(Select[#,(hasTadpolesQ[#]||hasDanglingSunsetQ[#])&]&/@basiswfact);
factorized=(Select[#,(hasTadpolesQ[#]||hasDanglingSunsetQ[#])&]&@@@basisclasseswfact);


If[FileExistsQ["factorized.m"],DeleteFile["factorized.m"]]
Save["factorized.m",{basisfactorized,factorized}]
