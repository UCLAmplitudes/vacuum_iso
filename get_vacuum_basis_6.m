(* ::Package:: *)

nloops=6;


If[$MachineName=="julio-mba", SetDirectory[NotebookDirectory[]]];
<<vacuum_iso_bubbles.m
Get["tables/toplevel_canonical_"<>ToString[nloops]<>".m"];


(* We define associations for each of the top level graphs *)
nprop=Length[Union@@Abs[top[1]]];
ClearAll[family,representatives,classes]
For[x=1,x<=ntop,x++,
  family[x] = Association[{nprop->{top[x]}}]
];
(*Table[representatives[i]=family[i],{i,ntop}];*)
Table[classes[i]=List/@family[i],{i,ntop}];


depthToDo =2;


(* We obtain all the daughters down to nloops propagators up to isomophisms, sliding bubbles, and loop level drop *)
If[$Notebooks,ProgressIndicator[Dynamic[i/ntop]]]
If[$Notebooks,ProgressIndicator[Dynamic[j/depthToDo]]]
For[i=1,i<=ntop,i++,
  For[j=1,j<=depthToDo,j++,
    AppendTo[family[i],(nprop-j)-> Select[#,nLoops[#]==nloops&]&@(collapsePropagator[top[i],#]&/@Subsets[Fold[Union,Abs@top[i]],{j}])];
    (*AppendTo[representatives[i],(nprop-j)-> Union[family[i][nprop-j],SameTest->isomorphicQuptoBubbles]];  Don't need representative now that we are calculating using Gather*)
  ]
]


If[$Notebooks,ProgressIndicator[Dynamic[diagNum/ntop]]]
Do[
Clear[basisnofact,basisclassesnofact,basiswfact,basisclasseswfact,autoTable,isoTables,basisfactorized,factorized];

basisclasseswfact[diagNum] = Gather[#,isomorphicQuptoBubbles]&/@family[diagNum];
basiswfact[diagNum] = #[[All,1]]&/@basisclasseswfact[diagNum];

basisclassesnofact[diagNum] = Select[#,Not[hasTadpolesQ[#[[1]]] ] &&Not[hasDanglingSunsetQ[#[[1]]]]&]&/@basisclasseswfact[diagNum];
basisnofact[diagNum] = #[[All,1]]&/@basisclassesnofact[diagNum];


(* Old way of doing it.  Does a lot of extra computations
basiswfact[diagNum]=Union[#,SameTest->isomorphicQuptoTadpoles]&/@representatives[diagNum];
basisnofact[diagNum] = (Select[#,(Not[hasTadpolesQ[#]]&&Not[hasDanglingSunsetQ[#]])&]&/@basiswfact[diagNum]);

basisclasseswfact[diagNum]=<||>;
For[x=0,x<=depthToDo,x++,
AppendTo[basisclasseswfact[diagNum], (nprop-x)->Table[Select[family[diagNum][nprop-x],isomorphicQuptoBubbles[#,basiswfact[diagNum][nprop-x][[k]]]&],{k,Length@basiswfact[diagNum][nprop-x]}]];
]

basisclassesnofact[diagNum]=<||>;
For[x=0,x<= depthToDo,x++,
AppendTo[basisclassesnofact[diagNum], (nprop-x)->Table[Select[family[diagNum][nprop-x],isomorphicQuptoBubbles[#,basisnofact[diagNum][nprop-x][[i]]]&],{i,Length@basisnofact[diagNum][nprop-x]}]];
];
*)

autoTable[diagNum]=Map[automorphismRules]/@basisnofact[diagNum];

isoTables[diagNum]=Association@@Table[(nprop-i)->Table[(isomorphismRules[#[[3]],#[[2]]]&@findVacuumRep[#,Association[(nprop-i)->{basisnofact[diagNum][nprop-i][[j]]}]])&/@basisclassesnofact[diagNum][nprop-i][[j]],
					{j,Length@basisnofact[diagNum][nprop-i]}],{i,0,depthToDo}];
					
savefile="tables/vacuum_basis_"<>ToString[nloops]<>"/vacuum_basis_"<>ToString[nloops]<>"_"<>ToString[diagNum]<>".m";
If[FileExistsQ[savefile],DeleteFile[savefile]];
Save[savefile,{basisnofact,basisclassesnofact,basiswfact,basisclasseswfact,autoTable,isoTables}];

basisfactorized[diagNum]=(Select[#,(hasTadpolesQ[#]||hasDanglingSunsetQ[#])&]&/@basiswfact[diagNum]);
factorized[diagNum]=(Select[#,(hasTadpolesQ[#]||hasDanglingSunsetQ[#])&]&@@@basisclasseswfact[diagNum]);

savefile="tables/vacuum_basis_"<>ToString[nloops]<>"/factorized_"<>ToString[nloops]<>"_"<>ToString[diagNum]<>".m";
If[FileExistsQ[savefile],DeleteFile[savefile]];
Save[savefile,{basisfactorized,factorized}];

,{diagNum,14}]



