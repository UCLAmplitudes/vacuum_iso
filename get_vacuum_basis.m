(* ::Package:: *)

If[$MachineName=="julio-mba", SetDirectory[NotebookDirectory[]]];
<<vacuum_iso_bubbles.m
<<toplevel_canonical.m


(* First we check that the top graph labels align with our canonical form*)
Table[l/@Range[12]/.momCons[top[i],0]/.extrashift[i],{i,1,4}];
Join[{l/@Range[12]},%]//TableForm
(* There are 14 independent propagators and l[5] is the missing ISP *)
Length@(Union@@%%)


(* We define associations for each of the top level (12 edge) graphs *)
ClearAll[family]
family[1] = Association[{12->{cube}}];
family[2] = Association[{12->{xcube}}];
family[3] = Association[{12->{tennis}}];
family[4] = Association[{12->{xtennis}}];


(* We obtain all the daughers dow to 5 propagators up to isomophisms, sliding bubbles, and loop level drop *)
For[j=1,j<5,j++,
  For[i=1,i<8,i++,
    AppendTo[family[j],(12-i)-> Union[Select[#,nLoops[#]==5&]&@(collapsePropagator[top[j],#]&/@Subsets[Fold[Union,Abs@top[j]],{i}]),SameTest->isomorphicQuptoBubbles]]
  ]
]


(* We merge all four associations taking into account common daughers*)
basiswfact=Union[#,SameTest->isomorphicQuptoTadpoles]&/@Merge[family/@Range[4],Union[Flatten[#,Depth[#]-4]&@#,SameTest->isomorphicQuptoBubbles]&];


(* The counting matches that of the thesis *)
Length/@basiswfact


(* Now we drop graphs with tadpoles and graphs with dangling sunsets (i.e., factorized graphs) *)
basisnofact=(Select[#,(Not[hasTadpolesQ[#]]&&Not[hasDanglingSunsetQ[#]])&]&/@basiswfact);
(* And the counting again matches*)
Length/@basisnofact


If[FileExistsQ["vacuum_basis.m"],DeleteFile["vacuum_basis.m"]]
Save["vacuum_basis.m",{basisnofact,basiswfact}]
