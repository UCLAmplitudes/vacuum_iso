(* ::Package:: *)

If[$MachineName=="julio-mba",
  SetDirectory[NotebookDirectory[]];
  <<VacuumData5Loops;,
  Get["/u/project/bern/bern/fourloop/bcj/math_integrateN8/VacuumData5Loops"]
];
<<vacuum_iso_bubbles.m;
<<fiveloop.m
<<vacuum_basis.m;


(* ::Subchapter::Closed:: *)
(*Some Easy Tests*)


testgraphs=cont[0][[{5,6,7}]];
{graphPlot[#],hasBubbleLikeQ[#]}&/@testgraphs
findBubbleLike/@testgraphs
findBubbleLike[testgraphs[[-1]],2] (* There is one bubble in the last diagram *)
findBubbleLike[testgraphs[[-1]],3] (* But there are no sunsets *)


(* Now diagrams with a sunset inside *)
sunsetgraph=cont[2][[26]];
sunsetdaughters={sunsetgraph,collapsePropagator[sunsetgraph,1],collapsePropagator[sunsetgraph,10],collapsePropagator[sunsetgraph,{10,1}]};
{graphPlot[#],hasBubbleLikeQ[#]}&/@sunsetdaughters (* The last one is not detected because the sunset cannot be moved *)
hasBubbleLikeQ[#,2]&/@sunsetdaughters(* There are no bubbles *)
hasBubbleLikeQ[#,3,-1]&/@sunsetdaughters (* But there are sunsets *)
hasBubbleLikeQ[#,3,2]&/@sunsetdaughters 
findBubbleLike[#,3,-1]&/@sunsetdaughters


(* Now we take each graph and slide the sunsets arround *)
graphPlot/@slideBubbles[sunsetdaughters[[1]]]
graphPlot/@slideBubbles[sunsetdaughters[[2]]]
graphPlot/@slideBubbles[sunsetdaughters[[3]]]


(* ::Subchapter::Closed:: *)
(*Timing test*)


testgraph=cont[0][[-5]]
testcontact=collapsePropagator[testgraph,{12,11}]
graphPlot/@{testgraph,testcontact}
isomorphicQuptoBubblesOld[testgraph,testcontact]//AbsoluteTiming
isomorphicQuptoBubbles[testgraph,testcontact]//AbsoluteTiming


AbsoluteTiming[isomorphicQuptoBubblesOld[#,collapsePropagator[#,{11,12}]]][[1]]&/@cont[0]
AbsoluteTiming[isomorphicQuptoBubbles[#,collapsePropagator[#,{11,12}]]][[1]]&/@cont[0]


(* ::Subchapter::Closed:: *)
(*Finding vacuum representatives and isomorphism*)


graphPlot@multToGraph[{2,2,1,1,1,1,1,1,1,1,0,0},cont[0][[-5]]]
findVacuumRep[multToGraph[{2,2,0,1,-1,-1,1,1,1,1,0,0},cont[0][[-5]]],basiswfact]


diag=400;
{testgraph1,testgraph2}=findVacuumRep[toVacuum[Diag[diag]],basiswfact];
graphPlot/@%
isomorphismRule[testgraph1,testgraph2]


vacreps=Table[findVacuumRep[toVacuum[Diag[i]],basiswfact],{i,1,410}];
vacrepsnumbers=Table[findVacuumRep[toVacuum[Diag[i]],basiswfact,True],{i,1,410}];
vacrepdots=graphToMult/@((collapsePropagator[#,Range[4]]&/@(Diag/@Range[410]))/.{a_Integer :> Sign[a](Abs[a]-4)});
vacrepisos=isomorphismRule@@@vacreps;


If[FileExistsQ["vacuum_representatives.m"],DeleteFile["vacuum_representatives.m"]]
Save["vacuum_representatives.m",{vacreps,vacrepsnumbers,vacrepdots,vacrepisos}]
