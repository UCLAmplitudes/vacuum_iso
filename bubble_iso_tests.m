(* ::Package:: *)

If[$MachineName=="julio-mba",
  SetDirectory[NotebookDirectory[]];
  <<VacuumData5Loops;,
  Get["/u/project/bern/bern/fourloop/bcj/math_integrateN8/VacuumData5Loops"]
];
<<vacuum_iso_bubbles.m;


(* ::Subchapter:: *)
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


(* ::Subchapter:: *)
(*Other test*)


testgraph=cont[0][[-6]]
testcontact=collapsePropagator[testgraph,{12,11}]
graphPlot/@{testgraph,testcontact}
isomorphicQuptoBubblesOld[testgraph,testcontact]//Timing
isomorphicQuptoBubbles[testgraph,testcontact]//Timing


Timing[isomorphicQuptoBubblesOld[#,collapsePropagator[#,{11,12}]]][[1]]&/@cont[0]
Timing[isomorphicQuptoBubbles[#,collapsePropagator[#,{11,12}]]][[1]]&/@cont[0]



