(* ::Package:: *)

<<"/u/project/bern/aedison/Amplitudes/int5loop/relabelNum.m";


<<JLink`;
InstallJava[];
ReinstallJava[JVMArguments -> "-Xmx1024m"]


<<"/u/project/bern/aedison/Amplitudes/int5loop/dotsAndNumsInterface.m";


propLevel=nprop;
diagNumber=ndiag;

Print["Running SQL prep and insert with "<>ToString[propLevel]<>","<>ToString[diagNumber]];

sqlAutos = SQLExecute[dotsAndNumsConn,"SELECT rowid,auto,loopRel FROM autos WHERE propNum=? AND diagNum =?;",{propLevel,diagNumber}];
sqlAutos = sqlAutos/.{SQLExpr->Identity};

(*Build the dot relabeling table for this diagram*)
inserts=Reap[Do[
repRule = relabeling[[1]];

numerLocs = Position[repRule[[1]],0]//Flatten;
allAutos = Sort/@(DeleteCases[relabeling[[2]],Rule[a_,b_]/;MemberQ[numerLocs,a]&&MemberQ[numerLocs,b],Infinity]);
neededSQLAutos = Select[sqlAutos,MemberQ[allAutos,#[[2]]]&];
propPower = Plus@@(repRule[[1]]);
numerPowers = DeleteCases[propPower-{11,12},x_/;x<=0];

bestAuto = selectBestAuto[numerLocs,neededSQLAutos,numerPowers];
Sow[{repRule[[1]],bestAuto[[1]],repRule[[2]]},dots];
If[numerPowers =!= {},
Sow[{#,bestAuto[[1]],bestAuto[[3]],numerLocs},nums]&/@numerPowers;
];

,{allDots,dotsRules[propLevel,diagNumber][[2;;]]},{relabeling,allDots}],{dots,nums}][[2]];

Print["Data gathered, reformatting..."];
(*Reformat dots data*)
toInsert = {ToString[FromDigits[List@@#[[1]]]],#[[2]],ToString[FromDigits[List@@#[[3]]]]}&/@(inserts[[1,1]]);

(*Process the numerator information and load into DB*)
numData=inserts[[2,1]]//Union;
numRels=Flatten[Table[rels=relabWithPower[numD[[4]],numD[[3]],deg];{rels[[1]],numD[[2]],rels[[2]]},{numD,numData},{deg,findAllDegree[numD[[4]],{numD[[1]]}]}],1];
numInsert = {ToString[FromDigits[-List@@#[[1]]]],#[[2]],ToString[#[[3]]]}&/@numRels;


(*dotsCanonicalInsert[dotsCanonical[propLevel,diagNumber]];*)

insertIntoDots[]:=(SQLBeginTransaction[dotsAndNumsConn];
SQLExecute[dotsAndNumsConn,"INSERT INTO dots VALUES(?,?,?);",toInsert];
SQLCommitTransaction[dotsAndNumsConn];
)


insertIntoNums[]:=(SQLBeginTransaction[dotsAndNumsConn];
SQLExecute[dotsAndNumsConn,"INSERT INTO nums VALUES(?,?,?);",numInsert];
SQLCommitTransaction[dotsAndNumsConn];
)

Print["Reformat done.  Inserting..."];
insertDotsWithCheck[]:=Check[insertIntoDots[],Print["Busy...Waiting 10 seconds and trying again."];Pause[10];insertDotsWithCheck[]];

insertCanonWithCheck[]:=Check[dotsCanonicalInsert[dotsCanonical[propLevel,diagNumber]],Print["Busy...Waiting 10 seconds and trying again."];Pause[10];insertCanonWithCheck[]];

insertNumsWithCheck[]:=Check[insertIntoNums[],Print["Busy...Waiting 10 seconds and trying again."];Pause[10];insertNumsWithCheck[]];

insertDotsWithCheck[];
insertNumsWithCheck[];
insertCanonWithCheck[];