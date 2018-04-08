(* ::Package:: *)

(* Top level vacuum graphs with canonical labels *)

ntop = 1;

top[1] = {{-1, 2, 4}, {-2, 3, 6}, {-3, 1, -5}, {5, -4, -6}};


(* Choice of labels *)

vars = {q[1] = k[1], 
       q[2] = k[2], 
       q[3] = k[3], 
       q[4] = k[1] - k[2],
       q[5] = k[1] - k[3],
       q[6] = k[2] - k[3]};
