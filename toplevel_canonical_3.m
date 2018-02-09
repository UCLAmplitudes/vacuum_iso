(* ::Package:: *)

(* Top level vacuum graphs with canonical labels *)

ntop = 1;

top[1] = {{-1, 2, 5}, {-2, 3, 7}, {-3, 1, -6}, {6, -5, -7}};


(* Choice of labels *)

vars = {q[1] = k[1], 
       q[2] = k[2], 
       q[3] = k[3], 
       q[4] = k[4], 
       q[5] = k[1] - k[2],
       q[6] = k[1] - k[3],
       q[7] = k[2] - k[3]};
