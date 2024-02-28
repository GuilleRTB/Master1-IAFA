param fichier := "tsp5.txt" ;

set Villes := { read fichier as "1n" comment "#" skip 1, <0> } ;
param x[villes] := { read fichier as "<1s> 2n" comment "#" skip 1, <0> 0 } ;
param y[villes] := { read fichier as "<1s> 2n" comment "#" skip 1, <0> 0 } ;

var z{i in Villes, j in Villes, i != j} binary;

minimize distance: sum{i in Villes, j in Villes, i != j} sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2) * z[i, j];
subto visiteVilles : forall <i> in Villes : sum <j> in Villes : z[i, j] + sum <j> in Villes : z[j, i] = 1;
subto alleeRetour : forall <i, j> in Villes : i != j : z[i, j] + z[j, i] <= 1;