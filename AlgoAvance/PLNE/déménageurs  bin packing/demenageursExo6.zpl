param fichier := "u120_00.bpa" ;
param capacite := read fichier as "1n" skip 1 use 1;
param nbObjets := read fichier as "2n" skip 1 use 1;

set Objets := {0 to (nbObjets-1)} ;
set Boites := {0 to (nbObjets-1)} ;
set tmp[<i> in Objets] := {read fichier as "<1n>" skip 2+i use 1};
param taille[<i> in Objets] := ord(tmp[i],1,1);


var x[Objets*Boites] binary;
var y[Boites] binary;

minimize valeur : sum<i> in Objets : y[i];
subto presenceObjets : forall<i> in Objets : sum<j> in Boites : x[i,j] == 1;
subto utilBoite : forall<i> in Objets : forall<j> in Boites : x[i,j] <= y[j];
subto maxCapacite : forall<j> in Boites : sum<i> in Boites : x[i,j] * taille[i] <= capacite;