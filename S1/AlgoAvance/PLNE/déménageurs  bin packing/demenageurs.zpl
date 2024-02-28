
param capacite := 9;
set Objets := {1 .. 24} ;
set Boites := {1 .. 24} ;

param taille[Objets] := <1> 6, <2> 6, <3> 5, <4> 5, <5> 5, <6> 4, <7> 4, <8> 4, 
                        <9> 4, <10> 2, <11> 2, <12> 2, <13> 2, <14> 3, <15> 3, 
                        <16> 7, <17> 7, <18> 5, <19> 5, <20> 8, <21> 8, <22> 4, 
                        <23> 4, <24> 5;

var x[Objets*Boites] binary;
var y[Boites] binary;

minimize valeur : sum<i> in Objets : y[i];
subto presenceObjets : forall<i> in Objets : sum<j> in Boites : x[i,j] == 1;
subto utilBoite : forall<i> in Objets : forall<j> in Boites : x[i,j] <= y[j];
subto maxCapacite : forall<j> in Boites : sum<i> in Boites : x[i,j] * taille[i] <= capacite;
