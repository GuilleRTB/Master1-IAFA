import sys
import random
import numpy as np
import math
import time

MAX_depl = 100
MAX_essais = 10


# Question 2.1
def initial_solution(villes) :
    random.shuffle(villes)
    return villes


# Question 2.2
def compute_dist(ville1, ville2):
    v1 = np.array((ville1[1],ville1[2]))
    v2 = np.array((ville2[1],ville2[2]))
    x1, y1 = v1
    x2, y2 = v2
    distance = math.sqrt((x2 - x1)**2 + (y2 - y1)**2)

    return distance

def calcul_dist_total(villes, taille):
    point_dep = [0,0,0]
    villedep = villes[0]
    som_dist = compute_dist(point_dep,villedep)

    for i in range(1,taille):
        som_dist += compute_dist(villedep,villes[i])
        villedep = villes[i]

    som_dist += compute_dist(villes[taille-1],point_dep)
    return som_dist


# Question 2.3
def get_voisins(villes, taille) :
    voisins = []
    for i in range(taille):
        for j in range(i+1,taille):
            voisin = np.array(villes)
            tempv = voisin[i]
            voisin[i] = voisin[j]
            voisin[j] = tempv
            voisins.append(voisin)
    return voisins

def meilleur_voisin(villes, taille) :
    voisins = get_voisins(villes, taille)
    meilleur_voisin = voisins[0]
    min_distv = calcul_dist_total(meilleur_voisin, taille)

    for i in range(1,taille):
        calcul_distv = calcul_dist_total(voisins[i], taille)

        if calcul_distv < min_distv :
            min_distv = calcul_distv
            meilleur_voisin = voisins[i]

    return min_distv,meilleur_voisin


# Question 2.4
def stetepest_hill_climbing(villes, taille):
    calcul_villes = calcul_dist_total(villes, taille)
    deplacement=0

    for i in range(MAX_depl):
        calcul_villes_v,villes_v = meilleur_voisin(villes, taille)
        if calcul_villes_v < calcul_villes:
            calcul_villes = calcul_villes_v
            villes = villes_v
            deplacement+=1
        else:
            return calcul_villes,villes,deplacement

    return calcul_villes,villes,deplacement




################################
#####        MAIN          #####
################################

start = time.time()

args = sys.argv[1:]
if args != []:
    nom_fichier = args[0]
else:
    print("Command Error: Usage: program.py <nom_fichier>")


with open(nom_fichier,'r') as f:
    i = 0

    taille = int(f.readline())
    villes = []
    ville = [0,0,0]

    for line in f:
        for val in line.split():
            if i == 0:
                ville[0] = int(val)
            if i == 1:
                ville[1] = int(val)
            if i == 2:
                ville[2] = int(val)
            i+=1
        i=0
        #print("--",id,"--",x,"--",y,"\n")
        villes.append(ville.copy())
    #print(villes)
    print(stetepest_hill_climbing(villes, taille))
