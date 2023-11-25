import sys
import random
import numpy as np
import math

# Question 2.1
def initial_solution(villes) :
    random.shuffle(villes)
    return villes


# Question 2.2
def compute_dist(ville1, ville2):
    v1 = np.array((ville1.x,ville1.y))
    v2 = np.array((ville2.x,ville2.y))
    x1, y1 = v1
    x2, y2 = v2
    distance = math.sqrt((x2 - x1)**2 + (y2 - y1)**2)

    return distance

def calcul_dist_total(villes, taille):
    point_dep = Ville(0,0,0)
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
def steepest_hill_climbing(villes, taille) :
    