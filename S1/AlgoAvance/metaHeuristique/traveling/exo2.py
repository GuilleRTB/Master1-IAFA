import sys
import numpy as np
import random
import queue
import math
import time

MAX_depl = 100
MAX_essais = 10
KSIZE = 10

# Question 1.1

def initial_solution(villes, taille):
    random.shuffle(villes)

    return villes

def compute_dist(ville1, ville2):
    v1 = np.array((ville1.x,ville1.y))
    v2 = np.array((ville2.x,ville2.y))

    return np.linalg.norm(v1-v2)

# Question 2.2

def calcul_dist_total(villes, taille):

    point_dep = Ville(0,0,0)
    villedep = villes[0]
    som_dist = compute_dist(point_dep,villedep)
    for i in range(1,taille):
        som_dist+=compute_dist(villedep,villes[i])
        villedep=villes[i]

    som_dist+=compute_dist(villes[taille-1],point_dep)
    return som_dist

def display_vil(villes, nom_ville, taille):
    print("Tournée ",nom_ville,end=': ')
    for ville in villes:
        print(ville.id, end="-")

def display_sol(villes, nom_ville, taille, calcul_villes, deplacement = -1):
    display_vil(villes, nom_ville, taille)
    if deplacement == -1:
        print("=> dist:", calcul_villes)
    else:
        print("=> dist:", calcul_villes, " | deplacements = ", deplacement)

def display_tabou(tabou):
    print("<<< ",end="")
    for tournee in tabou:
        print("|",end="")
        for ville  in tournee:
            print(ville.id, end=" ")
    print(">>>")


# Question 2.3

def get_voisins(villes, taille):
    voisins = []
    for i in range(taille):
        for j in range(i+1,taille):
            voisin = np.array(villes)
            tempv = voisin[i]
            voisin[i] = voisin[j]
            voisin[j] = tempv
            voisins.append(voisin)
    return voisins

def meilleur_voisin(villes, taille):

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

def stetepest_hill_climbing_redemar(villes, taille):
    display_sol(villes, "init", taille, calcul_dist_total(villes, taille))

    calcul_villes, villes, deplacement = stetepest_hill_climbing(villes, taille)
    display_vil(villes,"init S",taille)
    print("=> dist:", calcul_villes, " | deplacements = ", deplacement)

    for i in range(2,MAX_essais+1):
        print("---------> redem")
        villes_redem = initial_solution(villes, taille)
        display_sol(villes, i, taille, calcul_dist_total(villes, taille))
        calcul_redem, villes_redem, deplacement = stetepest_hill_climbing(villes, taille)
        display_sol(villes, str(i)+"S", taille, calcul_redem, deplacement)

        if calcul_redem < calcul_villes:
            calcul_villes = calcul_redem
            villes = villes_redem

    return calcul_villes,villes

# Question 2.5

def get_voisins_non_tabou(villes, tabou, taille):
    voisins = []
    for i in range(taille):
        for j in range(i,taille):
            voisin = np.array(villes)
            tempv = voisin[i]
            voisin[i] = voisin[j]
            voisin[j] = tempv

            est_tabou = False
            for vil_tabou in tabou.queue:
                if (voisin == tabou).all():
                    est_tabou = True

            if est_tabou != True:
                voisins.append(voisin)
    return voisins

def meilleur_voisin_non_tabou(villes, voisins, tabou, taille):

    meilleur_voisin = voisins[0]
    min_distv = calcul_dist_total(meilleur_voisin, taille)

    for i in range(1,taille):
        calcul_distv = calcul_dist_total(voisins[i], taille)

        if calcul_distv < min_distv :
            min_distv = calcul_distv
            meilleur_voisin = voisins[i]

    return min_distv,meilleur_voisin

def f_tabou(villes, taille, taille_tabou):
    calcul_villes = calcul_dist_total(villes, taille)
    deplacement=0

    tabou = queue.Queue(taille_tabou)

    for i in range(MAX_depl):
        voisins=get_voisins_non_tabou(villes, tabou, taille)
        if (voisins!=[]):
            calcul_villes_v,villes_v = meilleur_voisin_non_tabou(villes, voisins, tabou, taille)
        else:
            print("Liste  tabou: ",end="")
            display_tabou(tabou.queue)
            return calcul_villes,villes

        if (tabou.full()):
            tabou.get()
            tabou.put(villes_v)
        else:
            tabou.put(villes_v)

        if calcul_villes_v < calcul_villes:
            calcul_villes = calcul_villes_v
            villes = villes_v
            deplacement+=1

        else:
            print("Liste  tabou: ",end="")
            display_tabou(tabou.queue)
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

class Ville():
    def __init__(self, id, x, y):
        self.id = id
        self.x = x
        self.y = y


with open(nom_fichier,'r') as f:
    i = 0

    taille = int(f.readline())
    villes = []

    for line in f:
        for val in line.split():
            if i == 0:
                id = int(val)
            if i == 1:
                x = int(val)
            if i == 2:
                y = int(val)
            i+=1
        i=0

        villes.append(Ville(id,x,y))
 

'''
#print("##############\n",taille,"\n",villes,"##############\n")
solution_init = initial_solution(villes,taille)
#display_sol(solution_init,taille)
calcul_solution,solution_stetepest = stetepest_hill_climbing_redemar(solution_init, taille)
print("\nSolution Stepest Hill Climb avec Redemarage:")
display_sol(solution_stetepest, "final", taille, calcul_solution)
print("\n")
'''

solution_init = initial_solution(villes,taille)
print("########## TABOU ############")
taille_tabou = 16
print("taille_tabou =",taille_tabou)
calcul_solution, villes, deplacement = f_tabou(solution_init,taille,taille_tabou)
display_sol(villes, "final tabou", taille, calcul_solution)
print("_____________")


end = time.time()
print("Durée totale d'execution: ", end-start)