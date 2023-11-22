import random

MAX_depl = 100
MAX_essais = 20

mat_q = [-17,10,10,10,0,20,10,-18,10,10,10,20,10,10,-29,10,20,20,10,10,10,-19,10,10,0,10,20,10,-17,10,20,20,20,10,10,-28]
vec_x = [1,1,0,1,0,0]


# Question 1.1
def initial_solution(n):
    x = []
    for i in range(n):
        x.append(random.randint(0, 1))

    return x


# Question 1.2
def calculValeur(mat_q, vec_x, n) :
     
    somme = 0

    for i in range(n) :
        for j in range (n) :
            somme += mat_q[i*n+j] * vec_x[i]*vec_x[j]
    
    return somme


# Question 1.3
def modifierVec(indice, vec) :
    if vec[indice] == 0 :
        vec[indice] = 1
    else :
        vec[indice] = 0
    
    return vec 

def meilleur_voisin(mat, vec, n) :

    vec0 = modifierVec(0, vec[:])
    meilleur_voisin = vec0
    min_score = calculValeur(mat,vec0,n)
 

    for i in range(1,n) :
        vec_i = modifierVec(i, vec[:]) 
        score = calculValeur(mat,vec,n)

        if score < min_score :
            min_score = score
            meilleur_voisin = vec_i
        

    return meilleur_voisin, min_score


# Question 1.4
def steepest_hill_climbing(mat_q, mat_x, n):
    calcul_x = calculValeur(mat_q, mat_x, n)

    for i in range(MAX_depl):
        mat_xv,calcul_xv = meilleur_voisin(mat_q, mat_x, n)
        if calcul_xv < calcul_x:
            calcul_x = calcul_xv
            mat_x = mat_xv
        else:
            return calcul_x,mat_x

    return calcul_x,mat_x


# Question 1.5
def steepest_hill_climbing_redemar(mat_q, mat_x, n):

    calcul_x, mat_x = steepest_hill_climbing(mat_q, mat_x, n)

    for i in range(MAX_essais-1):
        mat_redem = initial_solution(n)
        calcul_redem, mat_redem = steepest_hill_climbing(mat_q, mat_redem, n)
        if calcul_redem < calcul_x:
            calcul_x = calcul_redem
            mat_x = mat_redem

    return calcul_x,mat_x

print(steepest_hill_climbing_redemar(mat_q,vec_x,6))


