from mpi4py import MPI
import numpy as np
import sys

comm = MPI.COMM_WORLD
nb_proc = comm.Get_size()
rank = comm.Get_rank()

colors = {0: 'blue', 1:'green'}

if rank == 0 :
    teams = np.random.randint(2, size=nb_proc, dtype='i')
    print (f'\nrank :{rank} le tirage des équipes {teams}  \n ')
else : 
    teams = None

team  = np.empty(1, dtype='i')
comm.Scatter(teams,team, root=0)
my_team = team[0]


print (f'\nrank : {rank} my team is {colors[my_team]} \n ') 




