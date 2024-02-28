from mpi4py import MPI
import numpy as np
import sys

comm = MPI.COMM_WORLD
nb_proc = comm.Get_size()
rank = comm.Get_rank()

def get_max(tab):
    pos = np.argmax(tab)
    return [tab[pos], pos] 

if rank == 0 :
    dim_tab = np.empty(1, dtype = 'i')
    dim_tab[0] = np.random.randint(5)
    if dim_tab[0] < 2 : dim_tab[0] = nb_proc
    if dim_tab[0] % 2 != False : dim_tab[0] = dim_tab[0] + 1
    global_list = np.zeros(dim_tab[0], dtype = 'i')
    global_list = np.random.randint(99, size=dim_tab[0], dtype='i') 
else : 
    dim_tab  = np.zeros(1, dtype = 'i')

comm.Bcast(dim_tab, root=0)

if rank != 0 :
    global_list =  np.zeros(dim_tab[0], dtype = 'i') 

comm.Bcast(global_list, root=0)

glob_result = np.zeros(2*nb_proc, dtype = 'i')
result = np.zeros(2, dtype = 'i')
c_max, c_pos = get_max(global_list) 
result.itemset(0,c_max)
result.itemset(1,c_pos)
print(f'rank:{rank} dim_tab : {dim_tab[0]} result: {result} global_list: {global_list}') 

comm.Gather(result, glob_result, root=0) 

if rank ==0 :
    print(f'\nrank:{rank} dim_tab: {dim_tab[0]} glob_result: {glob_result} global_list: {global_list} \n')