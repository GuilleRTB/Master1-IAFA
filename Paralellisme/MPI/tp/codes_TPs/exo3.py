import numpy as np
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

np.random.seed(0)
numbers = np.random.randint(100, size=10, dtype='i')

A = np.empty(10, dtype = 'i')
comm.Gather(numbers, A, root=0)

if rank == 0:
    mini = np.zeros(1, dtype='i')
    maxi = np.zeros(1, dtype='i')
    
    comm.Reduce(A, mini, op=MPI.MIN, root=0)
    comm.Reduce(A, maxi, op=MPI.MAX, root=0)
    
    if maxi[0] == mini[0] :
        print(True)
    else:
        print(False)


