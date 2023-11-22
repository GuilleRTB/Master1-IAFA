# python3 ./number.py 42

from mpi4py import MPI
import numpy as np
import sys

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
number = np.empty(1,dtype='i')

if rank == 0:
    number[0] = int(sys.argv[1])
else:
    number[0] = 0

print("At start in process of rank ", rank,"the passnumber is ", number[0])

number = comm.bcast(number, root=0)

print("After collective in process of rank ", rank,"the passnumber is ", number[0])




