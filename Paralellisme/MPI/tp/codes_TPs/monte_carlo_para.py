#! /usr/bin/python3

import time
import random
from mpi4py import MPI
from mpi4py.MPI import ANY_SOURCE

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

nb = 100001
inside = 0
random.seed(rank)

start_time = time.time()
for _ in range(nb//size):
    x = random.random()
    y = random.random()
    if x*x + y*y <= 1:
        inside +=1
end_time = time.time()

total = comm.reduce(4 * inside/nb, op=MPI.SUM, root=0)

print("Process ",rank ,"- inside = ", inside,"\n")

if comm.rank == 0:
    print("Pi =", total/size, "in ", end_time-start_time, 'seconds')