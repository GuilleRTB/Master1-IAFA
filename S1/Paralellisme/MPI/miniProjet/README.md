ubuntu / debian:

sudo apt install openmpi-bin python3-matplotlib libopenmpi-dev
sudo apt install python3-pip
pip3 install mpi4py


sur fedora:

sudo dnf install openmpi-devel
sudo dnf install python3-devel
module load mpi/openmpi-x86_64
pip3 install --user mpi4py


sur mac OSX:

brew install openmpi 
pip3 install mpi4py   ou conda install mpi4py