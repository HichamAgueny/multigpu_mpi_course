#!/bin/bash -l
#SBATCH --job-name=gpuaware-mpiomp
#SBATCH --account=project_465002387
#SBATCH --time=00:05:00
#SBATCH --partition=standard-g
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --gpus=8
#SBATCH --gpus-per-node=8
#SBATCH -o %x-%j.out
#SBATCH --exclusive

#Load the LUMI software stack
module load LUMI/24.03 partition/G
module load cpeCray

#To enable GPU-aware MPI
export MPICH_GPU_SUPPORT_ENABLED=1

# Binding option
CPU_BIND="map_cpu:49,57,17,25,1,9,33,41"

time srun --cpu-bind=${CPU_BIND} ./laplace.gpuaware.mpiomp.exe

echo
#rocm-smi --showtoponuma
