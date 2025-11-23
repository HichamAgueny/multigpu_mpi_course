#!/bin/bash -l
#SBATCH --job-name=gpuaware_mpiomp
#SBATCH --account=project_465002387
#SBATCH --time=00:10:00
#SBATCH --partition=dev-g
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=2
#SBATCH --gpus=2
#SBATCH --gpus-per-node=2
#SBATCH -o %x-%j.out
##SBATCH --exclusive

#Load the LUMI software stack
module load LUMI/24.03 partition/G
module load cpeCray

#To enable GPU-aware MPI
export MPICH_GPU_SUPPORT_ENABLED=1

# Binding option
#CPU_BIND="map_cpu:49,57"

#srun --cpu-bind=${CPU_BIND} ./gpuaware.mpiomp.exe

srun ./gpuaware.mpiomp.exe
echo

#rocm-smi --showtoponuma
