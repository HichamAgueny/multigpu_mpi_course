#!/bin/bash -l
#SBATCH --job-name=2-node_only_mpi
#SBATCH --account=project_465002387
#SBATCH --time=00:10:00
#SBATCH --partition=dev-g
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=1
#SBATCH --gpus=2
#SBATCH --gpus-per-node=1
#SBATCH -o %x-%j.out
##SBATCH --exclusive

#Load the LUMI software stack
module load LUMI/24.03 partition/G
module load cpeCray


srun ./only.mpi.exe 
echo
