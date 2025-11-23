#!/bin/bash -l
#SBATCH --job-name=setDevice_omp
#SBATCH --account=project_465002387
#SBATCH --time=00:10:00
#SBATCH --partition=dev-g
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --gpus=8
#SBATCH --gpus-per-node=8
#SBATCH -o %x-%j.out
#SBATCH --exclusive

#Load the LUMI software stack
module load LUMI/24.03 partition/G
module load cpeCray

# Binding option
CPU_BIND="map_cpu:49,57,17,25,1,9,33,41"

srun --cpu-bind=${CPU_BIND}\
     ./assignDevice.omp.exe | sort
echo

rocm-smi --showtoponuma
