         program gpuaware_mpiacc

           use mpi
           use openacc

           implicit none
            integer status(MPI_STATUS_SIZE)
            integer :: myid,nproc
            integer :: ix,iter,ierr,tag1,tag2
            integer :: host_rank,host_comm
            integer :: myDevice,numDevice
            integer :: resulten, ierror, nx
            integer(kind=8) :: nxmax,n_bits
            integer, parameter :: float_size_bit = 64 !for double precision 64 bit=8 byte
            integer, parameter :: N0=8*1024
            integer, parameter :: n_iter=10
            double precision   :: result_GB
            double precision   :: start_time, end_time, elapsed_time,average_time,bandwidth_Gbps
            double precision, allocatable :: f(:), f_send(:,:)
            character*(MPI_MAX_PROCESSOR_NAME) :: name

! Initialise MPI communication      
        call MPI_Init(ierr)
! Identify the ID rank (process)        
        call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
! Get number of active processes (from 0 to nproc-1)        
        call MPI_COMM_SIZE( MPI_COMM_WORLD, nproc, ierr )

        if(myid.eq.0) then
          print*, ""
          print*, "--Measuring the Bandwidth during Data Transfer: MPI-OpenACC test--"
          print*, ""
       endif
! Split the world communicator into subgroups of commu, each of which
! contains processes that run on the same node, and which can create a
! shared memory region (via the type MPI_COMM_TYPE_SHARED).
! The call returns a new communicator "host_comm", which is created by
! each subgroup.        
        call MPI_COMM_SPLIT_TYPE(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0,&
                               MPI_INFO_NULL, host_comm,ierr)
        call MPI_COMM_RANK(host_comm, host_rank,ierr)

! Gets the node name
        call MPI_GET_PROCESSOR_NAME(name, resulten, ierror)

        myDevice = host_rank

! Sets the device number and the device type to be used
        call acc_set_device_num(myDevice, acc_get_device_type())

! Returns the number of devices available on the host
        numDevice = acc_get_num_devices(acc_get_device_type())

        if(myid.eq.0) then
                print*,''
                print*, '--nbr of MPI processes: ', nproc
                print*, '--nbr of gpus on each node: ', numDevice
                print*, '--nbr of nodes: ', nproc/numDevice
                print*,''
        endif

!        write(*,'(A,I3,A,A,A,I3,A,I3)') "MPI-rank ", myid, " - Node ", trim(name), " - GPU_ID ", myDevice, " - GPUs-per-node ", numDevice


!Generate the Initial Conditions (ICs)
!Distribute the ICs over all processes using the operation MPI_Scatter
!Transfer a max of 1 GB of data
result_GB = 1./8. * 1e9 
nxmax = int(result_GB)
   nx = 4
   do while (nx.le.nxmax)
     
     nx = 2*nx
     allocate(f(nx));

     f=0d0; tag1=2020; tag2=2021

     if(myid.eq.0) then
       allocate(f_send(nx,1:nproc+1))
        CALL RANDOM_NUMBER(f_send)
      endif

      call MPI_Scatter(f_send,nx,MPI_DOUBLE_PRECISION,&
                      f(:), nx,MPI_DOUBLE_PRECISION,&
                      0,MPI_COMM_WORLD, ierr)

      call MPI_Barrier(MPI_COMM_WORLD, ierr)

      if(myid.eq.0) deallocate(f_send)

      
       start_time = MPI_Wtime()
!Unstructed data locality
!$acc enter data copyin(f) 
        do iter=1, n_iter
!Device pointer f will be passed to MPI_send & MPI_recv       
!$acc host_data use_device(f)

!transfer the data to the neighbouring MPI-process
!send f(:) from myid-1 to be stored in f(:) in myid+1
         if(myid.eq.0) then
          call MPI_Send(f(:),nx,MPI_DOUBLE_PRECISION,myid+1,tag1,&
                       MPI_COMM_WORLD, ierr)
!receive f(:) from myid-1
          call MPI_Recv(f(:),nx,MPI_DOUBLE_PRECISION,myid+1,&
                      tag2,MPI_COMM_WORLD, status,ierr)
         endif

!receive f(:) from myid-1
         if(myid.eq.1) then
          call MPI_Recv(f(:),nx,MPI_DOUBLE_PRECISION,myid-1, &
                      tag1,MPI_COMM_WORLD, status,ierr)
!send f(:) from myid+1 to be stored in f(:) in myid-1
          call MPI_Send(f(:),nx,MPI_DOUBLE_PRECISION,myid-1,tag2,&
                       MPI_COMM_WORLD, ierr)
         endif
!$acc end host_data        

     enddo
!$acc exit data copyout(f) 
        end_time = MPI_Wtime()

       elapsed_time = end_time - start_time

       if(myid.eq.0) then
         n_bits = 2*nx * float_size_bit !factor 2 comes from: copyin + copyout
         average_time = dble(elapsed_time/n_iter)
         bandwidth_Gbps = dble(n_bits/average_time)*1e-9
         if(nx.gt.8) write(*,'(A,F15.5,A,I13,A,F15.5)')" --Time (s) ", average_time, " Data size (B) ", n_bits/8/2*float_size_bit, " Bandwidth (GBps) ", bandwidth_Gbps/8.0
      endif

      deallocate(f)
  enddo

        call MPI_FINALIZE( ierr )
        end
 
