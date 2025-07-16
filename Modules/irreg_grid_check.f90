module irreg_grid_check
  use AI_kinds
  use OCEAN_mpi
  ! variable and constant declarations
  type irreg_grid
          integer num_coord
          real(DP), allocatable :: curvi_coord(:,:), weights(:)
          logical :: have_curvi, have_weights
  end type

  type(irreg_grid) :: curvi_coord

contains 
  subroutine check_for_grid( ierr ) 
    implicit none
    integer, intent(inout) :: ierr
    integer :: i, j

    if (myid .eq. root ) then
            inquire(file='reduced_uniform.txt', exist=grid%have_curvi)
            if ( grid%have_curvi ) then
                    open(unit=99, file='reduced_uniform.txt', form='formatted', status='old', action='read')
                    ! read number of coordinates from first line
                    read(99, *) grid%num_coord
                    allocate( grid%curvi_coord(3, grid%num_coord) )
                    do i = 1, grid%num_coord
                    read(99, *) grid%curvi_coord(1, i), grid%curvi_coord(2, i), grid%curvi_coord(3, i)
                    enddo
                    close(99)
            endif
            
            inquire(file='integration_weight.txt', exist=grid%have_weights)
            if (grid%have_weights) then
                    open(unit=99, file='integration_weight.txt', form='formatted', status='old', action='read')
                    allocate(grid%weights(grid%num_coord))
                    do j = 1, grid%num_coord
                      read(99, *) grid%weights(j)
                    enddo
                    close(99)
            else
                    ! assume all weights = 1
                    do j = 1, grid%num_coord
                      grid%weights(j) = 1
                    enddo
            endif
    endif

#ifdef MPI
    call MPI_BCAST(grid%have_curvi, 1, MPI_LOGICAL, 0, comm, ierr)
    if (ierr /= 0) goto 111
    if (grid%have_curvi) then
            call MPI_BCAST(grid%num_coord, 1, MPI_INTEGER, 0, comm, ierr)
            if (ierr /= 0) goto 111
            ! if not root, allocate array of size curvi_coord
            if (myid /= 0) then
                    allocate( grid%curvi_coord(3, grid%num_coord) )
            endif
            call MPI_BCAST(grid%curvi_coord, grid%num_coord*3, MPI_DOUBLE_PRECISION, 0, comm, ierr)
            if (ierr /= 0) goto 111
    endif
    
    call MPI_BCAST(grid%have_weights, 1, MPI_LOGICAL, 0, comm, ierr)
    if (ierr /= 0) goto 111
    if (grid%have_weights) then
            if (myid /= 0) then
                    allocate(grid%weights(grid%num_coord))
            endif
            call MPI_BCAST(grid%weights, grid%num_coord, MPI_DOUBLE_PRECISION, 0, comm, ierr)
            if (ierr /= 0) goto 111
    endif
#endif
111 continue
  end subroutine check_for_grid

end module irreg_grid_check
