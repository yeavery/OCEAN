module irreg_grid_check
  use AI_kinds
  ! variable and constant declarations

contains 
  subroutine check_for_grid( ierr, have_curvi, num_coord, curvi_coord ) ! TODO: figure out parameters
          ! might not need any since we're broadcasting the result?
    use OCEAN_mpi
    implicit none

    ! initialize parameters
    integer, intent(inout) :: ierr
    real(DP), allocatable, intent(out) :: curvi_coord(:, :)
    logical, intent(out) :: have_curvi
    integer, intent(out) :: num_coord
    integer :: i

    if (myid .eq. root ) then
            inquire(file='reduced_uniform.txt', exist=have_curvi)
            if ( have_curvi ) then
                    open(unit=99, file='reduced_uniform.txt', form='formatted', status='old', action='read')
                    ! read number of coordinates from first line
                    read(99, *) num_coord
                    allocate( curvi_coord(3, num_coord) )
                    do i = 1, num_coord
                    read(99, *) curvi_coord(1, i), curvi_coord(2, i), curvi_coord(3, i)
                    enddo
                    close(99)
            endif
    endif

#ifdef MPI
    call MPI_BCAST(have_curvi, 1, MPI_LOGICAL, 0, comm, ierr)
    if (ierr /= 0) goto 111
    if (have_curvi) then
            call MPI_BCAST(num_coord, 1, MPI_INTEGER, 0, comm, ierr)
            if (ierr /= 0) goto 111
            ! if not root, allocate array of size curvi_coord
            if (myid /= 0) then
                    allocate( curvi_coord(3, num_coord) )
            endif
            call MPI_BCAST(curvi_coord, num_coord*3, MPI_DOUBLE_PRECISION, 0, comm, ierr)
            if (ierr /= 0) goto 111
    endif
#endif
111 continue
  end subroutine check_for_grid

end module irreg_grid_check
