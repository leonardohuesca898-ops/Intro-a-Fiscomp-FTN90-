program euler_method_1D

! ===================================== MAIN =====================================================

    implicit none

    integer, parameter :: double_precision = kind(0.0D0)
    real(kind = double_precision) :: v ! v(idt + dt)
    real(kind = double_precision) :: v0, dt
    integer :: P = 400, m = 80, T, file_unit = 10, ierr, i

    ! Read the values of Period, time interval and initial velocity from the user: 

    write(*,*) 'Input, in meters per second and in this order the: Period T, time interval dt and initial velocity v0'
    read(*,*) T, dt, v0

    ! Open the file

    open(unit = file_unit, file = 'vel1_out.dat', status = 'unknown', action = 'readwrite', iostat = ierr)

    ! write the first line of the archive, with the header of initial conditions

    write(unit = file_unit, fmt = '(1X, I9, 1X, F14.6)') 0, v0

    ! write the header with corresponding data columns in the archive 

    write(unit = file_unit, fmt = '(1X, A9, 1X, A14)') 't', 'v(t)'


    ! Loop to compute the values of v and write it on the file

    do i = 1, T + 1 

        ! Call the function to compute the values of v for earch ti

        v = euler_method(m, P, dt, v0, i)

        ! Write the outputs in the file

        write(unit = file_unit, fmt = '(1X, I9, 1X, F14.6)') i, v
    
    end do

    ! Close the file

    close(unit = file_unit)
    
! =================================================================================================

! ===================================== SUBPROGRAMS ====================================================

contains

! Subroutine to compute the bicycle velocity in 1D motion by euler method

    real(kind = double_precision) function euler_method(m, P, dt, v0, i)
 
        implicit none 

        real(kind = double_precision), intent(in) :: dt, v0
        integer, intent(in) :: m, P, i
        real(kind = double_precision) :: v_plusone
        
        

        v_plusone = sqrt((v0 ** 2) + (2 * P * i * dt)/m) & 
                        + (P * dt) /(m * (sqrt((v0 ** 2) + (2 * P * i * dt)/m)))

        

        euler_method = v_plusone


    end function euler_method


end program euler_method_1D