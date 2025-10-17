program main

    use operations ! Open the module where the operations of finite derivative were defined

    implicit none
    
    integer, parameter :: dp = kind(0.0D0)
    real(kind = dp), parameter :: x = 0.5000000 ! fixed parameters
    real(kind = dp) :: h       ! fixed parameters
    real(kind = dp) :: forward_diff, backward_diff, central_diff,  five_points_central_diff ! first derivative
    real(kind = dp) :: three_point_second_central_derivative, five_point_second_central_derivative ! scnd derivative
    real(kind = dp) :: five_pts_scnd_derivative, odd_order_central_third_derivative  ! 2nd & 3rd derivative
    integer :: i = 1, file_unit = 10, file_assert, count = 0 ! Open file parameters
    integer :: file_unit_1 = 20, file_assert_1

    ! Open file tab1_in.dat 

    open(unit = file_unit, file = 'tab1_in.dat', status = 'old', action = 'read', form = 'formatted')

    ! ============= Loops to compute derivatives for each method with each h as input ==========================

    ! Loop to count the number of inputs in the file

    do 

        read(file_unit, '(A)', iostat = file_assert)

        if (file_assert /= 0) exit 

        count = count + 1

    end do 

    rewind(file_unit) ! Rewind the archive

    ! Print the number of inputs in the open file

    write(*,*)  count 

    ! Open the archive where the outputs will be printed

    open(unit = file_unit_1, file = 'tab1_out.dat', status = 'replace', action = 'write', form = 'formatted')

    ! Write the header 

    write(file_unit_1, '(A12, 6(2X, A15))') 'h', 'forward', 'backward', 'central', &
                                         'five_pts', 'second_central', 'third_odd'
    write(file_unit_1, '(A)') repeat('-', 120)

    ! Read h values as inputs and use it in corresponding functions

    do

        ! read inputs

        read(file_unit, *, iostat = file_assert) h
        if (file_assert /= 0) exit ! if there is no values of h to read, finish the loop

        ! call functions to compute the derivative
        
        forward_diff = forward(x, h)
        backward_diff = backward(x, h)
        central_diff = central(x,h)
        five_points_central_diff = five_central(x,h)
        three_point_second_central_derivative = sec_derivative(x, h)
        odd_order_central_third_derivative = third_derivative(x, h)

        ! Write the outputs in tab1_out.dat


        write(file_unit_1, '(F12.10, 6(2X, E15.8))') h, forward_diff, backward_diff, central_diff, &
                                                      five_points_central_diff, three_point_second_central_derivative, &
                                                     odd_order_central_third_derivative

        


    end do
 
    close(unit = file_unit,iostat = file_assert) ! close the file tab1_in.dat
    close(unit = file_unit_1, iostat = file_assert_1) ! close the file tab1_out.dat


end program main