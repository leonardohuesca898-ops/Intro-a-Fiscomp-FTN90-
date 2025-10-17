module operations

    implicit none
    integer, parameter :: double = kind(0.0D0)


contains

    ! Subroutine to compute first derivative by foward difference method

    real function forward(x, h)

        implicit none

        real(kind = double), intent(in) :: x, h
        real(kind = double) :: forward_diff

        forward_diff = (log(1 + (x + h) ** 2) - log(1 + x)) /  h 
        forward = forward_diff

    end function forward

    ! Subroutine to compute first derivative by backward difference method

    real function backward(x, h)

        implicit none

        real(kind = double), intent(in) :: x, h
        real(kind = double) :: backward_diff

        backward_diff = (log(1 + x) - log(1 + (x - h) ** 2) ) /  h 
        backward = backward_diff

    end function backward

! Subroutine to compute first derivative by central difference method 

    real function central(x, h)

        implicit none 

        real(kind = double), intent(in) :: x, h
        real(kind = double) :: central_diff

        central_diff = (log(1 + (x + h)) - log(1 + (x - h))) / (2 * h)

        central = central_diff 

    end function central 

! Subroutine to compute first derivative by five points central difference method 

    real function five_central(x, h)

        implicit none 

        real(kind = double), intent(in) :: x, h
        real(kind = double) :: five_points_central_dif 

        five_points_central_dif = (-1 * log(1 + (x + 2 * h)) + 8 * log(1 + (x + h)) &
                                 - 8 * log(1 + (x - h)) + log(1 + (x - 2 * h))) / (12 * h)

        five_central  = five_points_central_dif 

    end function five_central 

! Subroutine to compute five point symmetric second derivative 

    real function sec_derivative(x , h)

        implicit none 

        real(kind = double), intent(in) :: x, h
        real(kind = double) :: five_pts_scnd_derivative

        five_pts_scnd_derivative  =  (-log(1 + (x + 2 * h)) + 16 * log(1 + (x + h)) &
                                    - 30 * log(1 + x) + 16 * log(1 + (x - h)) &
                                    - log(1 + (x - 2 * h))) / (12 * (h ** 2))


        sec_derivative = five_pts_scnd_derivative 

    end function sec_derivative

! Subroutine to compute five point odd-order central derivative 

    real function third_derivative(x, h) 

        implicit none 

        real(kind = double), intent(in) :: x, h
        real(kind = double) :: odd_order_central_third_derivative

        odd_order_central_third_derivative = (log(1 + (x - 2 * h)) - 2 * log(1 + (x - h)) &
                                            + 2 * log(1 + (x + h)) - log(1 + (x + 2 * h))) / (2 * (h ** 3))

        third_derivative = odd_order_central_third_derivative

    end function third_derivative 

end module operations 