program euler_pendulum 

! ========================================== MAIN ====================================================

    implicit none 

    ! ---------------------------------------variables declaration-------------------------------------

    integer, parameter :: dp = kind(0.0D0)
    real(kind = dp) :: m, l, theta0, delta_t, T
    real(kind = dp) :: omega_i_plus_one, omega_i, theta_i_plus_one, theta_i
    real(kind = dp), parameter :: g = 9.81

    ! ------------------------------------- variables manipulation & file operations  -----------------------------------------------------
    ! Variable initialization 

    omega_i = sqrt(g / l)

    ! functions call 

! ====================================================================================================

! =====================================  SUBPROGRAMS =====================================================

contains 

    ! Subroutine to compute omega values

    real(kind = dp) function updte_omega(omega_i, theta_i_plus_one, delta_t, g, l)

        real(kind = dp) :: new_omega
        real(kind = dp), intent(in) :: omega_i, theta_i_plus_one, delta_t, g, l

        new_omega = omega_i - (g / l) * theta_i_plus_one * delta_t

        updte_omega = new_omega

    end function updte_omega

    ! Subroutine to compute theta values 

    real(kind = dp) function updte_theta(omega_i_plus_one, theta_i, delta_t)

        real(kind = dp) :: new_theta
        real(kind = dp), intent(in) :: omega_i_plus_one, theta_i, delta_t 

        new_theta = theta_i + omega_i_plus_one * delta_t

        updte_theta = new_theta

    end function updte_theta

end program euler_pendulum


