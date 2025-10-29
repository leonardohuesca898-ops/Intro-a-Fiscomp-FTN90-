program euler_2D

! ===================================== MAIN =====================================================

    implicit none

    integer, parameter :: dp = kind(0.0D0)
    integer, parameter :: file_unit = 10, f_unit = 20
    real(kind = dp) :: x, y, v_x, v_y, v, dt, g, theta, t
    real(kind = dp) :: x_exact, y_exact
    real(kind = dp), parameter :: pi = 3.14159265

    ! initialize the variables : 

    x = 0.0 ! initial x position
    y = 0.0 ! initial y position
    v = 500.0 ! initial velocity
    dt = 0.1 ! time interval
    g = 9.81 ! gravity constant near earth surface
    t = 0.0_dp ! initial time for exact trajectory

    ! Read the value of the angle in degrees from the user

    write(*,*) 'type the launch angle in degress' 
    read(*,*) theta 

    ! Convert the angle to radians 

    theta = theta * (pi / 180)

    ! Computing the initial vx and vy positions 
    
    v_x = v * cos(theta)
    v_y = v * sin(theta)

    ! Open the files of the outputs

    open(unit = file_unit, file = 'output2.txt', status = 'unknown', action = 'write')
    open(unit = f_unit, file = 'trajexata.txt', status = 'unknown', action = 'write')

    ! Header for positions x,y of the loop 

    write(file_unit, '(2A12)') 'x', 'y'

    ! Loop to compute the values and call functions :

    do 

        ! functions call

         x = x_axis_pos(x, v_x, dt)
         y = y_axis_pos(y, v_y, dt)
         v_y = y_axis_velocity(v_y, g, dt)

        ! write results

         write(unit = file_unit, fmt = '(2F12.4)') x, y

        if (y < 0) exit

    end do

    ! Close the file output2.txt 

    close(file_unit)

    ! Header for positions x,y of the loop 

    write(f_unit, '(2A12)') 'x_exact', 'y_exact'

    ! Loop to compute the exact trajectory and write it in the trajexata.txt file 

    do 

        ! Functions call

        t = t + dt

        x_exact = exact_traj_x(v, t)
        y_exact = exact_traj_y(v, t, g)

        ! Write the results on the corresponding file

        write(unit = f_unit, fmt = '(2F12.4)') x_exact, y_exact

        if (y_exact <= 0.0_dp .and. t > 0.0_dp) exit 

    end do

    ! Close the file trajexata.txt

    close(f_unit)

! =====================================================================================================

! ===================================== SUBPROGRAMS ====================================

contains 

! Subroutine to compute the values of the position in x axis 

    real(kind = dp) function x_axis_pos(x, v_x, dt)

        implicit none 
        real(kind = dp) :: x_plus_one
        real(kind = dp), intent(in) :: x, v_x, dt 

        x_plus_one = x  + v_x * dt

        x_axis_pos = x_plus_one

    end function x_axis_pos

! Subroutine to compute the values of the position in y axis

    real(kind = dp) function y_axis_pos(y, v_y, dt) 

        implicit none 
        real(kind = dp) :: y_plus_one
        real(kind = dp), intent(in) :: y, v_y, dt 

        y_plus_one = y + v_y * dt
        
        y_axis_pos = y_plus_one

    end function y_axis_pos

! Subroutine to compute the values of velocity in y axis : 

    real(kind = dp) function y_axis_velocity(v_y, g, dt)

        implicit none 
        real(kind = dp) :: v_plus_one
        real(kind = dp), intent(in) :: v_y, g, dt 

        v_plus_one = v_y - g * dt 

        y_axis_velocity = v_plus_one

    end function y_axis_velocity

! Subroutines to compute the exact trajectory by kinematics equations

    ! x axis

    real(kind = dp) function exact_traj_x(v ,t)

        implicit none 
        real(kind = dp) :: x_t
        real(kind = dp), intent(in) :: t, v 

        x_t = v * t

        exact_traj_x = x_t

    end function exact_traj_x

    ! y_axis

    real(kind = dp) function exact_traj_y(v ,t, g)

        implicit none 
        real(kind = dp) :: y_t
        real(kind = dp), intent(in) :: t, v, g

        y_t = v * t - (0.5_dp) * g * (t ** 2)

        exact_traj_y = y_t         

    end function exact_traj_y


end program euler_2D