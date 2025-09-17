program exerB

    implicit none

    ! Declaracao do vetor que armazenara a soma parcial

    real :: sum, x, termo, precision
    integer :: i, j, n
    real, dimension(4) :: x_vals = (/ 0.1, 0.2, 0.3, 0.4 /)
    real, dimension(4) :: partial_sum, error, v_precision, m_prec_vec
    integer, dimension(4) :: e_prec_vec

    ! Calcula a soma parcial da serie para cada x fornecido e guarda em um vetor a sequencia das
    ! somas parciais

    do i = 1, 4

        sum = 0.0
        n = 1
        x = x_vals(i)

        ! executa a soma parcial até alcançar a precisão maxima permitida em 32 bits

        do

            termo = (((-1.0) ** (n + 1)) * (x ** n)) / n
            sum = sum + termo

            if (abs(termo) <= 1.0e-7) then


                error(i) = (((-1.0) ** (n + 1)) * (x ** n)) / n

            end if

            if (abs(termo) <= 1.0e-7) exit

            n = n + 1
            
        end do

        partial_sum(i) = sum


    end do

    ! Cálcula a precisão com base nos valores obtidos do erro e do resultado da soma parcial truncada
    ! e guarda a precisão obtido em cada termo em um vetor

    do j = 1, 4

        precision = error(j)/partial_sum(j)
        v_precision(j) = precision

    end do

    ! Ajustando a precisao dos termos obtidos para extrair apenas o expoente
    ! calcula mantissa e expoente decimal da precisão :

    do i = 1, 4

        if (v_precision(i) /= 0.0) then

            e_prec_vec(i) = int(log10(v_precision(i)))
            m_prec_vec(i) = v_precision(i) / 10.0**e_prec_vec(i)

        else

            e_prec_vec(i) = 0
            m_prec_vec(i) = 0.0

        end if

    end do

    ! Imprime os resultados

    write(*,'(A6, A20, A18, A25)') 'x', 'Soma Parcial', 'Precisao', 'Precisao (cientifica)'
    write(*,'(A6, A20, A18, A25)') '------', '--------------', '----------', '----------------------'

    do i = 1, 4

        write(*,'(F6.1, F20.10, F18.10, F15.8, "E", I3)') x_vals(i), partial_sum(i), v_precision(i), m_prec_vec(i), e_prec_vec(i)

    end do

end program exerB

! Como é possível observar na saída, o resultado torna-se extremamente preciso já a partir
! do terceiro termo, mesmo em precisão simples, tal que "estoura" a precisão suportada
! no formato 32 bits, de forma que dentro do intervalo de convergência proposto
! é razoável a aproximação da função considerada
! usando um polinômio de taylor
