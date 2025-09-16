program exerB

    implicit none

    ! Declaracao do vetor que armazenara a soma parcial

    real :: sum = 0
    real :: x
    real, dimension(5) :: x_vals = (/ 0.1, 0.2, 0.3, 0.4, 0.5 /)
    real, dimension(5) :: partial_sum
    integer :: i
    integer :: j
    integer :: precision = 0
    integer, dimension(5) :: v_precision
    real :: n

    ! Calcula a soma parcial da serie para cada x fornecido e guarda em um vetor a sequencia das
    ! somas parciais

    do i = 1, 5

        x = x_vals(i)

        do j = 1, 5

            sum = sum + (((-1.0) ** (i + 1)) * (x ** i)) / i

        end do

        partial_sum(i) = sum

    end do

    ! Calcula a precisao de cada valor da serie

    do i = 1, 5

        n = partial_sum(i)

        ! Fatora o expoente para obter a precisão da aproximação

        do while (n <= 1)

            n = n * 10
            precision = precision + 1

        end do

        v_precision(i) = -1 * precision

    end do

    ! Imprime os resultados

    write(*,'(A6, A15, A12)') 'x', 'Soma Parcial', 'Precisao'
    write(*,'(A6, A15, A12)') '------', '--------------', '----------'

    do i = 1, 5

        write(*,'(F6.1, F15.4, I12)') x_vals(i), partial_sum(i), v_precision(i)

        ! se o numero possui menos caracteres que o indicado na formatação
        ! o que restar é preenchido com espaços à esquerda

    end do


end program exerB

! Até que é possível aproximar funções logaritimicas
! com o uso de séries, porém com a devida parcimônia
! uma vez que uma precisão razoável
! só é alcançada em valores de x
! que são suficientemente próximos
! dos extremos do intervalo de convergência da série