program exerC

    implicit none

    integer :: M, i, temp, count, control, unit_num
    integer :: primes_per_line, prime_count

    ! Número de primos por linha na saída

    primes_per_line = 10
    prime_count = 0
    
    open(unit = unit_num, file = 'primos_out.dat', status = 'replace')

    write(*,*) 'Digite o valor de M: '
    read(*,*) M
    
    ! Cabeçalho do arquivo 

    write(unit_num,'(A, I0)') 'Para M = ', M
    write(unit_num,'(A)') 'Os números primos contidos neste intervalo são:'

    ! Loop para encontrar os primos

    do i = 2, M 

        control = 0
        count = 1
        temp = i 

        do while (count <= temp)

            if (mod(temp,count) == 0) then 

                control = control + 1 ! Variável que verifica se o número
                                      ! possui outros divisores além de 0 e ele mesmo 

            end if 

            count = count + 1

        end do

        ! Verificando se o número é primo e gravando-o no arquivo

        if (control == 2) then 

            
            ! Escreve o primo com largura 5 e organiza em linhas

            write(unit_num,'(I5, A)', advance = 'no') i, ' '

            prime_count = prime_count + 1

            if (mod(prime_count, primes_per_line) == 0) then

                write(unit_num,*)  ! pula para nova linha a cada 10 primos

            end if


        end if

    end do

    ! Pula linha final

    if (mod(prime_count, primes_per_line) /= 0) then

        write(unit_num,*) 

    end if

    ! Fecha o arquvo

    close(unit_num)

end program exerC



            

  



