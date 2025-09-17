PROGRAM IEEE
IMPLICIT NONE

! Specification-part (declaracao de variaveis)

REAL(4) :: a = 1.0e0 
REAL(4) :: j = 0.0e0
INTEGER :: i = 1

! Execution-part

DO i = 1, 1000


    a = a / 2.0e0
    j = j + a

    WRITE(*,*) a, j

 IF ( j == 1) EXIT

 END DO


 END PROGRAM IEEE 

    
    


