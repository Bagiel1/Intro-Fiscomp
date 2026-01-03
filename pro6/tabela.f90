program tabela
        IMPLICIT NONE

        CHARACTER(LEN=10), DIMENSION(9):: nomes
        REAL(8), DIMENSION(9):: raios

        REAL(8):: pi, v0, T, razao, GM
        INTEGER:: i
        

        nomes = ['Mercurio  ', 'Venus     ', 'Terra     ', 'Marte     ', 'Jupiter   ', 'Saturno   ', 'Urano     ', &
                'Netuno    ', 'Plutao    ']
        raios = [0.39D0, 0.72D0, 1.0D0, 1.52D0, 5.2D0, 9.24D0, 19.19D0, 30.06D0, 39.53D0]
        
        pi= 4.0D0 * ATAN(1.0D0)
        GM= 4.0D0 * (pi**2)

        OPEN(UNIT=10, FILE='tabA1_out.dat')

        WRITE(10, *) 'Planeta', '      v0 (UA/ano)', '             T^2/R^3'

        DO i = 1, 9
              v0 = 2.0D0*pi / sqrt(raios(i))
              T= sqrt(raios(i)**3)
              razao = (T**2) / (raios(i)**3)
              WRITE(10, *) nomes(i), v0, razao

        END DO

        close(10)

end program tabela

