program exerA
        IMPLICIT NONE
        
        INTEGER:: max_iter, descartado
        INTEGER:: i
        REAL(8):: lambda_deca, lambda, x0, xi, r, x_epsilon
        REAL(8), ALLOCATABLE:: x(:), d(:)
        REAL(8):: epsilons, soma
        REAL(8):: soma_x, soma_y, soma_xy, soma_x_quadrado
        REAL(8):: total

        soma= 0.0D0
        max_iter= 1000
        descartado= 100

        OPEN(UNIT=10, FILE="distA_out.dat")

        ALLOCATE(x(max_iter), d(max_iter))
        READ (*,*) x0, r, epsilons
        
        xi= x0
        x(1)= xi
        x_epsilon= x0 + epsilons
        d(1)= ABS(xi - x_epsilon)

        WRITE(10, *) 1, x(1), d(1)

        do i=2, max_iter
                xi= xi*r*(1-xi)
                x(i)= xi
                x_epsilon= x_epsilon*r*(1-x_epsilon)
                d(i)= ABS(xi - x_epsilon)
                WRITE(10, *) i, x(i), d(i) 

        end do

        do i=descartado+1, max_iter-1 
                soma= soma + log(ABS(r - 2*r*x(i)))
        end do

        lambda= soma/(REAL(max_iter-descartado-1, KIND=8))

        soma_x=0.0D0
        soma_y=0.0D0
        soma_xy=0.0D0
        soma_x_quadrado=0.0D0
        total= REAL(max_iter-descartado-1, KIND=8)

        do i=descartado+1, max_iter-1
                soma_x= soma_x + REAL(i, KIND=8)
                soma_y= soma_y + log(d(i))
                soma_xy= soma_xy + REAL(i, KIND=8) * log(d(i))
                soma_x_quadrado= soma_x_quadrado + REAL(i, KIND=8)**2
        end do

        lambda_deca= (total * soma_xy - soma_x * soma_y)/(total*soma_x_quadrado - (soma_x)**2)

        WRITE(*, *) lambda_deca
        WRITE(*, *) lambda

       CLOSE(10)
       DEALLOCATE(x)
       DEALLOCATE(d)

end program exerA
