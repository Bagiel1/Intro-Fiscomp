program exerA
        IMPLICIT NONE

        REAL(8):: y, yant, vo, GMs
        REAL(8):: x, xant, r, x_atual, y_atual
        REAL(8):: T, deltaT, rs, pi, T_atual
        INTEGER:: i, iter
        REAL(8) :: a_inv, a

        open(UNIT=10, FILE='trajA1_out.dat')

        READ(*,*) r, vo, deltaT

        pi= 4.0D0*atan(1.0D0)
       

        GMs= 4.0D0*pi**2
        T_atual= 0

        a_inv = (2.0D0 / r) - (vo**2 / GMs)
        a = 1.0D0 / a_inv

        T= sqrt(a**3)

        iter= int(T/deltaT)

        yant= 0.0D0
        xant= r

        x_atual= xant
        y_atual= yant + vo*deltaT  

        WRITE(10, *) T_atual, xant, yant
        T_atual= T_atual + deltaT
        WRITE(10, *) T_atual, x_atual, y_atual

        do i= 1, iter-1
                rs= sqrt(x_atual**2+y_atual**2)
                y= 2.0D0*y_atual - yant - (GMs)/(rs**3)*y_atual*(deltaT)**2
                x= 2.0D0*x_atual - xant - (GMs)/(rs**3)*x_atual*(deltaT)**2

                yant= y_atual
                xant= x_atual

                y_atual= y
                x_atual= x

                T_atual= T_atual + deltaT

                WRITE(10, *) T_atual, x, y

        end do

        close(10)

        WRITE(*,*) "Se dt for grande, como 0.1, a iteração faz com que a Terra ou o planeta percorra &
                em linha reta por mais tempo, &
                causando irregularidades na orbita. Por outro lado se dt for pequeno, como 0.001, &
                a iteração é tão curta que a linha &
                nao é percebida, gerando uma orbita estável. Portanto, para obter uma orbita estável, & 
                dt tem que ser bem menor que o &
                período. Além disso, a falta da conservação da energia se da & 
                pois o metodo de Verlet trunca a serie de Taylor em &
                ordem 4, quanto maior for dt, maior o erro associado."
        
end program exerA
