program exerB

        IMPLICIT NONE

        REAL(8):: rT, vxT, vyT, xT, yT, xT_ant, yT_ant, xT_prox, yT_prox, axT, ayT
        REAL(8):: rJ, vxJ, vyJ, xJ, yJ, xJ_ant, yJ_ant, xJ_prox, yJ_prox, axJ, ayJ
        REAL(8):: dist_TS, dist_JS, dist_TJ
        REAL(8):: tempo, dt, T 
        REAL(8):: pi, GMs, GMj, fator
        REAL(8):: Ms, Mt, Mj, GMt
        CHARACTER(LEN=20):: output

        INTEGER:: i, passos

        Ms= 2.0D30
        Mt= 6.0D24
        Mj= 1.9D27

        rT= 1.0D0
        rJ= 5.2D0
        
        PRINT *, "Digite dt, Tempo Total e Fator:"

        READ(*,*) dt, T, fator

        if (fator < 10.0D0) then
                output= "trajB_x1.dat"
        else if (fator < 500.0D0) then
                output= "trajB_x100.dat"
        else
                output= "trajB_x1000.dat"
        end if

        OPEN(UNIT=10, FILE=output)

        pi= 4.0D0 * ATAN(1.0D0)
        GMs= 4.0D0 * (pi**2)
        
        GMt= GMs * (Mt/Ms)

        GMj= GMs * (Mj/Ms)*fator

        tempo= 0.0D0

        xT_ant= rT
        yt_ant= 0.0D0
        vxT= 0.0D0
        vyT= 2.0D0*pi

        xJ_ant= rJ
        yJ_ant= 0.0D0
        vxJ= 0.0D0
        vyJ= (2.0D0*pi)/sqrt(rJ)


        WRITE(10, *) tempo, xT_ant, yT_ant, xJ_ant, yJ_ant

        xT= xT_ant + vxT * dt
        yT= yT_ant + vyT * dt

        xJ= xJ_ant + vxJ * dt
        yJ= yJ_ant + vyJ * dt

        tempo= tempo + dt

        WRITE(10, *) tempo, xT, yT, xJ, yJ

        passos= INT(T/dt)

        do i=1, passos-1

                dist_TS= sqrt(xT**2+yT**2)
                dist_JS= sqrt(xJ**2+yJ**2)
                dist_TJ= sqrt((xT - xJ)**2 + (yT - yJ)**2)

                axT= -(GMs / (dist_TS**3)) * xT - (GMj / (dist_TJ**3)) * (xT - xJ)
                ayT= -(GMs / (dist_TS**3)) * yT - (GMj / (dist_TJ**3)) * (yT - yJ)

                axJ= -(GMs / (dist_JS**3)) * xJ - (GMt / (dist_TJ**3)) * (xJ - xT)
                ayJ= -(GMs / (dist_JS**3)) * yJ - (GMt / (dist_TJ**3)) * (yJ - yT)

                xT_prox= 2.0D0*xT - xT_ant + axT*(dt**2)
                yT_prox= 2.0D0*yT - yT_ant + ayT*(dt**2)

                xJ_prox= 2.0D0*xJ - xJ_ant + axJ*(dt**2)
                yJ_prox= 2.0D0*yJ - yJ_ant + ayJ*(dt**2)

                xT_ant= xT
                yT_ant= yT
                xJ_ant= xJ
                yJ_ant= yJ
                xT= xT_prox
                yT= yT_prox
                xJ= xJ_prox
                yJ= yJ_prox

                tempo= tempo + dt

                WRITE(10, *) tempo, xT, yT, xJ, yJ

      end do

      close(10)






end program exerB
