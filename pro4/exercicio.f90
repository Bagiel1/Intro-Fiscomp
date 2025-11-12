program exercicio
        IMPLICIT NONE

        REAL(8):: xi, xid, yi, yid, zi, zid, m, gammaM, ti, O, phi, vi
        REAL(8):: vxi, vxid, vyi, vyid, vzi, vzid, betaM, omega, deltaT, g
        REAL(8):: a1, a2, vd, delta, pi
        INTEGER:: i

        g= 9.8D0
        vi= 100D0/3.6D0
        deltaT= 0.01D0
        a1= 0.0039D0
        a2= 0.0058D0
        ti= 0.0D0
        vd= 35.0D0
        delta= 5.0D0
        m= 1.0D0
        pi= 4.0D0 * atan(1.0D0)
        omega= 39.0D0

        OPEN(UNIT=10, FILE='chute_out.dat')

        READ (*,*) betaM, O, phi
        
        O= O * pi / 180
        phi= phi * pi / 180        

        vxi= vi * sin(O) * cos(phi)
        vyi= vi * sin(O) * sin(phi)
        vzi= vi * cos(O)

        xi= 0D0
        yi= 0D0
        zi= 0D0

        do i=0, 1000000
                
                WRITE(10, *) xi, yi

                if(xi >= 40)then
                        if(yi > 4 .AND. yi < 10 .AND. zi < 2.5 .AND. zi > 0)then
                                       print *, "sim"
                                       exit
                        else
                                print *, "nao"
                                exit
                        end if
                end if
                
                xid= xi + vxi*deltaT
                yid= yi + vyi*deltaT
                zid= zi + vzi*deltaT

                if(zid <= 0 .AND. i > 0)then
                        print *, "nao"
                        exit
                end if

                xi= xid
                yi= yid
                zi= zid

                vi= sqrt(vxi**2 + vyi**2 + vzi**2) 
                gammaM= a1 + (a2/(1 + exp((vi-vd)/delta)))
                vxid= vxi - (gammaM*vi*vxi + betaM*omega*vyi)*deltaT
                vyid= vyi - (gammaM*vi*vyi - betaM*omega*vxi)*deltaT
                vzid= vzi - (g + gammaM*vi*vzi)*deltaT

                vxi= vxid
                vyi= vyid
                vzi= vzid

                ti= ti + deltaT
               
        end do 

    close(10)

end program exercicio
