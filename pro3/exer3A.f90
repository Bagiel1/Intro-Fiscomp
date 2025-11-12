program exer3A 
    IMPLICIT NONE

    REAL(8):: m, l, o, deltaT, T, g, wi, E, ts, od
    INTEGER:: i
    REAL(8):: pi= 4.0D0 * atan(1.0D0)
    READ (*, *) m, l, o, deltaT, T

    OPEN(UNIT=10, file="exer3A_out.dat")

    o= o * pi / 180.0D0

    ts= 0.0D0
    g= 9.8D0
    wi= 0.0D0

    do i= 1, INT(T/deltaT)+1
        
        WRITE(10, FMT=*) ts, o

        od= o + wi * deltaT
        wi= wi - (g/l) * o * deltaT

        o= od

        if(o > pi) then
            o= o - 2*pi
        end if

        if(o < -pi)then
            o= o + 2*pi
        end if

        ts= ts + deltaT
        E= m*g*l*(1.0D0 - cos(o)) + 0.5D0 * m*(wi*l)**2
        

    end do

    close(10)

end program exer3A
