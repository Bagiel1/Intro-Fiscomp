program exer2
        IMPLICIT NONE

        REAL(8):: deltaT= 0.1D0
        REAL(8):: vel= 500.0D0
        REAL(8):: vx, xi, xid, vy, yi, yid, vyd
        INTEGER:: i
        REAL(8):: ang
        REAL(8):: g= 9.8D0
        REAL(8):: pi= 4.0D0*ATAN(1.0D0)
        INTEGER:: unidade_arquivo= 10

        OPEN(UNIT= unidade_arquivo, FILE= 'output2.txt')
        READ (*,*) ang
        ang= ang*2.0D0*pi/360.0D0

        xid= 0.0D0
        yid= 0.0D0

        WRITE(UNIT= unidade_arquivo, FMT=*) xid, yid   

        vx= vel*cos(ang)
        vyd= vel*sin(ang)

        do i=1, 100000
              xi= xid + vx*deltaT
              xid= xi
              vy= vyd - g*deltaT
              yi= yid + vy*deltaT
              vyd= vy
              yid= yi

              if(yid <= 0)then
                      exit
              end if

              WRITE(UNIT= unidade_arquivo, FMT=*) xi, yi
           
        end do

end program exer2
              


