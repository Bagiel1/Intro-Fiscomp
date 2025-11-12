program exer1 
        IMPLICIT NONE
        
        REAL(8):: deltaT
        REAL(8):: T
        REAL(8):: Ta= 0.0D0
        INTEGER:: P= 400
        REAL(8):: v0
        REAL(8):: vi
        REAL(8):: vid
        INTEGER:: unidade_arquivo= 10
        INTEGER:: M= 80
        INTEGER:: i

        OPEN(UNIT= unidade_arquivo, FILE= 'vel1_out.dat')
        READ (*,*) T, deltaT, v0 
        vi= v0
       
        WRITE(UNIT= unidade_arquivo, FMT=*) Ta, v0 

        do i=1, 100000
              vid= vi + (P/(M*vi))*deltaT
              Ta= Ta + deltaT
              vi= vid
    
              if(Ta > T) then
                      exit
              end if
              WRITE(UNIT= unidade_arquivo, FMT=*) Ta, vi
        end do


end program exer1
        
