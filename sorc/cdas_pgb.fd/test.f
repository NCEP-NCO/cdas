
        character*1 carray(10)

        carray = 'a'
        write(*,*) carray

        carray(2:4) = 'b'
        write(*,*) carray
        stop
        end
