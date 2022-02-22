	integer function ismax(n,x,inc)
c
c	returns index of first max value
c
	dimension x(*)
	if (n.le.0) then
	    ismax = 0
	    return
	endif
	r = x(1)
	j = 1
	do i = 1, (n-1)*inc+1, inc
	    if (r .lt. x(i)) then
		r = x(i)
		j = i
	    endif
	enddo
	ismax = (j-1)/inc + 1
	return
	end
	integer function ismin(n,x,inc)
c
c	returns index of first min value
c
	dimension x(*)
	if (n.le.0) then
	    ismin = 0
	    return
	endif
	r = x(1)
	j = 1
	do i = 1, (n-1)*inc+1, inc
	    if (r .gt. x(i)) then
		r = x(i)
		j = i
	    endif
	enddo
	ismin = (j-1)/inc + 1
	return
	end

	integer function isrchne(n, x, inc, target)
c
c	search a x(..) for 1st element ne to target
c
	integer x(*), target
	integer n, inc

	isrchne = 0
	if (n.le.0) return
	j = 1
	do i = 1, n
	    if (x(j).ne.target) then
		isrchne = i
		return
	    endif
	    j = j + inc
	enddo
	isrchne = n + 1
	return
	end

c real version of isrchne
        integer function isrchner(n, x, inc, target)
c
c       search a x(..) for 1st element ne to target
c
        real x(*), target
        integer n, inc

        isrchner = 0
        if (n.le.0) return
        j = 1
        do i = 1, n
            if (x(j).ne.target) then
                isrchner = i
                return
            endif
            j = j + inc
        enddo
        isrchner = n + 1
        return
        end


        integer function isrcheq(n, x, inc, target)
c
c       search a x(..) for 1st element eq to target
c
        integer x(*), target
        integer n, inc

        isrcheq = 0
        if (n.le.0) return
        j = 1
        do i = 1, n
            if (x(j).eq.target) then
                isrcheq = i
                return     
            endif     
            j = j + inc
        enddo
        isrcheq = n + 1
        return
        end   

c
c real version
c          
        integer function isrcheqr(n, x, inc, target)
c              
c       search a x(..) for 1st element eq to target
c              
        real x(*), target
        integer n, inc
               
        isrcheqr = 0
        if (n.le.0) return
        j = 1  
        do i = 1, n
            if (x(j).eq.target) then
                isrcheqr = i
                return
            endif
            j = j + inc
        enddo  
        isrcheqr = n + 1
        return 
        end    
c
c logical version             
c       
        integer function isrcheql(n, x, inc, target)
c       
c       search a x(..) for 1st element eq to target
c       
        logical x(*), target
        integer n, inc
        
        isrcheql = 0
        if (n.le.0) return
        j = 1
        do i = 1, n
            if (x(j).eqv.target) then
                isrcheql = i
                return
            endif
            j = j + inc
        enddo
        isrcheql = n + 1
        return
        end
        

        integer function isrchfle(n, x, inc, target)
c
c       search a x(..) for 1st element le to target
c
        real x(*), target
        integer n, inc

        isrchfle = 0
        if (n.le.0) return
        j = 1
        do i = 1, n
            if (x(j).le.target) then
                isrchfle= i
                return     
            endif     
            j = j + inc
        enddo
        isrchfle = n + 1
        return
        end

        integer function isrchflt(n, x, inc, target)
c              
c       search a x(..) for 1st element lt to target
c              
        real x(*), target
        integer n, inc  
               
        isrchflt = 0
        if (n.le.0) return
        j = 1  
        do i = 1, n
            if (x(j).lt.target) then
                isrchflt= i
                return 
            endif      
            j = j + inc
        enddo         
        isrchflt = n + 1
        return 
        end    

        integer function isrchfgt(n, x, inc, target)
c                     
c       search a x(..) for 1st element gt to target
c                                                  
        real x(*), target                          
        integer n, inc   
                         
        isrchfgt = 0    
        if (n.le.0) return
        j = 1             
        do i = 1, n
            if (x(j).gt.target) then
                isrchfgt= i
                return 
            endif     
            j = j + inc
        enddo         
        isrchfgt = n + 1
        return         
        end           

