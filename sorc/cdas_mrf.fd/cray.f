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
