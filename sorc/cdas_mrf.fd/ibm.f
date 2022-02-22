	subroutine exit(n)
c
c	want to set a return code on ibm/sp
c
	if (n.eq.0) stop 0
	if (n.eq.1) stop 1
	if (n.eq.2) stop 2
	if (n.eq.3) stop 3
	if (n.eq.4) stop 4
	if (n.eq.5) stop 5
	if (n.eq.6) stop 6
	if (n.eq.7) stop 7
	if (n.eq.8) stop 8
	if (n.eq.9) stop 9
	stop 10
	end
