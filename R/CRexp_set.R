CRexp_set <-
function(p,d){
y <- matrix(0,1,d+1)
y[1] <- 1
y[d] <- -d*p
y[d+1] <- d*p-1
r <- polyroot(rev(y))		
r <- Re(r)		
ri <- Im(r)
zrus <- which(ri != 0)	
if (is.empty(zrus) != TRUE){
	r <- r[-zrus]		
}			
zrus <- which((r<0) | (r>=1))
if (is.empty(zrus) != TRUE){
	r <- r[-zrus]	
}			
r <- sort(r)
CR <- r[1]
if (is.empty(r) != TRUE){
	CR <- 0
}	
return(CR)
}
