primes.sieve <- function(n)
{
	primes = rep(TRUE, n)
	primes[1] = FALSE
	cur.prime = 2L
	k = floor(sqrt(n))
	while(cur.prime <= k)	{
		primes[seq.int(2L*cur.prime, n, cur.prime)] <- FALSE
		sel <- which(primes[(cur.prime + 1):(k+1)])              #Graciously broken if n = 1
		if(any(sel))	  {
			cur.prime <- cur.prime + min(sel)
		} else {
			cur.prime <- k+1}
	}
	which(primes)
}

primes.rec <- function(n){
	primes <- function(p, i = 1)	{
		f <- p %% p[i] == 0 & p != p[i]
		if (any(f))
			p <- primes(p[!f], i+1)
		p
	}
	primes(2:n)
}