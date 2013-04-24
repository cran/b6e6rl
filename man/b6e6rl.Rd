\name{b6e6rl}
\alias{b6e6rl}
\title{
Adaptive differential evolution, b6e6rl algorithm
}
\description{
This function serches for the global minimum using b6e6rl variant of adaptive differential evolution.
}
\usage{
b6e6rl(fn_name, a, b, N, my_eps, max_evals, n0, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn_name}{
Name of function which minimum is to find
}
  \item{a}{
Vector of lower bounds of the search space (length=dimension of the search space)
}
  \item{b}{
Vector of upper bounds of the search space (length=dimension of the search space)
}
  \item{N}{
Size of population
}
  \item{my_eps}{
Small positive value, the algortihm stops when fmax-fmin < my_eps
}
  \item{max_evals}{
Maximum count of function evaluations per one dimension of the problem
}
  \item{n0}{
Input parameter controling the competition of the strategies, usualy n0=2
}
  \item{delta}{
Input parameter (critical probability), usualy delta=1/60
}
}
\value{
\item{x_star }{Aproximation of the global minimum point found by search (vector of length=d)}
\item{fn_star }{Functional value at x_star}
\item{func_evals }{Count of function evaluations}
\item{success }{Count of succesfull generations of the trial point}
\item{nrst}{Count of resets, when any probability value is less than delta}
\item{cni}{Counts of succesful selection of each strategy (vector of length=12)}

}
\references{
Tvrdik, J. Adaptation in Differential Evolution: A Numerical Comparison. APPL SOFT 
COMPUT. 2009, Vol. 9, pp. 1149-1155.

Tvrdik, J. Self-adaptive Variants of Differential Evolution with Exponential Crossover. 
Analele Universitatii de Vest, Timisoara.Seria Matematica-Informatica.. 2009, Vol. 47, pp. 151-
168. 
}
\author{
Marek Spruzina, University of Ostrava
}
\examples{
##Example of the b6e6rl call

fn_name <- ("f_dejong")
a <- c(-30,-30,-30)
b <- c(30,30,30)
N <- 60
max_evals <- 20000
my_eps <- 0.000001
n0 <- 2
delta <- 1/(5*12)

b6e6rl(fn_name, a, b, N, my_eps, max_evals, n0, delta)


}

\keyword{ b6e6rl }
\keyword{ adaptive }
\keyword{ differential }
\keyword{ evolution }
