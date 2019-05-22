\name{var.view}
\alias{var.view}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{View eigenvalues and variance explained
%%  ~~function to do ... ~~
}
\description{Succinctly view eigenvalues and variance explained by (metric) ordination objects returned by \code{vegan}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
var.view(ord, axes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ord}{A (metric) \code{vegan} ordination object.
%%     ~~Describe \code{ord} here~~
}
  \item{axes}{Number of axes to view. 
%%     ~~Describe \code{axes} here~~
}
}
\details{Simply rounds the results of \code{summary(eigenvalues(ord))}. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{A matrix of eigenvectors, Proportion Explained, and Cumulative Proportion for each axis defined by 1:axes.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Oksanen et al. (2019) vegan: Community Ecology Package. R package version 2.5-4.
  \url{https://CRAN.R-project.org/package=vegan}
%% ~put references to the literature/web site here ~
}
\author{Wrapped by Devan McGranahan (devan.mcgranahan@gmail.com) but based on key vegan functions by Jari Oksanen
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(vegan)
data(dune) 
dune.mds <- capscale(dune ~ 1)  
var.view(dune.mds, 10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~vegan}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~eigenvalues }% __ONLY ONE__ keyword per line
