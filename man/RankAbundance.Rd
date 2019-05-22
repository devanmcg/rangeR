\name{RankAbundance}
\alias{RankAbundance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate rank abundance curves without loading \code{BiodiversityR}
%%  ~~function to do ... ~~
}
\description{This is simply \code{BiodiversityR::rankabundance}, which is a convenient function for rank abundance curves but loading the whole package to get it is a PITA since it loads all sorts of dependencies.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RankAbundance(x, y = "", factor = "", level, digits = 1, t = qt(0.975, df = n - 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
The following is verbatim from \code{BiodiversityR::rankabundance}: 
  \item{x}{Community data frame with sites as rows, species as columns and species abundance as cell values.
%%     ~~Describe \code{x} here~~
}
  \item{y}{Environmental data frame.
}
  \item{factor}{Variable of the environmental data frame that defines subsets to calculate rank abundance curves for.
%%     ~~Describe \code{factor} here~~
}
  \item{level}{Level of the variable to create the subset to calculate rank abundance curves.
%%     ~~Describe \code{level} here~~
}
  \item{digits}{Number of digits in the results.
%%     ~~Describe \code{digits} here~~
}
  \item{t}{t-value to calculate confidence interval limits for the species proportion for cluster sampling (following Hayek and Buzas 1997).
%%     ~~Describe \code{t} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Kindt, R. & Coe, R. (2005) Tree diversity analysis. A manual and software for common statistical
  methods for ecological and biodiversity studies. World Agroforestry Centre (ICRAF), Nairobi. ISBN
  92-9059-179-X. \url{http://www.worldagroforestry.org/output/tree-diversity-analysis}
  
  Hayek, L.-A. C. & Buzas, M.A. (1997) Surveying Natural Populations. Columbia University Press
  
  \url{http://www.worldagroforestry.org/output/tree-diversity-analysis}
%% ~put references to the literature/web site here ~
}
\author{R. Kindt & R. Coe
%%  ~~who you are~~
}
\note{While the function could be used solely as presented, it is included here because it is the engine of other wrapper functions in the package (see \strong{See Also}).
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{bouncer}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
