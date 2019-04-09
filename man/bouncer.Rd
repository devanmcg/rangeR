\name{bouncer}
\alias{bouncer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Remove rare or low-frequency species from a community dataset
%%  ~~function to do ... ~~
}
\description{Sometimes rare/low-frequency species can make finding a good ordination solution difficult. This function identifies which species in a community dataset fall below a threshold level of frequency or relative abundance.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bouncer(data, measure, level, PrintResults)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a wide-format `data.frame` with species in columns and individual observations (sites) in rows.
%%     ~~Describe \code{data} here~~
}
  \item{measure}{One of `'abundance'` or `'proportion'`, indicating how the threshold should be determined.
%%     ~~Describe \code{measure} here~~
}
  \item{level}{The threshold value below which species will be identified for exclusion.
%%     ~~Describe \code{level} here~~
}
  \item{PrintResults}{Optional. Logical. Should the number of species identified for exclusion be displayed as a message in the console? Leave out or set `'FALSE'` if called in a Markdown file.
%%     ~~Describe \code{PrintResults} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{A vector of species names.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Devan Allen McGranahan (devan.mcgranahan@gmail.com)
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
  require(vegan)
  require(dplyr)
  data(dune)
  spp.rm <- bouncer(data=dune, measure="proportion", level=5, PrintResults = "TRUE")
  dune.Alist <- select(dune, -one_of(spp.rm))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~vegan }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~ordination }% __ONLY ONE__ keyword per line
