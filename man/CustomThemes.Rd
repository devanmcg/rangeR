\name{Custom ggplot2 themes}
\alias{theme_bm1, theme_bw2, theme_empty, theme_map, theme_ord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Some customized \code{ggplot} themes 
%%  ~~function to do ... ~~
}
\description{Minor adjustments to \code{theme_bw()} for customized \code{ggplot} graphics.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
theme_bm1( )    # transparent panel for color backgrounds, e.g. beamer presentations
theme_bw2( )    # update to theme_bw
theme_empty( )  # general graphics, e.g. conceptual models
theme_map( )    # maps
theme_ord( )    # ordinations
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Mostly just adjust the base font size.
%%     ~~Describe \code{x} here~~
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
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
# Make a plot with a transparent background
# for a beamer presentation: 

library(ggplot2)
ggplot(mtcars, aes(x=disp, y=mpg)) +
  theme_bm1(20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
