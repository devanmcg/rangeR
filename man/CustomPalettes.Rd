\name{Custom color palettes}
\alias{cbPal5, cbPal8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Some customized color palettes for \code{ggplot} 
%%  ~~function to do ... ~~
}
\description{Emphasis on contrast for color-blind individuals and grayscale printing. Applies to \code{ggplot} or other \code{R} graphics.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
These are pulled from the \code{magma} gradient in \code{viridisLite}: 

  cbPal5   # Five colors 
  cbPal8   # Eight colors

}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{A vector of hexadecimal colors.
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
library(ggplot2)
library(dplyr)
mtcars \%>\%
  mutate(cyl = as.factor(cyl)) \%>\%
    ggplot(aes(x=disp, y=mpg)) +
      geom_point(aes(color=cyl)) + 
      scale_color_manual(values=cbPal5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
