\name{mod_step}
\alias{mod_step}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{mod_step
%%  ~~function to do ... ~~
}
\description{
The function returns a model summary that is fitted for the given dataset and the response variable using the specified method (forward, backward, stepwise). This uses p-values and F- values of ANOVA test to fit the model.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mod_step(dataset, responsecol, direction)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{dataset that that need to be fitted a model.datset has to be only contain the response variable and the variables that need to be used for the model (no extra variables allowed)}
  \item{responsecol} {name of the response variable as a string}         \item{direction} {method of fitting the model as a string
                 (forward, backward or stepwise), default is stepwise.}

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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
