\name{rga.open}
\alias{rga.open}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Open GA API
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rga.open(instance = 'ga', client.id, client.secret, where, envir = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{instance}{
  this should be the name of your reference handle. By default it's set to "ga". This will be the class, that will carry all the API functions and token data.
  %\code{test blab albalbadf blabla bla}
%%     ~~Describe \code{rga} here~~
}
  \item{client.id}{
  this should be set the client id provided by Google Analytics API. By default it is set to a default application.
%%     ~~Describe \code{refresh} here~~
}
  \item{client.secret}{
  this should be set the client secret provided by Google Analytics API. By default it is set to a default application.
%%     ~~Describe \code{refresh} here~~
}
  \item{where}{
  if this is set to an arbitrary file, the instance will be saved here and later acquired from here. Use this for continous work with the Google Analytics API.
%%     ~~Describe \code{refresh} here~~
}
  \item{envir}{
  where should the instance be stored.
%%     ~~Describe \code{refresh} here~~
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
rga.open(instance = 'ga')
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
