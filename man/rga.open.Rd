\name{rga.open}
\alias{rga.open}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Open a GA API instance
}
\description{
  This function creates a Google Analytics API instance, bound to one Google Account.
}
\usage{
  rga.open(instance = "ga", client.id, client.secret, where, envir = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{instance}{
  this should be the name of your reference handle. By default it's set to \"ga\". This will be the class, that will carry all the API functions and token data.
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
  This function returns an class of type rga, this contains all the methods for data extraction, and keeps itself authenticated.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
%%  ~~who you are~~
}
\note{
  When this function runs for the first time, it will open an available browser, and direct you to the authentication on Google. You need to accept the authentication, and copy the code Google gives you, this code needs to be inputted into the console.

  If you can't run the function interactively (on a server for example), just create the instance in an environment where you can, and copy the instance to the non-interactive environment.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
  \dontrun{
    rga.open(instance = "ga");
    profiles <- ga$getProfiles();

    # explore class:
    ga$getRefClass()
  }
}
