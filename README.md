# R Google Analytics

This is a package for extracting data from Google Analytics into R.

The package uses OAuth 2.0 ([protocol](http://tools.ietf.org/html/draft-ietf-oauth-v2-22)) to access the Google Analytics API.

## Installation

### Manually

Download from GitHub.

	$ R CMD INSTALL rga -l /path/to/rga/rga_0.9.tar.gz

And then type:

	library("rga", lib.loc="/path/to/rga/rga_0.9.tar.gz")

### CRAN

Currently not on CRAN.

## Usage

The principle of this package is to create an instance of the API Authentication, which is a S4/5-class (utilizing the setRefClass). This instance then contains all the functions needed to extract data, and all the data needed for the authentication and reauthentication. The class is in essence self sustaining.

#### Basic use

The instance is created with the `rga.open` command:

	rga.open(instance="ga")

This will check if the instance is already created, and if it is, it'll prepare the token. If the instance is not created, it'll create the instance, and redirect the client to a browser for authentication with Google.

#### Advanced use

If you want to store the instance locally, this can be done by adding the `where` attribute:

	rga.open(instance="ga", where="~/ga.rga")

This means, that even if you delete the `.RData` workspace, the package will make sure you have access to the API.