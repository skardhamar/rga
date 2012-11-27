# R Google Analytics

This is a package for extracting data from Google Analytics into R.

The package uses OAuth 2.0 ([protocol](http://tools.ietf.org/html/draft-ietf-oauth-v2-22)) to access the Google Analytics API.

## Installation

### CRAN

Currently not on CRAN.

### Manually

Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

## Usage

The principle of this package is to create an instance of the API Authentication, which is a S4/5-class (utilizing the setRefClass). This instance then contains all the functions needed to extract data, and all the data needed for the authentication and reauthentication. The class is in essence self sustaining.

The instance is created with the `rga.open` command:

	rga.open(instance="ga")

