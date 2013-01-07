# R Google Analytics

This is a package for extracting data from Google Analytics into R.

The package uses OAuth 2.0 ([protocol](http://tools.ietf.org/html/draft-ietf-oauth-v2-22)) to access the Google Analytics API.

## Installation

### Manually

Install the [devtools](https://github.com/hadley/devtools) package:

	install.packages("devtools")
	library(devtools)

And then run the `install_github` command:

	install_github("rga", "skardhamar")
	library(rga)

## Authenticating

The principle of this package is to create an instance of the API Authentication, which is a S4/5-class (utilizing the setRefClass). This instance then contains all the functions needed to extract data, and all the data needed for the authentication and reauthentication. The class is in essence self sustaining.

This means that you can create as many instances as you need.

#### Basic use

The instance is created with the `rga.open` command:

	rga.open(instance="ga")

This will check if the instance is already created, and if it is, it'll prepare the token. If the instance is not created, it'll create the instance, and redirect the client to a browser for authentication with Google.

You then have to authenticate the application, Google will then output an access code, which you need to enter in the R console.

#### Advanced use

If you want to store the instance locally, this can be done by adding the `where` attribute:

	rga.open(instance="ga", where="~/ga.rga")

This means, that even if you delete the `.RData` workspace, the package will make sure you have access to the API.

#### Use own Google API Client

If you want to use your own Google API Client, you need to provide this data in the `rga.open`:

	rga.open(instance = "ga", 
			 client.id = "862341168163-qtefv92ckvn2gveav66im725c3gqj728.apps.googleusercontent.com", 
			 client.secret = "orSEbf0-S76VZv6RMHe46z_N")

Create a project in [Google API Console](https://code.google.com/apis/console/) to acquire `client.id` and `client.secret`.

## Extracting data

In order to extract data from the instance, there is a couple of commands to use. The most important one is `$getData`:

	ga$getData(ids, start.date, end.date, 
			   metrics = "ga:visits", dimensions = "ga:date", 
			   sort = "", filters = "", segment = "",
			   start = 1, max = 1000)

This will output the data in a data frame, with all the correct formats applied. The syntax follows the one dictated by Google - please refer to the documentation for further information.

The dates defaults to the current day, meaning that if you don't input these, only data from today will be extracted.