# R Google Analytics

This is a package for extracting data from Google Analytics into R.

The package uses OAuth 2.0 ([protocol](http://tools.ietf.org/html/draft-ietf-oauth-v2-22)) to access the Google Analytics API.

### News / changelist

- Pulling data in batches has been added
- Pulling unsampled data has been added
- No more SSL errors (thanks to Schaun Wheeler, who also has been added as a collaborator!)
- No more parseing errors
- A bunch of tweaks

## Installation

### Manually

Since `rga` is still under development it is not yet on CRAN, please download the development version. You can get the latest version from github with:

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

This will output the data in a data frame, with all the correct formats applied. 

`ids` refers to one's site-specific Analytics "profile ID"; if one doesn't know it, this can be typically found in URLs in the GA interface as the number following a "p" in the URL (eg. the ID for a URL like `https://www.google.com/analytics/web/#report/visitors-overview/a18912926w37930778p37491797/` would be `37491797`.)

The syntax for dimensions/filters/segments follows the one dictated by Google - please refer to the Google Analytics API documentation such as ["Dimensions & Metrics Reference"](https://developers.google.com/analytics/devguides/reporting/core/dimsmets) for further information. (Note that the argument to `metrics` is a comma-delimited character-string, not a vector of character-strings, so one specifies arguments like `"ga:pageviews,ga:sessions,ga:visitors"`.)

The dates defaults to the current day, meaning that if you don't input these, only data from today will be extracted.

## Extracting more observations than 10,000

The Google Analytics API has a natural limit of 10,000 observations pr. pull. Therefore there has been added the possibility to extract data in batches. The `$getData`-function will now throw a message if not all the observations are being extracted. 

In order to extract this data, just use the `batch`-attribute, for example:

	ga$getData(ids, batch = TRUE, start.date, end.date, 
			   metrics = "ga:visits", dimensions = "ga:date,ga:medium,ga:source", 
			   sort = "", filters = "", segment = "")

Alternatively you can set the batch to an integer, and the function will pull the date in batches of this integer. If you just set it to `TRUE` it will automatically pull the data in batches of 10.000 observations (which is what is the maximum allowed observations).

Notice that in this example the `max`-attribute is missing, if this is the case, the function will automatically pull ALL the data. If you however set the `max`-attribute and the `batch` to true, the function will pull the data in batches untill it reaches the `max`.

## Get the first date with data

In order to get the date that contains the first data, use the function:

	ga$getFirstDate(ids)

This function will do a lookup for first available data.

## Get the data unsampled

In some cases where there exists large amount of data, Google Analytics will return sampled data. In order to avoid this, you can partition the query into multiple small querys (day-by-day). One reason of sampling is if a query includes more than 500,000 sessions and is not one of the pre-aggregated queries. Using an advanced segment or filter in a query will generally mean that sampling will occur if the 500,000 sessions are exceeded.

You can get this day-by-day data by using the `walk`-attribute, which in effect will 'walk' through the the data set day-by-day, this results in unsampled data (set `batch` to TRUE to require ALL data), for example:

	ga$getData(ids, batch = TRUE, walk = TRUE, 
			   start.date, end.date, 
			   metrics = "ga:visits,ga:transactions", 
			   dimensions="ga:keyword",
			   filter="ga:country==Denmark;ga:medium==organic")

However, this will result in a lot of requests made to the API, this can result in hitting the quota limit. So use with care.
