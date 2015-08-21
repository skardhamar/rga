rga$methods(
    list(
        getMGMTData = function(url, keep, start, max, lastResult) {
          
            query <- paste(paste("access_token", .self$getToken()$access_token, sep = "="),
                           paste("start-index", start, sep = "="),
                           paste("max-results", 1000, sep = "="), sep = "&")
            queryUrl <- paste(url, query = query, sep = "?")
            request <- GET(queryUrl)
            ga.json <- jsonlite::fromJSON(content(request, "text"))
            if (is.null(ga.json)) {
                stop("data fetching did not output correct format")
            }

            # Check if results from last call were included
            if(!missing(lastResult)) {
              df <- rbind(ga.json$items[keep], lastResult)
            } else {
              df <- ga.json$items[keep]
            }

            # Has the max param been set? Are there more results than the max?
            if(!missing(max) && nrow(df) >= max) {
              print('cutting off process')
              return(df[1:max,])
            # If there's no max, or the results are still less than the max, continue sending calls
            } else {
              # If the API is pointing toward another results page, get it
              if (is.element('nextLink', names(ga.json))) {
                # Set new start point for next call
                newStart <- start + 1000
                print(paste('resultsLen', nrow(df)))
                print(paste('newStart', newStart))
                print(paste('url', url))
                print(paste('nextLink', ga.json$nextLink))
                if(!missing(max)){print(paste('max', max))}
                return(.self$getMGMTData(url = url, keep = keep, newStart, max = max, lastResult = df))
              # If the API doesn't have another page of results, return the total set
              } else {
                return(df)
              }
            }
        },
        getAccounts = function(start = 1, max) {
            url <- "https://www.googleapis.com/analytics/v3/management/accounts"
            return(.self$getMGMTData(url, c("id", "name", "created", "updated"), start = start, max = max))
        },
        getWebProperties = function(accountId = "~all", start = 1, max) {
            url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/",
                         accountId, "/webproperties", sep = "")
            return(.self$getMGMTData(url, c("id", "name", "websiteUrl", "level", "industryVertical", "created", "updated"), start = start, max = max))
        },
        getProfiles = function(accountId = "~all", webPropertyId = "~all", start = 1, max) {
            url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/",
                         accountId, "/webproperties/", webPropertyId, "/profiles", sep = "")
            return(.self$getMGMTData(url, c("id", "accountId", "webPropertyId", "name", "currency", "timezone", "eCommerceTracking", "websiteUrl", "created", "updated"), start = start, max = max))
        },
        getGoals = function(accountId = "~all", webPropertyId = "~all", profileId = "~all", start = 1, max) {
            url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/",
                         accountId, "/webproperties/", webPropertyId, "/profiles/", profileId, "/goals", sep = "")
            return(.self$getMGMTData(url, c("id", "accountId", "webPropertyId", "profileId", "name", "value", "active", "type", "created", "updated"), start = start, max = max))
        },
        getSegments = function(start = 1, max) {
            url <- paste("https://www.googleapis.com/analytics/v3/management/segments", sep = "")
            return(.self$getMGMTData(url, c("id", "segmentId", "name", "definition", "type"), start = start, max = max))
        }
    )
)
