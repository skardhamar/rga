rga$methods(
    list(
        getMGMTData = function(url, keep, start, max) {

            query <- paste(paste("access_token", .self$getToken()$access_token, sep = "="),
                           paste("start-index", start, sep = "="),
                           paste("max-results", max, sep = "="), sep = "&")
            url <- paste(url, query = query, sep = "?")
            request <- GET(url)
            ga.json <- jsonlite::fromJSON(content(request, "text"))
            if (is.null(ga.json)) {
                stop("data fetching did not output correct format")
            }
            df <- ga.json$items
            return(df[keep])
        },
        getAccounts = function(start = 1, max = 1000) {
            url <- "https://www.googleapis.com/analytics/v3/management/accounts"
            return(.self$getMGMTData(url, c("id", "name", "created", "updated"), start = start, max = max))
        },
        getWebProperties = function(accountId = "~all", start = 1, max = 1000) {
            url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/",
                         accountId, "/webproperties", sep = "")
            return(.self$getMGMTData(url, c("id", "name", "websiteUrl", "level", "industryVertical", "created", "updated"), start = start, max = max))
        },
        getProfiles = function(accountId = "~all", webPropertyId = "~all", start = 1, max = 1000) {
            url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/",
                         accountId, "/webproperties/", webPropertyId, "/profiles", sep = "")
            return(.self$getMGMTData(url, c("id", "accountId", "webPropertyId", "name", "currency", "timezone", "eCommerceTracking", "websiteUrl", "created", "updated"), start = start, max = max))
        },
        getGoals = function(accountId = "~all", webPropertyId = "~all", profileId = "~all", start = 1, max = 1000) {
            url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/",
                         accountId, "/webproperties/", webPropertyId, "/profiles/", profileId, "/goals", sep = "")
            return(.self$getMGMTData(url, c("id", "accountId", "webPropertyId", "profileId", "name", "value", "active", "type", "created", "updated"), start = start, max = max))
        },
        getSegments = function(start = 1, max = 1000) {
            url <- paste("https://www.googleapis.com/analytics/v3/management/segments", sep = "")
            return(.self$getMGMTData(url, c("id", "segmentId", "name", "definition", "type"), start = start, max = max))
        }
    )
)
