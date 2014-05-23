.extend.rga.mgmt <- function() {
    rga$methods(
        list(
            getMGMTData = function(url, keep) {
                query <- paste(paste("access_token", .self$getToken()$access_token, sep = "="),
                               paste("start-index", start, sep = "="),
                               paste("max-results", max, sep = "="), sep = "&")
                url <- modify_url(url, query = query)
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
                return(.self$getMGMTData(url, c("id", "name", "created", "updated")))
            },
            getWebProperties = function(accountId = "~all", start = 1, max = 1000) {
                url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/",
                             accountId, "/webproperties", sep = "", collapse = "")
                return(.self$getMGMTData(url, c("id", "name", "websiteUrl", "level", "industryVertical", "created", "updated")))
            },
            getProfiles = function(accountId = "~all", webPropertyId = "~all", start = 1, max = 1000) {
                url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/",
                             accountId, "/webproperties/", webPropertyId, "/profiles", sep = "", collapse = "")
                # possible deparse.error, sapply(test$items,length)
                return(.self$getMGMTData(url, c("id", "accountId", "webPropertyId", "name", "currency", "timezone", "eCommerceTracking", "websiteUrl", "created", "updated")))
            },
            getGoals = function(accountId = "~all", webPropertyId = "~all", profileId = "~all", start = 1, max = 1000) {
                # FIX: deparse error
                url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/",
                             accountId, "/webproperties/", webPropertyId, "/profiles/", profileId, "/goals", sep = "", collapse = "")
                return(.self$getMGMTData(url, c("id", "accountId", "webPropertyId", "profileId", "name", "value", "active", "type", "created", "updated")))
            },
            getSegments = function(start = 1, max = 1000) {
                url <- paste("https://www.googleapis.com/analytics/v3/management/segments", sep = "", collapse = "")
                return(.self$getMGMTData(url, c("id", "segmentId", "name", "definition", "type")))
            }
        )
    )
}
