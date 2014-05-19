.extend.rga.mgmt <- function() {
    rga$methods(
        list(
            processManagementData = function(url, keep) {
                ga.json <- jsonlite::fromJSON(content(GET(url), "text"))
                if (is.null(ga.json)) {
                    stop("data fetching did not output correct format")
                }
                df <- ga.json$items
                return(df[keep])
            },
            getAccounts = function(start = 1, max = 1000) {
                url <- paste("https://www.googleapis.com/analytics/v3/management/accounts",
                             "?access_token=", .self$getToken()$access_token,
                             "&start-index=", start,
                             "&max-results=", max,
                             sep = "", collapse = "")
                return(.self$processManagementData(url, c("id", "name", "created", "updated")))
            },
            getWebProperties = function(accountId = "~all", start = 1, max = 1000) {
                url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/", accountId, "/webproperties",
                             "?access_token=", .self$getToken()$access_token,
                             "&start-index=", start,
                             "&max-results=", max,
                             sep = "", collapse = "")
                return(.self$processManagementData(url, c("id", "name", "websiteUrl", "created", "updated")))
            },
            getProfiles = function(accountId = "~all", webPropertyId = "~all", start = 1, max = 1000) {
                url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/", accountId, "/webproperties/", webPropertyId, "/profiles",
                             "?access_token=", .self$getToken()$access_token,
                             "&start-index=", start,
                             "&max-results=", max,
                             sep = "", collapse = "")
                # possible deparse.error, sapply(test$items,length)
                return(.self$processManagementData(url, c("id", "accountId", "webPropertyId", "name", "currency", "timezone", "eCommerceTracking", "websiteUrl", "created", "updated")))
            },
            getGoals = function(accountId = "~all", webPropertyId = "~all", profileId = "~all", start = 1, max = 1000) {
                # FIX: deparse error
                url <- paste("https://www.googleapis.com/analytics/v3/management/accounts/", accountId, "/webproperties/", webPropertyId, "/profiles/", profileId, "/goals",
                             "?access_token=", .self$getToken()$access_token,
                             "&start-index=", start,
                             "&max-results=", max,
                             sep = "", collapse = "")
                return(.self$processManagementData(url, c("id", "accountId", "webPropertyId", "profileId", "name", "value", "active", "type", "created", "updated")))
            },
            getSegments = function(start = 1, max = 1000) {
                url <- paste("https://www.googleapis.com/analytics/v3/management/segments",
                             "?access_token=", .self$getToken()$access_token,
                             "&start-index=", start,
                             "&max-results=", max,
                             sep = "", collapse = "")
                return(.self$processManagementData(url, c("id", "segmentId", "name", "definition", "type")))
            }
        )
    )
}
