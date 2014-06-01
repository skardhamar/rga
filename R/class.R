# set rga refClass
rga <- setRefClass("rga",
                   fields = list(
                       client.id = "character",
                       client.secret = "character",
                       where = "character",
                       token = "list"),
                   methods = list(
                       initialize = function(client.id, client.secret, where, token) {
                           .self$where <- where
                           .self$client.id <- client.id
                           .self$client.secret <- client.secret
                           .self$setToken(token, initiate = TRUE)
                           return(.self)
                       },
                       prepare = function() {
                           if (.self$isTokenExpired()) {
                               .self$refreshToken()
                           }
                       },
                       isToken = function() {
                           if (!is.null(.self$token)) {
                               return(TRUE)
                           } else {
                               return(FALSE)
                           }
                       },
                       isTokenExpired = function() {
                           if (.self$isToken()) {
                               if (.self$tokenExpiresIn() <= 0) {
                                   return(TRUE)
                               } else {
                                   return(FALSE)
                               }
                           } else {
                               stop("token is not created")
                           }
                       },
                       tokenExpiresIn = function() {
                           if (.self$isToken()) {
                               return(.self$token$expires_in - (as.numeric(Sys.time()) - .self$token$timestamp.refresh))
                           } else {
                               stop("token is not created")
                           }
                       },
                       setToken = function(token, initiate = FALSE) {
                           .self$token <- token
                           if (.self$isWhere() && !initiate) {
                               saveRDS(.self, file = .self$where)
                           }
                       },
                       getToken = function(refresh = TRUE) {
                           if (.self$isToken()) {
                               if (.self$isTokenExpired() && refresh) {
                                   .self$refreshToken()
                               }
                               return(.self$token)
                           } else {
                               stop("token is not created")
                           }
                       },
                       refreshToken = function() {
                           raw.response <- POST("https://accounts.google.com/o/oauth2/token",
                                                body = list(
                                                    refresh_token = .self$token$refresh_token,
                                                    client_id = .self$client.id,
                                                    client_secret = .self$client.secret,
                                                    grant_type = "refresh_token"))

                           # remember to pass refresh token
                           token.data <- jsonlite::fromJSON(content(raw.response, "text"))

                           now <- as.numeric(Sys.time())
                           .self$setToken(c(
                               token.data, refresh_token = .self$token$refresh_token,
                               timestamp = c(first = .self$token$timestamp.first,refresh = now)))
                       },
                       isWhere = function() {
                           if (.self$where != "") {
                               return(TRUE)
                           } else {
                               return(FALSE)
                           }
                       }
                   )
)
