.extend.rga.status <- function() {
    rga$methods(
        list(
            explore = function() {
                return(get(class(.self)[1]))
            },
            status = function() {
                cat(paste("Client ID: ", .self$client.id, "\n", sep = ""))
                cat(paste("Client Secret: ", .self$client.secret, "\n", sep = ""))
                cat(paste("Where: ", .self$where, "\n", sep = ""))
                cat(paste("Is token valid: ", !.self$isTokenExpired(), "\n", sep = ""))
                cat(paste("Token expires in: ", .self$tokenExpiresIn(), " seconds\n", sep = ""))
            }
        )
    )
}
