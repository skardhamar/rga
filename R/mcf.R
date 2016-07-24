rga$methods(
    list(
        getMCFData = function(ids, start.date = format(Sys.Date() - 8, "%Y-%m-%d"), 
                              end.date = format(Sys.Date() - 1, "%Y-%m-%d"), date.format = "%Y-%m-%d",
                              metrics = "mcf:totalConversions,mcf:totalConversionValue", dimensions = "mcf:source,mcf:keyword", 
                              sort = "", filters = "", segment = "", fields = "", 
                              start = 1, max, messages = TRUE, batch, walk = FALSE,
                              output.raw, output.formats, return.url = FALSE, rbr = FALSE, envir = .GlobalEnv,
                              samplingLevel = "HIGHER_PRECISION") {

            if (missing(ids)) {
                stop("please enter a profile id")
            }

            if (missing(batch) || batch == FALSE) {
                isBatch <- FALSE
                if (missing(max)) {
                    # standard
                    max <- 1000
                }
            } else {
                isBatch <- TRUE
                if (!is.numeric(batch)) {
                    if (!missing(max) && max < 10000) {
                        # no need
                        batch <- max
                    } else {
                        # max batch size
                        batch <- 10000
                    }
                } else {
                    if (batch > 10000) {
                        # as per https://developers.google.com/analytics/devguides/reporting/core/v3/reference#maxResults
                        stop("batch size can be set to max of 10000")
                    }
                }

                adjustMax <- TRUE
                # arbitrary target, adjust later
                max <- 10000
            }

            # ensure that profile id begings with 'ga:'
            if (!grepl("ga:", ids)) {
                ids <- paste("ga:", ids, sep = "")
            }

            # remove whitespaces from metrics and dimensions
            metrics <- gsub("\\s", "", metrics)
            dimensions <- gsub("\\s", "", dimensions)

            # build url with variables
            url <- "https://www.googleapis.com/analytics/v3/data/mcf"
            query <- paste(paste("access_token", .self$getToken()$access_token, sep = "="),
                           paste("ids", ids, sep = "="),
                           paste("start-date", start.date, sep = "="),
                           paste("end-date", end.date, sep = "="),
                           paste("metrics", metrics, sep = "="),
                           paste("dimensions", dimensions, sep = "="),
                           paste("start-index", start, sep = "="),
                           paste("max-results", max, sep = "="),
                           paste("samplingLevel", samplingLevel, sep = "="),
                           sep = "&")

            if (sort != "") {
                query <- paste(query, paste("sort", sort, sep = "="), sep = "&")
            }
            if (segment != "") {
                query <- paste(query, paste("segment", segment, sep = "="), sep = "&")
            }
            if (fields != "") {
                query <- paste(query, paste("fields", fields, sep = "="), sep = "&")
            }
            if (filters != "") {
                # available operators
                ops <- c("==", "!=", ">", "<", ">=", "<=", "=@", "!@", "=-", "!-", "\\|\\|", "&&", "OR", "AND")
                # make pattern for gsub
                opsw <- paste("(\\ )+(", paste(ops, collapse = "|"), ")(\\ )+", sep = "")
                # remove whitespaces around operators
                filters <- gsub(opsw, "\\2", filters)
                # replace logical operators
                filters <- gsub("OR|\\|\\|", ",", filters)
                filters <- gsub("AND|&&", ";", filters)
                query <- paste(query, paste("filters", curlEscape(filters), sep = "="), sep = "&", collapse = "")
            }

            url <- paste(url, query = query, sep = "?")

            if (return.url) {
                return(url)
            }

            # thanks to Schaun Wheeler this will not provoke the weird SSL-bug
            if (.Platform$OS.type == "windows") {
                options(RCurlOptions = list(
                    verbose = FALSE,
                    capath = system.file("CurlSSL", "cacert.pem",
                                         package = "RCurl"), ssl.verifypeer = FALSE))
            }

            # get data and convert from json to list-format
            request <- GET(url)
            ga.data <- jsonlite::fromJSON(content(request, "text"))
            
            # possibility to extract the raw data
            if (!missing(output.raw)) {
              assign(output.raw, ga.data, envir = envir)
            }

            # output error and stop
            if (!is.null(ga.data$error)) {
                stop(paste("error in fetching data: ", ga.data$error$message, sep = ""))
            }

            if (ga.data$containsSampledData == "TRUE") {
                isSampled <- TRUE
                if (!walk) {
                    message(sprintf("Notice: Data set sampled from %s sessions (%d%% of all sessions)", 
                                    format(as.numeric(ga.data$sampleSize), big.mark=",", scientific=FALSE),
                                    round((as.numeric(ga.data$sampleSize) / as.numeric(ga.data$sampleSpace) * 100))))
                }
            } else {
                isSampled <- FALSE
            }

            if (isSampled && walk) {
                return(.self$getMCFDataInWalks(total = ga.data$totalResults, max = max, batch = batch,
                                               ids = ids, start.date = start.date, end.date = end.date, date.format = date.format,
                                               metrics = metrics, dimensions = dimensions, sort = sort, filters = filters,
                                               segment = segment, fields = fields, envir = envir, samplingLevel = samplingLevel))
            }

            # check if all data is being extracted
            if (NROW(ga.data$rows) < ga.data$totalResults && (messages || isBatch)) {
                if (!isBatch) {
                    message(paste("Only pulling", NROW(ga.data$rows), "observations of", ga.data$totalResults, "total (set batch = TRUE to get all observations)"))
                } else {
                    if (adjustMax) {
                        max <- ga.data$totalResults
                    }
                    message(paste("Batch: pulling", max, "observations in batches of", batch))
                    # pass variables to batch-function
                    return(.self$getMCFDataInBatches(total = ga.data$totalResults, max = max, batchSize = batch,
                                                     ids = ids, start.date = start.date, end.date = end.date, date.format = date.format,
                                                     metrics = metrics, dimensions = dimensions, sort = sort, filters = filters,
                                                     segment = segment, fields = fields, envir = envir, samplingLevel = samplingLevel))
                }
            }

            # get column names
            ga.headers <- ga.data$columnHeaders
            # remove mcf: from column headersView
            ga.headers$name <- sub("mcf:", "", ga.headers$name)

            # did not return any results
            if (!inherits(ga.data$rows, "list") && !rbr) {
                stop(paste("no results:", ga.data$totalResults))
            } else if (!inherits(ga.data$rows, "list") && rbr) {
              # If row-by-row is true, return NULL
              return(NULL)
            }

            # convert to data.frame
            if (!any(grepl("MCF_SEQUENCE", ga.headers$dataType))) {
                ga.data.df <- as.data.frame(do.call(rbind, lapply(ga.data$rows, unlist)), stringsAsFactors = FALSE)
                # insert column names
                names(ga.data.df) <- ga.headers$name
            } else {
                primitiveValues <- lapply(lapply(ga.data$rows, "[[", "primitiveValue"), "[", grep("MCF_SEQUENCE", ga.headers$dataType, invert = TRUE))
                primitiveValues <- do.call(rbind, primitiveValues)
                colnames(primitiveValues) <- ga.headers$name[grep("MCF_SEQUENCE", ga.headers$dataType, invert = TRUE)]
                conversionPathValues <- lapply(lapply(ga.data$rows, "[[", "conversionPathValue"), "[", grep("MCF_SEQUENCE", ga.headers$dataType))
                conversionPathValues <- lapply(conversionPathValues, function(x) lapply(x, function(i) apply(i, 1, paste, sep = "", collapse = ":")))
                conversionPathValues <- lapply(conversionPathValues, function(x) lapply(x, paste, collapse = " > "))
                conversionPathValues <- do.call(rbind, lapply(conversionPathValues, unlist))
                colnames(conversionPathValues) <- ga.headers$name[grep("MCF_SEQUENCE", ga.headers$dataType)]
                ga.data.df <- data.frame(primitiveValues, conversionPathValues, stringsAsFactors = FALSE)[, ga.headers$name]
            }
            
            # check if sampled; add attributes if so
            if (isSampled) {
              attr(ga.data.df, "containsSampledData") <- TRUE
              attr(ga.data.df, "sampleSize") <- as.numeric(ga.data$sampleSize)
              attr(ga.data.df, "sampleSpace") <- as.numeric(ga.data$sampleSpace)
            } else {
              attr(ga.data.df, "containsSampledData") <- FALSE
            }

            # find formats
            formats <- ga.headers
            
            # convert to r friendly
            formats$dataType[formats$dataType %in% c("INTEGER", "PERCENT", "TIME", "CURRENCY", "FLOAT")] <- "numeric"
            formats$dataType[formats$dataType == "STRING"] <- "character"
            formats$dataType[formats$dataType == "MCF_SEQUENCE"] <- "character"
            formats$dataType[formats$name == "conversionDate"] <- "Date"

            if ("conversionDate" %in% ga.headers$name) {
                ga.data.df$conversionDate <- format(as.Date(ga.data.df$conversionDate, "%Y%m%d"), date.format)
            }

            # looping through columns and setting classes
            for (i in 1:nrow(formats)) {
                column <- formats$name[i]
                class <- formats$dataType[[i]]
                if (!exists(paste("as.", class, sep = ""), mode = "function")) {
                    stop(paste("can't find function for class", class))
                } else {
                    as.fun <- match.fun(paste("as.", class, sep = ""))
                }
                if (class == "ordered") {
                  ga.data.df[[column]] <- as.numeric(ga.data.df[[column]])
                }
                ga.data.df[[column]] <- as.fun(ga.data.df[[column]])
            }

            # mos-def optimize
            if (!missing(output.formats)) {
                assign(output.formats, formats, envir = envir)
            }

            # and we're done
            return(ga.data.df)
        },
        getMCFDataInBatches = function(batchSize, total, ids, start.date, end.date, date.format,
                                       metrics, max, dimensions, sort, filters, segment, fields, envir, 
                                       samplineLevel) {
            runs.max <- ceiling(max/batchSize)
            chunk.list <- vector("list", runs.max)
            for (i in 0:(runs.max - 1)) {
                start <- i * batchSize + 1
                end <- start + batchSize - 1

                if (end > max) {
                    # adjust batch size if we're pulling the last batch
                    batchSize <- max - batchSize
                    end <- max
                }

                message(paste("Batch: run (", i + 1, "/", runs.max, "), observations [", start, ";", end, "]. Batch size: ", batchSize, sep = ""))
                chunk <- .self$getMCFData(ids = ids, start.date = start.date, end.date = end.date, metrics = metrics, dimensions = dimensions, sort = sort, 
                                          filters = filters, segment = segment, fields = fields, date.format = date.format, envir = envir, messages = FALSE, return.url = FALSE, 
                                          batch = FALSE, start = start, max = batchSize, samplingLevel = samplingLevel)
                message(paste("Batch: received", NROW(chunk), "observations"))
                chunk.list[[i + 1]] <- chunk
            }
            return(do.call(rbind, chunk.list, envir = envir))
        },
        getMCFDataInWalks = function(total, max, batch, ids, start.date, end.date, date.format,
                                     metrics, dimensions, sort, filters, segment, fields, envir,
                                     samplingLevel) {
            # this function will extract data day-by-day (to avoid sampling)
            walks.max <- ceiling(as.numeric(difftime(as.Date(end.date), as.Date(start.date), units = "days")))
            chunk.list <- vector("list", walks.max + 1)

            for (i in 0:(walks.max)) {
                date <- format(as.Date(start.date) + i, "%Y-%m-%d")

                message(paste("Walk: run (", i + 1, "/", walks.max + 1, ") for date ", date, sep = ""))
                chunk <- .self$getMCFData(ids = ids, start.date = date, end.date = date, date.format = date.format,
                                          metrics = metrics, dimensions = dimensions, sort = sort, filters = filters,
                                          segment = segment, fields = fields, envir = envir, max = max,
                                          rbr = TRUE, messages = FALSE,  return.url = FALSE, batch = batch, samplingLevel = samplingLevel)
                message(paste("Walk: received", NROW(chunk), "observations"))
                chunk.list[[i + 1]] <- chunk
            }
            return(do.call(rbind, chunk.list, envir = envir))
        }
    )
)
