.extend.rga.mcf <- function() {
  rga$methods(
    list(
        getMCFData = function(ids, start.date = format(Sys.time(), "%Y-%m-%d"), 
                 end.date = format(Sys.time(), "%Y-%m-%d"), metrics = 'ga:visits',
                 dimensions = 'ga:date', sort = '', filters = '', segment = '', fields = '', 
                 start = 1, max, date.format = '%Y-%m-%d', messages = TRUE, batch, walk = FALSE,
                 output.raw, output.formats, return.url = FALSE, rbr = FALSE, envir = .GlobalEnv) {

        if (missing(ids)) { stop('please enter a profile id'); }

        if (missing(batch) || batch == FALSE) {
          isBatch <- FALSE;
          if (missing(max)) {
            max <- 1000; # standard
          }
        } else {
          isBatch <- TRUE;
          if (!is.numeric(batch)) {
            if (!missing(max) && max < 10000) {
              batch <- max; # no need
            } else {
              batch <- 10000; # max batch size
            }
          } else {
            if (batch > 10000) {
              # as per https://developers.google.com/analytics/devguides/reporting/core/v2/gdataReferenceDataFeed#maxResults
              stop('batch size can max be set to 10000');
            }
          }

          if (missing(max)) {
            adjustMax <- TRUE;
            max <- 10000; # arbitrary target, adjust later
          } else {
            adjustMax <- FALSE;
          }
        }

        # ensure that profile id begings with 'ga:'
        if (!as.logical(length(as.numeric(grep('ga:', ids))))) { 
          ids <- paste('ga:', ids, sep = ''); 
        }

        # build url with variables
        url <- paste('https://www.googleapis.com/analytics/v3/data/mcf',
                       '?access_token=', .self$getToken()$access_token,
                         '&ids=', ids,
                         '&start-date=', start.date,
                         '&end-date=', end.date,
                         '&metrics=', metrics,
                         '&dimensions=', dimensions,
                         '&start-index=', start,
                         '&max-results=', max,
                         '&samplingLevel=HIGHER_PRECISION',
                         sep = '', collapse = '');

        if (sort != '') { url <- paste(url, '&sort=', sort, sep='', collapse=''); }
        if (segment != '') { url <- paste(url, '&segment=', segment, sep='', collapse=''); }
        if (fields != '') { url <- paste(url, '&fields=', fields, sep='', collapse=''); }
          
          if (filters != '') { 
          url <- paste(url, '&filters=', curlEscape(filters), sep='', collapse=''); 
        }         

        if (return.url) {
          return(url);
        }

        # get data and convert from json to list-format
        request <- httr::GET(url);
        ga.data <- fromJSON(httr::content(request,'text'));
    
        # output error and stop
        if (!is.null(ga.data$error)) {
          stop(paste('error in fetching data: ', ga.data$error$message,  sep=''))
        }

        if (ga.data$containsSampledData == 'TRUE') {
          isSampled <- TRUE;
          if (!walk) {
            message('Notice: Data set contains sampled data');
          }
        } else {
          isSampled <- FALSE;
        }

        if (isSampled && walk) {
          return(.self$getMCFDataInWalks(total = ga.data$totalResults, max = max, batch = batch,
                         ids = ids, start.date = start.date, end.date = end.date,
                         metrics = metrics, dimensions = dimensions, sort = sort,
                         filters = filters, segment = segment, fields = fields, 
                         date.format = date.format, envir = envir));
        }

        # check if all data is being extracted
        if (length(ga.data$rows) < ga.data$totalResults && (messages || isBatch)) {
          if (!isBatch) {
            message(paste('Only pulling', length(ga.data$rows), 'observations of', ga.data$totalResults, 'total (set batch = TRUE to get all observations)'));
          } else {
            if (adjustMax) {
              max <- ga.data$totalResults;
            }
            message(paste('Pulling', max, 'observations in batches of', batch));
            # pass variables to batch-function
            return(.self$getMCFDataInBatches(total = ga.data$totalResults, max = max, batchSize = batch,
                         ids = ids, start.date = start.date, end.date = end.date,
                         metrics = metrics, dimensions = dimensions, sort = sort,
                         filters = filters, segment = segment, fields = fields, 
                         date.format = date.format, envir = envir));
          }
        }
      
        # get column names
        ga.headers <- as.data.frame(do.call(rbind, ga.data$columnHeaders)); 
        
        # did not return any results
        if (class(ga.data$rows) != 'list' && !rbr) {
          stop(paste('no results: ', ga.data$totalResults, sep = ''));
        } else if (class(ga.data$rows) != 'list' && rbr) {
          # return data.frame with NA, if row-by-row setting is true
          row <- as.data.frame(matrix(rep(NA, length(sub('ga:', '', as.data.frame(do.call(rbind, ga.data$columnHeaders))$name))), nrow=1))
          names(row) <- sub('mcf:', '', as.data.frame(do.call(rbind, ga.data$columnHeaders))$name);
          return(row);
        }
        
        # convert to data.frame
        ga.data.df <- as.data.frame(do.call(rbind, ga.data$rows)); 

        # remove mcf: from column headersView
        ga.headers$name <- sub('mcf:', '', ga.headers$name); 
        names(ga.data.df) <- ga.headers$name; # insert column names

        # find formats
        formats <- as.data.frame(do.call(rbind, ga.data$columnHeaders));
        
        formats$name <- sub('mcf:', '', formats$name);
        formats$columnType <- tolower(formats$columnType);
        formats$dataType <- lapply(formats$dataType, as.character); # as suggested by @mtaanman
    
        # looping through columns and setting classes       
        for (i in 1:nrow(formats)) {
          column <- formats$name[i];
          class <- formats$dataType[[i]];
          if(class=='STRING') {
            ga.data.df[[column]] <- llply(ga.data.df[[column]],.fun=function(x){return(as.character(x['primitiveValue'][[1]]))});
          } else if (class=='INTEGER'||class=='CURRENCY'||class=='FLOAT') {
            ga.data.df[[column]] <- llply(ga.data.df[[column]],.fun=function(x){return(as.numeric(x['primitiveValue'][[1]]))});
          } else if (class=='MCF_SEQUENCE') {
            ga.data.df[[column]] <- llply(ga.data.df[[column]],.fun=function(x){
              return(as.vector(
                     llply(do.call(cbind, x['conversionPathValue']),.fun=function(x){
                        return(as.character(paste(x['interactionType'],x['nodeValue'],sep=":")))
                      })
                     ,mode="character"));
            })
          } else {
            print(paste("WARNING: data type not handled -",class))
          }
        }

        if ('conversionDate' %in% names(ga.data.df)) {
          ga.data.df$'conversionDate' <- ldply(ga.data.df$'conversionDate',.fun=function(x){return(as.Date(format(as.Date(x, '%Y%m%d'), date.format), format=date.format))})
          names(ga.data.df$'conversionDate') <- 'conversionDate'
        }

        if (!missing(output.formats)) {
          assign(output.formats, formats, envir = envir);
        }
      
        # and we're done
        return(ga.data.df);
      },
      getMCFDataInBatches = function(batchSize, total, ids, start.date, end.date, metrics, max, 
                    dimensions, sort, filters, segment, fields, date.format, envir) {
        runs.max <- ceiling(max/batchSize);
        chunk.list <- vector('list', runs.max);
        for (i in 0:(runs.max - 1)) {
          start <- i * batchSize + 1;
          end <- start + batchSize - 1;
          
          if (end > max) { # adjust batch size if we're pulling the last batch
            batchSize <- max - batchSize;
            end <- max;
          }

          message(paste('Run (', i + 1, '/', runs.max, '): observations [', start, ';', end, ']. Batch size: ', batchSize, sep = ""));
          chunk <- .self$getMCFData(ids = ids, start.date = start.date, end.date = end.date,
                       metrics = metrics, dimensions = dimensions, sort = sort,
                       filters = filters, segment = segment, fields = fields, 
                       date.format = date.format, envir = envir, 
                       messages = FALSE, return.url = FALSE, batch = FALSE, # static
                       start = start, max = batchSize); # dynamic
          message(paste('Recieved:', nrow(chunk), 'observations'));
          chunk.list[[i + 1]] <- chunk;
        }
        return(do.call(rbind, chunk.list, envir = envir));
      },
      getMCFDataInWalks = function(total, max, batch, ids, start.date, end.date,
                    metrics, dimensions, sort, filters, segment, fields, 
                    date.format, envir) {
        # this function will extract data day-by-day (to avoid sampling)
        walks.max <- ceiling(as.numeric(difftime(end.date, start.date, units='days')));
        chunk.list <- vector('list', walks.max + 1);

        for (i in 0:(walks.max)) {
          date <- format(as.POSIXct(start.date) + days(i), '%Y-%m-%d');

          message(paste('Run (', i + 1, '/', walks.max + 1, '): for date ', date, sep = ""));
          chunk <- .self$getMCFData(ids = ids, start.date = date, end.date = date,
                       metrics = metrics, dimensions = dimensions, sort = sort,
                       filters = filters, segment = segment, fields = fields, 
                       date.format = date.format, envir = envir, max = max, rbr = TRUE,
                       messages = FALSE, return.url = FALSE, batch = batch)
          message(paste('Recieved:', nrow(chunk), 'observations'));
          chunk.list[[i + 1]] <- chunk;
        }
        return(do.call(rbind, chunk.list, envir = envir));
      }
    )
  );
}