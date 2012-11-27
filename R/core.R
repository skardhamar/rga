.extend.rga.core <- function() {
	rga$methods(
		list(
			getData = function(ids, start.date = format(Sys.time(), "%Y-%m-%d"), 
							   end.date = format(Sys.time(), "%Y-%m-%d"), metrics = 'ga:visits',
							   dimensions = 'ga:date', sort = '', filters = '', segment = '', fields = '', 
							   start = 1, max = 1000, last.days, date.format = '%Y-%m-%d', 
							   output.raw, output.formats, rbr = F) {

				if (missing(ids)) {
					stop('please enter a profile ID');
				}

				if (!as.logical(length(as.numeric(grep('ga:', ids))))) {
					ids <- paste('ga:', ids, sep = '');
				}

				url <- paste('https://www.googleapis.com/analytics/v3/data/ga',
               				 '?access_token=', .self$getToken()$access_token,
               			  	 '&ids=', ids,
               			  	 '&start-date=', start.date,
               			  	 '&end-date=', end.date,
               			  	 '&metrics=', metrics,
               			  	 '&dimensions=', dimensions,
               			  	 '&start-index=', start,
               			  	 '&max-results=', max,
               			  	 sep = '', collapse = '');
  
  				if (sort != '') { url <- paste(url, '&sort=', sort, sep='', collapse=''); }
				if (segment != '') { url <- paste(url, '&segment=', segment, sep='', collapse=''); }
				if (fields != '') { url <- paste(url, '&fields=', fields, sep='', collapse=''); }
					
  				if (filters != '') { 
					url <- paste(url, '&filters=', curlEscape(filters), sep='', collapse=''); 
				}					

				# get data and convert from json to list-format
				ga.data <- fromJSON(getURL(url));

	  		  	# possibility to extract the raw data
				if (!missing(output.raw)) {
					assign(output.raw, ga.data, envir = .GlobalEnv);
				}
	  
	  		  	# output error and stop
				if (!is.null(ga.data$error)) {
					stop(paste('error in fetching data: ', ga.data$error$message,  sep=''))
				}
      
	  		  	# get column names
				ga.headers <- as.data.frame(do.call(rbind, ga.data$columnHeaders)); 
				
				# did not return any results
				if (class(ga.data$rows) != 'list' && !rbr) {
					stop(paste('Total results: ', ga.data$totalResults, sep = ''));
				} else if (class(ga.data$rows) != 'list' && rbr) {
					# return data.frame with NA, if row-by-row setting is true
					row <- as.data.frame(matrix(rep(NA, length(sub('ga:', '', as.data.frame(do.call(rbind, ga.data$columnHeaders))$name))), nrow=1))
					names(row) <- sub('ga:', '', as.data.frame(do.call(rbind, ga.data$columnHeaders))$name);
					return(row);
				}
				
				# convert to data.frame
				ga.data.df <- as.data.frame(do.call(rbind, ga.data$rows)); 
				
				ga.data.df <- data.frame(lapply(ga.data.df, as.character), stringsAsFactors=F); # convert to characters
				ga.headers$name <- sub('ga:', '', ga.headers$name); # remove ga: from column headers
      
				names(ga.data.df) <- ga.headers$name; # insert column names
      		  	
				# find formats
				formats <- as.data.frame(do.call(rbind, ga.data$columnHeaders));
				
				# convert to r friendly
				formats$name <- sub('ga:', '', formats$name);
				formats$dataType[formats$dataType == 'STRING'] <- 'character';
				formats$dataType[formats$dataType == 'INTEGER'] <- 'integer';
				formats$dataType[formats$name == 'date'] <- 'Date';
	  		  
				if (!missing(output.formats)) {
					assign(output.formats, formats, envir = .GlobalEnv);
				}
			  
				if ('date' %in% names(ga.data.df)) {
					# mos-def optimize
					ga.data.df$'date' <- as.Date(format(as.Date(ga.data.df$'date', '%Y%m%d'), date.format), format=date.format);
				}
      
      
				return(ga.data.df);
				
			},
			getFirstDate = function(ids) {
				first <- .self$getData(ids,
								  	   start.date='2005-01-01', # GA launched this date
									   filters='ga:visits!=0',
									   max=1);
				return(first$date);
			}
		)
	);
}