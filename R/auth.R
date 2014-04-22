# class constructor:
rga.open <- function(instance = 'ga', 
                     client.id = '862341168163-qtefv92ckvn2gveav66im725c3gqj728.apps.googleusercontent.com', 
                     client.secret = '4CLMnYSN0svvaEXeybygHslJ', 
                     where, envir = .GlobalEnv) {
  
	.define.rga(envir);
	
	.extend.rga.status();
	.extend.rga.mgmt();
	.extend.rga.core();
	.extend.rga.mcf();  
	
	if (exists(instance, where = envir)) {
		get(instance, envir = envir, mode='S4')$prepare();
	} else {
		if (missing('where')) { # where is not set
			token <- .rga.getToken(client.id, client.secret);
			assign(instance, rga$new(client.id, client.secret, where = '', token), envir = envir);
		} else {
			if (file.exists(where)) { # file exists
				assign(instance, readRDS(where), envir = envir);
				get(instance, envir = envir, mode='S4')$prepare()
      		} else { # create file
        		token <- .rga.getToken(client.id, client.secret);
				assign(instance, rga$new(client.id, client.secret, where, token), envir = envir);
				saveRDS(get(instance, envir = envir), file = where);
			}
		}
	}
}

.rga.getToken <- function(client.id, client.secret) {
	if (interactive()) {
		redirect.uri <- 'urn:ietf:wg:oauth:2.0:oob';
		url <- paste('https://accounts.google.com/o/oauth2/auth?',
	    			 'scope=https://www.googleapis.com/auth/analytics.readonly&',
	                 'state=%2Fprofile&',
	                 'redirect_uri=', redirect.uri, '&',
	                 'response_type=code&',
	                 'client_id=', client.id, '&',
	                 'approval_prompt=force&',
	                 'access_type=offline', sep='', collapse='');
	  
		browseURL(url);
		cat(paste('Browse URL:', url, '\n')); # in case of server
	  	code <- readline('Please enter code here: ');
  	} else {
  		code <- 'dummy';
  	}
  	.rga.authenticate(client.id = client.id, client.secret = client.secret, code = code, redirect.uri = redirect.uri);
}

.rga.authenticate <- function(client.id, client.secret, code, redirect.uri) {
	opts <- list(verbose = FALSE);
	raw.response <- POST('https://accounts.google.com/o/oauth2/token',
                     body = list(code = code, 
                       client_id = client.id,
                       client_secret = client.secret, 
                       redirect_uri = redirect.uri,
                       grant_type = 'authorization_code'
                     )
                   );

    token.data <- fromJSON(content(raw.response,'text'));
	
  	now <- as.numeric(Sys.time());
  	token <- c(token.data, timestamp = c('first' = now, 'refresh' = now));
  
  	return(token);
}
