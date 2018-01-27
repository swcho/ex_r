require(rjson)
require(xts)
require(quantmod)
require(plotly)
require(MASS)

BASE_PRICE = 10

get_bithumb_url <- function(tag = NULL) {
  json_file <- "https://www.bithumb.com/resources/csv/xcoinTrade_minute.json"
  switch(
    tag, 
    ETC={
      json_file <- "https://www.bithumb.com/resources/csv/ETC_xcoinTradeAltcoin_minute.json"
    },
    ETH={
      json_file <- "https://www.bithumb.com/resources/csv/xcoinTradeAltcoin_minute.json"
    }, {
      
    }
  )
  return(json_file)
}

get_bithumb_json <- function(tag = NULL) {
  json_file <- "https://www.bithumb.com/resources/csv/xcoinTrade_minute.json"
  #json_data <- fromJSON(paste(readLines(json_file), collapse=""))
  return(paste(readLines(json_file)))
}

df_from_json <- function(clean = FALSE, tag = NULL, file = NULL) {
  if (is.null(file)) {
    json_file <- get_bithumb_url(tag) # "https://www.bithumb.com/resources/csv/xcoinTrade_minute.json"
    df <- fromJSON(paste(readLines(json_file), collapse=""))
  } else {
    df <- fromJSON(file=file)
  }
  df <- as.data.frame(do.call('rbind', df))
  col_name <- function(name) {
    if (!is.null(tag)) {
      return(paste(tag, name, sep = '.'))
    }
    return(name)
  }
  colnames(df) <- c('time', col_name('Open'), col_name('Close'), col_name('High'), col_name('Low'), col_name('Volume'))
  df[,1] <- as.POSIXct(df[,1]/1000, origin = "1970-01-01")
  df <- df[, c(1, 2, 4, 5, 3, 6)]
  if (clean) {
    df <- na.omit(df)
    df <- df[which(BASE_PRICE < df[,2] & BASE_PRICE < df[,3] & BASE_PRICE < df[,4] & BASE_PRICE < df[,5] & BASE_PRICE < df[,6]),]
  }
  return(df)
}

xts_from_json <- function(...) {
  df <- df_from_json(...)
  xts_data <- xts(df[, -1], order.by = df[, 1])
  return (xts_data);
}

# minints<-cut(data1$Date, breaks="30 min")
apply.hourly <- function(x, FUN,...) {
  ep <- endpoints(x, 'hours')
  period.apply(x, ep, FUN, ...)
}

draw.volume <- function(x) {
  volume <- x[,5]
  summary(volume)
  hist(volume, breaks = 100)
  volume_mean <- mean(volume)
  abline(v = volume_mean)
  df_more_than_mean <- x[volume_mean < volume,]
  plot(as.numeric(df_more_than_mean[,4]), as.numeric(df_more_than_mean[,5]), type = "h")
  plot(df_more_than_mean[,4])
  
  dens <- kde2d(df_more_than_mean[,4], df_more_than_mean[,5], n=100)
  image(dens, xlab="Close", ylab="Volume")
  contour(dens, add=TRUE)
  persp(dens, xlab="Close", ylab="Volume", zlab="density", shade=.5)
  
  plot_ly(x = time(df), y = as.numeric(df$Close), z = as.numeric(df$Volume))
  plot_ly(x = as.numeric(df$Close), y = as.numeric(df$Volume))
}

draw.quant <- function(x) {
  x %>%
    chartSeries(TA='addBBands();
                addBBands(draw="p");
                addVo();
                addMACD()', 
                theme="white"
    ) 
}

draw.returns <- function(x) {
  log_returns <- x %>%
    Cl() %>%
    log() %>%
    diff()
  names(log_returns) <- "Log.Returns"
  # Plot the log-returns    
  # log_returns %>%    
  #   ggplot(aes(x = Log.Returns)) + 
  #   geom_histogram(bins = 100) + 
  #   geom_density() +
  #   geom_rug(alpha = 0.5) 
  
  # https://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
  # http://earlh.com/blog/2009/07/20/multiple-y-axes-in-r-plots-part-9-in-a-series/
  
  par(mar=c(5, 5, 4, 6) + 0.1)
  plot(time(log_returns), as.numeric(x[,4]), type="l", pch=16, ylab="", axes = FALSE)
  axis(2, col="black",las=1)
  par(new = T)
  plot(time(log_returns), as.numeric(log_returns[,1]), type="l", pch=15, col="green", xlab=NA, ylab=NA , bty = "n", axes = FALSE)
  abline(h=0)
  axis(side=4)
  mtext(side = 4, line = 3, 'Number genes selected')
}
