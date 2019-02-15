# Input load. Please do not change #
`dataset` = read.csv('', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE); ## csv file extracted from PowerBI
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #

#### data prep ####
df <- dataset
nas <- sum(is.na(df))

if (!require(imputeTS)) {
  install.packages("imputeTS")
  require(imputeTS)
}

if (nas > 0) {
  for(i in names(df)){
    df[i] <- na.interpolation(df[i], option = "linear")
  }
} else {
  print(nas)
}


#### date formating ####

if (!require(stringr)) {
  install.packages("stringr")
  require(stringr)
}

df$OrderDate <- substr(df$OrderDate, start = 1, stop = 10)

df$OrderDate <- as.Date(df$OrderDate, format = "%Y-%m-%d")

#### model prep ####

df <- df[,1:2]
colnames(df) <- c("ds","y")

if (!require(forecast)) {
  install.packages("forecast")
  require(forecast)
}

if (!require(textshape)) {
  install.packages("textshape")
  require(textshape)
}

if (!require(prophet)) {
  install.packages("prophet")
  require(prophet)
}


m <- prophet(df)
future <- make_future_dataframe(m, periods = 90)
forecast <- predict(m, future)

#plot(m, forecast, main = "", sub = "", xlab = "", ylab = "")
#prophet_plot_components(m, forecast)

#### plot the result ####

res <- subset(df, ds >= "2016-01-01")
res <- subset(res, ds <= "2016-03-01")
res$month <- format(res$ds,"%B")
res <- data.frame(month = unique(res$month),
                  data = tapply(res$y, res$month, FUN = mean))


res1 <- (forecast[c('ds', 'yhat')])
res1 <- subset(res1, ds >= "2016-03-01")
res1$month <- format(res1$ds,"%B")
res1 <- data.frame(month = unique(res1$month),
                   data = tapply(res1$yhat, res1$month, FUN = mean))



if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}


ggplot(res1, aes(x=factor(month,levels=month.name), y=data*100)) + 
  geom_line() + geom_hline(aes(yintercept = mean(res$data*100)), color="red") +
  geom_bar(stat="identity", width=.5, fill=(values=c("#999999", "#E69F00", "#56B4E9"))) + 
  labs(caption="Red Line: 2016 Monthly Average") + 
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  geom_text(aes(label = paste(format(round(data*100, 2), nsmall = 2), "%"), hjust = 0.5, vjust = 3))
            

