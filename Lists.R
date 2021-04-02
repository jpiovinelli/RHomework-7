## This homework project will demonstrate how to use lists and lapply on a
## Dataset
# Importing package 'rio' for easy file input
library(rio)
# Saving the dataset to a variable called 'stocks'
stocks <- import("FINAL_FROM_DF.csv")
# Custom function to compute attributes of Indian company stock prices
getPrices <- function(symbol) {
  print(symbol)
  temp <- stocks[stocks$SYMBOL == symbol,]
  mn <- min(temp$CLOSE, na.rm = T)
  mx <- max(temp$CLOSE, na.rm = T)
  avg <- mean(temp$CLOSE, na.rm = T)
  sdev <- sd(temp$CLOSE, na.rm = T)
  covariance <- cov(temp$CLOSE, temp$OPEN)
  out <- data.frame(Symbol = symbol, Min = mn, Max = mx, Average = avg, 
                    Standard_Deviation = sdev, Covariance = covariance)
  return(out)
}
# Will create a list of only unique company symbols from stocks data
lst <- as.list(unique(stocks$SYMBOL))
# Will apply the entire list of unique symbols to the getPrices function
results <- lapply(X = lst, FUN = getPrices)
# Will bind all list elements together into one data set
results2 <- do.call(rbind, results)
# Creates a csv file of data put together
write.csv(results2, file = "homework7.csv")