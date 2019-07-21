#'Outlier detection
#'
#'It will provide Outlier count with original data and after doing log transformation in the form of DataFrame
#'@param data= Dataframe
#'@author Tejas Vartak, Shivani Goyal
#'@return It will return DataFrame having outlier count on original data and after log transformation
#'@export

outlier <- function(data)
{

  if (class(fram) != "data.frame")
  {
    print('Not a data frame')
    return (NULL)
  }
  else
  {
    upperCount <- c()
    lowerCount <- c()
    logUpperCount <- c()
    logLowerCount <- c()
    names <- c()
    nums <- unlist(lapply(data, is.numeric))
    data=data[ , nums]
    for (i in 1:ncol(data))
    {
      o = log10(data[,i])
      x <- o[!is.na(o)]

      q25 = quantile(x)[2]
      q75 = quantile(x)[4]

      upperbound = q75 + 1.5*(q75-q25)
      lowerbound = q25 - 1.5*(q75-q25)

      names[i] = names(data)[i]
      upperCount[i] = sum(x>upperbound)
      lowerCount[i] = sum(x<lowerbound)
      logUpperCount[i] = sum(x>upperbound)
      logLowerCount[i] = sum(x<lowerbound)
    }
  }
  outlierTable = cbind(upperCount,lowerCount,logUpperCount,logLowerCount)
  rownames(outlierTable) = names
  return (outlierTable)

}
