#####
# UTIL
#####
# 返回升序游程中顺序最大值
# example:sequence<-c(1,4,5,6,10,11)返回3
runMaxSequence<-function(sequence){
  if(length(sequence)==0) return(0)
  depth<-runMaxSequence(which(sequence[2:length(sequence)]==(sequence+1)[1:length(sequence)-1]))+1
  return(depth)
}
# stock data filtering
stockDataFilter_beforenovol<-function(stock_df_withinperiod){
  # filtering data before no vol
  if(length(which(stock_df_withinperiod$vol==0))!=0){
    stock_df_withinperiod<-stock_df_withinperiod[max(which(stock_df_withinperiod$vol==0)):nrow(stock_df_withinperiod),]
  }
  return(stock_df_withinperiod)
}
stockDataFilter_novol<-function(stock_df_withinperiod){
  # filtering data no vol
  stock_df_withinperiod$vol[which(stock_df_withinperiod$vol==0)] <- NA
  return(na.omit(stock_df_withinperiod))
}