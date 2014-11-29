#####
# Evaluator:stockEvaluatedByDMI
# Special Sorter-Preference Order: stockSortedByDMITOPSIS
# suggest n_DX = 7
# suggest n_ADX = 7
# suggest dayperiod = 14
#####
# Evaluator
stockEvaluatedByDMI<-function(stock_df){
  stock_df_new<-cbind(stock_df,TR1=NA,PLUSDM1=NA,MINUSDM1=NA,TRn=NA,PLUSDMn=NA,MINUSDMn=NA,PLUSDIn=NA,MINUSDIn=NA,DXn=NA,ADXn=NA)
  # calculation
  # n_DX = 0 using all data (no ADX calculation)
  n_DX<-7
  n_ADX<-7
  for(day in 2:nrow(stock_df_new)){
    # True Range
    stock_df_new$TR1[day]<-max(abs(stock_df_new$close[day]-stock_df_new$low[day]),
                               abs(stock_df_new$high[day]-stock_df_new$close[day-1]),
                               abs(stock_df_new$low[day]-stock_df_new$close[day-1]))
    # Directional Movement
    stock_df_new$PLUSDM1[day]<-if(stock_df_new$high[day]-stock_df_new$high[day-1]>0)
                              {stock_df_new$high[day]-stock_df_new$high[day-1]}else{0}
    stock_df_new$MINUSDM1[day]<-if(stock_df_new$low[day-1]-stock_df_new$low[day]>0)
                              {stock_df_new$low[day-1]-stock_df_new$low[day]}else{0}
    if(day>n_DX-1&n_DX!=0){
      stock_df_new$TRn[day]<-sum(stock_df_new$TR1[max(day-n_DX,1):day],na.rm = TRUE)
      stock_df_new$PLUSDMn[day]<-sum(stock_df_new$PLUSDM1[max(day-n_DX,1):day],na.rm = TRUE)
      stock_df_new$MINUSDMn[day]<-sum(stock_df_new$MINUSDM1[max(day-n_DX,1):day],na.rm = TRUE)
      stock_df_new$PLUSDIn[day]<-stock_df_new$PLUSDMn[day]/stock_df_new$TRn[day]*100
      stock_df_new$MINUSDIn[day]<-stock_df_new$MINUSDMn[day]/stock_df_new$TRn[day]*100
      stock_df_new$DXn[day]<-abs((stock_df_new$PLUSDIn[day]-stock_df_new$MINUSDIn[day]))/(stock_df_new$PLUSDIn[day]+stock_df_new$MINUSDIn[day])*100
      if(length(which(!is.na(stock_df_new$DXn)))<=n_ADX){
        stock_df_new$ADXn[day]<-mean(stock_df_new$DXn[max(day-n_ADX,1):day],na.rm = TRUE)
      }else if(length(which(!is.na(stock_df_new$DXn)))>n_ADX){
        stock_df_new$ADXn[day]<-(stock_df_new$ADXn[day-1]*(n_ADX-1)+stock_df_new$DXn[day])/n_ADX
      }
    }
  }
  if(nrow(stock_df_new)<n_DX|n_DX==0){
    # True Range TOTAL
    TRSUM<-sum(stock_df_new$TR1,na.rm = TRUE)
    #TRMEAN<-mean(stock_df_new$TR1,na.rm = TRUE)
    # Directional Movement TOTAL
    PLUSDMSUM<-sum(stock_df_new$PLUSDM1,na.rm = TRUE)
    MINUSDMSUM<-sum(stock_df_new$MINUSDM1,na.rm = TRUE)
    # Directional Indicator
    PLUSDI<-PLUSDMSUM/TRSUM*100
    MINUSDI<-MINUSDMSUM/TRSUM*100
    # DX - directional movement index
    DX<-abs((PLUSDI-MINUSDI))/(PLUSDI+MINUSDI)*100
    ADX<-DX
  }else{
    PLUSDI<-tail(stock_df_new$PLUSDIn,1)
    MINUSDI<-tail(stock_df_new$MINUSDIn,1)
    DX<-tail(stock_df_new$DXn,1)
    ADX<-tail(stock_df_new$ADXn,1)
  }
  stockEvaluation<-NULL
  stockEvaluation$symbol<-stock_df_new$symbol[1]
  stockEvaluation$PLUSDI<-PLUSDI
  stockEvaluation$MINUSDI<-MINUSDI
  stockEvaluation$DX<-DX
  stockEvaluation$ADX<-ADX
  return(stockEvaluation)
}
# Sorter
stockSortedByDMITOPSIS<-function(stock_evaluation_df){
  stock_evaluation_df<-cbind(stock_evaluation_df,DXmADX=stock_evaluation_df$DX-stock_evaluation_df$ADX)
  # TOPSIS
  decision<-as.matrix(stock_evaluation_df[,c("PLUSDI","MINUSDI","DXmADX")])
  weight <- c(1, 1, 1)
  impacts <- c("+", "-", "+")
  result<-topsis(decision, weight, impacts)
  # Sort
  stock_evaluation_df<-cbind(stock_evaluation_df,rank=result$rank)
  stock_evaluation_df_sorted<-stock_evaluation_df[order(stock_evaluation_df$rank,decreasing=F),]
  return(stock_evaluation_df_sorted)
}
#####
# Evaluator: stockEvaluatedByTXS
# Special Sorter-Preference Order: 
# suggest dayperiod = 15
# 前五日
# suggest n_vol = 5
#####
# Evaluator
stockEvaluatedByTXS<-function(stock_df){
  stock_df_new<-cbind(stock_df,priceIncrease=NA,volRate=NA)
  n_vol = 5
  for(day in 1:nrow(stock_df_new)){
    # increase of each day
    stock_df_new$priceIncrease[day]<-(stock_df_new$close[day]-stock_df_new$open[day])/stock_df_new$open[day]
    if(day>n_vol-1){
      stock_df_new$volRate[day]<-stock_df_new$vol[day]/mean(stock_df_new$vol[max(day-n_vol,1):day],na.rm = TRUE)
    }
  }
  
}
# Sorter

#####
# General Sorter-Preference Order:
#####

#####
# switcher
#####
switchStockEvaluator<-function(evaluatorname){
  cmpfun(
    switch(evaluatorname,
           stockEvaluatedByDMI
    ))
}

switchStockSorter<-function(sortername){
  cmpfun(
    switch(sortername,
           stockSortedByDMITOPSIS
    ))
}