#####
# Evaluator:stockEvaluatedByDMI
# Special Sorter-Preference Order: stockSortedByDMITOPSIS
# suggest n_DX = 7
# suggest n_ADX = 7
# suggest dayperiod = 14
#####
# Evaluator
stockEvaluatedByDMI<-function(stock_df){
  # calculation
  n_DX<-7
  n_ADX<-7
  # Filter stock data
  stock_df<-stockDataFilter_novol(stock_df)
  if(nrow(stock_df)<=(n_DX+n_ADX)){return(NULL)}
  stock_df_new<-cbind(stock_df,TR1=NA,PLUSDM1=NA,MINUSDM1=NA,TRn=NA,PLUSDMn=NA,MINUSDMn=NA,PLUSDIn=NA,MINUSDIn=NA,DXn=NA,ADXn=NA)
  # True Range
  stock_df_new$TR1<-pmax(abs(stock_df_new$close-stock_df_new$low),
                         c(rep(NA,1),abs(stock_df_new$high[-1]-stock_df_new$close[-nrow(stock_df_new)])),
                         c(rep(NA,1),abs(stock_df_new$low[-1]-stock_df_new$close[-nrow(stock_df_new)])))
  # Directional Movement
  stock_df_new$PLUSDM1 <- c(rep(NA,1),ifelse(stock_df_new$high[-1]-stock_df_new$high[-nrow(stock_df_new)]>0,
                                             stock_df_new$high[-1]-stock_df_new$high[-nrow(stock_df_new)],0))
  stock_df_new$MINUSDM1 <- c(rep(NA,1),ifelse(stock_df_new$low[-nrow(stock_df_new)]-stock_df_new$low[-1]>0,
                                             stock_df_new$low[-nrow(stock_df_new)]-stock_df_new$low[-1],0))
  # DX
  stock_df_new$TRn <- c(rep(NA,n_DX-1),rollapply(stock_df_new$TR1,n_DX,na.rm = TRUE,sum))
  stock_df_new$PLUSDMn <- c(rep(NA,n_DX-1),rollapply(stock_df_new$PLUSDM1,n_DX,na.rm = TRUE,sum))
  stock_df_new$MINUSDMn <- c(rep(NA,n_DX-1),rollapply(stock_df_new$MINUSDM1,n_DX,na.rm = TRUE,sum))
  stock_df_new$PLUSDIn<-stock_df_new$PLUSDMn/stock_df_new$TRn*100
  stock_df_new$MINUSDIn<-stock_df_new$MINUSDMn/stock_df_new$TRn*100
  stock_df_new$DXn<-ifelse((stock_df_new$PLUSDIn+stock_df_new$MINUSDIn)>0,
                           abs((stock_df_new$PLUSDIn-stock_df_new$MINUSDIn))/(stock_df_new$PLUSDIn+stock_df_new$MINUSDIn)*100,0)
  # ADX
  stock_df_new$ADXn <- c(rep(NA,n_ADX-1),rollapply(stock_df_new$DXn,n_ADX,na.rm = TRUE,mean))
  #
#   ggplot() + 
#     geom_line(data = tail(stock_df_new,20),aes(x = ymd(date),y = PLUSDIn),color = 'red') +
#     geom_line(data = tail(stock_df_new,20),aes(x = ymd(date),y = MINUSDIn),color = 'green')+
#     geom_line(data = tail(stock_df_new,20),aes(x = ymd(date),y = DXn),color = 'grey') + 
#     geom_line(data = tail(stock_df_new,20),aes(x = ymd(date),y = ADXn),color = 'blue')
#     
  # prediction
  n_predict<-10
  stock_df_new<-tail(stock_df_new,n_predict)
  #stockEvaluation
  stockEvaluation<-NULL
  stockEvaluation$symbol<-stock_df_new$symbol[1]
  #the Last Day +DI
  stockEvaluation$PLUSDI<-tail(stock_df_new$PLUSDIn,1)
  #The Last Day -DI
  stockEvaluation$MINUSDI<-tail(stock_df_new$MINUSDIn,1)
  #The Last Day DX
  stockEvaluation$DX<-tail(stock_df_new$DXn,1)
  #The Last Day ADX
  stockEvaluation$ADX<-tail(stock_df_new$ADXn,1)
  # self-defined - 斜率
  stockEvaluation$PLUSDIK <- lm(PLUSDIn~c(1:nrow(stock_df_new)),data=stock_df_new)$coefficients[2]
  stockEvaluation$MINUSDIK <- lm(MINUSDIn~c(1:nrow(stock_df_new)),data=stock_df_new)$coefficients[2]
  stockEvaluation$ADXK <- lm(ADXn~c(1:nrow(stock_df_new)),data=stock_df_new)$coefficients[2]
  stockEvaluation$DXPDICOR <- cor(stock_df_new$DXn,stock_df_new$PLUSDIn)
  stockEvaluation$DXMDICOR <- cor(stock_df_new$DXn,stock_df_new$MINUSDIn)
  return(stockEvaluation)
}
# Sorter
stockSortedByDMITOPSIS<-function(stock_evaluation_df){
  # TOPSIS
  decision<-as.matrix(stock_evaluation_df[,c("PLUSDI","MINUSDI","PLUSDIK","MINUSDIK","ADXK")])
  weight <- c(1, 1, 1, 1, 1, 1)
  impacts <- c("+", "-", "+", "-", "+")
  result<-topsis(decision, weight, impacts)
  # Sort
  stock_evaluation_df<-cbind(stock_evaluation_df,rank=result$rank)
  stock_evaluation_df_sorted<-stock_evaluation_df[order(stock_evaluation_df$rank,decreasing=F),]
  return(stock_evaluation_df_sorted)
}
#####
# Evaluator: stockEvaluatedByTXS
# Special Sorter-Preference Order: stockSortedByTXS
# suggest dayperiod = 20
# 前五日成交量均值
# suggest n_vol = 5
# 评价最后10天
# suggest n_day = 10
# 时间段：最近10天（计算量比需要最近15天数据），最后一天为第10天
# 比较指标：涨幅、量比（当日成交量与（前五日成交量均值）的比值）
# 说明：股价涨（当天的收盘价比开  盘价高），量比涨（当天的量比大于前一天的）
#####
# Evaluator
stockEvaluatedByTXS<-function(stock_df){
  n_day <- 10
  n_vol <- 5
  if(nrow(stock_df)<=n_day+n_vol){return(NULL)}
  #
  stock_df_new<-cbind(stock_df,priceIncrease=NA,volRate=NA)
  stock_df_new$priceIncrease<-(stock_df_new$close-stock_df_new$open)/stock_df_new$open
  stock_df_new$volRate<-stock_df_new$vol/c(rep(NA,n_vol-1),rollapply(stock_df_new$vol, n_vol, mean))
  stock_df_new<-tail(stock_df_new,n_day)
  stockEvaluation<-NULL
  stockEvaluation$symbol<-stock_df_new$symbol[1]
  # 股价指标
  #10天内总体处于上涨状态（最后一天收盘价高于10天前收盘价）涨幅
  stockEvaluation$PRICEINCREASETOTAL<-(stock_df_new$close[n_day]-stock_df_new$close[1])/stock_df_new$close[1]
  #10天中上涨天数大于5天（>5）
  stockEvaluation$PRICEINCREASEDAYS<-length(which(stock_df_new$priceIncrease>0))
  #第9、10天必须涨（若无，则第10天必须涨）
  stockEvaluation$LASTPRICEINCREASE<-stock_df_new$priceIncrease[n_day]
  stockEvaluation$LASTSECONDPRICEINCREASE<-stock_df_new$priceIncrease[n_day-1]
  stockEvaluation$LASTTWODAYPRICEINCREASE<-(stock_df_new$close[n_day]-stock_df_new$open[n_day-1])/stock_df_new$open[n_day-1]
  #第6~10天中，至少涨3天（>3）
  stockEvaluation$LASTFIVEPRICEINCREASEDAYS<-length(which(stock_df_new$priceIncrease[n_day-4:n_day]>0))
  # 量比指标
  #10天中无量比大于10（>10）的现象
  stockEvaluation$VOLRATEDAYSOVERTEN<-length(which(stock_df_new$volRate>10))
  #10天中无股价连续下跌（连续2天及以上），而这几天的量比连续上涨的现象
  stockEvaluation$PRICEDECREASEVOLRATEOVERCONTINUESDAY<-runMaxSequence(which(stock_df_new$volRate>0&stock_df_new$priceIncrease<0))
  #10天中无连续三天的量比大于3
  stockEvaluation$VOLRATEOVERTHREECONTINUESDAY<-runMaxSequence(which(stock_df_new$volRate>3))
  #若第x天股价下跌，第x+1天股价上涨，则第x+1天的量比不能大于5
  stockEvaluation$MAXVOLRATEFORDOWNUP<-max(stock_df_new$volRate[which(stock_df_new$priceIncrease[-1]>0&stock_df_new$priceIncrease[-n_day]<0)])
  #第9、10天两天的量比成上涨态势
  stockEvaluation$LASTSECONDVOLRATEINCREASE<-(stock_df_new$volRate[n_day-1]-stock_df_new$volRate[n_day-2])/stock_df_new$volRate[n_day-2]
  stockEvaluation$LASTVOLRATEINCREASE<-(stock_df_new$volRate[n_day]-stock_df_new$volRate[n_day-1])/stock_df_new$volRate[n_day-1]
  return(stockEvaluation)
}
# Sorter
stockSortedByTXSTOPSIS<-function(stock_evaluation_df){
  #10天内总体处于上涨状态（最后一天收盘价高于10天前收盘价）涨幅
  if(length(which(stock_evaluation_df$PRICEINCREASETOTAL>0))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$PRICEINCREASETOTAL>0)]
  }
  #10天中上涨天数大于5天（>5）
  if(length(which(stock_evaluation_df$PRICEINCREASEDAYS>5))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$PRICEINCREASEDAYS>5)]
  }
  #第9、10天必须涨（若无，则第10天必须涨）
  if(length(which(stock_evaluation_df$LASTPRICEINCREASE>0))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$LASTPRICEINCREASE>0)]
  }
  if(length(which(stock_evaluation_df$LASTSECONDPRICEINCREASE>0))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$LASTSECONDPRICEINCREASE>0)]
  }
  stock_evaluation_df<-cbind(stock_evaluation_df,LASTTWOPRICEINCREASEDELTA=stock_evaluation_df$LASTPRICEINCREASE-stock_evaluation_df$LASTSECONDPRICEINCREASE)
  #第6~10天中，至少涨3天（>3）
  if(length(which(stock_evaluation_df$LASTFIVEPRICEINCREASEDAYS>3))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$LASTFIVEPRICEINCREASEDAYS>3)]
  }
  #10天中无量比大于10（>10）的现象
  if(length(which(stock_evaluation_df$VOLRATEDAYSOVERTEN<10))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$VOLRATEDAYSOVERTEN<10)]
  }
  #10天中无股价连续下跌（连续2天及以上），而这几天的量比连续上涨的现象
  if(length(which(stock_evaluation_df$PRICEDECREASEVOLRATEOVERCONTINUESDAY<2))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$PRICEDECREASEVOLRATEOVERCONTINUESDAY<2)]
  }
  #10天中无连续三天的量比大于3
  if(length(which(stock_evaluation_df$VOLRATEOVERTHREECONTINUESDAY<=3))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$VOLRATEOVERTHREECONTINUESDAY<=3)]
  }
  #若第x天股价下跌，第x+1天股价上涨，则第x+1天的量比不能大于5
  if(length(which(stock_evaluation_df$MAXVOLRATEFORDOWNUP<=5))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$MAXVOLRATEFORDOWNUP<=5)]
  }
  #第9、10天两天的量比成上涨态势
  if(length(which(stock_evaluation_df$LASTVOLRATEINCREASE>0))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$LASTVOLRATEINCREASE>0)]
  }
  if(length(which(stock_evaluation_df$LASTSECONDVOLRATEINCREASE>0))>0){
    stock_evaluation_df<-stock_evaluation_df[which(stock_evaluation_df$LASTSECONDVOLRATEINCREASE>0)]
  }
  stock_evaluation_df<-cbind(stock_evaluation_df,LASTTWOVOLRATEINCREASESUM=stock_evaluation_df$LASTVOLRATEINCREASE+stock_evaluation_df$LASTSECONDVOLRATEINCREASE)
  stock_evaluation_df<-cbind(stock_evaluation_df,LASTTWOVOLRATEINCREASEDELTA=stock_evaluation_df$LASTVOLRATEINCREASE-stock_evaluation_df$LASTSECONDVOLRATEINCREASE)
  # TOPSIS
  decision<-as.matrix(stock_evaluation_df[,c("PRICEINCREASETOTAL","LASTTWODAYPRICEINCREASE","LASTTWOPRICEINCREASEDELTA","LASTTWOVOLRATEINCREASESUM","LASTTWOVOLRATEINCREASEDELTA")])
  weight <- c(1, 1, 1)
  impacts <- c("+", "+", "+", "+", "+")
  result<-topsis(decision, weight, impacts)
  # Sort
  stock_evaluation_df<-cbind(stock_evaluation_df,rank=result$rank)
  stock_evaluation_df_sorted<-stock_evaluation_df[order(stock_evaluation_df$rank,decreasing=F),]
  return(stock_evaluation_df_sorted)
}
#####
# General Sorter-Preference Order:
#####

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
  stock_df_withinperiod$vol[which(stock_df_withinperiod$vol==0)]=NA
  na.omit(stock_df_withinperiod)
  return(stock_df_withinperiod)
}
#####
# switcher
#####
switchStockEvaluator<-function(evaluatorname){
  cmpfun(
    switch(evaluatorname,
           stockEvaluatedByDMI=stockEvaluatedByDMI,
           stockEvaluatedByTXS=stockEvaluatedByTXS
    ))
}

switchStockSorter<-function(sortername){
  cmpfun(
    switch(sortername,
           stockSortedByDMITOPSIS=stockSortedByDMITOPSIS,
           stockSortedByTXSTOPSIS=stockSortedByTXSTOPSIS
    ))
}