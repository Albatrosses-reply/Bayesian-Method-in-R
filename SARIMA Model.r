
    library(tseries)
    library(urca)
    library(ggplot2)
    library(forecast)
    library(stats)
    library(lmtest)
    library(dplyr)

    #data input
    hair <- read.csv('hairshop.csv')
    head(gaur)
    dim(hair)
    hair$date
    date<-as.Date(1:951, origin='2016-11-2')
    class(date)
    da<-as.data.frame(date)
    head(da)
    hair$date<-as.Date(hair$date)
    daaa<-merge(da, hair, by='date', all=TRUE)

    head(daaa)
    daaa[[is.na](http://is.na/)(daaa)]<-0

    hair<-daaa
    head(hair)

    #3years (total)
    hair1<-ts(daaa$total, frequency=7)
    #2years 10 month (insample)
    hair2<-ts(daaa$total[1:891], frequency=7)
    #2month (outofsample)
    hair3<-ts(daaa$total[892:952], frequency=7)

    head(hair2)
    plot.ts(hair2)

    hair_decom<-decompose(hair2)
    plot(stl(hair2, s.window="periodic"))
    hair_log<-log(hair2)
    hair_sdiff<-diff(hair2, lag=7)
    hair_diff<-diff(hair_sdiff, differences = 1)
    plot.ts(hair_diff)
    adf.test(hair_diff)
    summary(ur.kpss(hair_diff))

    #MA1, SMA3
    ggtsdisplay(hair_diff)
    acf(death_adf, lag.max=50)
    #SAR3
    pacf(death_adf, lag.max=50)
    apa_arima<-Arima(hair2, order=c(1,1,3), seasonal = list(order=c(0,1,3), period=7,drift=TRUE))
    apa_arima$arma
    auto.arima(hair2)

    d2<-as.data.frame(0)
    d3<-as.data.frame(0)
    d4<-as.data.frame(0)
    d5<-as.data.frame(0)
    d6<-as.data.frame(0)
    d7<-as.data.frame(0)
    #Arima
    for(i in 3:0){
        a1<-try(Arima(hair2, order=c(i,1,0), seasonal=list(order=c(0,1,0), period=7)))
        for(j in 3:0){
            b1<-try(Arima(hair2, order=c(i,1,j), seasonal=list(order=c(0,1,0), period=7)))
            for(k in 3:0){
                c1<-try(Arima(hair2, order=c(i,1,j), seasonal=list(order=c(k,1,0), period=7)))
                for(l in 3:0){
                    d1<-try(Arima(hair2, order=c(i,1,j), seasonal=list(order=c(k,1,l), period=7)))
                    d_test<-try(Box.test(d1$residuals, type="Ljung-Box"))
                    d_test2<-try(coeftest(d1))
                    d_test3<-try(d_test2[,4])
                    d_fcast<-try(forecast(d1, h=61))
                    diffs<-try(rmse(hair3, ts(d_fcast$mean, frequency=7)))
                    d3<-try(as.data.frame(cbind(d1$aic, d1$aicc, d1$bic, d_fcast$method, d_test$p.value,diffs, t(d1$coef), t(d_test3))))
                    d4<-tryCatch(bind_rows(d4, d3),
                                 error=function(e){
                                     d4<-d4
                                 })
                    d4[[is.na](http://is.na/)(d4)]<-0
                    d5<-as.data.frame(0)
                    d5<-try(bind_rows(as.data.frame(t(d_fcast$mean)), as.data.frame(t(d_fcast$lower)), as.data.frame(t(d_fcast$upper))))
                    try({rownames(d5)=c('mean', '80lower', '95lower', '80upper', '90upper')})
                    d6<-try(cbind(d5, as.data.frame(d_fcast$method), d1$aic))
                    d7<-tryCatch(bind_rows(d7, d6),
                                 error=function(e){
                                     d7<-d7
                                 })
                }
            }
        }
    }

    #ARIMA Test
    b2<-as.data.frame(0)
    b3<-as.data.frame(0)
    b4<-as.data.frame(0)
    b5<-as.data.frame(0)
    b6<-as.data.frame(0)
    b7<-as.data.frame(0)

    for(i in 3:0){
        a1<-try(Arima(hair2, order=c(i,1,0)))
        for(j in 3:0){
            b1<-try(Arima(hair2, order=c(i,1,j)))
            b_test<-try(Box.test(b1$residuals, type="Ljung-Box"))
            b_test2<-try(coeftest(b1))
            b_test3<-try(b_test2[,4])
            b_fcast<-try(forecast(b1, h=61))
            diffs<-try(rmse(hair3, ts(b_fcast$mean, frequency=7)))
            b3<-try(as.data.frame(cbind(b1$aic, b1$aicc, b1$bic, b_fcast$method, b_test$p.value,diffs, t(b1$coef), t(b_test3))))
            b4<-tryCatch(bind_rows(b4, b3),
                         error=function(e){
                             b4<-b4
                         })
            b4[[is.na](http://is.na/)(b4)]<-0
            b5<-as.data.frame(0)

            b5<-try(bind_rows(as.data.frame(t(b_fcast$mean)), as.data.frame(t(b_fcast$lower)), as.data.frame(t(b_fcast$upper))))
            try({rownames(b5)=c('mean', '80lower', '95lower', '80upper', '90upper')})
            b6<-try(cbind(b5, as.data.frame(b_fcast$method), b1$aic))
            b7<-tryCatch(bind_rows(b7, b6),
                         error=function(e){
                             b7<-b7
                         })
        }
    }

    head(b4)

    b4<-as.data.frame(0)

    hair_result1<-b4[c(order(b4$V1)),]
    hair_result11<-hair_result[1:10,]
    hair_result2<-b4[c(order(b7$b1$aic)),]
    hair_result22<-hair_result[1:10,]
    hair_result22
    hair_predict<-b7[c(order(b7$b1$aic)),]

    rownames(hair_result)<-NULL
    rownames(hair_predict)<-NULL

    head(hair_result)
    head(hair_predict)
    auto.arima(hair2,stepwise = FALSE)
    write.csv(hair_result1, 'hair_result_ARIMA.csv') ; write.csv(hair_predict, 'hair_predict_ARIMA.csv')

    #test
    x1<-coeftest(apa_arima)
    hair_result1<-d4[c(order(d4$)),]
    hair_result11<-hair_result[1:10,]
    hair_result2<-d4[c(order(d5$1)),]
    hair_result22<-hair_result[1:10,]
    hair_result22
    hair_predict<-d7[c(order(d7$d1$aic)),]
    rownames(hair_result)<-NULL
    rownames(hair_predict)<-NULL
    write.csv(hair_result, 'hair_result.csv') ; write.csv(hair_predict, 'hair_predict.csv')

    hair_arima<-Arima(hair1, order=c(3,1,3), seasonal=list(order=c(2,1,0), period=7))
    checkresiduals(hair_arima$residuals)
    p<-checkresiduals(apa_arima)
    plot(apa_fcast2$fitted)
    ts.plot(death4, apa_fcast2$fitted)

    write.csv(hair_fcast, 'hair_predict2.csv')

    #ts.plot
    ts.plot(death2, exp(apa_fcast$fitted) ,gpars=list(xlab="year", ylab="deaths", lty=c(1:3)))
