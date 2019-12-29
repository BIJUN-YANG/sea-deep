library(ggpubr)
cal_month_1 <- function(month1,month2,df){
  monthslsit=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'ctober', 'November', 'December')
  df=df[df$year%in%seq(2000,2017),]
  if (month1 %in% monthslsit && month2 %in% monthslsit){
    idx1=which(monthslsit==month1)
    idx2=which(monthslsit==month2)
    if (abs(idx1-idx2)==1){
      df1=df[which(df$month==month1),]
      df2=df[which(df$month==month2),]
      df2$temperature=df2$temperature-df1$temperature
      df2=df2[,c(1,3,4)]
      for(i in seq(2000,2017)){
        df3=df2[which(df2$year==i),]
        tmp=ggplot(data=df3, aes(x=depth, y=temperature)) + geom_point() + labs(title = paste('year:',i))
        assign(paste('t',i-1999,sep='_'),tmp)
      }
      avg=group_by(df2,depth)
      avg=summarise(avg,mean=mean(temperature))
      avg=ggplot(data=avg, aes(x=depth, y=mean)) + geom_point() + labs(title = paste('all year average'))
      ggarrange(t_1,t_2,t_3,t_4,t_5,t_6,t_7,t_8,t_9,t_10,t_11,t_12,t_13,t_14,t_15,t_16,t_17,t_18,avg,ncol=3,nrow=3,widths =c(1,1,1))
    }else{
      print('Input months are not adjacent')
    }
  }
  else{
    print('wrong input format')
  }
}