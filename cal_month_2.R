library(ggpubr)
cal_month_2 <- function(month1,df){
  monthslsit=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'ctober', 'November', 'December')
  if (month1 %in% monthslsit){
    df_now=df[which(df$year%in%seq(2000,2017)),]
    #print(dim(df_now))
    df_past=df[which(!df$year%in%seq(2000,2017)),]
    #print(dim(df_past))
    df1=df_now[which(df_now$month==month1),]
    #print(df1)
    df2=df_past[which(df_past$month==month1),]
    #print(df2)
    for(i in seq(2000,2017)){
      df3=df1[which(df1$year==i),]
      df4=df2[which(df1$year==i),]
      df3$temperature=df3$temperature-df4$temperature 
      tmp=ggplot(data=df3, aes(x=depth, y=temperature)) + geom_point() + labs(title = paste('year:',i,'& before'))
      assign(paste('t',i-1999,sep='_'),tmp)
    }
    avg=group_by(df3,depth)
    #print(avg)
    avg=summarise(avg,mean=mean(temperature))
    avg=ggplot(data=avg, aes(x=depth, y=mean)) + geom_point() + labs(title = paste('all year average'))
    ggarrange(t_1,t_2,t_3,t_4,t_5,t_6,t_7,t_8,t_9,t_10,t_11,t_12,t_13,t_14,t_15,t_16,t_17,t_18,avg,ncol=3,nrow=3,widths =c(1,1,1))
  }
  else{
    print('wrong input format')
  }
}