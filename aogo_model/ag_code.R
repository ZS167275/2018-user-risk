setwd("C:/Users/Administrator/Desktop/bigdata/JDATA-f")

set.seed(100)
library(data.table)
library(stringr)
library(dplyr)

####导入数据集#####

uid_train = fread("uid_train.txt" )

###训练集
sms_train = fread("sms_train.txt" )
voice_train = fread("voice_train.txt",colClasses='character',verbose = T)
wa_train = fread("wa_train.txt" ,colClasses='character',encoding = "UTF-8")

###测试集-A
sms_test_a = fread("sms_test_a.txt" )
voice_test_a = fread("voice_test_a.txt",colClasses='character',verbose = T)
wa_test_a = fread("wa_test_a.txt" ,colClasses='character',encoding = "UTF-8")


###测试集-B
sms_test_b = fread("sms_test_b.txt" )
voice_test_b = fread("voice_test_b.txt",colClasses='character',verbose = T)
wa_test_b = fread("wa_test_b.txt" ,colClasses='character',encoding = "UTF-8")




###合并数据集
uid_test_a<-wa_test_a[,list(V2= -1),by=.(V1)]
uid_test_b<-wa_test_b[,list(V2= -2),by=.(V1)]

uid_cal = rbind(uid_train,uid_test_a,uid_test_b)

sms_cal  = rbind(sms_train ,sms_test_a,sms_test_b)
voice_cal = rbind(voice_train,voice_test_a,voice_test_b)
wa_cal= rbind(wa_train,wa_test_a,wa_test_b)

rm(uid,sms_train ,sms_test_a,sms_test_b,voice_train,voice_test_a,voice_test_b,wa_train,wa_test_a,wa_test_b,uid_train,uid_test_a,uid_test_b)

names(uid_cal )[1:ncol(uid_cal) ] = c("uid","label")
names(sms_cal )[1:ncol(sms_cal) ] = c("uid","opp_num","opp_head","opp_len","start_time","in_out")
names(voice_cal )[1:ncol( voice_cal) ] = c("uid","opp_num","opp_head","opp_len","start_time","end_time","call_type","in_out")
names(wa_cal )[1:ncol( wa_cal) ] = c("uid","wa_name","visit_cnt","visit_dura","up_flow","down_flow","wa_type","date" )


wa_cal2<-wa_cal[wa_name!="NULL",] ###清洗WA数据中的null

feature<-uid_cal

#########短信风险识别######
{
  ###单个对端手机号最大联系次数，类别
  
  #### 统计不同的 对端号码前N位 的联系频次
  ddd<-sms_cal %>% group_by(uid  ) %>% mutate(a1=1   ) %>% count(opp_head)
  
  aaa<-dcast(ddd, uid ~ opp_head, value.var = "n")
  names(aaa)[2:ncol(aaa)] = str_c('sms_N_',names(aaa)[2:ncol(aaa)])
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  #####用户收发短信的数量比例
  
  aaa=sms_cal[,list(in_out_1=length(which(in_out==1))/length( opp_num  ),in_out_0=length(which(in_out==0))/length( opp_num  )  ),by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  #####最近短信的时间距离
  
  ##转化成时间（秒）
  
  sms_cal$time<- (sms_cal$start_time %/% 1000000)%% 100 *86400 +(sms_cal$start_time %/% 10000)%%100 *3600 +(sms_cal$start_time %/% 100)%%100 *60 +(sms_cal$start_time %% 100)
  
  #######最近一次发送短信时间
  
  aaa=sms_cal[in_out==0,list(time_inout0=start_time[which.max( start_time   ) ]),by=.(uid)]
 
   ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  

  
  
  #######最近一次收取短信时间
  
  aaa=sms_cal[in_out==1,list(time_inout1=start_time[which.max( start_time   ) ]),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  

  #######最近一次发送,接收短信时间
  
  aaa=sms_cal[in_out==0,list(timemin_inout0=start_time[which.min( start_time   ) ]),by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  aaa=sms_cal[in_out==1,list(timemin_inout1=start_time[which.min( start_time   ) ]),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  #####对端号码前N位+对端号码长度 组合后的联系频次
  
  sms_cal$head_len<-paste(sms_cal$opp_head,sms_cal$opp_len,sep = "_")
  
  ddd<-sms_cal %>% group_by(uid  ) %>% mutate(a1=1   ) %>% count(head_len)
  
  aaa<-dcast(ddd, uid ~ head_len, value.var = "n")
  names(aaa)[2:ncol(aaa)] = str_c('sms_Nlen_',names(aaa)[2:ncol(aaa)])
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  #####(收到的短信)对端号码前N位+对端号码长度 组合后的的联系频次
  
  sms_cal$head_len_s<-paste(sms_cal$opp_head,sms_cal$opp_len,sep = "_s_")
  
  ddd<-sms_cal[in_out==1,] %>% group_by(uid  ) %>% mutate(a1=1   ) %>% count(head_len_s)
  
  aaa<-dcast(ddd, uid ~ head_len_s, value.var = "n")
  names(aaa)[2:ncol(aaa)] = str_c('sms_s_',names(aaa)[2:ncol(aaa)])
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  

  
  
  
  
  
  ###sms全部、发送 及 收取的短信 时间间隔 的均值 和 方差
  
  ###全部sms 时间间隔的均值
  aaa=sms_cal[,list(timecell_avg_all =  mean( abs( diff(time )  ) ) ),by=.(uid) ]
  
  aaa$timecell_avg_all[which( is.nan(  aaa$timecell_avg_all )   )]<-NA

  ccc=merge(feature,aaa,by=c("uid"),all=T)
  
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  ###发送sms 时间间隔的均值
  aaa=sms_cal[in_out==0,list(timecell_avg_send =  mean( abs( diff(time )  ) ) ),by=.(uid) ]
  
  aaa$timecell_avg_send[which( is.nan(  aaa$timecell_avg_send )   )]<-NA

  ccc=merge(feature,aaa,by=c("uid"),all=T)
  
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  ###接收sms 时间间隔的均值
  aaa=sms_cal[in_out==1,list(timecell_avg_receive =  mean( abs( diff(time )  ) ) ),by=.(uid) ]
  
  aaa$timecell_avg_receive[which( is.nan(  aaa$timecell_avg_receive )   )]<-NA

  ccc=merge(feature,aaa,by=c("uid"),all=T)
  
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  
  ####全部sms  时间间隔的方差
  
  aaa=sms_cal[,list(timecell_var_all =  var( abs( diff(time )  )   )),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  ####发送sms  时间间隔的方差
  
  aaa=sms_cal[in_out==0,list(timecell_var_send =  var( abs( diff(time )  )   )),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  ####接收sms  时间间隔的方差
  
  aaa=sms_cal[in_out==1,list(timecell_var_receive=  var( abs( diff(time )  )   )),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  
  #######短信时间间隔最小值
  aaa=sms_cal[length( (time)  )>=2,list(timecell_min_all =  min( abs( diff(as.double(time) )  )  )),by=.(uid)]
  

  aaa$timecell_min_all[which( is.infinite(  aaa$timecell_min_all )   )]<-NA
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
}


#########通话风险识别######
{
  voice_cal$start_time1<-as.numeric( voice_cal$start_time   )  
  voice_cal$end_time1  <-as.numeric( voice_cal$end_time   ) 
  
  voice_cal$timestart<- (voice_cal$start_time1 %/% 1000000)%% 100 *86400 +(voice_cal$start_time1 %/% 10000)%%100 *3600 +(voice_cal$start_time1 %/% 100)%%100 *60 +(voice_cal$start_time1 %% 100)
  voice_cal$timeend<- (voice_cal$end_time1 %/% 1000000)%% 100 *86400 +(voice_cal$end_time1 %/% 10000)%%100 *3600 +(voice_cal$end_time1 %/% 100)%%100 *60 +(voice_cal$end_time1 %% 100)
  
  voice_cal$call_time<-voice_cal$timeend - voice_cal$timestart
  
  voice_cal$call_time[which( voice_cal$call_time<0 )]<-voice_cal$call_time[which( voice_cal$call_time<0 )] + 86400 
  
  
  
  ####每个客户的手机号 通话时间 对端号码前N位 的联系频次
  ddd<-voice_cal %>% group_by(uid  ) %>% mutate(a1=1   ) %>% count(opp_head)
  
  aaa<-dcast(ddd, uid ~ opp_head, value.var = "n")
  names(aaa)[2:ncol(aaa)] = str_c('voice_',names(aaa)[2:ncol(aaa)])
  ccc=merge(feature,aaa,by=c("uid"),all = T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  #####用户 拨打电话 的数量比例
  aaa=voice_cal[,list(in_out_1=length(which(in_out==1))/length( opp_num  ),in_out_0=length(which(in_out==0))/length( opp_num  )  ),by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  #####对端号码前N位+对端号码长度 组合后的的联系频次
  
  voice_cal$head_len<-paste(voice_cal$opp_head,voice_cal$opp_len,sep = "_")
  
  ddd<-voice_cal %>% group_by(uid  ) %>% mutate(a1=1   ) %>% count(head_len)
  
  aaa<-dcast(ddd, uid ~ head_len, value.var = "n")
  names(aaa)[2:ncol(aaa)] = str_c('voice_',names(aaa)[2:ncol(aaa)])
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  #####(收听的通话)前N位+长度 组合后的的联系频次
  
  voice_cal$head_len_s<-paste(voice_cal$opp_head,voice_cal$opp_len,sep = "_s_")
  
  ddd<-voice_cal[in_out==1,] %>% group_by(uid  ) %>% mutate(a1=1   ) %>% count(head_len_s)
  
  aaa<-dcast(ddd, uid ~ head_len_s, value.var = "n")
  names(aaa)[2:ncol(aaa)] = str_c('voice_',names(aaa)[2:ncol(aaa)])
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
 
  
  ###全部 通话时间 的均值
  aaa=voice_cal[,list(calltime_avg_all =  mean( call_time ) ),by=.(uid) ]

  ccc=merge(feature,aaa,by=c("uid"),all=T)
  
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  ###发送 通话时间 的均值
  aaa=voice_cal[in_out==0,list(calltime_avg_send =  mean( call_time ) ),by=.(uid) ]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  ###接收 通话时间 的均值
  aaa=voice_cal[in_out==1,list(calltime_avg_receive =  mean( call_time   ) ),by=.(uid) ]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  
  ####全部  通话时间  的方差
  
  aaa=voice_cal[,list(calltime_var_all =  var(  call_time  )  ),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  ####发送  通话时间 的方差
  
  aaa=voice_cal[in_out==0,list(calltime_var_send =  var( call_time  )   ),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  ####接收  通话时间 的方差
  
  aaa=voice_cal[in_out==1,list(calltime_var_receive=  var(  call_time   )),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  
  #######通话时间 最小值
  aaa=voice_cal[,list(calltime_min_all =  min( call_time )  ) ,by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  #####拨打电话 的时长总和 、主动通话时长、接听通话时长、拨打/全部通话的比例、接听时长/全部时长的比例
  aaa=voice_cal[,list(calltimesum = sum(call_time ),calltimesum_1=sum( call_time[which(in_out==1)] ),calltimesum_0=sum( call_time[which(in_out==0)] )
                      ,calltimeratio_1=sum( call_time[which(in_out==1)] )/sum( call_time  ),calltimeratio_0=sum( call_time[which(in_out==0)] )/sum( call_time  ) ),by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  ##### 不同 通话类型 的次数占总通话次数的比例
  
  aaa=voice_cal[,list( call_type1=length(which(call_type==1))/length( opp_num  ),call_type2=length(which(call_type==2))/length( opp_num  ),call_type3=length(which(call_type==3))/length( opp_num  ),call_type4=length(which(call_type==4))/length( opp_num  ), call_type5=length(which(call_type==5))/length( opp_num  ) ),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  ##### 不同 通话类型 的时长占总通话时长的比例
  aaa=voice_cal[,list( call_typesum1=sum(call_time[ which(call_type==1)])/sum(call_time ),call_typesum2=sum(call_time[ which(call_type==2)])/sum(call_time ),call_typesum3=sum(call_time[ which(call_type==3)])/sum(call_time ),call_typesum4=sum(call_time[ which(call_type==4)])/sum(call_time ),call_typesum5=sum(call_time[ which(call_type==5)])/sum(call_time ) ),by=.(uid)]
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd)
  
  
  
  
  ####五、 通话人数的丰富度（通话过多少个不同号码，共多少通电话，不同号码/通话总数的比例）
  
  aaa<-voice_cal[ ,list(opp_numkind=length(unique( opp_num )) ,opp_numsum=length( opp_num ) , opp_numratio =length(unique( opp_num ))/length( opp_num ) ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  
  
  
  
  
  
  
  
  
}




#########上网风险识别#####
{
  wa_cal2$visit_cnt   <- as.numeric( wa_cal2$visit_cnt )
  wa_cal2$visit_dura  <- as.numeric( wa_cal2$visit_dura )
  wa_cal2$up_flow     <- as.numeric( wa_cal2$up_flow  )
  wa_cal2$down_flow   <- as.numeric( wa_cal2$down_flow  )
  wa_cal2$wa_type     <- as.numeric( wa_cal2$wa_type )
  wa_cal2$date        <- as.numeric( wa_cal2$date)
  
  
  
  
  
  
  ###一、分别多个时段的使用频次

  
  ####每个客户的 所有的网站和APP ，45天内  日均每个应用 使用频次
  aaa<-wa_cal2[ ,list(wa_cnt45_mean = mean(visit_cnt ) , wa_cnt30_mean = mean(visit_cnt[which(date>=15 )] ),wa_cnt14_mean = mean(visit_cnt[which(date>=31 )] ),wa_cnt7_mean = mean(visit_cnt[which(date>=38 )] ) ) ,by=.(uid)]
  aaa$wa_cnt45_mean[which(is.nan(aaa$wa_cnt45_mean))]<-NA
  aaa$wa_cnt30_mean[which(is.nan(aaa$wa_cnt30_mean))]<-NA
  aaa$wa_cnt14_mean[which(is.nan(aaa$wa_cnt14_mean))]<-NA
  aaa$wa_cnt7_mean[which(is.nan(aaa$wa_cnt7_mean))]<-NA
  
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee,wa_cal)
  
  ####每个客户的 所有的网站和APP ，45天内  日均 使用频次
  aaa<-wa_cal2[ ,list(wa_cnt45_avg = sum(visit_cnt )/45 , wa_cnt30_avg = sum(visit_cnt[which(date>=15 )] )/30,wa_cnt14_avg = sum(visit_cnt[which(date>=31 )] )/14,wa_cnt7_avg = sum(visit_cnt[which(date>=38 )] )/7 ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee,wa_cal)
  
  
  
  
  ###二、分别多个时段的上行和下载的总流量；下载/上行 总流程之比
  ######每个客户的 45天内的，上行和下载的总流量；下载/上行 总流程之比
  
  ddd<-wa_cal2[,list(down_flowsum45=sum(up_flow )+sum( down_flow ),down_up45=sum( down_flow ) /sum(up_flow ) ,up_flowsum45= sum(up_flow ) ,updown_flowsum45=sum( down_flow )   ),by=.(uid)]
  
  ccc=merge(feature,ddd,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  ######每个客户的 30天内的，上行和下载的总流量；下载/上行 总流程之比
  
  ddd<-wa_cal2[date>=15 ,list(down_flowsum30=sum(up_flow )+sum( down_flow ),down_up30=sum( down_flow ) /sum(up_flow ) ,
                              up_flowsum30= sum(up_flow ) ,updown_flowsum30=sum( down_flow )       ),by=.(uid)]
  
  ccc=merge(feature,ddd,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  ######每个客户的 14天内的，上行和下载的总流量；下载/上行 总流程之比
  
  ddd<-wa_cal2[date>=31 ,list(down_flowsum14=sum(up_flow )+sum( down_flow ),down_up14=sum( down_flow ) /sum(up_flow ) ,
                              up_flowsum14= sum(up_flow )    ,updown_flowsum14=sum( down_flow )       ),by=.(uid)]
  
  ccc=merge(feature,ddd,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  ######每个客户的 7天内的，上行和下载的总流量；下载/上行 总流程之比
  
  ddd<-wa_cal2[date>=38 ,list(down_flowsum7=sum(up_flow )+sum( down_flow ),down_up7=sum( down_flow ) /sum(up_flow ) ,
                              up_flowsum7= sum(up_flow )    ,updown_flowsum7=sum( down_flow )       ),by=.(uid)]
  
  ccc=merge(feature,ddd,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  ######每个客户的 3天内的，上行和下载的总流量；下载/上行 总流程之比
  
  ddd<-wa_cal2[date>=42 ,list(down_flowsum3=sum(up_flow )+sum( down_flow ),down_up3=sum( down_flow ) /sum(up_flow ) ,
                              up_flowsum3= sum(up_flow )    ,updown_flowsum3=sum( down_flow )       ),by=.(uid)]
  
  ccc=merge(feature,ddd,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  
  
  
  
  ###三、分别多个时段的每天的 使用频率之和
  ############每个客户的 所有的网站和APP ，45天内 每天的 使用频率之和
  
  ddd<-wa_cal2[ ,list(wa_date_cntsum = sum(visit_cnt )   ) ,by=.(uid,date)]
  
  aaa<-dcast(ddd, uid ~ date, value.var = "wa_date_cntsum")
  names(aaa)[2:ncol(aaa)] = str_c('wa_',names(aaa)[2:ncol(aaa)])
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  ###四、分别多个时段的每天的使用时长之和
  ############每个客户的 所有的网站和APP ，45天内 每天的 使用时长之和
  
  ddd<-wa_cal2[ ,list(wa_date_durasum = sum(visit_dura )   ) ,by=.(uid,date)]
  
  aaa<-dcast(ddd, uid ~ date, value.var = "wa_date_durasum")
  names(aaa)[2:ncol(aaa)] = str_c('wa_',names(aaa)[2:ncol(aaa)])
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  
  ###四、分别多个时段的平均使用时长（使用时长之和/使用频率）
  ###########3天内，平均使用时长（使用时长之和/使用频率）
  aaa<-wa_cal2[ date>=42,list( avg_dura_cnt3= sum(visit_dura )/sum(visit_cnt )     ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  ###########7天内，平均使用时长（使用时长之和/使用频率）
  aaa<-wa_cal2[ date>=38,list( avg_dura_cnt7= sum(visit_dura )/sum(visit_cnt )     ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  ###########14天内，平均使用时长（使用时长之和/使用频率）
  aaa<-wa_cal2[ date>=31,list( avg_dura_cnt14= sum(visit_dura )/sum(visit_cnt )     ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  ###########30天内，平均使用时长（使用时长之和/使用频率）
  aaa<-wa_cal2[ date>=15,list( avg_dura_cnt30= sum(visit_dura )/sum(visit_cnt )     ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  ###########45天内，平均使用时长（使用时长之和/使用频率）
  aaa<-wa_cal2[ ,list( avg_dura_cnt45= sum(visit_dura )/sum(visit_cnt )     ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  
  
  ####五、 APP,网站访问的丰富度（多少不同种类，总数，不同种类/总数的比例）
  ####只计算APP（多少不同种类，总数，不同种类/总数的比例）
  aaa<-wa_cal2[wa_type==1 ,list( appkind = length(unique( wa_name ) )  ,appkindsum =length(wa_name)  ,appkindratio =length(unique( wa_name ) )/length(wa_name)   ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  ####只计算网站
  aaa<-wa_cal2[wa_type==0 ,list( webkind = length(unique( wa_name ) )  ,webkindsum =length(wa_name)  ,webkindratio =length(unique( wa_name ) )/length(wa_name)   ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  ###all（多少不同种类，总数，不同种类/总数的比例）
  aaa<-wa_cal2[,list( wakind = length(unique( wa_name ) )  ,wakindsum =length(wa_name)  ,wakindratio =length(unique( wa_name ) )/length(wa_name)   ) ,by=.(uid)]
  
  ccc=merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  
  
}

####特征结束####



##############
{ 
  
  ###APP 时间间隔 分析######
  aaa<-wa_cal2[wa_type==1,list(a=1),by=.(uid,date)]
  bbb<-aaa[order(date,decreasing=F)]
  
  
  ccc<-bbb[ ,list( diff=diff(date)    ),by=.(uid)]
  
  ddd<-ccc[diff>0,list(diff_mean=mean(diff) ,diff_max =max(diff),diff_min=min(diff),diff_var=var(diff)),by=.(uid)]
  
  eee=merge(feature,ddd,by=c("uid"),all=T)
  feature<-eee
  rm(aaa,bbb,ccc,ddd,eee)
  
  ###WEB 时间间隔 分析
  aaa<-wa_cal2[wa_type==0,list(a=1),by=.(uid,date)]
  bbb<-aaa[order(date,decreasing=F)]
  
  
  ccc<-bbb[ ,list( diff=diff(date)    ),by=.(uid)]
  
  ddd<-ccc[diff>0,list(diff_mean=mean(diff) ,diff_max =max(diff),diff_min=min(diff),diff_var=var(diff)),by=.(uid)]
  
  eee=merge(feature,ddd,by=c("uid"),all=T)
  feature<-eee
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  
  
  ######交叉表####
  ###月使用总金额（短信统计发送条数，通话统计拨打时长的分钟数，流量统计总量；然后按短信条数*0.1，通话*0.15，转化成M为单位的流量*0.1
  
  aaa<-sms_cal[in_out==0 ,list(sms_money=length(opp_num)   )  ,by=.(uid)]
  bbb<-voice_cal[in_out==0  ,list( voice_money=sum(call_time)/60   )  ,by=.(uid)]
  ccc<-wa_cal2[  ,list( wa_money=sum(up_flow) + sum(down_flow)   )  ,by=.(uid)]
  
  ddd<-merge(aaa,bbb,by=c("uid"),all=T)
  eee<-merge(ddd,ccc,by=c("uid"),all=T)
  eee[is.na(eee)]<-0
  eee$money<- (0.1*eee$sms_money )+ 0.15 * eee$voice_money +0.1*(eee$wa_money/1024/1024)
  
  fff<-merge(feature,eee,by=c("uid"),all=T)
  feature<-fff
  rm(aaa,bbb,ccc,ddd,eee,fff)
  
  
  #####
  
  ####各个不同地区通话时长比例#####
  aaa<-voice_cal[, list( bendi_calltime=sum(call_time[which(call_type==1)])/sum(call_time) ,   shengnei_calltime=sum(call_time[which(call_type==2)])/sum(call_time) , 
                         shengji_calltime=sum(call_time[which(call_type==3)])/sum(call_time),gangaotai_calltime=sum(call_time[which(call_type==4)])/sum(call_time),
                         guoji_calltime=sum(call_time[which(call_type==5)]) /sum(call_time)   )       ,by=.(uid)]
  
  eee=merge(feature,aaa,by=c("uid"),all=T)
  feature<-eee
  rm(aaa,bbb,ccc,ddd,eee)
  
  
  #########凌晨短信发送总条数，发送的短信占发送总数的比例（23-5点）##########
  
  sms_cal$hour<-(sms_cal$start_time %/% 10000)%%100
  
  aaa<-sms_cal[in_out==0 , list( night_sms_sum= length(which(hour>=23|hour<=5 ) ) , night_sms_ratio= length(which(hour>=23|hour<=5 ) )/length(opp_num)           )        ,by=.(uid)]
  
  ccc<-merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  
  rm(aaa,bbb,ccc,ddd)
  
  #########凌晨通话总时长，通话总时长占总通话的比例（23-5点）##########
  
  
  voice_cal$hour<-(voice_cal$start_time1 %/% 10000)%%100
  
  aaa<-voice_cal[, list( night_voice_timesum= sum( call_time[ which(hour>=23|hour<=5 ) ]    ) , night_voice_time_ratio=sum( call_time[ which(hour>=23|hour<=5 ) ]    ) /sum(call_time)           )        ,by=.(uid)]
  
  ccc<-merge(feature,aaa,by=c("uid"),all=T)
  feature<-ccc
  
  rm(aaa,bbb,ccc,ddd)
  
  ###########短信，通话 间隔分析——最长间隔，最短间隔，间隔的方差#####
  
  aaa<-sms_cal[,day:=(start_time %/% 1000000)%% 100 ]
  bbb<-voice_cal[,day:= (start_time1 %/% 1000000)%% 100   ]
  ccc<-rbind(aaa[,c("uid","day")],bbb[,c("uid","day")])
  ddd<-ccc[order(day ,decreasing = F)]
  
  
  eee<-ddd[ ,list( diff=diff(day)    ),by=.(uid)]
  fff<-eee[diff>0,list(smsvodiff_mean=mean(diff) ,smsvodiff_max =max(diff),smsvodiff_min=min(diff),smsvodiff_var=var(diff)),by=.(uid)]
  ggg<-merge(feature,fff,by=c("uid"),all=T)
  feature<-ggg
  
  rm(aaa,bbb,ccc,ddd,eee,fff,ggg)
  
  
  #########通话时长在各个时段的比例##########
  
  
  voice_cal$hour<-(voice_cal$start_time1 %/% 10000)%%100
  
  aaa<-voice_cal[, list(  night_voice_hour_sum=sum( call_time    )    )        ,by=.(uid,hour)]
  
  bbb<-voice_cal[, list(  night_voice_allhour_sum=sum( call_time    )    )        ,by=.(uid)]
  
  ddd<-merge(aaa,bbb,by=c("uid"),all.x =T)
  
  ddd$night_voice_hour_ratio<-ddd$night_voice_hour_sum/ddd$night_voice_allhour_sum
  
  eee<-ddd[,c("uid","hour","night_voice_hour_ratio")]
  
  
  fff<-dcast(eee, uid ~ hour, value.var = "night_voice_hour_ratio")
  fff[is.na(fff)]<-0
  names(fff)[2:ncol(fff)] = str_c('voice_hour_ratio_',names(fff)[2:ncol(fff)])
  
  
  
  ggg<-merge(feature,fff,by=c("uid"),all=T)
  feature<-ggg
  
  rm(aaa,bbb,ccc,ddd,eee,fff,ggg)
  
  #########短信条数在各个时段的总比例##########
  
  sms_cal$hour<-(sms_cal$start_time %/% 10000)%%100
  
  aaa<-voice_cal[, list(  night_voice_hour_sum=length( start_time   )    )        ,by=.(uid,hour)]
  
  bbb<-voice_cal[, list(  night_voice_allhour_sum=length( start_time   )    )        ,by=.(uid)]
  
  ddd<-merge(aaa,bbb,by=c("uid"),all.x =T)
  
  ddd$night_voice_hour_ratio<-ddd$night_voice_hour_sum/ddd$night_voice_allhour_sum
  
  eee<-ddd[,c("uid","hour","night_voice_hour_ratio")]
  
  
  fff<-dcast(eee, uid ~ hour, value.var = "night_voice_hour_ratio")
  fff[is.na(fff)]<-0
  names(fff)[2:ncol(fff)] = str_c('sms_hour_ratio_',names(fff)[2:ncol(fff)])
  
  
  
  ggg<-merge(feature,fff,by=c("uid"),all=T)
  feature<-ggg
  
  rm(aaa,bbb,ccc,ddd,eee,fff,ggg)
  
}
############通话记录与短信中的号码重合情况#############

aaa<-sms_cal[,list( sms_sum=length(opp_head)  ),by=.(uid,opp_num)]

bbb<-voice_cal[,list( voice_sum=length(opp_head)  ),by=.(uid,opp_num)]

ccc<-merge(aaa,bbb,by=c("uid","opp_num"),all=T)
ccc[is.na(ccc)]<-0

ddd<-ccc[,list( sms_voice= length(opp_num[which(sms_sum!=0   )])   / length(opp_num) , voice_sms= length(opp_num[which(voice_sum!=0   )])   / length(opp_num) ,
                sms_voice_jiaoji= length(opp_num[which(sms_sum!=0&voice_sum!=0   )])   / length(opp_num)   ),by=.(uid)]

eee<-merge(feature,ddd[,c("uid","sms_voice_jiaoji")],by=c("uid"),all=T)
feature<-eee

rm(aaa,bbb,ccc,ddd,eee,fff,ggg)

##############


library(fBasics)

##时间间隔的偏度系数
#SMS
ddd<-sms_cal[order(day ,decreasing = F)]
eee<-ddd[ ,list( diff=diff(day)    ),by=.(uid)]

aaa=eee[,list(smstimecell_skewness = as.numeric(  skewness(diff   ,method = c("fisher")) )  ),by=.(uid) ]

ccc=merge(feature,aaa,by=c("uid"),all=T)
feature<-ccc
rm(aaa,bbb,ccc,ddd,eee)

#voice
ddd<-voice_cal[order(day ,decreasing = F)]
eee<-ddd[ ,list( diff=diff(day)    ),by=.(uid)]

aaa=eee[,list(voicetimecell_skewness = as.numeric(  skewness(diff   ,method = c("fisher")) )  ),by=.(uid) ]

ccc=merge(feature,aaa,by=c("uid"),all=T)
feature<-ccc
rm(aaa,bbb,ccc,ddd,eee)


#wa
ddd<-wa_cal2[order(date ,decreasing = F)]
eee<-ddd[ ,list( diff=diff(date)    ),by=.(uid)]

aaa=eee[,list(watimecell_skewness = as.numeric(  skewness(diff   ,method = c("fisher")) )  ),by=.(uid) ]

ccc=merge(feature,aaa,by=c("uid"),all=T)
feature<-ccc
rm(aaa,bbb,ccc,ddd,eee)


##时间间隔的峰度系数kurtosis(x)
#sms
ddd<-sms_cal[order(day ,decreasing = F)]
eee<-ddd[ ,list( diff=diff(day)    ),by=.(uid)]

aaa=eee[,list(smstimecell_kurtosis = as.numeric(  kurtosis(diff )  )  ),by=.(uid) ]

ccc=merge(feature,aaa,by=c("uid"),all=T)
feature<-ccc
rm(aaa,bbb,ccc,ddd,eee)

#voice
ddd<-voice_cal[order(day ,decreasing = F)]
eee<-ddd[ ,list( diff=diff(day)    ),by=.(uid)]

aaa=eee[,list(voicetimecell_kurtosis = as.numeric(  kurtosis(diff )  )  ),by=.(uid) ]

ccc=merge(feature,aaa,by=c("uid"),all=T)
feature<-ccc
rm(aaa,bbb,ccc,ddd,eee)

#wa
ddd<-wa_cal2[order(date ,decreasing = F)]
eee<-ddd[ ,list( diff=diff(date)    ),by=.(uid)]

aaa=eee[,list(watimecell_kurtosis = as.numeric(  kurtosis(diff )  )  ),by=.(uid) ]

ccc=merge(feature,aaa,by=c("uid"),all=T)
feature<-ccc
rm(aaa,bbb,ccc,ddd,eee)







######XGBOOT##############
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(Matrix)

set.seed(100)

##OLINE 的数据集划分
train1 = subset(feature,label> -1)
test1  = subset(feature,label==(-1))
test2  = subset(feature,label==(-2))

gc()

############################

##OLINE
output_vector<-train1$label

x<-data.matrix(train1[,c(3:1786),with=FALSE] )
x2<-data.matrix(test1[,c(3:1786),with=FALSE]  )
y<-data.matrix(test2[,c(3:1786),with=FALSE]  )##线上测试???




#参数设置
param =  list(objective = "binary:logistic", 
              eval_metric = "auc",
              booster = "gbtree", 
              #gamma=0.1,
              #lambda=2,			  
              eta = 0.005,
              subsample = 0.75,
              colsample_bytree = 0.65,
              min_child_weight = 3,
              max_depth = 5,
              silent = 0
)



gc()
#交叉验证		  
x  = xgb.DMatrix(x, label = output_vector)


model.cv = xgb.cv(data = x, 
                  params=param, 
                  nrounds = 10000,
                  nfold = 5,
                  print_every_n = 20,
                  early_stopping_rounds = 50
)


#模型建立
model_xgb = xgboost(
  data = x, 
  params=param, 
  print_every_n = 100,
  nrounds = 1815)

###预测test——a
x2  = xgb.DMatrix(x2)
pred_x2 <- predict( model_xgb, x2)
test1$yuce<-pred_x2
test1_x<-test1[yuce<=0.21|yuce>=0.76]

test1_x$label<-ifelse(test1_x$yuce>=0.76,1,0)###收集预测test——a中预测比较有把握的数据


##########将训练集中加入test_a中有把握的数据，然后再次训练数据集



train1_x<-rbind(train1,test1_x[,-c("yuce")])

output_vector3<-train1_x$label
x3<-data.matrix(train1_x[,c(3:1786),with=FALSE] )

#交叉验证		  
x3  = xgb.DMatrix(x3, label = output_vector3)


model.cv = xgb.cv(data = x3, 
                  params=param, 
                  nrounds = 10000,
                  nfold = 5,
                  print_every_n = 20,
                  early_stopping_rounds = 50
)


#模型建立
model_xgb = xgboost(
  data = x3, 
  params=param, 
  print_every_n = 100,
  nrounds = 2487)

####预测数据test-b
y  = xgb.DMatrix(y)
pred <- predict( model_xgb, y)
test2$yuce<-pred



test_pred<-test2[,c("uid","label","yuce")]
eee<-test_pred[order(yuce ,decreasing = T)]
eee$p<-ifelse(eee$yuce>=0.29,1,0)

#result<-eee[,.(uid, p)]
#sum(result$p)

#write.table(result,"20180606_ag_a8911.csv",
#            quote = F,col.names=F,row.names=F,sep=",")



result<-eee[,.(uid, p,yuce)]
sum(result$p)

write.table(result,"20180610_ag_a8887.csv",
            quote = F,col.names=F,row.names=F,sep=",")







