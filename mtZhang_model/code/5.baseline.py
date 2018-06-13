#coding:utf-8
import pandas as pd
import numpy as np
# import lda
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.decomposition import NMF
import warnings
from scipy.sparse import csr_matrix
# from tqdm import tqdm
# sklearn.decomposition.TruncatedSVD
from sklearn.decomposition import TruncatedSVD
warnings.filterwarnings('ignore')

def get_data(data,name,time):
    data[name + '_second'] = data[time].apply(lambda x: str(x)[-2:]).astype(int)
    data[name + '_minute'] = data[time].apply(lambda x: str(x)[-4:-2]).astype(int)
    data[name + '_hour'] = data[time].apply(lambda x: str(x)[-6:-4]).astype(int)
    data[name + '_day'] = data[time].apply(lambda x: str(x)[:-6]).replace('', 0).astype(int)
    # data.columns = [name + '_' + str(i) for i in data.columns]
    data[name + '_day_hour_minute_second'] = (data[name + '_day']) * 24 * 60 * 60 + (data[name + '_hour']) *60 * 60+ (data[name + '_minute']) * 60 + (data[name + '_second'])
    return data

def time_cov(data):
    # 单位为 s second
    print('calc time')
    data['start_second'] = data['start_time'].apply(lambda x: str(x)[-2:])
    data['end_second'] = data['end_time'].apply(lambda x: str(x)[-2:])

    data['start_minute'] = data['start_time'].apply(lambda x: str(x)[-4:-2])
    data['end_minute'] = data['end_time'].apply(lambda x: str(x)[-4:-2])

    data['start_hour'] = data['start_time'].apply(lambda x: str(x)[-6:-4])
    data['end_hour'] = data['end_time'].apply(lambda x: str(x)[-6:-4])

    data['start_day'] = data['start_time'].apply(lambda x: str(x)[:-6]).replace('', 0)
    data['end_day'] = data['end_time'].apply(lambda x: str(x)[:-6]).replace('', 0)

    data['day_offset'] = (data['end_day'].astype(int) - data['start_day'].astype(int)) * 24
    del data['start_day'], data['end_day']
    data['end_hour'] = data['end_hour'].astype(int) + data['day_offset']
    del data['day_offset']
    data['hour_offset'] = (data['end_hour'] - data['start_hour'].astype(int)) * 60
    del data['end_hour'],data['start_hour']
    data['end_minute'] = data['end_minute'].astype(int) + data['hour_offset']
    del data['hour_offset']
    data['second_offset'] = (data['end_minute'] - data['start_minute'].astype(int)) * 60
    del data['end_minute'],data['start_minute']
    data['end_second'] = data['end_second'].astype(int) + data['second_offset']
    del data['second_offset']
    data['voice_time'] = data['end_second'] - data['start_second'].astype(int) + 1
    del data['end_second'],data['start_second']
    # log平滑
    data['voice_time'] = np.log1p(data['voice_time'])
    return data

def opp_len_map(data):
    if int(data) >= 16:
        return '20'
    else:
        return data

def opp_head_map(data):
    try:
        data = int(data)
        if (int(data)>801) & (int(data)<900):
            return '800'
        if (int(data)>600) & (int(data)<700):
            return '600'
        if (int(data)>80) & (int(data)<90):
            return '80'
        if (int(data)>20) & (int(data)<=24):
            return '20'
        if (int(data)>500) & (int(data)<600):
            return '500'
        if (int(data)>10) & (int(data)<20):
            return '10'
        if (int(data)>52) & (int(data)<80):
            return '52'
        else:
            return str(data)
    except:
        return '1000000'

# 自定义函数
def row_2_col_static(data,col,index,value,function,name,is_ratdio=True,keep_ord_feat=True,keep_totle=False,log_change=True):
    tmp = pd.pivot_table(data,values=value,index=index,columns=col,aggfunc=function).reset_index().fillna(0)
    tmp.columns = [name + str(i) for i in tmp.columns]
    tmp.rename(columns={name+'uid':'uid'},inplace=True)
    col_list = [i for i in tmp.columns]
    col_list.remove('uid')
    if is_ratdio:
        tmp[name + 'totle'] = np.sum(tmp.drop(['uid'],axis=1),axis=1)
        for i in col_list:
            tmp[i+'_ratdio'] = tmp[i] / tmp[name + 'totle']
    if keep_ord_feat:
        for i in col_list:
            del tmp[i]
    if keep_totle:
        del tmp[name + 'totle']
    if log_change:
        tmp[name + 'totle'] = np.log1p(tmp[name + 'totle'])

    return tmp

def pro_voice(uid_train,data):
    data.columns = ['uid','opp_num','opp_head','opp_len','start_time','end_time','call_type','in_out']
    # 单条记录的通话时长
    data = pd.DataFrame(data).drop_duplicates()
    data = time_cov(data)

    # 时间转化为秒
    data = get_data(data, 'voice_end', 'end_time')
    data = get_data(data, 'voice_start', 'start_time')

    # 用户不同号码段，拨出和拨入的时间计数
    # 号码段的映射
    data['opp_head_map'] = data['opp_head'].astype(str)
    data['opp_head_map'] = data['opp_head_map'].apply(opp_head_map)

    # 数据按照开始时间排序
    # 上一次的结束时间
    data = data.sort_values(['uid', 'start_time'])

    data['last_end_call_time'] = data.groupby(['uid']).voice_end_day_hour_minute_second.shift(+1)

    # 下一次的开始时间 - 上一次的结束时间 = 计算通话间隔
    data['end_last_begin_next_time'] = data['voice_start_day_hour_minute_second'] - data['last_end_call_time']
    data['end_last_begin_next_time'] = data['end_last_begin_next_time'].fillna(0).apply(np.log1p)

    # 统计时间差特征
    end_last_begin_next_time_feat = data.groupby(['uid'],as_index=False)['end_last_begin_next_time'].agg({
        'end_last_begin_next_time_max': np.max,
        # 'end_last_begin_next_time_min':np.min,
        'end_last_begin_next_time_mena': np.mean,
        'end_last_begin_next_time_std': np.nanstd,
        'end_last_begin_next_time_median': np.median
    })

    # 下一次通话的时间差
    data['opp_head_nextCall'] = (data.groupby(['uid']).voice_start_day_hour_minute_second.shift(-1) - data.voice_start_day_hour_minute_second).astype(np.float32)
    data['opp_head_nextCall'] = data['opp_head_nextCall'].fillna(0)
    data['opp_head_nextCall'] = data['opp_head_nextCall'].apply(np.log1p)
    head_next_call = data.groupby(['uid'],as_index=False)['opp_head_nextCall'].agg({
        'opp_head_nextCall_max':np.max,
        # 'opp_head_nextCall_min':np.min,
        'opp_head_nextCall_mean':np.mean,
        'opp_head_nextCall_std':np.nanstd,
        'opp_head_nextCall_median':np.median
    })

    # 用户小时通话特征 / count / ratdio feature
    data['voice_start_hour'] = data['voice_start_hour'].astype(int)
    voice_start_hour_feat_ = row_2_col_static(data, 'voice_start_hour', 'uid', 'voice_time', np.mean, 'voice_start_hour_feat_',True,False,True,False)

    # 用户不同号码段的通话时长

    opp_head_time = data.groupby(['uid','opp_head_map'])['voice_time'].sum().reset_index().set_index(['uid','opp_head_map']).unstack()['voice_time']
    opp_head_time_columns = ['opp_head_map_' + i for i in opp_head_time.columns]
    opp_head_time.columns = opp_head_time_columns
    opp_head_time = opp_head_time.reset_index()

    # 用户不同 opp_len 号码长度的通话时长
    data['opp_len_map'] = data['opp_len'].astype(str)
    data['opp_len'] = data['opp_len'].astype(str)
    data['opp_len_map'] = data['opp_len_map'].apply(opp_len_map)
    opp_len_time = data.groupby(['uid', 'opp_len_map'])['voice_time'].sum().reset_index().set_index(['uid', 'opp_len_map']).unstack()[
        'voice_time']
    opp_len_time_columns = ['opp_len_map_' + i for i in opp_len_time.columns]
    opp_len_time.columns = opp_len_time_columns
    opp_len_time = opp_len_time.reset_index()

    # 用户呼入呼出的时长
    user_in_out_time = data.groupby(['uid','in_out'])['voice_time'].sum().reset_index().set_index(['uid','in_out']).unstack()['voice_time']
    user_in_out_time = pd.DataFrame({
        'uid':list(user_in_out_time.index),
        'in_out_0':list((user_in_out_time.values[:,0])),
        'in_out_1':list((user_in_out_time.values[:,1])),
    }).fillna(0)
    user_in_out_time['in_out_0/in_out'] = user_in_out_time['in_out_0'] / (user_in_out_time['in_out_0'] + user_in_out_time['in_out_1'] )
    user_in_out_time['in_out_1/in_out'] = user_in_out_time['in_out_1'] / (user_in_out_time['in_out_0'] + user_in_out_time['in_out_1'] )

    # 用户通话类型时长 call_type
    user_call_type_time = data.groupby(['uid', 'call_type'])['voice_time'].sum().reset_index().set_index(['uid', 'call_type']).unstack()[
        'voice_time']

    user_call_type_time = pd.DataFrame({
        'uid': list(user_call_type_time.index),
        'call_type_0_time': list((user_call_type_time.values[:, 0])),
        'call_type_1_time': list((user_call_type_time.values[:, 1])),
        'call_type_2_time': list((user_call_type_time.values[:, 2])),
        # 'call_type_3_time': list((user_call_type_time.values[:, 3])),
        # 'call_type_4_time': list((user_call_type_time.values[:, 4]))
    }).fillna(0)
    user_call_type_time['totle_call_type_time'] = user_call_type_time['call_type_0_time'] + user_call_type_time['call_type_1_time'] +\
               user_call_type_time['call_type_2_time']
    user_call_type_time['call_type_0_time/totle_call_type_time'] = user_call_type_time['call_type_0_time'] / (user_call_type_time['totle_call_type_time'])
    user_call_type_time['call_type_1_time/totle_call_type_time'] = user_call_type_time['call_type_1_time'] / (user_call_type_time['totle_call_type_time'])
    user_call_type_time['call_type_2_time/totle_call_type_time'] = user_call_type_time['call_type_2_time'] / (user_call_type_time['totle_call_type_time'])
    # user_call_type_time['call_type_3_time/totle_call_type_time'] = user_call_type_time['call_type_3_time'] / (user_call_type_time['totle_call_type_time'])
    # user_call_type_time['call_type_4_time/totle_call_type_time'] = user_call_type_time['call_type_4_time'] / (user_call_type_time['totle_call_type_time'])
    del user_call_type_time['totle_call_type_time']

    # 用户 全局 平均/最大/最小/方差 通max话时长
    user_voice = data.groupby(['uid'],as_index=False)['voice_time'].agg({
        'voice_max':np.max,
        'voice_min':np.min,
        'voice_mean':np.mean,
        'voice_std':np.nanstd,
        'voice_median':np.median
    })

    # 用户通话对象的个数
    user_voice_nuniuqe = data.groupby(['uid'])['opp_num'].nunique().reset_index()
    user_voice_nuniuqe.columns = ['uid','number_phone']

    # 用户不同类型号码的统计
    data['opp_len'] = data['opp_len'].astype('str')
    data['opp_len'] = data['opp_len'].apply(opp_len_map)
    user_opp_len = pd.get_dummies(data['opp_len'],prefix='opp_len')
    user_opp_len = pd.concat([data['uid'],user_opp_len],axis=1)
    user_opp_len = user_opp_len.groupby(['uid'],as_index=False).sum()
    user_opp_len['opp_len_sum'] = user_opp_len.drop(['uid'],axis=1).sum(axis=1)

    for radio_name in  user_opp_len.drop(['uid','opp_len_sum'],axis=1).columns:
        user_opp_len['%s/opp_len_sum'%(radio_name)] = user_opp_len[radio_name] / user_opp_len['opp_len_sum']
        del user_opp_len[radio_name]
    del user_opp_len['opp_len_sum']

    # 频次统计
    data['call_type'] = data['call_type'].astype('str')
    call_type_len = pd.get_dummies(data['call_type'], prefix='call_type')
    call_type_len = pd.concat([data['uid'],call_type_len],axis=1)
    call_type_len = call_type_len.groupby(['uid'], as_index=False).sum()
    call_type_len['call_type_sum'] = user_opp_len.drop(['uid'],axis=1).sum(axis=1)

    for radio_name in call_type_len.drop(['uid', 'call_type_sum'], axis=1).columns:
        call_type_len['%s/call_type_sum' % (radio_name)] = call_type_len[radio_name] / call_type_len['call_type_sum']
        del call_type_len[radio_name]
    del call_type_len['call_type_sum']

    # 合并特征
    data = pd.merge(uid_train, user_voice, on=['uid'], how='left')

    data = pd.merge(data, user_voice_nuniuqe, on=['uid'], how='left')

    data = pd.merge(data, user_in_out_time, on=['uid'], how='left')

    data = pd.merge(data, user_call_type_time, on=['uid'], how='left')

    data = pd.merge(data, user_opp_len, on=['uid'], how='left')

    data = pd.merge(data, call_type_len, on=['uid'], how='left')

    data = pd.merge(data, opp_head_time, on=['uid'], how='left')

    data = pd.merge(data, opp_len_time, on=['uid'], how='left')

    data = pd.merge(data, voice_start_hour_feat_, on=['uid'], how='left')

    data = pd.merge(data, head_next_call, on=['uid'], how='left')

    data = pd.merge(data, end_last_begin_next_time_feat, on=['uid'], how='left')

    return data

def pro_wa(uid_train,data):
    data.columns = ['uid', 'wa_name', 'visit_cnt', 'visit_dura', 'up_flow',
                        'down_flow', 'wa_type', 'date']
    data = data.sort_values('date')

     # 用户上网花费的时间特征统计
    data['visit_dura_log'] = data['visit_dura'].apply(np.log1p)
    wa_data_visit_dura = data.groupby(['uid'],as_index=False)['visit_dura_log'].agg({
        'visit_dura_max_':np.max,
        'visit_dura_mean_':np.mean,
        'visit_dura_median_':np.median,
        'visit_dura_std_':np.nanstd
    })

    # 用户不同类型所花费的时间 wa_type
    wa_type_feat = row_2_col_static(data, 'wa_type', 'uid', 'visit_dura', np.sum, 'visit_dura_wa_type_',True,True)

    # 用户不同类型up/down流量 down_flow/up_down
    wa_type_feat_down_flow = row_2_col_static(data, 'wa_type', 'uid', 'down_flow', np.sum, 'down_flow_wa_type_',True,True,False,True)
    wa_type_feat_up_flow = row_2_col_static(data, 'wa_type', 'uid', 'up_flow', np.sum, 'up_flow_wa_type_',True,True,False,True)
    wa_type_up_down_flow_feat = pd.merge(wa_type_feat_down_flow,wa_type_feat_up_flow,on=['uid'],how='outer')
    wa_type_up_down_flow_feat['down/up_flow_wa_type_totle'] = wa_type_up_down_flow_feat['down_flow_wa_type_totle'] / wa_type_up_down_flow_feat['up_flow_wa_type_totle']


    data = pd.merge(uid_train, wa_type_feat, on=['uid'], how='left')

    data = pd.merge(data, wa_type_up_down_flow_feat, on=['uid'], how='left')

    data = pd.merge(data, wa_data_visit_dura, on=['uid'], how='left')

    # 总消耗流量 log
    data['totle_flow'] = data['up_flow_wa_type_totle'] + data['down_flow_wa_type_totle']

    del data['label']

    return data

def pro_sms(uid_train,data):
    data.columns = ['uid','opp_num','opp_head','opp_len','start_time','in_out']
    # 拆分send和接受
    data_in_out = pd.get_dummies(data['in_out'],prefix='in_out')
    data = pd.concat([data,data_in_out],axis=1)

    data = get_data(data,'sms_send','start_time')

    data = data.sort_values(['uid','sms_send_day_hour_minute_second'])

    # 发短信的时间差
    data['next_time_sms'] = data.groupby(['uid'])['sms_send_day_hour_minute_second'].shift(-1)
    data['sms_time_diff'] = data['next_time_sms'] - data['sms_send_day_hour_minute_second']
    data['sms_time_diff'] = data['sms_time_diff'].fillna(0)
    data['sms_time_diff'] = data['sms_time_diff'].apply(np.log1p)

    sms_time_diff_feat = data.groupby(['uid'],as_index=False)['sms_time_diff'].agg({
        'sms_time_diff_mean':np.mean,
        'sms_time_diff_max':np.max,
        'sms_time_diff_std':np.nanstd,
        'sms_time_diff_median':np.median
    })


    data['opp_head_map'] = data['opp_head'].apply(opp_head_map)
    # 号码段
    opp_head_map_feat_0 = row_2_col_static(data, 'opp_head_map', 'uid', 'in_out_0', np.sum, 'opp_head_map_feat_send_',False,False,False,False)
    # data['opp_head_map_sp'] = data['opp_head_map'].apply(opp_head_map_sms)
    opp_head_map_feat_1 = row_2_col_static(data, 'opp_head_map', 'uid', 'in_out_1', np.sum, 'opp_head_map_sp_feat_rec_',False,False,False,False)
    opp_head_map_feat_1_0 = row_2_col_static(data, 'opp_head_map', 'uid', 'in_out', np.sum, 'opp_head_map_feat_send/res_',False,False,False,False)



    data = pd.merge(uid_train,opp_head_map_feat_0,on=['uid'],how='outer')

    data = pd.merge(data,opp_head_map_feat_1,on=['uid'],how='outer')

    data = pd.merge(data,opp_head_map_feat_1_0,on=['uid'],how='outer')

    data = pd.merge(data,sms_time_diff_feat,on=['uid'],how='outer')

    del data['label']
    return data


# 训练数据标签
uid_train = pd.read_csv('../data/uid_train.txt',sep='\t',header=None)
uid_test = pd.read_csv('../data/uid_test_b.txt',sep='\t',header=None)
uid_data = pd.concat([uid_train,uid_test],axis=0)
uid_data.columns = ['uid','label']

# 900 / 4099 1 / 0
# 提取通话的特征
voice_train = pd.read_csv('../data/voice_train.txt',sep='\t',header=None)
voice_test_a = pd.read_csv('../data/voice_test_b.txt',sep='\t',header=None)
voice_data = pd.concat([voice_train,voice_test_a],axis=0)

# 提取 sms_train 特征
sms_train = pd.read_csv('../data/sms_train.txt',sep='\t',header=None)
sms_test_a = pd.read_csv('../data/sms_test_b.txt',sep='\t',header=None)
sms_data = pd.concat([sms_train,sms_test_a],axis=0)

# 提取wa train 特征
wa_train = pd.read_csv('../data/wa_train.txt',sep='\t',header=None)
wa_test_a = pd.read_csv('../data/wa_test_b.txt',sep='\t',header=None)
wa_data = pd.concat([wa_train,wa_test_a],axis=0)

print('---------------------------------------------')


sms_data = pro_sms(uid_data,sms_data)

voice_data = pro_voice(uid_data,voice_data)

wa_data = pro_wa(uid_data,wa_data)


# exit()
print('=============================')
# print(sms_train.head())
# print(voice_train.head())
# print(wa_train.head())

data = pd.merge(wa_data,voice_data,on=['uid'],how='outer')
data = pd.merge(data,sms_data,on=['uid'],how='outer')
data = data.fillna(0)

print(data.shape)


# exit()
# print(train)
train = data[data['uid'].isin(uid_train[0].unique())]
test = data[data['uid'].isin(uid_test[0].unique())]

y = train.pop('label')
test_y = test.pop('label')

from sklearn.feature_extraction.text import HashingVectorizer,TfidfVectorizer
from scipy.sparse import csr_matrix, hstack
import jieba

train_uid = train.pop('uid')
train_uid_shape = train_uid.shape[0]
test_uid = test.pop('uid')

# tmp_data = pd.concat([train,test],copy=False)
# 文本特征加入
# tmp_data['seq_wa_name'] = tmp_data['seq_wa_name'].fillna('无')
# tmp_data['seq_wa_name'] = tmp_data['seq_wa_name'].apply(lambda x:' '.join(list(set(jieba.cut(x)))))
# print('tfidf')
# tf1 = TfidfVectorizer(ngram_range=(1,2),analyzer='char')
# discuss_tf_1 = tf1.fit_transform(tmp_data['seq_wa_name'])
# del tmp_data['seq_wa_name']

# cst_v = csr_matrix((tmp_data.fillna(0)))
# all_feat = hstack((cst_v,discuss_tf_1)).tocsr()

# train = cst_v[:train_uid_shape]
# test = cst_v[train_uid_shape:]

imp = pd.DataFrame()
col = train.columns
imp['name'] = train.columns

X = train[col].values
test = test[col].values

print('train shape',X.shape)
print('test shape',test.shape)

from sklearn.metrics import roc_auc_score,f1_score
import lightgbm as lgb
from sklearn.model_selection import StratifiedKFold


N = 5
kf = StratifiedKFold(n_splits=N,random_state=42,shuffle=True)
xx_auc = []
xx_fscore_1 = []
xx_fscore_2 = []
xx_fscore_3 = []
xx_fscore_4 = []
xx_fscore_5 = []
xx_fscore_6 = []
xx_prob = []
for k,(train_in,test_in) in enumerate(kf.split(X,y)):
    print('=======================%d========================='%(k))
    X_train,X_test,y_train,y_test = X[train_in],X[test_in],y[train_in],y[test_in]
    lgb_train = lgb.Dataset(X_train, y_train)
    lgb_eval = lgb.Dataset(X_test, y_test, reference=lgb_train)

    # params={'lambda_l1': 0.5597573591732039, 'min_data_in_leaf': 80, 'metric': 'auc', 'lambda_l2': 0.179769311360561,
    #          'verbose': -1,  'min_gain_to_split': 0.5266448036258556,
    #          'num_leaves': 130, 'objective': 'binary', 'max_depth': 9, 'bagging_freq': 6,
    #          'learning_rate': 0.0752224553166228, 'feature_fraction': 0.6013256259237649, 'boosting_type': 'gbdt',
    #          'max_bin': 255, 'bagging_fraction': 0.8424880452597392}

    params = {
        'boosting_type': 'gbdt',
        'objective': 'binary',
        'metric': {'auc'},
        'num_leaves': 31,
        'learning_rate': 0.01,
        'feature_fraction': 0.9,
        'bagging_fraction': 0.8,
        'bagging_freq': 5,
        'verbose': 0,
        'seed':42
    }


    gbm = lgb.train(params,
                    lgb_train,
                    num_boost_round=20000,
                    valid_sets=lgb_eval,
                    early_stopping_rounds=250,
                    verbose_eval=250)

    imp[k] = list(gbm.feature_importance())

    #=============================================================
    test_y = gbm.predict(X_test, num_iteration=gbm.best_iteration)

    # 0.759047503277063
    # 0.9576688629798387
    # xx_score 0.8384960471581733

    # test_y_label = np.where(test_y>0.4,1,0)

    xx_fscore_1.append(f1_score(y_test, np.where(test_y>0.3,1,0)))
    xx_fscore_2.append(f1_score(y_test, np.where(test_y>0.35,1,0)))
    xx_fscore_3.append(f1_score(y_test, np.where(test_y>0.4,1,0)))
    xx_fscore_4.append(f1_score(y_test, np.where(test_y>0.45,1,0)))
    xx_fscore_5.append(f1_score(y_test, np.where(test_y>0.25,1,0)))
    xx_fscore_6.append(f1_score(y_test, np.where(test_y>0.20,1,0)))

    xx_auc.append(roc_auc_score(y_test, test_y))
    xx_prob.append(gbm.predict(test, num_iteration=gbm.best_iteration))

print('0.3',np.mean(xx_fscore_1))
print('0.35',np.mean(xx_fscore_2))
print('0.4',np.mean(xx_fscore_3))
print('0.45',np.mean(xx_fscore_4))
print('0.25',np.mean(xx_fscore_5))
print('0.2',np.mean(xx_fscore_6))

print(np.mean(xx_auc))

print('xx030_score',0.4*np.mean(xx_fscore_1)+0.6*np.mean(xx_auc))
print('xx035_score',0.4*np.mean(xx_fscore_2)+0.6*np.mean(xx_auc))
print('xx040_score',0.4*np.mean(xx_fscore_3)+0.6*np.mean(xx_auc))
print('xx045_score',0.4*np.mean(xx_fscore_4)+0.6*np.mean(xx_auc))
print('xx025_score',0.4*np.mean(xx_fscore_5)+0.6*np.mean(xx_auc))
print('xx020_score',0.4*np.mean(xx_fscore_6)+0.6*np.mean(xx_auc))

# imp = imp.sort_values(['name'])
xx_prob_mean = 0
for i in xx_prob:
    xx_prob_mean = xx_prob_mean + i

xx_prob_mean = xx_prob_mean / N

# exit()
submit = pd.DataFrame()
submit['uid'] = test_uid
submit['prob'] = xx_prob_mean
submit['label_1'] = np.where(xx_prob_mean>0.25,1,0)
submit['label_2'] = np.where(xx_prob_mean>0.3,1,0)
submit['label_3'] = np.where(xx_prob_mean>0.35,1,0)
submit = submit.sort_values(['prob'],ascending=False)


# best_socre = pd.read_csv('../submit/20180522xx0.85757.csv',header=None)
#
# cop_best = pd.concat([submit['uid'].reset_index(),best_socre[0]],axis=1)
# print(np.sum(cop_best[0]==cop_best['uid']))


print(submit[submit['label_2']==1].shape)
print(submit[submit['label_1']==1].shape)
print(submit[submit['label_3']==1].shape)

import datetime
submit[['uid','label_1']].to_csv('../submit/%s_%.5f.csv'%(str(datetime.date.today()),0.6*np.mean(xx_fscore_5)+0.4*np.mean(xx_auc)),index=False,header=None)
submit[['uid','label_2']].to_csv('../submit/%s_%.5f.csv'%(str(datetime.date.today()),0.6*np.mean(xx_fscore_1)+0.4*np.mean(xx_auc)),index=False,header=None)
submit[['uid','label_3']].to_csv('../submit/%s_%.5f.csv'%(str(datetime.date.today()),0.6*np.mean(xx_fscore_2)+0.4*np.mean(xx_auc)),index=False,header=None)
#




'''
0.5431602928422997
0.8705627318330571
xx_score 0.6741212684386026

0.5422394858987629
0.8661617272369305
xx_score 0.67180838243403

0.5728589490675763
0.8738563329925118
xx_score 0.6932579026375505

0.5792387310991829
0.8761983738182926
xx_score 0.6980225881868267

0.5498270057758617
0.8794412579290629
xx_score 0.6816727066371422

0.5823632509573395
0.8810351393562776
xx_score 0.7018320063169148

0.772687496242179
0.9581473821270571
xx_score 0.8468714505961302

0.35
0.770699171932941
0.9586378540159028
xx_score 0.8458746447661258

0.7694680891345392
0.9600068710271963
xx_score 0.8456836018916021


0.9621295022351932
xx030_score 0.8574285966500457
xx035_score 0.8549725144129139
xx040_score 0.8516691191896613
xx045_score 0.847121099099278
xx025_score 0.8553996961823125
xx020_score 0.8490599517136304
'''
