rm(list=ls())

#データミックスプロジェクト用データ
data_pre = read.csv('bank_marketing_train.csv')
data_pre[c('duration',"emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")]=NULL


#特徴量エンジニアリング

data=data_pre
#data['any_loan'] =ifelse((data['loan']=='yes'|data['housing']=='yes'),1,0)
#data[c('day_of_week','default','loan','housing')]=NULL

#年齢のビニング
#library(infotheo)
#data['age_disc'] = discretize(data['age'], disc="equalwidth")
#data['age']=NULL


#職業のグルーピング
#retired, student, unemployedをunemploydedに集約する
for(i in row(data['job'])){
  if(data[i,'job']=='retired'|data[i,'job']=='student'|data[i,'job']=='unemployed'){
    data[i,'job_category'] = data[21,'job']
    }else{
    data[i,'job_category'] = data[i,'job']
  }
  
}
data['job']=NULL



#testデータ・trainデータの分割
set.seed(1234)
num_row = dim(data)[1]
idx = 1:num_row
train_idx = sample(idx, size=num_row*0.5)
train_data = data[train_idx,]
remaining_data = data[-train_idx,]

num_row_r=dim(remaining_data)[1]
idx_r = 1:num_row_r
train2_idx = sample(idx_r, size=num_row_r*0.5)
train2_data = remaining_data[train2_idx,]
test_data = remaining_data[-train2_idx,]


#各種特徴量の視覚化
#summary(data)

#boxplot(age~y, data=data)
table(data$job, data$y)/apply(table(data$job, data$y),1,sum)
#table(data$job, data$y)
#table(data$marital, data$y)/apply(table(data$marital, data$y),1,sum)
#table(data$education, data$y)/apply(table(data$education, data$y),1,sum)
#table(data$housing, data$y)/apply(table(data$housing, data$y),1,sum)
#table(data$loan, data$y)/apply(table(data$loan, data$y),1,sum)
#boxplot(data$campaign, data$y, data=data)


#モデルの作成
#ロジスティック回帰で予測した場合
mymodel_1 = glm(y~., data=train_data, family='binomial')
summary(mymodel_1)
#AIC(mymodel_1)
mymodel_1 = step(mymodel_1)
summary(mymodel_1)
ypred = predict(mymodel_1, newdata=train2_data, type='response')


#�@Revenueを最大化する閾値と�ARevenue最大値を出力する関数

max_th=function(ypred){
        max_threshold = 0
        max_revenue = 0
        
        for(i in seq(0,1,0.001)){
          ypred_flag = ifelse(ypred > i, 1, 0)
          conf_mat = table(train2_data$y, ypred_flag)
          if(is.na(conf_mat[3])){
          }else{
            Sales = conf_mat[2,2]*2000
            cost = (conf_mat[1,2]+conf_mat[2,2])*500
            Revenue = Sales-cost
            
            if(Revenue>max_revenue){
              max_revenue = Revenue
              max_threshold = i
            }
          }
        }
        return(c(max_threshold,max_revenue))
      }
  
#関数の実行
result = max_th(ypred)
print('max_threshold:')
result[1]
print('max_revenue')
result[2]



#テストデータでの検証
ypred_test = predict(mymodel_1, newdata=test_data, type='response')

threshold=result[1]
ypred_flag_test = ifelse(ypred_test > threshold, 1, 0)
conf_mat_test = table(test_data$y, ypred_flag_test)
Sales_test = conf_mat_test[2,2]*2000
cost_test = (conf_mat_test[1,2]+conf_mat_test[2,2])*500
Revenue_test = Sales_test-cost_test
Revenue_test

#現在最高値:103,500 (特徴量エンジニアリングなし)
#現在最高値:105,500 (職業のグループ化)

################################################################
#ケース２：ランダムフォレストで予測した場合
library(randomForest)
mymodel_2 = randomForest(y~., data = train_data, mtry=2, ntree=700)


#グリッドサーチでハイパーパラメータを決定
#train2_dataをハイパーパラメータ決定用、train_dataをモデル作成用とする。
#mymodel_2.tune = tuneRF(train2_data[,-15],train2_data[,15], doBest=TRUE)

#グリッドサーチの結果
#mtry = 3  OOB error = 7.23% 
#Searching left ...
#mtry = 2 	OOB error = 7.02% 
#0.02835821 0.05 
#Searching right ...
#mtry = 6 	OOB error = 7.73% 
#-0.06865672 0.05 

#上記の結果、mtry=2を選択

summary(mymodel_2)
ypred_rf = predict(mymodel_2, newdata=test_data, type='response')

#ランダムフォレストの予測(ypred_rf)はY/Nで返ってくるので、閾値は設定不要

#テストデータでの検証
conf_mat_test_rf = table(test_data$y, ypred_rf)
Sales_test_rf = conf_mat_test_rf[2,2]*2000
cost_test_rf = (conf_mat_test_rf[1,2]+conf_mat_test_rf[2,2])*500
Revenue_test_rf = Sales_test_rf-cost_test_rf
Revenue_test_rf
