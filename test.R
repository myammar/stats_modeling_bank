#データミックスプロジェクト用データ

data = read.csv('bank_marketing_train.csv')
summary(data)

#factor型データをnumericに変更する関数
data$y = ifelse(data$y =='yes',1,0)

#データの再現性を担保する
set.seed(1234)
num_row = dim(data)[1]
idx = 1:num_row
train_idx = sample(idx, size=num_row*0.7)
train_data = data[train_idx,]
test_data = data[-train_idx,]


#sapply関数 成約率を出すのに使えそう？
#apply


boxplot(age~y, data=data)
table(data$job, data$y)/apply(table(data$job, data$y),1,sum)
table(data$job, data$y)

table(data$marital, data$y)/apply(table(data$marital, data$y),1,sum)
data$marital

table(data$education, data$y)/apply(table(data$education, data$y),1,sum)
data$education

table(data$housing, data$y)/apply(table(data$housing, data$y),1,sum)
data$housing

table(data$loan, data$y)/apply(table(data$loan, data$y),1,sum)
data$loan

boxplot(data$campaign, data$y, data=data)


#single, education-university, job=retired, stdunetが成約率が高そう。


#studentを掘り下げる

train_data_s_and_r = train_data[train_data$job=='student'|train_data$job=='retired',]
head(train_data_s_and_r)

mymodel_1 = glm(y~age+job, data=data, family='binomial')
summary(mymodel_1)

AIC(mymodel_1)
my_model = step(mymodel_1)



