#--------Reshape---------------
install.packages("reshape")
library(reshape)
data(airquality)
head(airquality)
names(airquality)
plot(airquality$Ozone, airquality$Temp,type = 'h')
names(airquality)
names(airquality) = tolower(names(airquality))#변수명 다 소문자로
aqm <- melt(airquality, id = c('month', 'day'),na.rm = TRUE) #결측치 제거
tail(aqm)
a<-cast(aqm, day~month~variable)#y축은 day, x축은 month, measure에 해당되는 변수표시
a
b<-cast(aqm, month~variable,mean)#월별 각 변수들의 평균값
b
c<-cast(aqm, month~.|variable,mean)#variable별로 따로 표시
c
d<-cast(aqm,month~variable, mean, margins=c("grand_row","grand_col"))#각 행과 열에 대해 소계 산출
d
e <- cast(aqm, day~month,mean,subset=variable=="ozone")#특정 변수(onzone)만 처리
e        
objects()


#----SQL----------------
install.packages('sqldf')
library(sqldf)
data(iris)
sqldf("select * from iris")
sqldf("select * from iris limit 10") #10개까지만
sqldf("select count(*) from iris where Species like 'se%'")#se로 시작하는 종 count
set.seed(1)
d <- data.frame(year = rep(2012:2014, each=6), count=round(runif(9,0,20)))
#year: 2012년부터 2014년까지 각각 6개씩
#count: runif로 0~20사이의 난수 20개 생성 후 반올림하여 정수화(round)

library(plyr)
ddply(d,"year",function(x){
  cv = sd(x$count)/mean(x$count)#변동계수
  data.frame(cv.count=cv)
})
ddply(d,"year",summarise, mean.count=mean(count))#count컬럼은 사라지는 거임
ddply(d,"year", transform, total.count= mean(count))#아예 새로운 컬럼 추가

install.packages("data.table")
library(data.table)
DT = data.table(x=c("b","b","b","a","a"), v= rnorm(5))
DT
data(cars)
head(cars)
Cars<-data.table(cars)#data.frame -> data.table
head(Cars)
tables()#테이블의 정보(이름, 행 수, 용량, 컬럼 이름, key유무)
sapply(Cars,class)#각 컬럼 이름과 데이터타입
DT[DT$x =='a',]
setkey(DT,x)#key지정(x에 의해 ordering)
tables()

DT["b",]#"b"가 들어간 모든 데이터표시
DT["b" , mult = "first"]#'b'가 들어간 모든 데이터 중 첫번째것만 표시
DT['b', mult = 'last']#마지막것만 표시

grpsize <- ceiling(1e7/26^2)#천만개의 행과 676개의 그룹
tt<-system.time(DF<-data.frame(
  x=rep(LETTERS, each=26*grpsize),#알파벳 대문자 각각 26*grpsizes개씩
  y = rep(letters, each=grpsize),#알파벳 소문자 각각 grpsizes개씩
  v=runif(grpsize*26^2),#난수
  stringAsFactors=FALSE
))

tt
head(DF,3)
tail(DF,3)
dim(DF)#행,열 개수조회
tt<-system.time(ans1<-DF[DF$x=="R" & DF$y=="h",])
#데이터프레임 DF에서 x는 "R", y는 "h"를 갖는 데이터 프레임 DF에서 자료를 찾는 시간
tt

head(ans1,3)
dim(ans1)

DT <- data.table(DF)
setkey(DT,x,y)
ss<-system.time(ans2<-DT[J("R","h")])#index를 활용한 binary search를 수행하므로 빠름
ss
head(ans2,3)

DT[,sum(v)]#v열 sum
DT[,sum(v), by=x]#x(알파벳)별로(grouping) v값들의 sum계산
DT[,sum(v), by="x,y"]#x,y별로(grouping) v값들의 sum계산

str(DT)#파이썬의 infor()과 비슷(데이터 구조 파악)
summary(DT)
cov(DT)

data_1 = c(1,2,NA,5,6,6,7,2,45,34)
data_1[data_1==1]<-NA #결측값으로 바꾸고 싶은값 
data_1
mean(data_1, na.rm=T)#결측값을 제외하고 data_1의 평균구하기
data_1
data_1[complete.cases(data_1)]#실제로 반영되진 않음(inplace=False)
data_1

#---------결측값 처리------------------
install.packages('Amelia')
library(Amelia)
data(freetrade)
head(freetrade)
summary(freetrade)
str(freetrade)

# 변수들의 관계를 이용해 imputation하는 방법
#m: 몇개의 imputation 데이터 세트를 만들지 결정하는 값
#ts: 시계열에 대한 정보, cs: cross-sectional 분석에 포함될 정보
a.out = amelia(freetrade,m=5,ts='year', cs = 'country') #연도와 국가를 고려해 결측값 처리
hist(a.out$imputations[[3]]$tariff, col='grey', border='white')
save(a.out, file = 'imputations.RData')
write.amelia(obj=a.out, file.stem = "outdata")

#위의 과정을 통해 a.out은 결측값들이 imputation방법에 의해 대체된 5개의 데이터 셋을 포함하고
#각 데이터셋이 outdata1.csv ~ outdata5.csv의 이름으로 생성된다.
missmap(a.out) #결측값 처리 전
freetrade$tariff<-a.out$imputations[[5]]$tariff #대체
missmap(freetrade)#처리 후

#-------이상값 처리-----------
x=rnorm(100)
boxplot(x)
x=c(x,19,28,30)#이상값 3개 추가

#boxplot으로 직접 확인
outwith = boxplot(x)
outwith$out#이상값 프린트


#outliers 패키지로 확인: 평균과 가장 큰 차이가 있는 값을 알려줌
install.packages('outliers')
library(outliers)
outlier(x)
outlier(x,opposite=TRUE)#반대편으로 평균과 가장 큰 값을 알려줌
boxplot(x)
