set.seed(2)
u = runif(10,0,11)
v = runif(10,11,20)
w = runif(10,1,30)
y = 3+0.1*u+2*v-3*w+rnorm(10,0,0.1)
dfrm = data.frame(y,u,v,w)
dfrm

#lm : 선형회귀분석
m<-lm(y~u+v+w) #종속변수: y, 독립변수:u,v,m
m
#결정계수, F통계량, 잔차의 표준오차 등 주요통계량 정보 출력
summary(m)

install.packages('MASS')
library(MASS)
head(ChickWeight)
#식이요법 1을 적용한 데이터만 조회
Chick<- ChickWeight[ChickWeight$Diet ==1,]
Chick
#1번 닭만 조회
Chick<-ChickWeight[ChickWeight$Chick == 1,]
Chick

#-----최적회귀방정식의 선택: 설명변수의 선택--------------
# 후진제거법(backward elimination)

x1 <- c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2 <- c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3 <- c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4 <- c(60,52,20,47,33,22,6,44,22,26,34,12,12)
Y <- c(78.5,74.5,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
df<-data.frame(x1,x2,x3,x4,Y)
df

a<-lm(Y~x1+x2+x3+x4, data=df)
summary(a)
# Y=63.67585 + 1.53451*x1 + 0.49742*x2 + 0.08724*x3 + (-0.15555)*x4로 추정된다.
#여기서 F통계량은 109.7, p-value는 0.0000005073으로 유의수준 5% 하에서 추정된 회귀 모형이 통계적으로 유의함을 알 수 있다.
#결정계수는 0.9821로(수정 후: 0.9731), 이 추정된 회귀식이 데이터를 98% 적절하게 설명하고 있다.

#설명변수의 p-value중 x3이 0.9114로 가장 유의하지 않다고 볼 수 있다. 따라서 x3을 제거하고 다시 회귀분석을 해본다.

b <- lm(Y~x1+x2+x4, data = df)
summary(b)
# Y= 71.5879 + 1.4496*x1 + 0.4169*x2 + (-0.2347)*x4로 추정된다.
# F통계량은 164.2, p-value는 0.00000003564로 유의수준 5%하에서 추정된 회귀모형이 통계적으로 유의함을 알 수 있다.
#결정계수는 x3을 제거하기 전보다 수정된 결정계수가 0.9761로 조금 더 높아졌다.
#이번엔 여기서 p-value가 0.211208로 가장 높은 x4를 제거해보도록 하겠다.

c<-lm(Y~x1+x2, data =df)
summary(c)
# Y= 52.66428 + 1.46586*x1 + 0.66114*x2로 추정된다.
# F통계량은 227, p-value는 0.000000004652로 유의수준 5% 하에서 추정된 회귀모형이 통계적으로 유의함을 알 수 있다.
# 결정계수는 수정 전 0.9784, 후 0.9741로, 이전보다 낮아졌음을 알 수 있다.
# 이제는 두 설명변수 모두 유의하므로 변수제거를 멈춘다. 
# 따라서 최종 회귀식은 Y= 52.66428 + 1.46586*x1 + 0.66114*x2으로 추정된다.

#------ 변수가 많은 경우를 위해 step함수를 활용하자 -> 자동으로 변수 선택 수행--------
#step(분석방법, 변수범위, 변수 선택방법) --> 범위에서 ~1은 상수항

#회귀분석, 상수항~항이 4개인 다항식까지, 전진선택법으로 진행 
step(lm(Y~1, data=df), scope=list(lower=~1, upper=~x1+x2+x3+x4),direction="forward")
#결과: Y = 71.5879 + (-0.2347)*x4 + 1.4496*x1 + 0.4169*x2

#단계별방법으로 진행
step(lm(Y~1, data=df), scope=list(lower=~1, upper=~x1+x2+x3+x4),direction="both")
#결과: Y = 71.5879 + (-0.2347)*x4 + 1.4496*x1 + 0.4169*x2 => 전진선택법과 같다.

#----예제 : MASS패키지의 hills데이터에 전진선택법을 적용해보라.--------------
library(MASS)
data(hills)
names(hills)
summary(step(lm(time~1, data=hills), scope = list(lower=~1, upper =~dist+climb ), direction="forward"))
# time = -8.99204 + 6.21796*dist + 0.01105*climb으로 추정된다. (결정계수: 0.9191)


#-----상관 분석(Correlation Analysis)--------
install.packages("Hmisc")
library(Hmisc)
data(mtcars)
head(mtcars)

#피어슨 상관계수(Pearson correlation)
#drat,disp변수 간의 산점도
plot(mtcars$drat, mtcars$disp) # -> drat이 커질수록 disp가 작아지는것으로 보인다.
cor(mtcars$drat, mtcars$disp) # ->상관계수 -0.7102139: 뚜렷한 음의 상관관계
cov(mtcars)#각 변수들 사이의 공분산(선형관계의 크기)

#rcorr로 모든 변수의 상관관계를 한번에 구하자
rcorr(as.matrix(mtcars),type="pearson") #mtcars의 모든 변수들 사이의 상관계수 및 H0: p=0에 대한 p-value값 출력

#스피어만 상관계수(Spearman correlation)
rcorr(as.matrix(mtcars), type= "spearman")

#-----다차원 척도법(Multidimensional Scaling, MDS)----------
data(eurodist) #각 도시 사이의 거리를 포함하는 행렬 형태의 자료
eurodist
loc <- cmdscale(eurodist) #각 도시의 상대적 위치를 도식화 할 수 있도록 x,y좌표 계산
x <- loc[,1]
y <- loc[,2]
plot(x,y, type = "n",main="eurodist")#type = no plotting, title = "eurodist"
text(x,y,rownames(loc),cex=0.8) #type이 n이라 암것도 표시 안돼있음 대신 text로 대체
abline(v=0,h=0)#수직, 수평선

#-----주성분 분석(Principal Coomponent Analysis, PCA)------------
library(datasets)
data(USArrests)
USArrests
summary(USArrests)
fit<- princomp(USArrests, cor=T)#주성분분석 수행(cor=T는 주성분분석을 공분산행렬이 아닌 상관계수 행렬을 사용한다는 것)
summary(fit)
#-> proportion of variance를 보면, 첫번째와 두번째 주성분만을 이용해 전체분산의 87%를 설명할 수 있다는 것을 알 수 있음.

loadings(fit)#주성분의 로딩 벡터를 보여줌(회귀계수와 비슷)
plot(fit)
fit$scores #관측치를 주성분으로 표현한 값
biplot(fit)#첫번째, 두번째 주성분의 좌표에 그린 그림 -> 첫번째 주성분의 값이 클수록 세가지의 범죄 발생율이 크고, 두번째 주성분값이 작을수록 도심인구 비율이 큰 주

#-------시계열분석(Time-series Analysis)--------------
Nile#1871년~1970년의 아스완댐에서 측정한 나일강의 연간 유입량에 관한 시계열 데이터
ldeaths#1974년~1979년의 영국 내 월별 폐질환 사망자에 관한 시계열 자료
plot(Nile) #비계절성을 띄고 평균이 변화하는 추세를 보이므로 정상성을 만족하지 X
plot(ldeaths)#년도 별로 계절성을 띄고 있음. 매년 일정 주기별로 사망자 수가 늘었다 줄었다 하는 경향을 보임
ldeaths_decompose<-decompose(ldeaths)
ldeaths_decompose$seasonal
plot(ldeaths_decompose)#자료를 4가지 요인으로 분해하는 함수: 계절성을 띄는 것을 확인함

#계절요인을 추정해 그 값을 원 시계열자료에서 빼자
ldeaths_adj<-ldeaths - ldeaths_decompose$seasonal
plot(ldeaths_adj)

#비정상 시계열 차분(diff)
Nile_dif <- diff(Nile,differences = 2) #differences: 차분횟수
plot(Nile_dif) #차분횟수를 늘려가며 정상성을 만족하는지 확인하기

#ARIMA모델 적합 및 결정 -> 자기상관함수(acf), 부분자기상관함수(pacf) 활용
acf(Nile_dif,lag.max=20) #lag개수->너무 많으면 acf 그래프를 보고 모형 식별 판단이 힘듦
pacf(Nile_dif, lag.max=20)
pacf(Nile_dif, lag.max=20, plot=F)

install.packages("forecast")
library(forecast)
auto.arima(Nile) #적절한 모형 찾기 : ARIMA(1,1,1)모형

#모형이 결정되었으니 ARIMA(1,1,1)모형을 통해 미래의 수치값을 예측해보자
Nile_arima <- arima(Nile, order=c(1,1,1))
Nile_arima
Nile_forecast <- forecast(Nile_arima,h=50) #forecast함수로 미래의 수치 값 50개 년도 예측해보자
Nile_forecast
plot(Nile_forecast)
