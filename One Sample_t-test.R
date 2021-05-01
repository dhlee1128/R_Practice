#### 일표본(One Sample) t-test #####

#A초등학교 6학년 학생들의 키 분석을 통한 영양실태조사

# 01.데이터 불러오기 
ost <- read.csv("D:/R 실습/grade6_ost.csv", header=TRUE, na.strings = ".")
str(ost)


# 02.기본통계치 확인: describe(psych패키지 이용)
attach(ost) # 객체연결 (attach  -> detach)
library(psych)
describe(ost)


# 03.그래프 그리기(히스토그램)
hist(height, breaks=10,
     xlab="키", ylab="명", ylim = c(0,40), 
     col = "Cornflower Blue",
     main="A초등학교 6학년학생의 키에 대한 히스토그램 및 정규분포") 


# 04.통계분석
options("scipen" = 20) #지수 표기법 수정
t.test(ost,
       alternative = c("two.sided"),
       mu = 152.2,
       conf.level = 0.95)


# 05.통계결과 그래프
mu=152.2
se=0.55
data <- rnorm(1000, mu, se)
data <- sort(data)
plot(data, dnorm(data, mu, se), type='l', 
     main="6학년 학생의 키(Mu=152.2) 검정", 
     xlim=c(150,154))
abline(v=mu, col="green", lty=5)
abline(v=mu+1.96*se, col="blue", lty=5)
abline(v=mu-1.96*se, col="blue", lty=5)
abline(v=150.44, col="red", lty=5)

detach(ost)

