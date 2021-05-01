#### 대응표본 (Paired Sample) t-test #####

#광고전후 선호도 변화

# 01.데이터 불러오기 
pst <- read.csv("d:/R 실습습/adv_pst.csv", 
                header=TRUE, 
                na.strings = "."
)
str(pst)

attach(pst) # 객체연결 (attach  -> detach)


# 02.기본통계치 확인: describe(psych패키지 이용)
library(psych)
describe(pst)
dif <- c(after-before) # 선호도 차이
describe(dif)


# 03.그래프 그리기(박스그래프,히스토그램)
opar <- par(no.readonly = TRUE)
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE))
hist(before, main="광고전 선호도", col = "Orange red")
hist(after, main="광고후 선호도", col = "Cornflower blue")
boxplot(dif, main="선호도 변화", col = "yellow") 
par(opar)
options("scipen" = 20) #지수 표기법 수정


# 04.통계분석
t.test(after, before, 
       alternative = c("two.sided"),
       paired = TRUE,
       conf.level = 0.95
)

detach(pst)
par(mfrow=c(1,1))

# 05.통계결과 그래프
mu=0
se=0.72 
data <-rnorm(1000, mu, se)
data <- sort(data)
plot(data, dnorm(data, mu, se), type='l', 
     main="선호도 변화 검정", 
     xlim=c(-4,4))
abline(v=mu, col="green", lty=5)
abline(v=mu+1.96*se, col="blue", lty=5)
abline(v=mu-1.96*se, col="blue", lty=5)
abline(v=4, col="red", lty=5)

