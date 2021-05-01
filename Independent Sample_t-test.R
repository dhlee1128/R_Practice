#### 독립표본(Independent Sample) t-test #####

#OECD대비 한국의 경제성장률

# 01.데이터 불러오기 
ist <- read.csv("d:/R 실습/kor_ist.csv", 
                col.names = c("com", "rate"), 
                na.strings = ".")
ist$com <- factor(ist$com,
                  levels=c(1, 2),
                  labels=c("korea","oecd"))
str(ist)
ist$rate <- round(ist$rate, 2)

attach(ist) # 객체연결 (attach  -> detach)


# 02.기본통계치 확인: summary or describe(psych패키지 이용)
tapply(rate, com, summary)

library(psych)
describeBy(rate, com, mat=T)


# 03.그래프 그리기(박스그래프,히스토그램)

opar <- par(no.readonly = TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)) # 화면분할
boxplot(rate ~ com, col = c("Orange red", "Cornflower blue"))
hist(rate[com=="korea"], col = "Orange red")
hist(rate[com=="oecd"], col = "Cornflower blue")
par(opar)

# 04.통계분석
# 등분산 검정
var.test(rate ~ com, data = ist)
options("scipen" = 20) #지수 표기법 수정
t.test(rate ~ com, 
       data = ist, 
       alternative = c("two.sided"), 
       var.equal = TRUE,
       conf.level = 0.95)
detach(ist)


# 05.통계결과 그래프
x = 4.780323 #한국평균
se = 0.4407161
data <-rnorm(1000, x, se) 
data <- sort(data)
plot(data, dnorm(data, x, se), col="blue",type='l', main="OECD대비 한국의 경제성장률", xlim=c(0, 7), ylim=c(0, 1.3))
abline(v=x, col="blue", lty=3)

par(new=T) 
x = 2.318710 #OECD평균
se = 0.3429887
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type='l', col="red",
     xlim=c(0, 7), ylim=c(0, 1.3))
abline(v=x, col="red", lty=3)
legend("topright", lty = 1, col = c("blue", "red"), legend = unique(com))

