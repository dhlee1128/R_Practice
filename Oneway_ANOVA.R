#### 일원 분산분석(One-way ANOVA) #####

#신재생 에너지 사용현황 (단위 : 천 toe)

# 01.데이터 불러오기 
energy <- read.csv("d:/R 실습/energy_owa.csv", 
                    header=TRUE, 
                    na.strings = "."
)

energy$group <- factor(energy$group,
                       levels=c(1:4),
                       labels=c("태양광", "풍력", "수력", "바이오"))
str(energy)

attach(energy) # 객체연결 (attach  -> detach)


# 02.기본통계치 확인: describe(psych패키지 이용)
library(psych)
describeBy(toe, group, mat=T)


# 03.그래프 그리기(박스그래프,히스토그램)
## ggplot2 설명참조 : https://m.blog.naver.com/definitice/221159633224

#install.packages("ggplot2")
library(ggplot2)
ggplot(energy, aes(x = group, y = toe, fill = group)) + 
  geom_boxplot(outlier.colour="red") +
  ggtitle("에너지별 이용현황") +
  theme_bw() + # ggplot2 테마
  theme(title = element_text(color="darkblue", size=10))


ggplot(energy, aes(x=toe)) + 
  geom_histogram(binwidth=10) + 
  facet_grid(. ~ group) +
  ggtitle("에너지별 이용현황") + 
  theme_classic()

# 04.통계분석
# 등분산 검증

bartlett.test(toe ~ group, data=energy)

#leveneTest
#install.packages("car")
library(car)
leveneTest(toe ~ group, data=energy, center=mean)

# ANOVA분석
Result <- aov(toe ~ group, data=energy)
summary(Result)

# 부록: 이분산일때 Welch's ANOVA test
oneway.test(toe ~ group, data=energy, var.equal = FALSE)
#install.packages("nparcomp")
library(nparcomp)
result = mctp(toe ~ group, data=energy)
summary(result)


# 사후검정(Multicamparison test )
# 그룹별 표본수가 같을 때 : Tukey HSD, Duncan LSR
# 그룹별 표본수가 다를 때 : Scheffe
TukeyHSD(Result) #diff 값이 가장 중요

#install.packages("agricolae")
library(agricolae)

# group=TRUE: 그룹으로 묶어서 표시, FALSE: 1:1로 비교
# console=TRUE: 결과를 화면에 표시
duncan.test(Result, "group", group=TRUE, console = TRUE)
scheffe.test(Result, "group", group=FALSE, console = TRUE)

# 05.통계결과 그래프
tukeyPlot <- TukeyHSD(Result) # 그룹간 차이 비교
plot(tukeyPlot)
duncanPlot <- duncan.test(Result, "group")
plot(duncanPlot)


x=340.4238 # 태양광 평균
se=121.98986
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), col="blue",type='l', 
     main="신재생 에너지별 이용현황", 
     xlim = c(0, 1900), ylim = c(0, 0.015))
abline(v=x, col="blue", lty=3)

par(new=T) # 그래프를 겹쳐서 표현하기 
x=148.0143 # 풍력
se=34.41597
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type='l', col="red",
     xlim = c(0, 1900), ylim = c(0, 0.015))
abline(v=x, col="red", lty=3)

par(new=T)
x=604.1381 # 수력
se=82.02553
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type='l', col="black",
     xlim = c(0, 1900), ylim = c(0, 0.015))
abline(v=x, col="black", lty=3)

par(new=T)
x=1119.7286 # 바이오
se=293.42154
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type='l', col="green",
     xlim = c(0, 1900), ylim = c(0, 0.015))
abline(v=x, col="green", lty=3)
legend("topright", lty = 1, col = c("blue", "red", "black", "green"), legend = unique(group))


detach(energy)

