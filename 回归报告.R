library(car)

#######读取处理数据
data <- read.csv('C:/Users/Lenovo/Desktop/housing.csv')
data1 <- data.frame(data)
datas <- scale(data1)


lm1 <- lm(MEDV~.,data = data1)
e <- resid(lm1,digits = 5)
e2 <- e^2

###########逐步回归去除变量
both1 <- step(lm1,direction = 'both')

data3 <- data.frame(data[,1:2],data[,4:6],data[,8:14])

lm3 <- lm(MEDV~.,data = data3)

###########异方差检验
data2 <- data.frame(data[,1:2],data[,4:6],data[,8:13],e2)
lm2 <- lm(e2~.,data = data2)
an1 <- anova(lm2)

###########多元加权最小二乘估计
s <-seq(10,20,1)
result1 <- vector(length = 9,mode = "list")
result2 <- vector(length = 9,mode = "list")
for (j in 1 : 9)
{
  w <- data[,13] ^ (-s[j])
  lm4 <- lm(MEDV~.,weights = w,data = data4)
  result1[[j]] <- logLik(lm4)
  result2[[j]] <- summary(lm4)
}
result1


#X <- data.frame()

#cor1 <- cor.test(X,abse,alternative="two.sided",method="spearman",conf.level=0.95)
