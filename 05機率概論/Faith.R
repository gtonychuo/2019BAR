# 載入資料
head(faithful)
D = faithful$eruptions  # copy to a short name

# 先畫一個圖框 
par(cex=0.7)
plot(0,0,xlim=c(1.5,5.25),ylim=c(0,1.1),xlab="噴發時間(分鐘)", 
     ylab="密度 or (累計)機率", main="分布、機率與密度")
abline(h=1, col='lightgray', lwd=0.25, lty=2)

# Empirical Rug PDF, 實證(數值標記)機率密度函數
rug(D)
# Empirical CDF, 實證(數值標記)累計機率密度函數
plot(ecdf(D), cex=0, verticals=T, lwd=2, col='darkgray', add=T)

# Histogram PDF 直方圖機率密度函數
Bins = 20                               # no. bins
bx = seq(min(D), max(D), length=Bins+1) # break sequence 
hist(D, col="#B3FFFF7F", border="white", 
     freq=F, breaks=bx, add=T)
abline(h=0, col='lightgray', lwd=0.25)
# Histogram CDF
adj = (bx[2] - bx[1])/2
steps = stepfun(bx-adj, c(0, sapply(bx, function(b) mean(D <= b))))
plot(steps, cex=0, col='#33CC337F', lwd=3, lty=1, add=T)

# Smooth PDF 平滑機率密度函數
Adjust = 0.5    # bandwidth adjustment
DEN = density(D, adjust = Adjust)
lines(DEN, col='gold', lwd=3)

# Smooth CDF 畫出累計機率密度函數  
PDF = approxfun(DEN$x, DEN$y, yleft=0, yright=0)
x = seq(1,6,0.1)
y = sapply(x, function(i) integrate(PDF, -Inf, i)$value)
lines(x, y, col='red', lwd=3, lty=2) 

# Mark Range 標示範圍[x1, x2]
x1 = 3.8; x2 = 4.8
# rect(x1,-0.1,x2,1.2,col= rgb(0,1,0,alpha=0.2),border=NA)
x = seq(x1, x2, length=100)
polygon(c(x, x2, x1),  c(PDF(x), 0, 0), col="#FF99003F", border=NA)

# Calculate Probability 算出範圍[x1, x2]的機率
(integrate(PDF, x1, x2)$value)

# 問題討論 D-1
x1 = seq(0,5,0.1)
p = sapply(x1, function(x) (integrate(PDF, x, x+1)$value))
data.frame(x1, p)

# 問題討論 D-2
x = seq(1,6,1/6)
cx = sapply(x, function(i) integrate(PDF, -Inf, i)$value)
p = diff(c(0, cx), diff=1)
payoff = 100*p - 5
df = data.frame(x, cx, p, payoff)
df = df[order(-df$payoff),]
df$cumsum =  cumsum(df$payoff)
round(df[df$payoff > 0,], 3)

# 問題討論 D-3

  
