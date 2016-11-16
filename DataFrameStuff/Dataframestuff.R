dat.df <- read.csv("Advertising.csv")[,-1] #[rows,columns]

dim(dat.df)
str(dat.df)
head(dat.df)
tail(dat.df)
nrow(dat.df)
summary(dat.df)
colMeans(dat.df)
mode(dat.df)
class(dat.df)
sapply(dat.df,mode)


mod <- lm(Sales ~ sqrt(TV), dat.df)
res <-residuals(mod)
plot(res ~ predict(mod),
                 main = "Residual Plot",
                 xlab = "Observation",
                 ylab = "Residuals")

X <- dat.df[,1:3] #design matrix
Y <- dat.df[,4] #response variable

X<-cbind(1,X)
XM <- data.matrix(X, rownames.force = NA)
XMT <- t(XM)
I <- solve(XMT%*%XM)
B <- I%*%XMT%*%Y
SSE <- t(Y-(XM%*%B))%*%(Y-(XM%*%B))

uninstall_all_courses()
install_course()

