library(DataExplorer)
library(Hmisc)
library(olsrr)

#create_report(stepdata)

dat<-marketing
c<-rcorr(as.matrix(dat))

flattenCorrMatrix2 <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
t<-flattenCorrMatrix2(c$r,c$P)

head(stepdata)

model<- lm(y ~ 0 + . ,data=stepdata)

summary(model)


db<-surgical

mod<-lm(y~ 0+.,data=db)
summary(mod)



