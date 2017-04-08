#####################################################################################################################################
#Meta-analysis  (Method:matefor)
library("metafor")
data=read.delim("D:/demo/DEMO.txt",head=T,sep="\t")
result=escalc(measure="MN",mi=data$mi,sdi=data$sdi,ni=data$ni,data)
result_rma=rma(yi,vi,data=result,method="REML")
forest(result_rma)
funnel(result_rma)
names(result_rma)
contributions=1/result_rma$vi/sum(1/result_rma$vi)*100
data$Group[which(contributions==max(contributions))]
barplot(contributions,horiz=T,names=data$Group)