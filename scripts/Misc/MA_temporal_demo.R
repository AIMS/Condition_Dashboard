library(tidyverse)
load("data/macroalgae/reef.index.ma.RData")
load("data/macroalgae/ma.baselines.reef.draws.RData")
load("data/macroalgae/reef.depth.consequence.metric.summary.ma.RData")

ma.baselines<-ma.baselines.reef.draws%>%
  filter(REEF=="Temple")%>%
  group_by(REEF)%>%
  summarise(mean=mean_cl_boot(baseline.value)[,1],
            min=mean_cl_boot(baseline.value)[,2],
            max=mean_cl_boot(baseline.value)[,3])

bl<-data.frame(year=unique(reef.index.ma$fYEAR),
               mean.v=rep(ma.baselines$mean,length(unique(reef.index.ma$fYEAR))),
               min.v=rep(ma.baselines$min,length(unique(reef.index.ma$fYEAR))),
               max.v=rep(ma.baselines$max,length(unique(reef.index.ma$fYEAR)))
)

reef.index.ma%>%
  filter(REEF=="Temple")%>%
  mutate(ma.p=reef.year.value*100)%>%
  ggplot(aes(fYEAR,ma.p))+
  stat_summary(fun.data="mean_cl_normal", fun.args=list(conf.int=0.975), geom="point", size=5)+
  stat_summary(fun.data="mean_cl_boot", linewidth=0.5, color="dark grey", geom="line")+
  geom_hline(aes(yintercept=ma.baselines$mean*100), linetype="dashed", linewidth=2, color="dark grey")+
  geom_hline(aes(yintercept=40), linetype="dashed", linewidth=2, color="red")+
  theme_bw()+
  xlab("Year")+ylab("Macroalgae Proportion")+ylim(c(0,75))
