install.packages("cowplot")
require(cowplot)
m2<-lm(EGGS~DENSITY+SEASON, data=limp)
m2_augmented <- augment(m2)


p <- qplot(data=m1_augmented,
           x=DENSITY,
           y=.fitted,
           ylab="No. of Eggs",
           colour=SEASON,
           shape=SEASON,
           geom=c("line"))+
  geom_point(aes(y=EGGS))

p2 <- qplot(data=m2_augmented,
           x=DENSITY,
           y=.fitted,
           ylab="No. of Eggs",
           colour=SEASON,
           shape=SEASON,
           geom=c("line"))+
  geom_point(aes(y=EGGS))

plot_grid(p, p2, labels = c("A", "B"))



