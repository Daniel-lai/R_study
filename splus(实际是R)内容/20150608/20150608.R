attach(pressure)
x<-temperature
y<-pressure
lm.bod<-lm(y~x)
lm.bod.0<-lm(y~x-1)
summary(lm.bod)
summary(lm.bod.0)
plot(y,x)
t0<-data.frame(x=5)
lm.preds<-predict(lm.bod,t0)
lm.preds
detach(pressure)

attach(trees)
lm.tree<-lm(Volume~Girth+Height)
summary(lm.tree)
plot(Volume~Girth+Height)
detach(trees)