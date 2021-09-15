library(chngpt)

browseVignettes("chngpt")

entropy$date_num<-as.numeric(entropy$date)

fit=chngptm(formula.1=entropy~1,
            formula.2=~date_num,
            entropy,
            type="step",
            family="gaussian",
            var.type="bootstrap",
            ci.bootstrap.size=100,
            m.out.of.n=20)

plot(fit)
summary(fit)



# working -----------------------------------------------------------------

entropy2<-entropy %>% filter(source=="theguardian.com")

fit=chngptm(formula.1=entropy~1,
            formula.2=~date_num,
            entropy2,
            type="stegmented",
            family="gaussian",
            var.type="bootstrap",
            ci.bootstrap.size=100,
            m.out.of.n=20)

plot(fit)
summary(fit)
as.numeric(pandemic_start_who)


