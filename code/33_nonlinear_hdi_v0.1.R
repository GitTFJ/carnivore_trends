library(lme4)
dat_test = subset(TrendsTrim_lag10, !is.na(PopTrend_perc))
m_test = lmer(ihs(PopTrend_perc) ~
                poly(HDI, 2) +
                HDI_c +
                (1|Genus/Species) +
                (1|sub.region/alpha.3),
              data = dat_test)
summary(m_test)
confint(m_test)
hist(residuals(m_test))
vif(m_test)
