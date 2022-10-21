hdi_long = read.csv("Data/HDI/hdi_past.csv")

plt_list = list()

for(cun in c("ARG", "IND", "TZA", "USA", "BRA", "FIN", "RWA", "BWA", "KOR")){
  pt = ggplot() +
    geom_line(data = Governance[which(Governance$Code == cun),], aes(x = Year, y = HDI_mean)) +
    geom_ribbon(data = Governance[which(Governance$Code == cun),], aes(x = Year, ymax = HDI_mean + (sqrt(HDI_var)*1.96), ymin = HDI_mean - (sqrt(HDI_var)*1.96)), alpha = 0.1) +
    geom_line(data = hdi_long[which(hdi_long$Code == cun & hdi_long$Year > 1959),], aes(x = Year, y = hdi + (mean(Governance[which(Governance$Code == cun),]$HDI_mean)) - mean(hdi_long[which(hdi_long$Code == cun & hdi_long$Year > 1959),]$hdi)), colour = "red") +
    labs(title = cun, y = "Standardised human development") +
    theme_classic()
  plt_list[[cun]] = pt
}

ggarrange(plotlist=plt_list, ncol = 3, nrow = 3)



hdi_long2 = left_join(hdi_long, Governance[which(Governance$Year < 1990),c("Code", "Year", "HDI_mean")])
hdi_long2 = subset(hdi_long2, !is.na(HDI_mean))

cor(hdi_long2$hdi, hdi_long2$HDI_mean)
