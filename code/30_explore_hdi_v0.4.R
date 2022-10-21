Governance = readRDS("Data/WG/GovernanceFormatted.rds")

beta_hdi_c = -0.44
beta_hdi_cl = -0.23
beta_hdi_cu = -0.67

cmp3 = NULL
for(a in unique(Governance$Code)){
  tmp2 = subset(Governance, Code == a)
  tmp2 = cbind(tmp2[-nrow(tmp2),],HDI_1 = tmp2$HDI_mean[-1])
  tmp2$HDI_diff = ((tmp2$HDI_1/tmp2$HDI_mean)-1)*100
  tmp3 = data.frame(
    mn_HDI = mean(tmp2$HDI_mean),
    st_HDI = tmp2$HDI_mean[1],
    mn_HDI_diff = mean(tmp2$HDI_diff),
    int_HDI_diff = coef(lm(HDI_diff ~ Year, data = tmp2))[1],
    coef_HDI_diff = coef(lm(HDI_diff ~ Year, data = tmp2))[2]
  )
  
  
  cmp3 = rbind(cmp3, tmp3)
}

plot(cmp3$mn_HDI_diff, cmp3$st_HDI)

plt_a = ggplot() +
  geom_point(data = cmp3, aes(x = mn_HDI_diff, y = coef_HDI_diff)) +
  geom_point(aes(x = c(1.25,1.5, 1.75), y = c(-0.02,-0.02,-0.02)), colour = "red", shape = "cross", size = 6) +
  labs(x = "Change in Human development (%) - mean",
       y = "Deceleration of\n'Change in Human development'\nover time") +
  theme_classic()

plt_b = ggplot() +
  geom_point(data = cmp3, aes(x = mn_HDI_diff, y = st_HDI)) +
  geom_point(aes(x = c(1.25,1.5, 1.75), y = c(0.2,0.2,0.2)), colour = "red", shape = "cross", size = 6) +
  labs(x = "Change in Human development (%) - mean",
       y = "Human Development in 1960") +
  theme_classic()
  
ggarrange(
  plt_a,
  plt_b,
  ncol = 2,
  labels = c("a,", "b,")
)

vec = 0.2


inters = 1.25
rates = -0.02
roc_y = rates*c(-30:30) + inters
roc_x = 1960:2020
vec1 = c(vec)
for(c in c(1:60)){
  vec1 = c(vec1, vec1[c]*((100+roc_y[c])/100))
}


diff_s = (roc_y - mean(Trends[[3]]$HDI_c))/sd(Trends[[3]]$HDI_c)
slp1 = sinh(0.49 +  diff_s*beta_hdi_c)
slp1l = sinh(0.49 +  diff_s*beta_hdi_cl)
slp1u = sinh(0.49 +  diff_s*beta_hdi_cu)

abs1 = c(100)
abs1l = c(100)
abs1u = c(100)
for(d in c(1:(60))){
  abs1 = c(abs1, abs1[d]*((100+slp1[d])/100))
  abs1l = c(abs1l, abs1l[d]*((100+slp1l[d])/100))
  abs1u = c(abs1u, abs1u[d]*((100+slp1u[d])/100))
}


scen1 = data.frame(
  type = "Slow",
  year = c(1960:2020),
  rate = roc_y,
  hdi = vec1,
  abs = abs1,
  absu = abs1u,
  absl = abs1l
)


inters = 1.5
rates = -0.02
roc_y = rates*c(-30:30) + inters
roc_x = 1960:2020
vec1 = c(vec)
for(c in c(1:60)){
  vec1 = c(vec1, vec1[c]*((100+roc_y[c])/100))
}


diff_s = (roc_y - mean(Trends[[3]]$HDI_c))/sd(Trends[[3]]$HDI_c)
slp1 = sinh(0.49 +  diff_s*beta_hdi_c)
slp1l = sinh(0.49 +  diff_s*beta_hdi_cl)
slp1u = sinh(0.49 +  diff_s*beta_hdi_cu)

abs1 = c(100)
abs1l = c(100)
abs1u = c(100)
for(d in c(1:(60))){
  abs1 = c(abs1, abs1[d]*((100+slp1[d])/100))
  abs1l = c(abs1l, abs1l[d]*((100+slp1l[d])/100))
  abs1u = c(abs1u, abs1u[d]*((100+slp1u[d])/100))
}


scen2 = data.frame(
  type = "Moderate",
  year = c(1960:2020),
  rate = roc_y,
  hdi = vec1,
  abs = abs1,
  absu = abs1u,
  absl = abs1l
)

inters = 1.75
rates = -0.02
roc_y = rates*c(-30:30) + inters
roc_x = 1960:2020
vec1 = c(vec)
for(c in c(1:60)){
  vec1 = c(vec1, vec1[c]*((100+roc_y[c])/100))
}


diff_s = (roc_y - mean(Trends[[3]]$HDI_c))/sd(Trends[[3]]$HDI_c)
slp1 = sinh(0.49 +  diff_s*beta_hdi_c)
slp1l = sinh(0.49 +  diff_s*beta_hdi_cl)
slp1u = sinh(0.49 +  diff_s*beta_hdi_cu)

abs1 = c(100)
abs1l = c(100)
abs1u = c(100)
for(d in c(1:(60))){
  abs1 = c(abs1, abs1[d]*((100+slp1[d])/100))
  abs1l = c(abs1l, abs1l[d]*((100+slp1l[d])/100))
  abs1u = c(abs1u, abs1u[d]*((100+slp1u[d])/100))
}


scen3 = data.frame(
  type = "Fast",
  year = c(1960:2020),
  rate = roc_y,
  hdi = vec1,
  abs = abs1,
  absu = abs1u,
  absl = abs1l
)

scens = rbind(scen1, scen2, scen3)
scens$type = factor(scens$type, levels = c("Slow", "Moderate", "Fast"))


plt_a = ggplot(scens) +
  geom_line(aes(x = year, y = rate, group = type, colour = type), size = 1.2) +
  scale_x_continuous(expand = c(0,0), breaks = c(1970,1990,2010)) +
  scale_color_manual(values = c("#d6791c", "#10b38d", "#7210b3"), guide = "none") +
  labs(x = "Year", y = "Change in Human development (%)") +
  theme_classic()

plt_b = ggplot(scens) +
  geom_line(aes(x = year, y = hdi, group = type, colour = type), size = 1.2) +
  scale_x_continuous(expand = c(0,0), breaks = c(1970,1990,2010)) +
  scale_color_manual(values = c("#d6791c", "#10b38d", "#7210b3"), guide = "none") +
  labs(x = "Year", y = "Human Development Index") +
  theme_classic()

plt_c = ggplot(scens) +
  geom_line(aes(x = year, y = abs, group = type, colour = type), size = 1.2) +
  geom_ribbon(aes(x = year, ymin = absl, ymax = absu, fill = type), alpha = 0.1) +
  geom_hline(aes(yintercept = 100), linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), breaks = c(1970,1990,2010)) +
  scale_color_manual(values = c("#d6791c", "#10b38d", "#7210b3"), name = "") +
  scale_fill_manual(values = c("#d6791c", "#10b38d", "#7210b3"), name = "") +
  labs(x = "Year", y = "Abundance") +
  theme_classic()

jpeg("Results/hdi_plot.jpeg", width = 8, height = 5, units = "in", res = 300)
ggarrange(
  plt_a,
  plt_b,
  plt_c,
  ncol = 3,
  common.legend = T,
  legend = "bottom",
  labels = c("a,", "b,", "c,")
)
dev.off()
