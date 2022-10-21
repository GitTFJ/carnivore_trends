SpeciesMerge = read.csv("Data/Phylogeny/SpeciesListRAW.csv")
SpeciesMerge$Species = paste(SpeciesMerge$Genus, SpeciesMerge$Species)
extant = st_read(
  "Data/DistributionMaps_Mammals_IUCN/TERRESTRIAL_MAMMALS.shp")
extant = subset(extant, presence == 1)
extant2 = extant[extant$binomial %in% SpeciesMerge$Species, ]
extant2 = subset(extant2, binomial != "Ursus maritimus")
x <- raster(ncol=360/5, nrow=180/5, xmn=-180, xmx=180, ymn=-90, ymx=90)
x[] <- seq(from = 1, to = 1, length.out = ncell(x))
plot(x)

rast_list = stack()
for(a in unique(extant2$binomial)){
  trim_extant = subset(extant2, binomial == a)
  trim_intact = mask(x, trim_extant)
  rast_list <- stack(rast_list , trim_intact)
}

datasum = sum(rast_list, na.rm  = T)
plot(datasum)

test_spdf <- as(datasum, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")
test_df$value[test_df$value == 0] = NA

sf_use_s2(FALSE)
map_a = ggplot() +
  geom_tile(data = test_df, aes(x = x, y = y, fill = value), alpha = 0.9) +
  geom_sf(data = world, alpha = 0.1, size = 0.1) +
  scale_x_continuous(breaks = c(-120,-60,0,60,120)) +
  scale_y_continuous(breaks = c(-60,0,60)) +
  coord_sf(ylim = c(-60,90), xlim = c(-175,175)) +
  scale_fill_gradient(low = "grey80", high = "darkcyan", name = "Species\nrichness", breaks = c(0,6,12), limits = c(0,13), na.value = "white") +
  labs(x = " ", y = " ", title = "Species richness") +
  theme_classic() +
  theme(legend.position = "bottom")

x2 <- raster(ncol=360/5, nrow=180/5, xmn=-180, xmx=180, ymn=-90, ymx=90)

pointcount = function(r, pts){
  # make a raster of zeroes like the input
  r2 = r
  r2[] = 0
  # get the cell index for each point and make a table:
  counts = table(cellFromXY(r,pts))
  # fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}

r2 = pointcount(x2, TrendsTrim_lag10[,c("Longitude", "Latitude")])
plot(r2)
datasum[values(datasum) == 0] = NA
diff = (log10(r2/datasum))
diff[is.infinite(diff)] = -2
values(diff)
plot(diff)

test_spdf <- as(diff, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

sf_use_s2(FALSE)
map_b = ggplot() +
  geom_tile(data = test_df, aes(x = x, y = y, fill = value), alpha = 0.8) +
  geom_sf(data = world, alpha = 0.1, size = 0.1) +
  scale_x_continuous(breaks = c(-120,-60,0,60,120)) +
  scale_y_continuous(breaks = c(-60,0,60)) +
  coord_sf(ylim = c(-60,90), xlim = c(-175,175)) +
  scale_fill_gradient(low = "#782B9D", high = "#ffaa3b", name = "Observation count/\nSpecies richness", breaks = c(-2,-1,0,1), labels = c("x0", "x0.1", "x1", "x10")) +
  labs(x = " ", y = " ", title = "Observation density") +
  theme_classic() +
  theme(legend.position = "bottom")

TrendsTemp =  TrendsTrim_lag0[order(TrendsTrim_lag0$Study_year_start),]
TrendsTemp$id = c(1:nrow(TrendsTemp))

plt_c = ggplot() +
  geom_linerange(data = TrendsTemp, aes(y = id, xmin = Study_year_start, xmax = Study_year_end), alpha = 0.5) +
  scale_y_continuous(breaks = c(0,250,500,750,1000), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = "Year", y = "Trend record", title = "Temporal coverage")

TrendsTrim_lag10$PopTrend_perc = (exp(TrendsTrim_lag10$PopulationTrend) - 1)*100
plt_d = ggplot() +
  geom_density(data = TrendsTrim_lag10, aes(x = PopTrend_perc), fill = "grey80") +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  coord_cartesian(xlim = c(-30,30)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = "Annual rate of change (%)", y = "Density")

TrendsTrim_lag0$qual = ifelse(
  TrendsTrim_lag0$QualitativeIncrease == 1,
  "Increase",
  NA
)
TrendsTrim_lag0$qual = ifelse(
  TrendsTrim_lag0$QualitativeDecrease == 1,
  "Decrease",
  TrendsTrim_lag0$qual
)
TrendsTrim_lag0$qual = ifelse(
  TrendsTrim_lag0$QualitativeStable == 1,
  "Stable",
  TrendsTrim_lag0$qual
)
TrendsTrim_lag0$qual = ifelse(
  TrendsTrim_lag0$Quantitative_method == "Qualitative only",
  TrendsTrim_lag0$qual,
  NA
)
TrendsTrim_lag0$qual = factor(TrendsTrim_lag0$qual, levels = c("Decrease", "Stable", "Increase"))

plt_e = ggplot(data = TrendsTrim_lag0[which(TrendsTrim_lag0$Quantitative_method == "Qualitative only"),]) + geom_bar(aes(x = qual), fill = "grey80") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Qualitative trend", y = "Count") +
  theme_classic()


  ggarrange(
  ggarrange(map_a, map_b, ncol = 1, labels = c("a,", "b,")), plt_c, ncol = 2, widths = c(1,0.4), labels = c("", "c,"))

ggarrange(plt_d, plt_e, labels = c("a,", "b,"))
