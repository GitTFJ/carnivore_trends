#Load CaPTrends
CaPTrendsRaw = read.csv("Data/Trends/captrends.csv")
CaPTrendsRaw$UniqueID = paste("cpt", CaPTrendsRaw$DataTableID, sep = "_")
CaPTrends = subset(CaPTrendsRaw, select = c(
  UniqueID,
  DataTableID,
  Citation_key,
  Species,
  Singular_country,
  Study_year_start,
  Study_year_end,
  N_observations,
  Quantitative_trend,
  Quantitative_method,
  Qualitative,
  Field_method,
  Modelling_method,
  Genetic_data,
  Harvest_data,
  Invasive_species,
  Record_labelled_inaccurate,
  Asymptotic_growth,
  Metric_unusual,
  Latitude,
  Longitude
))
rm(CaPTrendsRaw)

#Load time-series
ManualTrend = read.csv("Data/Trends/abundance.csv")

CombAbundance = NULL
ManTrendTableID = as.character(unique(ManualTrend$DataTableID))
for(a in ManTrendTableID){
  skip_to_next <- FALSE
    Temp = subset(ManualTrend, DataTableID == a)
     if((nrow(Temp)-sum(Temp$Value == 0)) < 2){
       ZeroN = sum(Temp$Value == 0)
       ExtinctRecol = T
     } else {
       ExtinctRecol = F
       ZeroN = sum(Temp$Value == 0)
     }
    if(min(Temp$Value) < 0){
      Temp$Value = Temp$Value + abs(min(Temp$Value))
    }
       Temp$Value = Temp$Value/max(Temp$Value)
       Temp$Value = Temp$Value + 0.01
       Temp$ValueScale = log(Temp$Value)
       Temp = subset(Temp, !is.infinite(ValueScale))
       

       if(is.nan(Temp$ValueScale[1])){
         print(paste("skip:",a))
       } else {
         if(nrow(Temp) < 3){
           TempOutput = summary(lm(ValueScale ~ Year, data = Temp))
           Est = TempOutput$coefficients[2,1]
           N_observations = nrow(Temp)
           Method = "Abundance/Density"
           DataTableID = a
           TempDF = data.frame(DataTableID, Est, N_observations, Method, ExtinctRecol, ZeroN)
           CombAbundance = rbind(CombAbundance, TempDF)
         } else {
           tryCatch((
           TempOutput = summary(gls(ValueScale ~ Year, data = Temp,
                                    correlation = corCAR1(form = ~Year)))
           ), error = function(e) { skip_to_next <<- TRUE})
           if(skip_to_next) { next }  
           Est = TempOutput$coefficients[2]
           N_observations = nrow(Temp)
           Method = "Abundance/Density"
           DataTableID = a
           TempDF = data.frame(DataTableID, Est, N_observations, Method, ExtinctRecol, ZeroN)
           CombAbundance = rbind(CombAbundance, TempDF)
         }
       }
}



CaPTrends$DataTableID = as.character(CaPTrends$DataTableID)
CaPTrends = left_join(CaPTrends, CombAbundance, by = "DataTableID")
CaPTrends$N_observations = ifelse(CaPTrends$Quantitative_method != "Manual calculation required", CaPTrends$N_observations.x, CaPTrends$N_observations.y)
CaPTrends$N_observations = ifelse(CaPTrends$N_observations < 2,
                                  1,
                                  CaPTrends$N_observations)
CaPTrends$N_observations.x = NULL
CaPTrends$N_observations.y = NULL


CombPopulationTrend = NA
for(a in 1:nrow(CaPTrends)){
  Meth = CaPTrends$Quantitative_method[a]
  Trend = CaPTrends$Quantitative_trend[a]
  
  N = CaPTrends$N_observations[a]
  Ys = CaPTrends$Study_year_start[a]
  Ye = CaPTrends$Study_year_end[a]
  Pt = NA
  Va = NA
  if(grepl("Qualitative only", Meth)){
    Trend_t = NA
    Va = NA
  } else if (grepl("Manual calculation required", Meth)){
    Pt = CaPTrends$Est[a]
  } else {
    if(grepl("Lambda", Meth)){
      Trend_t = log(Trend)
    } else if(grepl("Percentage change", Meth)){
      if(Trend == 0){
        CaPTrends$ExtinctRecol[a] = T
      } else {
        ab = NULL
        ab[1] = 100
        ab[2] = ab[1]*(Trend/100)
        tim = c(Ys, Ye)
        Trend_t = coef(lm(log(ab) ~ tim))[2]
      }
    } else if(grepl("change", Meth)){
      if(Trend == 0){
        CaPTrends$ExtinctRecol[a] = T
      } else {
        ab = NULL
        ab[1] = 100
        ab[2] = ab[1]*Trend
        tim = c(Ys, Ye)
        Trend_t = coef(lm(log(ab) ~ tim))[2]
      }
    } else if (grepl("R trend", Meth)){
      Trend_t = Trend
    } else {
      message("skip")
    }
    Pt = Trend_t
  }
  CaPTrends$PopulationTrend[a] = Pt
}



#Load living planet
LPI = read.csv("Data/Trends/lpi_master.csv", row.names = NULL)
LPI = subset(LPI, is.na(Tag))
ManualTrend2 = read.csv("Data/Trends/lpi_abundance.csv")

CombAbundance2 = NULL
ManTrendTableID2 = as.character(unique(ManualTrend2$id))
for(a in ManTrendTableID2){
  skip_to_next <- FALSE
  Temp = subset(ManualTrend2, id == a)
  if(nrow(Temp) > 1){
    if((nrow(Temp)-sum(Temp$Value == 0)) < 2){
      ZeroN = sum(Temp$Value == 0)
      ExtinctRecol = T
    } else {
      ExtinctRecol = F
      ZeroN = sum(Temp$Value == 0)
    }
    if(min(Temp$Value) < 0){
      Temp$Value = Temp$Value + abs(min(Temp$Value))
    }
    Temp$Value = Temp$Value/max(Temp$Value)
    Temp$Value = Temp$Value + 0.01
    Temp$ValueScale = log(Temp$Value)
    Temp = subset(Temp, !is.infinite(ValueScale))
    
    
    if(is.nan(Temp$ValueScale[1])){
      print(paste("skip:",a))
    } else {
      if(nrow(Temp) < 3){
        TempOutput = summary(lm(ValueScale ~ Year, data = Temp))
        Est = TempOutput$coefficients[2,1]
        N_observations = nrow(Temp)
        Method = "Abundance/Density"
        LPIID = as.integer(a)
        Study_year_start = min(Temp$Year)
        Study_year_end = max(Temp$Year)
        TempDF = data.frame(LPIID, Study_year_start, Study_year_end, Est, N_observations, Method, ExtinctRecol, ZeroN)
        CombAbundance2 = rbind(CombAbundance2, TempDF)
      } else {
        tryCatch((
          TempOutput = summary(gls(ValueScale ~ Year, data = Temp,
                                   correlation = corCAR1(form = ~Year)))
        ), error = function(e) { skip_to_next <<- TRUE})
        if(skip_to_next) { next }  
        Est = TempOutput$coefficients[2]
        N_observations = nrow(Temp)
        Method = "Abundance/Density"
        LPIID = as.integer(a)
        Study_year_start = min(Temp$Year)
        Study_year_end = max(Temp$Year)
        TempDF = data.frame(LPIID, Study_year_start, Study_year_end, Est, N_observations, Method, ExtinctRecol, ZeroN)
        CombAbundance2 = rbind(CombAbundance2, TempDF)
      }
    }
  } 
  else {
  }
}


LPI = LPI[,c(1,2,8,9,17,19,20,25)]
LPI$Species = paste(LPI$Genus, LPI$Species, sep = " ")
LPI$Genus = NULL
LPI = subset(LPI, 
             CountryList != "Serbia, Montenegro" &
               CountryList != "Poland, Slovakia" &
               CountryList != "Namibia, Botswana, South Africa, Tanzania, United Republic Of, Kenya" &
               CountryList != "Italy, France" &
               CountryList != "Argentina, Brazil")
LPI$Species[LPI$Species == "Hyaena brunnea"] <- "Parahyaena brunnea"
LPI$Species[LPI$Species == "Leopardus Pardalis"] <- "Leopardus pardalis"
LPI$Species[LPI$Species == "Uncia uncia"] <- "Panthera uncia"

colnames(LPI) = c(
  "Citation_key",
  "LPIID",
  "Species",
  "Singular_country",
  "Latitude",
  "Longitude",
  "Invasive_species")
LPI$Invasive_species = ifelse(LPI$Invasive_species == F, 1, NA)
LPI = left_join(LPI, CombAbundance2)
LPI$UniqueID = paste("lpi", LPI$LPIID, sep = "_")

Trends = rbind.fill(CaPTrends, LPI)
Trends$PopulationTrend = ifelse(!is.na(Trends$LPIID), 
                                Trends$Est,
                                Trends$PopulationTrend)
Trends$ZeroN[is.na(Trends$ZeroN)] = 0
Trends$N_observations[is.na(Trends$N_observations)] = 1
Trends$N_observations[Trends$N_observations < 2] = 1

Trends$LowQualityRecord = ifelse(Trends$N_observations - Trends$ZeroN < 2 |
                                   !is.na(Trends$Genetic_data) |
                                   !is.na(Trends$Harvest_data) |
                                   !is.na(Trends$Record_labelled_inaccurate) |
                                   !is.na(Trends$Asymptotic_growth) |
                                   !is.na(Trends$Metric_unusual), 
                                 T, 
                                 F)
Trends$ExtinctRecol[is.na(Trends$ExtinctRecol)] <- F
Trends$PopulationTrend[is.infinite(Trends$PopulationTrend)] <- NA
Trends$QualitativeIncrease = ifelse((is.na(Trends$PopulationTrend) | Trends$ExtinctRecol == T) &
                                      Trends$Qualitative == "Increase", 1, 0)
Trends$QualitativeDecrease = ifelse((is.na(Trends$PopulationTrend) | Trends$ExtinctRecol == T) &
                                      Trends$Qualitative == "Decrease", 1, 0)
Trends$QualitativeStable = ifelse((is.na(Trends$PopulationTrend) | Trends$ExtinctRecol == T) &
                                    (Trends$Qualitative == "Stable"  |
                                       Trends$Qualitative == "Varied: both declines and recoveries present"), 1, 0)
Trends = left_join(Trends, Coords[,c("Area", "UniqueID")], by = "UniqueID")

Trends = subset(Trends,
                Study_year_start > 1969 &
                  Study_year_end < 2016 &
                  Singular_country != "GLOBAL" &
                  Singular_country != "" &
                  is.na(Invasive_species), 
                select = c(
                  "UniqueID",
                  "DataTableID",
                  "Species",
                  "Singular_country",
                  "Study_year_start",
                  "Study_year_end",
                  "Latitude",
                  "Longitude",
                  "Area",
                  "N_observations",
                  "PopulationTrend",
                  "Quantitative_method",
                  "QualitativeIncrease",
                  "QualitativeDecrease",
                  "QualitativeStable",
                  "Field_method",
                  "Modelling_method",
                  "ExtinctRecol",
                  "LowQualityRecord"
                ))

CountryAccess = read.csv("Data/CountryData/CountryInAccess.csv")
CountryISO3 = read.csv("Data/CountryData/CountryContinent.csv")
Country = left_join(CountryISO3, CountryAccess, by = c("alpha.2" = "ISO_3166.1"))
Trends$Singular_country = as.character(Trends$Singular_country)
Trends$Singular_country[Trends$Singular_country == "Tanzania, United Republic Of"] = "Tanzania, United Republic of"
Trends$Singular_country[Trends$Singular_country == "Svalbard And Jan Mayen"] = "Norway"
Trends$Singular_country[Trends$Singular_country == "Korea, Republic Of"] = "Korea, Republic of"
Trends$Singular_country[Trends$Singular_country == "Kosovo"] = "Serbia"


Trends = left_join(Trends, Country[,c("Country", "alpha.3", "sub.region", "Conts")], by = c("Singular_country" = "Country"))

Trends$PopulationTrend[is.infinite(Trends$PopulationTrend)] = NA



ID_df = read.csv("Data/Trends/ID(save).csv")
Trends = left_join(Trends, ID_df)

