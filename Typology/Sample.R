---
  date: "`r Sys.Date()`"
author: Ebba Mark
title: "Typology Work New"
output: 
  html_document:
  theme: "lumen"
code_download: true
toc: true
toc_float: true
toc_depth: 2
number_sections: true
df_print: paged
knit: (function(inputFile, encoding) { 
  rmarkdown::render(inputFile, 
                    encoding = encoding, 
                    output_format = "html_document", 
                    output_dir = here::here("output")) 
})
---
  
  # Set-up
  
  ```{r, echo=FALSE}
knitr::opts_chunk$set(comment = NA, echo = TRUE, eval = TRUE, 
                      warning = FALSE, message = FALSE, 
                      fig.width = 6, fig.height = 4)
```

# Libraries
```{r, message = FALSE}
# core libraries
library(conflicted)
library(tidyverse)
library(readxl)
library(here)
library(kableExtra)
library(vtable)
library(cluster)   
library(factoextra)
library(flexclust)
library(xtable)
library(stargazer)
library(usmap)
library(ggplot2)
library(viridis)
library(fmsb)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
source("ref_lists.R")
```

# Data
```{r,  echo=FALSE}

coal_emp <- read_excel(here("allcomp_brief.xlsx"))
typ_us <- read_excel(here("typ_total_us.xlsx"))
typ_coal <- typ_us %>% 
  subset(fips %in% c(coal_emp$fips[which(coal_emp$active_mines != 0 & coal_emp$year == 2019)], 
                     "51195", "51720")) %>% 
  select("fips","RUC_2013","POPESTIMATE2019", 
         "ed_over_25_bachelor_or_higher", "med_earnings",
         "lfpr_20_64_female", "diversity_index") %>% 
  data.frame(row.names = 1)

scaled_typ <- scale(typ_coal)


# test <- read_excel("/Users/ebbamark/Desktop/Coal USA/coal_usa/data/allcomp_final.xlsx") %>% 
#   select(year, fips, active_mines, mines_diff, lag_diff, lag_diff2, uer, diff_uer, 
#          employed, diff_log_employed, unemployed, diff_log_unemployed, labour_force, 
#          diff_log_lf, pop, diff_log_pop, realgdp, diff_log_realgdp, log_pop,
#          diff_log_realgdp_pc)

# writexl::write_xlsx(test, here("allcomp_brief.xlsx"))

# Creates typology sheet using coal counties as identified in regression work
# final_cc has combined statistical area of Wise and Norton (fips: 51955) but 
# demographic characteristics are divided into Wise (fips: 51195) and Norton (fips: 51720)

```

# Analysis
```{r}
# Clustering on 251 coal counties claiming analysed in regression section
# cluster_coal <- cc[,c("fips","RUC_2013","POPESTIMATE2019", 
#                             "ed_over_25_bachelor_or_higher", "med_earnings",
#                             "lfpr_20_64_female", "diversity_index")]

# cluster_coal2 <- data.frame(cluster_coal, row.names = 1)
# scaled_coalcluster <- scale(cluster_coal2)

# Indicates optimal clusters between 2-4
fviz_nbclust(scaled_typ, hcut, method = "wss") +
  labs(title= NULL)

res_k3 <- eclust(scaled_typ, k = 3,  "hclust")

cc <- data.frame(typ_coal, cluster = res_k3$cluster)

ct_voting <- function(clust) {
  n_clust = sum(with(cc, cc$cluster == clust))
  p_16 = sum(with(cc, party_16 == "REPUBLICAN" & cc$cluster == clust))
  p_20 =sum(with(cc, party == "REPUBLICAN" & cc$cluster == clust))
  if(p_16 == p_20) {return(p_16)}else{return(as.list[p_16,p_20])}
}

# Create table of cluster characteristics
cc_output <- as.data.frame(aggregate(typ_coal, by=list(cluster=res_k3$cluster), mean)) %>%
  cbind(res_k3$size) %>%
  arrange(RUC_2013) %>% t %>% data.frame %>%
  setNames(c("Type 1", "Type 2", "Type 3")) %>% 
  cbind("US Average" = t(summarize(typ_us, cluster = "us_total", mean(RUC_2013),
                                   mean(POPESTIMATE2019),mean(ed_over_25_bachelor_or_higher), 
                                   mean(med_earnings), mean(lfpr_20_64_female), 
                                   mean(diversity_index, na.rm = TRUE), nrow(typ_us)))) %>% 
  magrittr::set_rownames(c("Cluster", "2013 Rural-Urban Code","Population Size",
                           "Educational Attainment: Bachelor's Degree or Higher %; aged 25-64",
                           "Median Earnings USD","Female Labour Force Participation %; aged 25-64",
                           "Chmura Diversity Index", "Number of counties"))
# cc_output <- cbind(cc_output, res_k3$size)
# cc_output <- as.data.frame(t(cc_output))
# cc_output <- cc_output[, c(2, 31)]
# colnames(cc_output) = c("Type 1", "Type 2", "Type 3")
# cc_output <- rbind(cc_output, c(ct_voting(2), ct_voting(1), ct_voting(3)))

# ct_voting(2)
# typ_coal$party[which(typ_coal$fips == "06077")] = "DEMOCRAT"
# us_avg <- cbind(t(summarize(typ_us, cluster = "us_total", mean(RUC_2013),mean(POPESTIMATE2019),mean(ed_over_25_bachelor_or_higher), mean(med_earnings), mean(lfpr_20_64_female), mean(diversity_index, na.rm = TRUE), nrow(typ_us))))
# cc_output1 <- cbind(cc_output, us_avg)

# NOTE THAT FIVE VIRGINIA COUNTIES ARE MISSING FROM THIS GROUP
# typ_coal$fips[which(is.na(typ_coal$diversity_index))]
# cc_output1 <- cc_output1[-1,]
# print(cc_output1)

# cluster_coal <- cc[,c("fips","RUC_2013","POPESTIMATE2019", 
#                             "ed_over_25_bachelor_or_higher", "med_earnings",
#                             "lfpr_20_64_female", "diversity_index", "party", "party_16")]
# 
# cluster_coal$party_20_bin <- ifelse(cluster_coal$party == "REPUBLICAN", 1, 0)
# cluster_coal$party_16_bin <- ifelse(cluster_coal$party_16 == "REPUBLICAN", 1, 0)

stargazer(as.data.frame(cc_output[-1,]), digits = 1, 
          notes = "* The number of coal counties included in the typology (252) differs from the number identified in the econometric analysis (251) because Wise County, Virginia and Norton City, Virginia  are considered separately by the various entities reporting data for the typology characteristics. They are combined into one county area by the Bureau of Economic Analysis whose method was used as the standard used for the econometric analysis.", summary = FALSE, type = "text")


```
# Radar plot
```{r}

mins <- typ_coal[,1:6] %>% summarise_all(min) %>% append(., 1) %>% as.data.frame  %>% `rownames<-`("min")
maxs <- typ_coal[,1:6] %>% summarise_all(max)  %>% append(., 0) %>% as.data.frame %>% `rownames<-`("max")
names(mins) <-  c(names(mins[-6]),"rep_pct")
names(maxs) <-  c(names(maxs[-6]),"rep_pct")

cc_radar <- cc_output %>%  t %>% as.data.frame %>% mutate(rep_pct = `9`/`res_k3$size`) %>%  select(RUC_2013, POPESTIMATE2019, ed_over_25_bachelor_or_higher, med_earnings, lfpr_20_64_female, diversity_index, rep_pct) %>% rbind(maxs, mins, .)


cc_radar$RUC_2013[which(rownames(cc_radar) == "max")] = 1
cc_radar$RUC_2013[which(rownames(cc_radar) == "min")] = 9

cc_radar$diversity_index[which(rownames(cc_radar) == "max")] = min(cluster_coal$diversity_index)
cc_radar$diversity_index[which(rownames(cc_radar) == "min")] = max(cluster_coal$diversity_index)

par(xpd = TRUE, mar = c(2, 1, 2, 1))
radarchart(cc_radar, 
           vlabels = c("Urban",
                       "Population     \n Size     ",
                       "Educational             \nAttainment             ",
                       "Median\nEarnings",
                       "     Female\n     LFPR",
                       "          Economic\n          Diversity",
                       "Democrat"), 
           pcol = viridis(3), vlcex = 0.9, plty = 1, plwd = 3, cglcol = "gray10")

```

```{r}

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend(
  x = "bottom", legend = rownames(cc_radar[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20, col = viridis(3),
  text.col = "black", cex = 1, pt.cex = 2, inset = c(0,-.3), xpd = TRUE
)
```



```{r}
cluster_map <- read_excel(here("data/cc_clusters_251.xlsx"))
cluster_map$type = 0
cluster_map$type[which(cluster_map$cluster == 1)] = 2
cluster_map$type[which(cluster_map$cluster == 2)] = 1
cluster_map$type[which(cluster_map$cluster == 3)] = 3

pdmif_map <- subset(cluster_map, fips %in% cc_pdmif$fips)
cluster_map$type = as.factor(cluster_map$type)
pdmif_map$type = as.factor(pdmif_map$type)


plot_usmap(data = pdmif_map, values = "type", regions = "counties", col = "gray90", size = 0.04, exclude = c("AK", "HI" )) + 
  scale_fill_viridis_d(name = "County Type", labels = c("Type 1", "Type 2", "Type 3", "No active mines 2002-2019"), na.value = "gray80") +
  theme(panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold"), legend.background=element_blank())

#ggsave("County_types_252.jpg", units="in", width=9, height=6, dpi=300)
```
```{r}
library(sf)
require(sf)
library(viridis)
coalfields <- read_sf(dsn = "data", layer = "CoalFieldsUS")
coalfields <- subset(coalfields, PROVINCE != "Alaska")
counties <- read_sf(dsn = "data", layer = "cb_2018_us_county_20m")
counties <- subset(counties, STATEFP != "02" & STATEFP != "15" & STATEFP != "72")


shp_mine <- cluster_map[,c("fips", "type")]
shp_mine <- subset(rbind(shp_mine, c("51720", 2), c("51195", 2)), fips != "51955")


names(shp_mine)[names(shp_mine) == "fips"] <- "GEOID"
shp_mine %>% filter(!(shp_mine$GEOID %in% counties$GEOID)) %>% View()

counties_test <- merge(counties, shp_mine, by = "GEOID", all.x = TRUE)
mines_layer <- subset(counties_test, !is.na(type))
counties_test$type <- as.factor(counties_test$type)


plot_usmap(data = cluster_map, values = "type", regions = "counties", col = "gray90", size = 0.04, exclude = c("AK", "HI" )) + 
  scale_fill_viridis_d(name = "County Type", labels = c("Type 1", "Type 2", "Type 3", "No active mines 2002-2019"), na.value = "gray80") +
  theme(panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold"), legend.background=element_blank())


ggplot() + 
  geom_sf(data = coalfields, colour = NA, fill = alpha("lightsteelblue", 0.5), size = 1) +
  geom_sf(data = counties_test, colour = "grey", size = 0.04, mapping = aes(fill = type)) +
  geom_sf(fill = "transparent", color = "grey50", size = 0.5, 
          data = counties_test %>% summarise()) +
  #scale_fill_continuous(name = "Active Mines", na.value = "white") + 
  scale_fill_viridis_d(name = "County Type", labels = c("Type 1", "Type 2", "Type 3", "No active mines 2002-2019"), na.value = alpha("grey",0.2))+
  theme(panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold"), legend.background=element_blank())+
  coord_sf(datum = NA)
```


```{r}
# plot_usmap(data = cluster_mines, values = "cluster_type_trans", regions = "counties", col = "black", size = 0.07, exclude = c("AK", "HI" )) + 
#   scale_fill_manual(name = "Cluster Type", labels = c("Type 1", "Type 2", "Type 3", "No active mines in 2019"), values = c("sienna3",  "palegreen4", "darkblue"), na.value = "seashell2") +
#   theme(panel.background = element_rect(color = "black", fill = "white"))


# mine_df_19 <- subset(mine_df, year == 2019)
# mine_df_19 <- subset(mine_df_19, active_mines != 0)
# mine_df_19 <- subset(mine_df_19, state != "AK")
# 
# plot_usmap(data = mine_df_19, values = "active_mines", regions = "counties", col = "black", size = 0.05, exclude = c("AK", "HI")) + 
#   labs(title = "Geographical distribution of active coal mines in 2019") + 
#   scale_fill_continuous(name = "Active Mines", na.value = "white") + 
#   theme(panel.background = element_rect(color = "black", fill = "gray90"), legend.key.size = unit(0.3, 'cm'))
# 
# plot_usmap(data = ff_2019, values = "ff_exists", regions = "counties", col = "black", size = 0.05, exclude = c("AK", "HI")) + 
#   labs(title = "Geographical Distribution of US Employment in Fossil Fuel Production", 
#        subtitle = "Counties reporting oil, gas, and/or coal employment in 2019.") + 
#   scale_fill_manual(name = "Fossil Fuel Employment", values = "slateblue", na.value = "white") + 
#   theme(panel.background = element_rect(color = "black", fill = "gray90"), legend.position = "none")
```

# Create df with clusters for regressions
```{r}
cc_cluster <- cc[c("fips", "cluster")]
cc_cluster$type[which(cc_cluster$cluster == 1)] = 2
cc_cluster$type[which(cc_cluster$cluster == 2)] = 1
cc_cluster$type[which(cc_cluster$cluster == 3)] = 3

cc_cluster$fips[which(cc_cluster$fips == "51195" | cc_cluster$fips == "51720")] <- "51955"
cc_cluster <- cc_cluster[!duplicated(cc_cluster$fips),]

# writexl::write_xlsx(cc_cluster, here("Final Data Products/cc_clusters_251.xlsx"))
```


```{r, eval = FALSE, include = FALSE}

# Clustering on mines only: NEED TO REMOVE HAWAII FROM CLUSTER_MINES!

cluster_mines2 <- data.frame(cluster_mines, row.names = 1)
scaled_minescluster <- scale(cluster_mines2)

writexl::write_xlsx(cluster_mines, "/Users/ebbamark/OneDrive - Nexus365/Ebba Dissertation Materials/Final Data Products/cluster_mines.xlsx")

library(stargazer)

# stargazer(as.data.frame(select(cluster_mines, -1, -8)), digits = 1, 
#           covariate.labels=c("2013 Rural-Urban Code","Population Size",
#                              "Educational Attainment: Bachelor's Degree or Higher (%; aged 25-64)",
#                              "Median Earnings (USD)","Female Labour Force Participation (%; aged 25-64)",
#                              "Chmura Diversity Index"))
# 
# stargazer(as.data.frame(cluster_usa2), digits = 1, covariate.labels=c("2013 Rural-Urban Code","Population Size",
#                              "Educational Attainment: Bachelor's Degree or Higher (%; aged 25-64)",
#                              "Median Earnings (USD)","Female Labour Force Participation (%; aged 25-64)",
#                              "Chmura Diversity Index"))

# # Indicates optimal clusters 3-4
fviz_nbclust(scaled_minescluster, hcut, method = "wss")
# 
# # Indicates optimal clusters 2
fviz_nbclust(scaled_minescluster, hcut, method = "silhouette")
# 
# # Indicates optimal clusters 1
gap_stat <- clusGap(scaled_minescluster, hcut, K.max = 10)
fviz_gap_stat(gap_stat)


res_mines3 <- eclust(scaled_minescluster, k = 3,  "hclust")
res_mines3$size
aggregate(cluster_mines2, by=list(cluster=res_mines3$cluster), mean)
mines_means <- aggregate(cluster_mines, by=list(cluster=res_mines3$cluster), mean)

mines_means <- data.frame(mines_means, row.names = 1)
mines_means <- mines_means[,c("fips","RUC_2013","POPESTIMATE2019", "ed_over_25_bachelor_or_higher", "med_earnings","lfpr_20_64_female", "diversity_index")]
fviz_cluster(res_mines3)

cluster_mines <- data.frame(cluster_mines, res_mines3$cluster)

names(cluster_mines)[names(cluster_mines) == "res_mines3.cluster"] <- "type"

euclidean <- function(a, b) {sqrt(sum((a - b)^2))}

cluster_usa2 <- data.frame(cluster_usa, row.names = 1)
mines_means2 <- mines_means[,-1]

usa_types <- data.frame(matrix(ncol = 2, nrow = 3136))
for (i in (1:nrow(cluster_usa2))) {
  ft = row.names(cluster_usa2[i,])
  print(ft)
  usa_types$fips[i] = ft
  if (ft %in% cluster_mines$fips) {
    usa_types$type[i] = cluster_mines$type[which(cluster_mines$fips == ft)]
    print("in")
  } else if (!(ft %in% cluster_mines$fips)) {
    print("ok")
    # calculate euclidean distance to cluster 1
    dist1 = euclidean(cluster_usa2[i,], mines_means2[1,])
    # calculate euclidean distance to cluster 2
    dist2 = euclidean(cluster_usa2[i,], mines_means2[2,])
    # calculate euclidean distance to cluster 3
    dist3 = euclidean(cluster_usa2[i,], mines_means2[3,])
    # find min of previous assignments
    if (min(dist1, dist2, dist3) == dist1) {usa_types$type[i] = 1
    } else if (min(dist1, dist2, dist3) == dist2) {usa_types$type[i] = 2
    } else if (min(dist1, dist2, dist3) == dist3) {usa_types$type[i] = 3}
    
  } else {print("failed")}
}

#test they are the same
cluster_mines <- subset(cluster_mines, fips != "02068")

for (i in (1:nrow(cluster_mines))) {
  print(i)
  ftest = cluster_mines$fips[i]
  print(ftest)
  typetest = usa_types$type[which(usa_types$fips == ftest)]
  print(typetest)
  if (cluster_mines$type[i] !=  typetest) {print(0)}
}

aggregate(cluster_usa, by=list(cluster=res_usa$cluster), mean)

table(cluster_mines['type'])

#11 counties voted Democrat
dem = 0
#141 counties voted Republican
rep = 0
for (j in (1:nrow(cluster_mines))) {
  print(j)
  partytest = cluster_mines$fips[j]
  print(partytest)
  ptest = typ_us$type[which(usa_types$fips == ftest)]
  if (typ_us$party_16[which(typ_us$fips == partytest)] == "DEMOCRAT")
  {dem = dem + 1}
  if (typ_us$party_16[which(typ_us$fips == partytest)] == "REPUBLICAN")
  {rep = rep + 1}
  else {print("warning")}
}


```


```{r, eval = FALSE, include = FALSE}

cluster_mines$type_trans = 0
cluster_mines$type_trans[which(cluster_mines$type == 1)] = 1
cluster_mines$type_trans[which(cluster_mines$type == 2)] = 3
cluster_mines$type_trans[which(cluster_mines$type == 3)] = 2


cluster_mines$cluster_type_trans = as.factor(cluster_mines$type_trans)

plot_usmap(data = cluster_mines, values = "cluster_type_trans", regions = "counties", col = "black", size = 0.07, exclude = c("AK", "HI" )) + 
  scale_fill_manual(name = "Cluster Type", labels = c("Type 1", "Type 2", "Type 3", "No active mines in 2019"), values = c("sienna3",  "palegreen4", "darkblue"), na.value = "seashell2") +
  theme(panel.background = element_rect(color = "black", fill = "white"))


typ_og$res_kog.cluster <- as.factor(typ_og$res_kog.cluster)
plot_usmap(data = typ_og, values = "res_kog.cluster", regions = "counties", col = "black", size = 0.05) + 
  labs(title = "Geographical Distribution of Oil and Gas County Types") + 
  scale_fill_discrete(name = "Cluster Type", na.value = "white") + 
  theme(panel.background = element_rect(color = "black", fill = "white"))

plot_usmap(data = typ_og, values = "res_kog.cluster", regions = "counties", col = "black", size = 0.05) + 
  labs(title = "Geographical Distribution of Oil and Gas County Types",
       subtitle = "Each of the aforementioned oil and gas county types is outlined above and denoted below by distinct colors.") + 
  scale_fill_discrete(name = "Cluster Type", na.value = "white") + 
  theme(panel.background = element_rect(color = "black", fill = "gray90"))

plot_usmap(data = typ_us, values = "us_clustertype", regions = "counties", col = "black", size = 0.05) + 
  labs(title = "Geographical Distribution of Overall US County Types",
       subtitle = "Each of the aforementioned US county types is outlined above and denoted below by distinct colors.") + 
  scale_fill_discrete(name = "Cluster Type", na.value = "white") + 
  theme(panel.background = element_rect(color = "black", fill = "gray90"))

```

