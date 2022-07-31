# Preparation  ----
## Download and install the pacman
install.packages("pacman", dependencies = TRUE)
library(pacman)

## Run this following line to download/load all packages ----
pacman::p_load(pacman, dplyr, ggplot2, ggthemes, rio, stringr, tidyr, 
               broom, readr, dendextend, cluster, corrplot, viridisLite,
               dotwhisker, factoextra, CGPfunctions, gmodels, ggpubr)

## Download the file ----
jewelry_raw <- read.csv("/Users/13000543/Documents/GitHub/jewelry/jewelry.csv") %>%
        mutate(country = factor(country, levels = c("Thailand","Taiwan")),
               generation = factor(generation, levels = c("Y","Z"))) %>%
        mutate(monthly_income = factor(monthly_income, levels = c("Below 15,000 THB","15,001 to 30,000 THB","30,001 to 60,000  THB","Above 60,001 THB")),
               education = factor(education, levels = c("High school","Bachelor's degree","Master's degree","PH.D. or higher")))

# 1. Compute descriptive statistics ----
table(jewelry_raw$country)
table(jewelry_raw$country) %>% prop.table()
table(jewelry_raw$generation)
table(jewelry_raw$generation) %>% prop.table()
table(jewelry_raw$gender)
table(jewelry_raw$gender) %>% prop.table()
table(jewelry_raw$monthly_income)
table(jewelry_raw$monthly_income) %>% prop.table()
table(jewelry_raw$education)
table(jewelry_raw$education) %>% prop.table()
table(jewelry_raw$marital_status)
table(jewelry_raw$marital_status) %>% prop.table()
table(jewelry_raw$employment_status)
table(jewelry_raw$employment_status) %>% prop.table()

# 2. Visualise and compare importance from Conjoint Analysis ----
## Transform the data
jewelry_imp_long <- jewelry_raw %>%
        pivot_longer(cols = 10:15, names_to = "importance", values_to = "percent") %>%
        mutate(importance = factor(importance, levels = colnames(jewelry_raw[,10:15])))

## 2.1 Visualise importance by country ----
conjoint_country_plot <- ggplot(jewelry_imp_long, aes(x = importance, y  = 100*percent, color = country, group = country)) +
        stat_summary(geom = "line", fun = "mean") +
        stat_summary(aes(label=round(..y..,2)), fun = mean, geom="text", size=3, vjust = -1, color = "black") +
        #scale_color_manual(values = c("Thailand" = "blue", "Taiwan" = "red")) +
        scale_color_viridis_d(option = "mako", end = 0.8) +
        theme_light() +
        ylab("Importance level (%)") +
        theme(axis.title.y = element_blank(),
              legend.position = "right") +
        coord_flip(ylim = c(12,20))

## 2.2 Test the differences of importance between countries ----
t.test(formula = sustainable_process ~ country, data = jewelry_raw)
t.test(formula = supply_transparency ~ country, data = jewelry_raw)
t.test(formula = sustainable_materials ~ country, data = jewelry_raw)
t.test(formula = heritage ~ country, data = jewelry_raw)
t.test(formula = quality ~ country, data = jewelry_raw)
t.test(formula = uniqueness ~ country, data = jewelry_raw)

## 2.3 Visualise importance by generation ----
conjoint_gen_plot <- ggplot(jewelry_imp_long, aes(x = importance, y  = 100*percent, color = generation, group = generation)) +
        stat_summary(geom = "line", fun = "mean") +
        stat_summary(aes(label=round(..y..,2)), fun = mean, geom="text", size=3, vjust = -1, color = "black") +
        #scale_color_manual(values = c("Thailand" = "blue", "Taiwan" = "red")) +
        scale_color_viridis_d(option = "plasma", end = 0.8) +
        theme_light() +
        ylab("Importance level (%)") +
        theme(axis.title.y = element_blank(),
              legend.position = "right") +
        coord_flip(ylim = c(12,20))

## 2.4 Test the differences of importance between generation ----
t.test(formula = sustainable_process ~ generation, data = jewelry_raw)
t.test(formula = supply_transparency ~ generation, data = jewelry_raw)
t.test(formula = sustainable_materials ~ generation, data = jewelry_raw)
t.test(formula = heritage ~ generation, data = jewelry_raw)
t.test(formula = quality ~ generation, data = jewelry_raw)
t.test(formula = uniqueness ~ generation, data = jewelry_raw)

## 2.5 Plot
ggarrange(conjoint_country_plot, conjoint_gen_plot, ncol = 1, nrow=2, common.legend = FALSE, legend="right")

## 2.6 Visualise importance using a boxplot and show means ----
ggplot(jewelry_imp_long, aes(x = importance, y = 100*percent)) +
        geom_boxplot() +
        stat_summary(geom = "point", fun = "mean", color = "tomato", group = 1) +
        stat_summary(geom = "line", fun = "mean", color = "salmon", group = 1) +
        stat_summary(aes(label=round(..y..,2)), fun = mean, geom="text", size=3, color = "red",
                     hjust = -0.25) +
        theme(axis.title.y = element_blank()) +
        theme_light() +
        ylab("Importance level (%)") +
        coord_flip()

## 2.7 Test the difference of importance between attribute
t.test(jewelry_raw$sustainable_process, jewelry_raw$supply_transparency, paired = TRUE)
t.test(jewelry_raw$sustainable_process, jewelry_raw$sustainable_materials, paired = TRUE)
t.test(jewelry_raw$sustainable_process, jewelry_raw$heritage, paired = TRUE)
t.test(jewelry_raw$sustainable_process, jewelry_raw$quality, paired = TRUE)
t.test(jewelry_raw$sustainable_process, jewelry_raw$uniqueness, paired = TRUE)

# 3. k-means cluster analysis ----
## Prepare the data
jewelry_for_kmeans <- jewelry_raw %>% dplyr::select(10:15)

## 3.1 Determine the most suitable number of clusters ----
## Use the elbow method
fviz_nbclust(jewelry_for_kmeans, kmeans, method = "wss", k.max = 20)
## Use the silhouette method
fviz_nbclust(jewelry_for_kmeans, kmeans, method = "silhouette", k.max = 20)

## 3.2 K-Means cluster analysis ----
## Determine the most appropriate number of k
k <- 6
## Because cluster analysis is random, set seed to ensure reproducibility
set.seed(2022)
## Perform a cluster analysis with the k number of clusters
## iter.max is the number of iterations in each round
## nstart is the number of times we do cluster analysis to select the best one
km_output <- kmeans(jewelry_for_kmeans, centers = k, iter.max = 1000, nstart = 1000)

## Observe the cluster centers #
km_output$centers
## Observe the number of observations in each cluster
km_output$size

## 3.3 Visualise the cluster centres ----
## Create a dataframe that contains cluster centres
km_centers <- as.data.frame(km_output$centers)
## Adjust the data structure to be suitable for visualisation
km_centers_cluster <- km_centers %>%
        mutate(cluster = as.factor(1:k))
## Transform to a long format and reorder the constructs
km_centers_long <- km_centers_cluster %>%
        pivot_longer(cols = 1:length(km_centers), values_to = "score", names_to = "attribute") %>%
        mutate(attribute = factor(attribute, levels = colnames(km_centers))) %>%
        mutate(cluster = factor(cluster, levels = c(2,3,6,4,1,5), labels = c("customizers","perfectionists","legacy lovers","zero-waste warriors","transparency trackers","green operators")))

## Visualise the cluster centres using ggplot()
ggplot(km_centers_long, aes(x = cluster, y = 100*score, fill = attribute)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = round(100*score)), 
                  position = position_dodge(width = .9), size = 3, vjust = -0.4) +
        ylab("Importance level (%)") +
        theme_light() +
        xlab("") +
        scale_fill_viridis_d(option = "turbo")

## Visualise the cluster analysis by two dimensions
fviz_cluster(km_output, data = jewelry_for_kmeans, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_light())

## 3.4 Bind cluster membership to the original dataframe ----
km_output$cluster # Check the assigned cluster of each respondent
jewelry_new <- jewelry_raw %>%
        bind_cols(segment = as.factor(km_output$cluster)) %>% # Bind the new column "cluster" to the original dataset
        mutate(segment = factor(segment, levels = 1:k)) %>%
        mutate(segment = factor(segment, levels = c(2,3,6,4,1,5), labels = c("customizers","perfectionists","legacy lovers","zero-waste warriors","transparency trackers","green operators")))


## 3.5 Visualise the number of respondents in each cluster ----
ggplot(jewelry_new, aes(x = segment)) + geom_bar(width = 0.75) +
        geom_text(stat='count', aes(label=..count..), vjust = 1.5, color = "white", size = 4) +
        theme_light() 
## By country
ggplot(jewelry_new, aes(x = segment, fill = country)) + 
        geom_bar(position = "dodge", width = 0.75) +
        geom_text(position = position_dodge(width = .8), stat='count', aes(label=..count..), hjust = 1.2, color = "black", size = 3) +
        theme_light() +
        theme(legend.title = element_blank()) +
        scale_fill_viridis_d(option = "mako", begin = 0.75) +
        coord_flip()
        
# 4. Cross-tab to identify the characteristics of each segment ----
CrossTable(jewelry_new$segment, jewelry_new$country, expected = TRUE)
PlotXTabs2(jewelry_new, segment, country)
PlotXTabs2(jewelry_new, segment, generation)
PlotXTabs2(jewelry_new, segment, gender)
PlotXTabs2(jewelry_new, segment, age)
PlotXTabs2(jewelry_new, segment, monthly_income)
PlotXTabs2(jewelry_new, segment, education)
PlotXTabs2(jewelry_new, segment, employment_status)
PlotXTabs2(jewelry_new, segment, marital_status)

