## Food systems sustainability and economic development ##
## Empirical evidence from a multi-country analysis     ##
# Alliance of Bioversity International and CIAT
# December, 2021

## R options
g <- gc(reset = T); rm(list = ls()) # Emptying the garbage collector
.rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))   # Loading R-packages
suppressMessages(pacman::p_load(tidyverse,tgp,corrplot,randomForest,minerva,
                                ranger,DALEX,sf,spData,tmap,RColorBrewer,gtools))

# source('https://raw.githubusercontent.com/CIAT-DAPA/african_crisis_observatory/main/base__lowest_gadm.R') # Main functions

## Define working directory
# root <- '.'
root <- 'D:/cefssed_repo'

## Read computed indices
sfs_indx <- read.csv(paste0(root,'/data/sfs_final_index2share.csv'), row.names = 1)
sfs_indx <- sfs_indx %>% tidyr::drop_na()
sfs_indx$Country <- rownames(sfs_indx); rownames(sfs_indx) <- 1:nrow(sfs_indx)
sfs_indx <- sfs_indx %>%
  dplyr::select(iso3c,Country,Environment,Economic,Social,Food_nutrition,GFSSI)

# ------------------------------------------------------- #
# Figure 1
# ------------------------------------------------------- #

out <- paste0(root,'/figures/Figure1.tiff')
dir.create(dirname(out), F, T)
grDevices::tiff(filename = out, height = 7.5, width = 7.5, res = 300, units = 'in', compression = 'lzw')
par(cex = 0.7)
sfs_indx %>%
  dplyr::select('Environment','Economic','Social','Food_nutrition') %>%
  cor(method = 'pearson') %>%
  corrplot.mixed(lower         = 'number',
                 upper         = 'ellipse',
                 lower.col     = 'black',
                 tl.cex        = 1.7,
                 cl.cex        = 2,
                 number.cex    = 2,
                 number.digits = 2,
                 mar           = c(0,0,0,0))
dev.off()
rm(out)

# ------------------------------------------------------- #
# Figure 2
# ------------------------------------------------------- #

# Load worldwide shapefile with Robinson projection
world  <- sf::st_read(paste0(root,'/data/world_robinson/all_countries.shp'))
world2 <- dplyr::left_join(x = world, y = sfs_indx, by = c('ISO3' = 'iso3c'))
# Create the final map
tmap_options(output.format = 'tiff')
tsv <- tmap::tm_shape(world2) +
  tmap::tm_polygons(col     = "GFSSI",
                    n       = 20,
                    palette = rev(brewer.pal(n = 20, name = "OrRd")),
                    title   = "GFSSI") +
  tmap::tm_borders("gray20", lwd = .5) +
  tmap::tm_layout(inner.margins         = c(0, .02, .02, .02),
                  legend.title.size     = 1.8,
                  legend.title.fontface = 'bold',
                  legend.text.size      = 1.2)
# Save the results
out <- paste0(root,'/figures/Figure2.tiff')
tmap::tmap_save(tm = tsv, filename = out, width = 7.5, height = 3.75, units = 'in', dpi = 300, compression = 'lzw')
rm(tsv, world, world2, out)

# ------------------------------------------------------- #
# Figure 3 & 7
# ------------------------------------------------------- #

gdp <- read.csv(paste0(root,'/data/gdp_per_capita_2010_us_dollars.csv'))
gdp <- gdp %>% dplyr::select(iso3c, X2004:X2015)
gdp$GDP <- rowMeans(gdp[,2:ncol(gdp)], na.rm = T)
sfs_indx <- dplyr::left_join(x  = sfs_indx, y  = gdp %>% dplyr::select(iso3c, GDP), by = 'iso3c')
rm(gdp)

set.seed(123)
gg <- sfs_indx %>%
  ggplot2::ggplot(aes(x = GDP, y = GFSSI)) +
  ggplot2::geom_point() +
  ggplot2::xlab('GDP per capita 2004-2015 (constant 2010 US$)') +
  ggplot2::ylab('Global Food System Sustainability Index') +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom") +
  ggplot2::stat_smooth(method = "lm", formula = y~log(x), se = FALSE)
ggplot2::ggsave(filename = paste0(root,'/figures/Figure3.tiff'), plot = gg, device = 'tiff', width = 7.5, height = 7.5, units = 'in', dpi = 300, compression = 'lzw')
rm(gg)

fit <- lm(formula = GFSSI~log(GDP), data = sfs_indx)
summary(fit); rm(fit)

# ------------------------------------------------------- #
# Figure 4
# ------------------------------------------------------- #

sfs_indx$GDPtertiles <- sfs_indx$GDP %>% gtools::quantcut(x = ., q = 3)
levels(sfs_indx$GDPtertiles) <- c('Terc 1','Terc 2','Terc 3')

gg <- sfs_indx %>%
  dplyr::left_join(x = ., y = read.csv(paste0(root,'/data/income_groups_worldbank.csv')), by = c('iso3c' = 'Code')) %>%
  dplyr::mutate(Igroup = replace(Igroup, Igroup %in% c('Lower middle income','Upper middle income'), 'Middle income') %>% factor(levels = c('Low income','Middle income','High income'))) %>%
  dplyr::select(Igroup, GFSSI) %>%
  dplyr::group_by(Igroup) %>%
  dplyr::summarise(Mean = mean(GFSSI),
                   Sd   = sd(GFSSI)) %>%
  ggplot2::ggplot(aes(x = Igroup, y = Mean)) +
  ggplot2::geom_bar(stat = "identity", width = .3, fill = 'dodgerblue', position = position_dodge()) +
  ggplot2::geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd), width=.2,
                         position = position_dodge(.9)) +
  ggplot2::ylab('Global Food System Sustainability Index') +
  ggplot2::xlab('') +
  ggplot2::ylim(0, 0.7) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom")
ggplot2::ggsave(filename = paste0(root,'/figures/Figure4_1.tiff'), plot = gg, device = 'tiff', width = 7, height = 7, units = 'in', dpi = 300, compression = 'lzw')

gg <- sfs_indx %>%
  dplyr::select(GDPtertiles, GFSSI) %>%
  dplyr::group_by(GDPtertiles) %>%
  dplyr::summarise(Mean = mean(GFSSI),
                   Sd   = sd(GFSSI)) %>%
  ggplot2::ggplot(aes(x = GDPtertiles, y = Mean)) +
  ggplot2::geom_bar(stat = "identity", width = .3, fill = 'dodgerblue', position = position_dodge()) +
  ggplot2::geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd), width=.2,
                         position = position_dodge(.9)) +
  ggplot2::ylab('Global Food System Sustainability Index') + # Global Food System Sustainability Index, Country food system sustainability scores
  ggplot2::xlab('') +
  ggplot2::ylim(0, 0.7) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom")
ggplot2::ggsave(filename = paste0(root,'/figures/Figure4_2.tiff'), plot = gg, device = 'tiff', width = 7, height = 7, units = 'in', dpi = 300, compression = 'lzw')
rm(gg)

# ------------------------------------------------------- #
# Figure 5
# ------------------------------------------------------- #

## Raw data have some restrictions to be shared
# grDevices::tiff(filename = paste0(root,'/figures/Figure5.tiff'), height = 12, width = 12, res = 350, units = "in", compression = 'lzw')
# par(cex = 1)
# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# sfs_indx2[,c(selected2,'GFSSI')] %>%
#   cor(method = 'spearman') %>%
#   corrplot(method        = 'square',
#            col           = col(200),
#            type          = "upper",
#            number.cex    = 0.75,
#            addCoef.col   = "black",
#            number.digits = 1,
#            diag          = TRUE,
#            mar           = c(0,0,0,0))
# dev.off()

# ------------------------------------------------------- #
# Figure 6
# ------------------------------------------------------- #

ssp <- readxl::read_excel(paste0(root,'/data/ssp_gdp_projections_iamc_db.xlsx'), sheet = 1)
ssp <- ssp %>% dplyr::filter(Model == 'OECD Env-Growth' & Scenario == 'SSP2')

names(ssp)[which(names(ssp) == 'Region')] <- 'iso3c'
names(ssp)[6:24] <- paste0('GDP_prj_',seq(2010, 2100, by = 5))

# GDP units: thousands of US$ Dollars
sfs_indx2 <- sfs_indx
sfs_indx2$GDP <- sfs_indx2$GDP/1000
sfs_indx2 <- dplyr::left_join(x = sfs_indx2, y = ssp %>% dplyr::select(iso3c,GDP_prj_2010:GDP_prj_2100))

# Obtain projected change under SSP2 scenario
sfs_indx2$GDP_chg_2050 <- sfs_indx2$GDP_prj_2050 - sfs_indx2$GDP_prj_2015
rm(ssp)

# Step 1: Get predictions from parametric/semi-parametric models
# Fit parametric/semi-parametric models of: Dimension_[i] = f(GDP) + error
env_fit <- mgcv::gam(Environment ~ s(GDP), data = sfs_indx2)
eco_fit <- mgcv::gam(Economic    ~ s(GDP), data = sfs_indx2)
soc_fit <- mgcv::gam(Social      ~ s(GDP), data = sfs_indx2)
fnt_fit <- stats::lm(Food_nutrition ~ log(GDP), data = sfs_indx2)

# Calculate the increase of GDP under SSP2 scenario
gdp_chg <- sfs_indx2 %>%
  dplyr::select(GDP_chg_2050) %>%
  dplyr::mutate(GDP_chg_2050 = sfs_indx2$GDP + GDP_chg_2050)
names(gdp_chg) <- 'GDP'

# Predictions per dimension
dim_preds <- data.frame(iso3c          = sfs_indx2$iso3c,
                        Environment    = sfs_indx2$Environment + (predict(object = env_fit, gdp_chg) - env_fit$fitted.values),
                        Economic       = sfs_indx2$Economic + (predict(object = eco_fit, gdp_chg) - eco_fit$fitted.values),
                        Social         = sfs_indx2$Social + (predict(object = soc_fit, gdp_chg) - soc_fit$fitted.values),
                        Food_nutrition = sfs_indx2$Food_nutrition + (predict(object = fnt_fit, gdp_chg) - fnt_fit$fitted.values),
                        GFSSI          = sfs_indx2$GFSSI)
rm(env_fit, eco_fit, soc_fit, fnt_fit, gdp_chg)

# Step 2: Get predictions from parametric/semi-parametric models
# SFS_index = f(Dimension_[i]) + error
sfs_env_fit <- mgcv::gam(GFSSI ~ s(Environment), data = sfs_indx2)
sfs_eco_fit <- mgcv::gam(GFSSI ~ s(Economic), data = sfs_indx2)
sfs_soc_fit <- stats::lm(GFSSI ~ log(Social), data = sfs_indx2)
sfs_fnt_fit <- mgcv::gam(GFSSI ~ s(Food_nutrition), data = sfs_indx2)

dim_preds <- dim_preds %>%
  dplyr::mutate(GFSSI_env = sfs_indx2$GFSSI + (predict(object = sfs_env_fit, dim_preds %>% dplyr::select(Environment)) - sfs_env_fit$fitted.values),
                GFSSI_eco = sfs_indx2$GFSSI + (predict(object = sfs_eco_fit, dim_preds %>% dplyr::select(Economic)) - sfs_eco_fit$fitted.values),
                GFSSI_soc = sfs_indx2$GFSSI + (predict(object = sfs_soc_fit, dim_preds %>% dplyr::select(Social)) - sfs_soc_fit$fitted.values),
                GFSSI_fnt = sfs_indx2$GFSSI + (predict(object = sfs_fnt_fit, dim_preds %>% dplyr::select(Food_nutrition)) - sfs_fnt_fit$fitted.values))
rm(sfs_env_fit, sfs_eco_fit, sfs_soc_fit, sfs_fnt_fit)

# Country groups
groups <- readxl::read_excel(paste0(root,'/data/country per region.xlsx'), sheet = 1)
groups <- groups[,c(1,2,4)]
names(groups)[3] <- 'Group'

# Environment
db_env <- dplyr::left_join(x = sfs_indx2 %>% dplyr::select(iso3c, Country, GFSSI),
                           y = dim_preds %>% dplyr::select(iso3c, GFSSI_env),
                           by = 'iso3c')
names(db_env)[3:4] <- c('GFSSI_2015','GFSSI_2050')
db_env <- dplyr::left_join(x = db_env, y = groups %>% dplyr::select(iso3c, Group), by = 'iso3c')
db_env <- dplyr::left_join(x = db_env, y = sfs_indx2 %>% dplyr::select(iso3c, GDP), by = 'iso3c')

# Economic
db_eco <- dplyr::left_join(x = sfs_indx2 %>% dplyr::select(iso3c, Country, GFSSI),
                           y = dim_preds %>% dplyr::select(iso3c, GFSSI_eco),
                           by = 'iso3c')
names(db_eco)[3:4] <- c('GFSSI_2015','GFSSI_2050')
db_eco <- dplyr::left_join(x = db_eco, y = groups %>% dplyr::select(iso3c, Group), by = 'iso3c')
db_eco <- dplyr::left_join(x = db_eco, y = sfs_indx2 %>% dplyr::select(iso3c, GDP), by = 'iso3c')

# Social
db_soc <- dplyr::left_join(x = sfs_indx2 %>% dplyr::select(iso3c, Country, GFSSI),
                           y = dim_preds %>% dplyr::select(iso3c, GFSSI_soc),
                           by = 'iso3c')
names(db_soc)[3:4] <- c('GFSSI_2015','GFSSI_2050')
db_soc <- dplyr::left_join(x = db_soc, y = groups %>% dplyr::select(iso3c, Group), by = 'iso3c')
db_soc <- dplyr::left_join(x = db_soc, y = sfs_indx2 %>% dplyr::select(iso3c, GDP), by = 'iso3c')

# Food and nutrition
db_fnt <- dplyr::left_join(x = sfs_indx2 %>% dplyr::select(iso3c, Country, GFSSI),
                           y = dim_preds %>% dplyr::select(iso3c, GFSSI_fnt),
                           by = 'iso3c')
names(db_fnt)[3:4] <- c('GFSSI_2015','GFSSI_2050')
db_fnt <- dplyr::left_join(x = db_fnt, y = groups %>% dplyr::select(iso3c, Group), by = 'iso3c')
db_fnt <- dplyr::left_join(x = db_fnt, y = sfs_indx2 %>% dplyr::select(iso3c, GDP), by = 'iso3c')

# Figure6_1: Economic
gg <- db_eco %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(GFSSI_2015 = mean(GFSSI_2015, na.rm = T),
                   GFSSI_2050 = mean(GFSSI_2050, na.rm = T),
                   GDP = mean(GDP, na.rm = T)) %>%
  dplyr::mutate(cGDP = paste0(Group,' - ',round(GDP,2)) %>% factor) %>%
  dplyr::mutate(cGDP = reorder(cGDP, GDP, median)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_segment(aes(x = cGDP, xend = cGDP, y = GFSSI_2015, yend = GFSSI_2050), color="grey") +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2015), color = rgb(0.2,0.7,0.1,0.5), size = 5) +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2050), color = rgb(0.7,0.2,0.1,0.5), size = 5) +
  ggplot2::coord_flip()+
  ggplot2::theme_bw() +
  ggplot2::xlab("GDP") +
  ggplot2::ylab("SFS projected change") +
  ggplot2::ylim(0, 1) +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom")
ggplot2::ggsave(filename = paste0(root,'/figures/Figure6_1.tiff'), plot = gg, device = 'tiff', width = 10, height = 6, units = "in", dpi = 300, compression = 'lzw')

# Figure6_2: Environment
gg <- db_env %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(GFSSI_2015 = mean(GFSSI_2015, na.rm = T),
                   GFSSI_2050 = mean(GFSSI_2050, na.rm = T),
                   GDP = mean(GDP, na.rm = T)) %>%
  dplyr::mutate(cGDP = paste0(Group,' - ',round(GDP,2)) %>% factor) %>%
  dplyr::mutate(cGDP = reorder(cGDP, GDP, median)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_segment(aes(x = cGDP, xend = cGDP, y = GFSSI_2015, yend = GFSSI_2050), color="grey") +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2015), color = rgb(0.2,0.7,0.1,0.5), size = 5) +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2050), color = rgb(0.7,0.2,0.1,0.5), size = 5) +
  ggplot2::coord_flip()+
  ggplot2::theme_bw() +
  ggplot2::xlab("GDP") +
  ggplot2::ylab("SFS projected change") +
  ggplot2::ylim(0, 1) +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom")
ggplot2::ggsave(filename = paste0(root,'/figures/Figure6_2.tiff'), plot = gg, device = 'tiff', width = 10, height = 6, units = "in", dpi = 300, compression = 'lzw')

# Figure6_3: Food and nutrition
gg <- db_fnt %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(GFSSI_2015 = mean(GFSSI_2015, na.rm = T),
                   GFSSI_2050 = mean(GFSSI_2050, na.rm = T),
                   GDP = mean(GDP, na.rm = T)) %>%
  dplyr::mutate(cGDP = paste0(Group,' - ',round(GDP,2)) %>% factor) %>%
  dplyr::mutate(cGDP = reorder(cGDP, GDP, median)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_segment(aes(x = cGDP, xend = cGDP, y = GFSSI_2015, yend = GFSSI_2050), color="grey") +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2015), color = rgb(0.2,0.7,0.1,0.5), size = 5) +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2050), color = rgb(0.7,0.2,0.1,0.5), size = 5) +
  ggplot2::coord_flip()+
  ggplot2::theme_bw() +
  ggplot2::xlab("GDP") +
  ggplot2::ylab("SFS projected change") +
  ggplot2::ylim(0, 1) +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom")
ggplot2::ggsave(filename = paste0(root,'/figures/Figure6_3.tiff'), plot = gg, device = 'tiff', width = 10, height = 6, units = "in", dpi = 300, compression = 'lzw')

# Figure6_4: Social
gg <- db_soc %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(GFSSI_2015 = mean(GFSSI_2015, na.rm = T),
                   GFSSI_2050 = mean(GFSSI_2050, na.rm = T),
                   GDP = mean(GDP, na.rm = T)) %>%
  dplyr::mutate(cGDP = paste0(Group,' - ',round(GDP,2)) %>% factor) %>%
  dplyr::mutate(cGDP = reorder(cGDP, GDP, median)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_segment(aes(x = cGDP, xend = cGDP, y = GFSSI_2015, yend = GFSSI_2050), color="grey") +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2015), color = rgb(0.2,0.7,0.1,0.5), size = 5) +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2050), color = rgb(0.7,0.2,0.1,0.5), size = 5) +
  ggplot2::coord_flip()+
  ggplot2::theme_bw() +
  ggplot2::xlab("GDP") +
  ggplot2::ylab("SFS projected change") +
  ggplot2::ylim(0, 1) +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom")
ggplot2::ggsave(filename = paste0(root,'/figures/Figure6_4.tiff'), plot = gg, device = 'tiff', width = 10, height = 6, units = "in", dpi = 300, compression = 'lzw')

# ------------------------------------------------------- #
# Figure S1
# ------------------------------------------------------- #

# (1,1)
gg <- sfs_indx %>%
  ggplot2::ggplot(aes(x = GDP, y = Social)) +
  ggplot2::geom_point() +
  ggplot2::xlab('GDP per capita 2004-2015 (constant 2010 US$)') +
  ggplot2::ylab('Social scores') +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom") +
  ggplot2::stat_smooth(method = "lm", formula = y~log(x), se = TRUE) +
  ggplot2::scale_x_continuous(breaks = seq(0,1e5,25e3))
ggplot2::ggsave(paste0(root,'/figures/FigureS1_1.tiff'), plot = gg, device = 'tiff', width = 7, height = 7, units = 'in', dpi = 300, compression = 'lzw')

fit <- lm(formula = Social~log(GDP), data = sfs_indx %>% dplyr::filter(iso3c != 'NOR'))
summary(fit)

# (1,2)
gg <- sfs_indx %>%
  ggplot2::ggplot(aes(x = GDP, y = Food_nutrition)) +
  ggplot2::geom_point() +
  ggplot2::xlab('GDP per capita 2004-2015 (constant 2010 US$)') +
  ggplot2::ylab('Food & nutrition scores') +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom") +
  ggplot2::stat_smooth(method = "lm", formula = y~log(x), se = TRUE) +
  ggplot2::scale_x_continuous(breaks = seq(0,1e5,25e3))
ggplot2::ggsave(paste0(root,'/figures/FigureS1_2.tiff'), plot = gg, device = 'tiff', width = 7, height = 7, units = 'in', dpi = 300, compression = 'lzw')

fit <- lm(formula = Food_nutrition~log(GDP), data = sfs_indx)
summary(fit)

# (2,1)
gg <- sfs_indx %>%
  ggplot2::ggplot(aes(x = GDP, y = Environment)) +
  ggplot2::geom_point() +
  ggplot2::xlab('GDP per capita 2004-2015 (constant 2010 US$)') +
  ggplot2::ylab('Environmental scores') +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom") +
  ggplot2::stat_smooth(method = "lm", formula = y~log(x), se = TRUE) +
  ggplot2::scale_x_continuous(breaks = seq(0,1e5,25e3))
ggplot2::ggsave(paste0(root,'/figures/FigureS1_3.tiff'), plot = gg, device = 'tiff', width = 7, height = 7, units = 'in', dpi = 300, compression = 'lzw')

fit <- lm(formula = Environment~log(GDP), data = sfs_indx)
summary(fit)

# (2,2)
gg <- sfs_indx %>%
  ggplot2::ggplot(aes(x = GDP, y = Economic)) +
  ggplot2::geom_point() +
  ggplot2::xlab('GDP per capita 2004-2015 (constant 2010 US$)') +
  ggplot2::ylab('Economic scores') +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text       = element_text(size = 17),
                 axis.title      = element_text(size = 20),
                 legend.text     = element_text(size = 17),
                 legend.title    = element_blank(),
                 plot.title      = element_text(size = 25),
                 plot.subtitle   = element_text(size = 17),
                 strip.text.x    = element_text(size = 17),
                 plot.caption    = element_text(size = 15, hjust = 0),
                 legend.position = "bottom") +
  ggplot2::stat_smooth(method = "lm", formula = y~poly(x,2), se = TRUE) +
  ggplot2::scale_x_continuous(breaks = seq(0,1e5,25e3))
ggplot2::ggsave(paste0(root,'/figures/FigureS1_4.tiff'), plot = gg, device = 'tiff', width = 7, height = 7, units = 'in', dpi = 300, compression = 'lzw')

fit <- lm(formula = Economic~log(GDP), data = sfs_indx)
summary(fit)
rm(fit)

# ------------------------------------------------------- #
# Figure S2
# ------------------------------------------------------- #

# Figure S2_1: Social
gg <- db_soc %>%
  dplyr::mutate(cGDP = paste0(iso3c,' - ',round(GDP,2)) %>% factor) %>%
  dplyr::mutate(cGDP = reorder(cGDP, -GDP, median)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_segment(aes(x = cGDP, xend = cGDP, y = GFSSI_2015, yend = GFSSI_2050), color="grey") +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2015), color = rgb(0.2,0.7,0.1,0.5), size = 3) +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2050), color = rgb(0.7,0.2,0.1,0.5), size = 3) +
  ggplot2::coord_flip()+
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::xlab("GDP") +
  ggplot2::ylab("SFS projected change") +
  ggplot2::ylim(0, 1)
ggplot2::ggsave(filename = paste0(root,'/figures/FigureS2_1.tiff'), plot = gg, device = 'tiff', width = 6, height = 14, units = "in", dpi = 300, compression = 'lzw')

# Figure S2_2: Economic
gg <- db_eco %>%
  dplyr::mutate(cGDP = paste0(iso3c,' - ',round(GDP,2)) %>% factor) %>%
  dplyr::mutate(cGDP = reorder(cGDP, -GDP, median)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_segment(aes(x = cGDP, xend = cGDP, y = GFSSI_2015, yend = GFSSI_2050), color="grey") +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2015), color = rgb(0.2,0.7,0.1,0.5), size = 3) +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2050), color = rgb(0.7,0.2,0.1,0.5), size = 3) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::xlab("GDP") +
  ggplot2::ylab("SFS projected change") +
  ggplot2::ylim(0, 1)
ggplot2::ggsave(filename = paste0(root,'/figures/FigureS2_2.tiff'), plot = gg, device = 'tiff', width = 6, height = 14, units = "in", dpi = 300, compression = 'lzw')

# Figure S2_3: Environment
gg <- db_env %>%
  dplyr::mutate(cGDP = paste0(iso3c,' - ',round(GDP,2)) %>% factor) %>%
  dplyr::mutate(cGDP = reorder(cGDP, -GDP, median)) %>%
  ggplot2::ggplot(data = .) +
  ggplot2::geom_segment(aes(x = cGDP, xend = cGDP, y = GFSSI_2015, yend = GFSSI_2050), color="grey") +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2015), color = rgb(0.2,0.7,0.1,0.5), size = 3) +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2050), color = rgb(0.7,0.2,0.1,0.5), size = 3) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::xlab("GDP") +
  ggplot2::ylab("SFS projected change") +
  ggplot2::ylim(0, 1)
ggplot2::ggsave(filename = paste0(root,'/figures/FigureS2_3.tiff'), plot = gg, device = 'tiff', width = 6, height = 14, units = "in", dpi = 300, compression = 'lzw')

# Figure S2_4: Food and nutrition
gg <- db_fnt %>%
  dplyr::mutate(cGDP = paste0(iso3c,' - ',round(GDP,2)) %>% factor) %>%
  dplyr::mutate(cGDP = reorder(cGDP, -GDP, median)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_segment(aes(x = cGDP, xend = cGDP, y = GFSSI_2015, yend = GFSSI_2050), color="grey") +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2015), color = rgb(0.2,0.7,0.1,0.5), size = 3) +
  ggplot2::geom_point(aes(x = cGDP, y = GFSSI_2050), color = rgb(0.7,0.2,0.1,0.5), size = 3) +
  ggplot2::coord_flip()+
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::xlab("GDP") +
  ggplot2::ylab("SFS projected change") +
  ggplot2::ylim(0, 1)
ggplot2::ggsave(filename = paste0(root,'/figures/FigureS2_4.tiff'), plot = gg, device = 'tiff', width = 6, height = 14, units = "in", dpi = 300, compression = 'lzw')
rm(gg, db_env, db_eco, db_soc, db_fnt, dim_preds, groups)
