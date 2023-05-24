##############################################################
##############################################################
# GBR benthic community dynamics indicator                #### 
# Summary function                                        ####
# -  Sun Kim, Tim Staples, Kerryn Crossman, John Pandolfi ####
##############################################################
##############################################################
### Basic setup ####
###...Working directory ####
setwd('/Users/uqskim14/Analyses/AIMS community index/')

###...Packages ####
list.of.packages <- c('sqldf','tidyr','vegan','data.table',
                      'ggplot2','patchwork','ggrepel','ggsci',
                      'Rlof','mgcv','gridExtra', 'gganimate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=T)

# Load packages
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages, new.packages)

##############################################################
##############################################################
### 1) Data injection - AIMS LTMP data (new taxonomy) ####
load('data for UQ/comp2021.transect.RData')

comp2021$P_CODE[comp2021$P_CODE=='RMRAP'] <- 
  gsub('RMRAP', 'RM', comp2021$P_CODE[comp2021$P_CODE=='RMRAP'])

INreefs <- levels(as.factor(comp2021[comp2021$P_CODE=='IN',]$REEF)) # record reefs with IN P_CODE

comp2021[comp2021$P_CODE=='IN',]$REEF <- paste0(comp2021[comp2021$P_CODE=='IN',]$REEF,
                                                '_',
                                                comp2021[comp2021$P_CODE=='IN',]$DEPTH,
                                                'm') # split IN sites into 2m and 5m assemblages

# Reef-level aggregation of cover data
coral.comms <- aggregate(cover ~ P_CODE + REEF + COMP_2021 + 
                           VISIT_NO + REPORT_YEAR, comp2021, mean)

coral.comms$StartYR <- ave(coral.comms$REPORT_YEAR,
                           coral.comms$REEF,
                           FUN = min)
coral.comms$EndYR <- ave(coral.comms$REPORT_YEAR,
                         coral.comms$REEF,
                         FUN = max)
coral.comms$SurveyDuration <- coral.comms$EndYR - coral.comms$StartYR
coral.comms <- coral.comms[!coral.comms$COMP_2021 %in% c('AB', 'CA', 'F_MIL', 'MA_BROWN',
                                                         'MA_GREEN', 'MA_RED', 'MA_OTH', 
                                                         'IN','OT', 'SG', 'SP', 'TA', 'ZOA'),] # remove taxa not considered in a coral community

coral.comms <- tidyr::spread(coral.comms, COMP_2021, cover)
coral.comms$SC <- coral.comms$SC_GORG_LIKE + coral.comms$SC_OTH + coral.comms$SC_OTH_E # merge soft coral taxa into a single level
coral.comms <- coral.comms[,-c(58:60)] # remove previous soft coral columns
coral.comms[,c(8:58)] <- replace(coral.comms[,c(8:58)], is.na(coral.comms[,c(8:58)]), 0)
coral.comms[,c(8:58)] <- apply(coral.comms[,c(8:58)], 2, sqrt) # sqrt transformation of cover data
coral.comms$REEF <- as.factor(coral.comms$REEF)
coral.comms <- cbind(coral.comms[,c(1:7)], prop.table(as.matrix(coral.comms[,c(8:58)]), margin=1)) # Relative abundance

### Coral cover summary ####
coral.cover <- aggregate(cover ~ REEF + GROUP_CODE + COMP_2021 + 
                           VISIT_NO + REPORT_YEAR, comp2021, mean)
coral.cover <- coral.cover[coral.cover$GROUP_CODE=='HC',] # limit data to hard corals
coral.cover <- aggregate(cover ~ REEF + REPORT_YEAR, coral.cover, sum)
coral.cover$adj.cover <- coral.cover$cover / 100 # adjusted cover data for plot later

# Add coral cover information to community data
coral.comms <- sqldf::sqldf("SELECT * from 'coral.comms' LEFT JOIN
                            'coral.cover' USING (REEF, REPORT_YEAR)")
# coral.comms[,c(8:58)] <- replace(coral.comms[,c(8:58)], is.na(coral.comms[,c(8:58)]), 0.00001) # cover merger has added years with no coral cover (NA). Need to replace NA with 0.
coral.comms <- coral.comms[,c(1:7, 59, 60, 8:58)]

### Test reef ####
load('data for UQ/regions.RData')

### need to add 2m-5m for inshore reefs
test.reefs <- as.character(droplevels(unique(regions[regions$NRM == 'Mackay Whitsunday',]$REEF)))
test.reefs.IN <- intersect(test.reefs, INreefs)
test.reefs <- setdiff(test.reefs, test.reefs.IN)
test.reefs.IN <- c(paste0(test.reefs.IN, '_2m'),
                   paste0(test.reefs.IN, '_5m'))
test.reefs <- c(test.reefs, test.reefs.IN)

# test.reefs <- c('Barnards','Horseshoe Reef','John Brewer Reef',
#                 'One Tree Island','Opal Reef', 'Rebe Reef',
#                 'Snapper South', 'Thetford Reef','Yonge Reef',
#                 'Reef 19-138')
# test.reefs <- c('Barnards_2m','Barnards_5m',
#                 'Horseshoe Reef','John Brewer Reef',
#                 'One Tree Island','Opal Reef', 'Rebe Reef',
#                 'Snapper South_2m','Snapper South_5m', 
#                 'Thetford Reef','Yonge Reef',
#                 'Reef 19-138')
test.reefs <- droplevels(coral.comms[coral.comms$REEF %in% test.reefs, ])

coral.comms <- split(coral.comms, coral.comms$REEF)

for (i in 1:length(coral.comms)) {
  coral.comms[[i]] <- coral.comms[[i]][order(coral.comms[[i]]$REPORT_YEAR),]
  row.names(coral.comms[[i]]) <- coral.comms[[i]]$REPORT_YEAR
}

test.reefs <- split(test.reefs, test.reefs$REEF)

for (i in 1:length(test.reefs)) {
  test.reefs[[i]] <- test.reefs[[i]][order(test.reefs[[i]]$REPORT_YEAR),]
  row.names(test.reefs[[i]]) <- test.reefs[[i]]$REPORT_YEAR
}

test.reef.nmds <- vector('list', length=length(test.reefs))

for (i in 1:length(test.reef.nmds)) {
  test.reef.nmds[[i]] <- vegan::metaMDS(test.reefs[[i]][,c(10:60)], k=2, weakties=F) # John Brewer in 2004 & 2005 | Rebe in 2013 have 0% HC/SC cover
  test.reef.nmds[[i]] <- as.data.frame(vegan::scores(test.reef.nmds[[i]], 'sites'))
  test.reef.nmds[[i]]$Year <- test.reefs[[i]]$REPORT_YEAR
  test.reef.nmds[[i]]$Reef <- unique(test.reefs[[i]]$REEF)
}

test.reef.nmds <- data.table::rbindlist(test.reef.nmds)

### 2) Metrics computation ####
### Local outlier factor (LOF) metric ####
source('Codes/Functions/gbrfLOF241122.R')

# Low k with 15 years of reference period
GBR.LOF.lowK.15yr <- vector('list', length=length(test.reefs))

for (i in 1:length(GBR.LOF.lowK.15yr)) {
  GBR.LOF.lowK.15yr[[i]] <- testLOF.Ref(commData=test.reefs[[i]][,c(10:60)],
                                        k=3, timecutoff=c(1995, as.numeric(last(row.names(test.reefs[[i]][,c(10:60)])))-3), 
                                        method='bray')
}

GBR.LOF.lowK.15yr.df <- mapply(GBR.LOF.lowK.15yr, FUN=minLOF, MoreArgs=list(sigmoid.a=3)) # adjust sigmoid.a value to change the sensitivity of LOF value threshold.

GBR.LOF.lowK.15yr.df <- LOF.summary(LOF.data=GBR.LOF.lowK.15yr.df,
                                    id.data=test.reefs,
                                    site.var='REEF',
                                    year.var='REPORT_YEAR')
GBR.LOF.lowK.15yr.df <- data.table::rbindlist(GBR.LOF.lowK.15yr.df)
GBR.LOF.lowK.15yr.df$k <- 3

GBR.LOF.lowK.15yr.table <- LOF.table.summary(LOF.data=GBR.LOF.lowK.15yr,
                                             id.data=test.reefs,
                                             site.var='REEF',
                                             year.var='REPORT_YEAR')
GBR.LOF.lowK.15yr.table$k <- 3

# High k with 15 years of reference period
GBR.LOF.highK.15yr <- vector('list', length=length(test.reefs))

for (i in 1:length(GBR.LOF.highK.15yr)) {
  GBR.LOF.highK.15yr[[i]] <- testLOF.Ref(commData=test.reefs[[i]][,c(10:60)],
                                     k=6, timecutoff=c(1995, 2016), method='bray')
}

GBR.LOF.highK.15yr.df <- mapply(GBR.LOF.highK.15yr, FUN=minLOF, MoreArgs=list(sigmoid.a=3)) # adjust sigmoid.a value to change the sensitivity of LOF value threshold.

GBR.LOF.highK.15yr.df <- LOF.summary(LOF.data=GBR.LOF.highK.15yr.df,
                                     id.data=test.reefs,
                                     site.var='REEF',
                                     year.var='REPORT_YEAR')
GBR.LOF.highK.15yr.df <- data.table::rbindlist(GBR.LOF.highK.15yr.df)
GBR.LOF.highK.15yr.df$k <- 6

GBR.LOF.highK.15yr.table <- LOF.table.summary(LOF.data=GBR.LOF.highK.15yr,
                                              id.data=test.reefs,
                                              site.var='REEF',
                                              year.var='REPORT_YEAR')
GBR.LOF.highK.15yr.table$k <- 6

# Combine data
GBR.LOF <- rbind(GBR.LOF.lowK.15yr.df, GBR.LOF.highK.15yr.df)
GBR.LOF <- sqldf::sqldf("SELECT * FROM 'GBR.LOF' LEFT JOIN
                        'test.reef.nmds' USING (Reef, Year)")
GBR.LOF$threshold_detection <- 'placeholder'
GBR.LOF$threshold_detection <- ifelse(GBR.LOF$LOF > 0.5, 'No', 'Yes')
GBR.LOF <- GBR.LOF[order(GBR.LOF$Year),]

GBR.LOF.table <- rbind(GBR.LOF.lowK.15yr.table, GBR.LOF.highK.15yr.table)
GBR.LOF.table$Year <- as.integer(GBR.LOF.table$Year)

rm(GBR.LOF.lowK.15yr.df, GBR.LOF.highK.15yr.df, GBR.LOF.lowK.15yr,
   GBR.LOF.lowK.15yr.table, GBR.LOF.highK.15yr.table, GBR.LOF.highK.15yr)

### Dissimilarity vs. original community composition
distance.list <- vector('list', length=length(test.reefs))

for (i in 1:length(distance.list)) {
  distance.list[[i]] <- data.frame(Reef=unique(test.reefs[[i]]$REEF),
                                   Year=test.reefs[[i]]$REPORT_YEAR,
                                   Dissimilarity=as.matrix(vegdist(test.reefs[[i]][,c(10:60)]))[,1])
}

distance.list <- data.table::rbindlist(distance.list)

################################################################################
################################################################################
### 3) Plots ####
source('Codes/Functions/plot_functions.R')

Comb.plot <- function(df, reef.name, pool.k) {
  Dissimilarity.outplot <- ggplot() +
    geom_line(data=coral.cover[coral.cover$REEF==reef.name,],
              aes(x=REPORT_YEAR, y=adj.cover), color='black', size=1) +
    geom_line(data=GBR.LOF[GBR.LOF$Reef==reef.name &
                             GBR.LOF$k==3,],
              aes(x=Year, y=LOF), color='coral', size=1) +
    geom_line(data=GBR.LOF[GBR.LOF$Reef==reef.name &
                             GBR.LOF$k==6,],
              aes(x=Year, y=LOF), color='dodgerblue3', size=1) +
    geom_hline(yintercept=0.5, linetype=2) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(name='Coral community indicator index',
                       sec.axis=sec_axis(~.*100, name='Coral cover')) +
    theme_Publication() +
    theme(panel.border=element_rect(color='black',fill=NA, size=1),
          # axis.title.x=element_blank(),
          # axis.title.y=element_blank(),
          plot.title=element_text(hjust=0)) +
    xlab('Year') +
    ylab('Coral community indicator index')
  
  NMDS.outplot <- ggplot() +
    geom_segment(aes(x=unique(df[df$Reef==reef.name,
                                 c('Year','NMDS1','NMDS2')])[1L,'NMDS1'], 
                     xend=((unique(df[df$Reef==reef.name,
                                      c('Year','NMDS1','NMDS2')])[1L,'NMDS1']) +
                             (unique(df[df$Reef==reef.name,
                                        c('Year','NMDS1','NMDS2')])[2L,'NMDS1']))/2,
                     y=unique(df[df$Reef==reef.name,
                                 c('Year','NMDS1','NMDS2')])[1L,'NMDS2'],
                     yend=((unique(df[df$Reef==reef.name,
                                      c('Year','NMDS1','NMDS2')])[1L,'NMDS2']) +
                             (unique(df[df$Reef==reef.name,
                                        c('Year','NMDS1','NMDS2')])[2L,'NMDS2']))/2),
                 arrow=arrow(type='closed', length=unit(5,'mm'))) +
    geom_path(data=unique(df[df$Reef==reef.name,
                             c('Year','NMDS1','NMDS2')]),
              aes(x=NMDS1, y=NMDS2)) +
    geom_point(data=unique(df[df$Reef==reef.name,
                              c('Year','NMDS1','NMDS2')]),
               aes(x=NMDS1, y=NMDS2)) +
    geom_point(data=df[df$Reef==reef.name &
                         df$k==3 &
                         df$threshold_detection=='Yes',],
               aes(x=NMDS1, y=NMDS2), color='coral', size=5) +
    geom_point(data=df[df$Reef==reef.name &
                         df$k==6 &
                         df$threshold_detection=='Yes',],
               aes(x=NMDS1, y=NMDS2), color='dodgerblue3', size=3) +
    geom_text_repel(data=unique(df[df$Reef==reef.name,
                                   c('Year','NMDS1','NMDS2')]),
                    aes(x=NMDS1, y=NMDS2, label=Year)) +
    theme_Publication() +
    theme(panel.border=element_rect(color='black',fill=NA, size=1),
          # axis.title.x=element_blank(),
          # axis.title.y=element_blank(),
          plot.title=element_text(hjust=0)) +
    xlab('NMDS1') +
    ylab('NMDS2')
  
  outplot <- (Dissimilarity.outplot | NMDS.outplot)
  
  outplot <- outplot +
    plot_annotation(title=reef.name) &
    theme(text=element_text(size=15, face='bold'))
  
  plot(outplot)
  
  ggsave(filename=paste0('/Users/uqskim14/Analyses/AIMS community index/Figure/Mackay_Whitsundays/',
                         reef.name, '.png'),
         outplot, height=150, width=300,
         units='mm', device='png', dpi=300)
  
  return(outplot)
}

### Reef-specific composition trajectory and metrics' detectability visualisation ####
for (i in 1:nlevels(as.factor(GBR.LOF$Reef))) {
  Comb.plot(df=GBR.LOF, reef.name=levels(as.factor(GBR.LOF$Reef))[[i]])
}

### Reef-specific taxonomic changes in relative abundance compared to k neighbours #### 
# when k=3
k3.change <- ggplot() +
  geom_tile(data=GBR.LOF.table[order(GBR.LOF.table$Year) &
                                 GBR.LOF.table$k==3,], aes(x=Reef, y=Taxon, fill=meanDiff),
            color='white') +
  scale_fill_gradient2() +
  transition_components(Year) +
  ggtitle("Year: {frame_time}") +
  theme_Publication() +
  theme(panel.border=element_rect(color='black',fill=NA, size=1),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title=element_text(hjust=0))

# # animate in a two step process:
animate(k3.change, height=800, width=1500)
anim_save("/Users/uqskim14/Analyses/AIMS community index/Figure/Mackay_Whitsundays/k3_change.gif")

# when k=6
k6.change <- ggplot() +
  geom_tile(data=GBR.LOF.table[order(GBR.LOF.table$Year) &
                                 GBR.LOF.table$k==6,], aes(x=Reef, y=Taxon, fill=meanDiff),
            color='white') +
  scale_fill_gradient2() +
  transition_components(Year) +
  ggtitle("Year: {frame_time}") +
  theme_Publication() +
  theme(panel.border=element_rect(color='black',fill=NA, size=1),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title=element_text(hjust=0))

# # animate in a two step process:
animate(k6.change, height=800, width=1500)
anim_save("/Users/uqskim14/Analyses/AIMS community index/Figure/Mackay_Whitsundays/k6_change.gif")
  


