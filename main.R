# load packages and import Nathan Nunn slave data
library(haven)
library(ggplot2)
library(stargazer)
library(AER) #for ivreg
library(ivmodel)
slave_data <- read_dta("slave_trade_QJE.dta")
View(slave_data)

#plot relationship between GDP and slave exports (replicate figure III)
ggplot(slave_data, aes(x=ln_export_area, y=ln_maddison_pcgdp2000))+
  geom_point(shape=1)+
  geom_text(label=slave_data$isocode)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Relationship between Slave Exports and GDP")+
  xlab("log of export / area between 1400-1900")+
  ylab("log of per capita GDP in 2000")
##############################################################################################
#get OLS estimates of slaves/area over various specifications (replicate Table III)
##only colonizer effects
ols1<- lm(ln_maddison_pcgdp2000~ln_export_area+colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=slave_data)
##colonizer and geographic effects
ols2<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=slave_data)
##colonizer and geographic effects w/o islands and North countries
remove_isl_and_north<- c('Morocco','Algeria','Tunisia','Libya','Egypt','Seychelles',
                         'Mauritius','Comoros','Sao Tome & Principe','Cape Verde Islands')
restricted_sample<-slave_data[!slave_data$country %in% remove_isl_and_north,]
ols3<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=restricted_sample)
##additional controls: island fixed effect, North Africa fixed effect, French legal origin, percent islamic
ols4<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+region_n+legor_fr+island_dum+islam+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=slave_data)
##additional controls: log of the annual average per capita production of gold, oil, diamonds
ols5<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+region_n+legor_fr+island_dum+islam+
            ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=slave_data)
##all control variables and drop islands and North African countries
ols6<- lm(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
            rain_min+humid_max+low_temp+ln_coastline_area+region_n+legor_fr+island_dum+islam+
            ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
            colony1+colony2+colony3+
            colony4+colony5+colony6+colony7, data=restricted_sample)
summary(ols1)
summary(ols2)
summary(ols3)
summary(ols4)
summary(ols5)
summary(ols6)
# generate table
stargazer(ols1,ols2,ols3,ols4,ols5,ols6,
          header = FALSE,
          digits = 3,
          title="Relationship between Slave Exports and Income",
          type = "text",
          dep.var.labels.include = FALSE,
          dep.var.caption =
            "Dependent variable is log real per capita GDP in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant'),
          add.lines=list(c('Colonizer fixed effects',"Yes","Yes","Yes","Yes","Yes","Yes")))

#Modify Table III by taking into account Heteroskedasticity-Consistent standard errors using vcovHC()
se1<-coeftest(ols1, vcov = vcovHC, type = "HC1")
se2<-coeftest(ols2, vcov = vcovHC, type = "HC1")
se3<-coeftest(ols3, vcov = vcovHC, type = "HC1")
se4<-coeftest(ols4, vcov = vcovHC, type = "HC1")
se5<-coeftest(ols5, vcov = vcovHC, type = "HC1")
se6<-coeftest(ols6, vcov = vcovHC, type = "HC1")
stargazer(se1,se2,se3,se4,se5,se6,
          header = FALSE,
          digits = 3,
          title="Relationship between Slave Exports and Income - corrected for HC std.err.",
          type = "text",
          dep.var.labels.include = FALSE,
          dep.var.caption =
            "Dependent variable is log real per capita GDP in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant'),
          add.lines=list(c('Colonizer fixed effects',"Yes","Yes","Yes","Yes","Yes","Yes")))
#######################################################################################################

#plot relationship between Population Density (as proxy for economic prosperity) and Slave Exports (replicate figure IV)
ggplot(slave_data, aes(x=ln_pop_dens_1400, y=ln_export_area))+
  geom_point(shape=1)+
  geom_text(label=slave_data$isocode)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Relationship between Initial Population Density and Slave Exports")+
  xlab("log population density in 1400")+
  ylab("slave exports (log of export / area)")

#######################################################################################################
#get IV 2SLS estimates of slaves/area over various specifications (replicate Table IV)

#SECOND STAGE

##w/o control variables
ivreg1<-ivreg(ln_maddison_pcgdp2000~ln_export_area|saharan_distance_minimum+
                atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=slave_data)
##includes colonizer fixed effects
ivreg2<-ivreg(ln_maddison_pcgdp2000~ln_export_area+colony1+colony2+colony3+
                colony4+colony5+colony6+colony7|colony1+colony2+colony3+
                colony4+colony5+colony6+colony7+saharan_distance_minimum+
                atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=slave_data)
##includes colonizer fixed effects and geographic controls
ivreg3<-ivreg(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
                rain_min+humid_max+low_temp+ln_coastline_area+
                colony1+colony2+colony3+
                colony4+colony5+colony6+colony7|abs_latitude+longitude+
                rain_min+humid_max+low_temp+ln_coastline_area+
                colony1+colony2+colony3+
                colony4+colony5+colony6+colony7+
                saharan_distance_minimum+
                atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=slave_data)
##includes colonizer fixed effects and geographic controls, but the sample excludes islands and North African countries.
ivreg4<-ivreg(ln_maddison_pcgdp2000~ln_export_area+abs_latitude+longitude+
                rain_min+humid_max+low_temp+ln_coastline_area+
                colony1+colony2+colony3+
                colony4+colony5+colony6+colony7|abs_latitude+longitude+
                rain_min+humid_max+low_temp+ln_coastline_area+
                colony1+colony2+colony3+
                colony4+colony5+colony6+colony7+
                saharan_distance_minimum+
                atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=restricted_sample)

#FIRST STAGE
stage1_1<-lm(ln_export_area~saharan_distance_minimum+
               atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=slave_data)
stage1_2<-lm(ln_export_area~saharan_distance_minimum+
               atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum+colony1+colony2+colony3+
               colony4+colony5+colony6+colony7, data=slave_data)
stage1_3<-lm(ln_export_area~saharan_distance_minimum+
               atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum+colony1+colony2+colony3+
               colony4+colony5+colony6+colony7+abs_latitude+longitude+
               rain_min+humid_max+low_temp+ln_coastline_area, data=slave_data)
stage1_4<-lm(ln_export_area~saharan_distance_minimum+
               atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum+colony1+colony2+colony3+
               colony4+colony5+colony6+colony7+abs_latitude+longitude+
               rain_min+humid_max+low_temp+ln_coastline_area, data=restricted_sample)

pvH_1<-round(summary(ivreg1, diagnostics = TRUE)$diagnostics[2,4], digits=2)#Hausman test pvalue
pvH_2<-round(summary(ivreg2, diagnostics = TRUE)$diagnostics[2,4], digits=2)
pvH_3<-round(summary(ivreg3, diagnostics = TRUE)$diagnostics[2,4], digits=2)
pvH_4<-round(summary(ivreg4, diagnostics = TRUE)$diagnostics[2,4], digits=2)
pvS_1<-round(summary(ivreg1, diagnostics = TRUE)$diagnostics[3,4], digits=2)#Sargan test pvalue
pvS_2<-round(summary(ivreg2, diagnostics = TRUE)$diagnostics[3,4], digits=2)
pvS_3<-round(summary(ivreg3, diagnostics = TRUE)$diagnostics[3,4], digits=2)
pvS_4<-round(summary(ivreg4, diagnostics = TRUE)$diagnostics[3,4], digits=2)

#generate table - panel 1/2
stargazer(ivreg1,ivreg2,ivreg3,ivreg4,
          header = FALSE,
          digits = 3,
          omit.table.layout = "n",
          title="Estimates of the relationship between Slave Exports and Income",
          type = "text",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          dep.var.caption =
            "Second Stage. Dependent variable is log income in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes')))

#generate table - panel 2/2
stargazer(stage1_1,stage1_2,stage1_3,stage1_4,
          header = FALSE,
          digits = 3,
          type = "text",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          dep.var.caption =
            "First Stage. Dependent variable is slave exports, ln(exports/area)",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes'),
                         c('Hausman test (pvalue)',pvH_1,pvH_2,pvH_3,pvH_4),
                         c('Sargan test (pvalue',pvS_1,pvS_2,pvS_3,pvS_4)))

#Modify Table IV by taking into account Heteroskedasticity-Consistent standard errors using vcovHC()
model.lst = list(ivreg1, ivreg2, ivreg3, ivreg4)
model.lst2 = list(stage1_1, stage1_2, stage1_3, stage1_4)
stargazer(ivreg1, ivreg2, ivreg3, ivreg4,
          header = FALSE,
          digits = 3,
          omit.table.layout = "n",
          title="Estimates of the relationship between Slave Exports and Income - corrected for HC std.err.",
          type = "text",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          se=lapply(model.lst, function(x) sqrt(diag(sandwich::vcovHC(x, type = "HC1")))),
          dep.var.caption =
            "Second Stage. Dependent variable is log income in 2000, ln y",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes')))
stargazer(stage1_1, stage1_2, stage1_3, stage1_4,
          header = FALSE,
          digits = 3,
          type = "text",
          dep.var.labels.include = FALSE,
          summary=TRUE,
          se=lapply(model.lst2, function(x) sqrt(diag(sandwich::vcovHC(x, type = "HC1")))),
          dep.var.caption =
            "First Stage. Dependent variable is slave exports, ln(exports/area)",
          omit=c('colony1','colony2','colony3','colony4','colony5','colony6','colony7','Constant','abs_latitude',
                 'longitude','rain_min','humid_max','low_temp','ln_coastline_area'),
          add.lines=list(c('Colonizer fixed effects',"No","Yes","Yes","Yes"),
                         c('Geography controls','No','No','Yes','Yes'),
                         c('Restricted sample','No','No','No','Yes')))
#########################################################################################################

#Channels through which the slave trades may have affected economic development:

#slave trades tended to weaken ties between villages (replicate figure VI)
ggplot(slave_data, aes(x=ln_export_area, y=ethnic_fractionalization))+
  geom_point(shape=1)+
  geom_text(label=slave_data$isocode)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Relationship between Slave Exports and Current Ethnic Fractionalization")+
  xlab("log of export / area")+
  ylab("Ethnic fractionalization (Alesina, 2003)")

#slave trades is linked to the weakening and underdevelopment of states. (replicate figure VII)
ggplot(slave_data, aes(x=ln_export_area, y=state_dev))+
  geom_point(shape=1)+
  geom_text(label=slave_data$isocode)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Relationship between Slave Exports and Nineteenth-Century State Development")+
  xlab("log of export / area")+
  ylab("19th century State development (Gennaioli & Rainer, 2006)")
########################################################################################################
