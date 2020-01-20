# This script creates a TEX file for tables containing descriptive statistics
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tableone, here)
load(file=here("data","districts.Rda"))

# Table: overview of observed outcomes by state
allvars <- c("sex", "age", "stunting", "underweight", "wasting", "anemia")
catvars <- c("sex", "stunting", "underweight", "wasting", "anemia")
contvars <- "age"
tab0 <- CreateTableOne(vars = allvars, data = d,
                       factorVars = catvars)
tab0.p <- print(tab0, nonnormal=contvars, quote=FALSE, test=FALSE, noSpaces=TRUE, printToggle = FALSE, contDigits=1, catDigits=0)
tab1.format <- t(t(as.matrix(tab0.p)) %>% as.data.frame %>% rename(
  "N"="n",
  "Male (vs. female)"="sex = male (%)",
  "Age (months)"="age (median [IQR])",
  "Stunting"="stunting = 1 (%)",
  "Underweight"="underweight = 1 (%)",
  "Wasting"="wasting = 1 (%)",
  "Anemia"="anemia = 1 (%)",
  ))
named1 <- rownames(tab1.format)
tags1 <- grepl("^ ", rownames(tab1.format))
rownames(tab1.format) <- c(ifelse(tags1==FALSE, named1, paste("\\hskip .5cm", named1, sep=' ')))
print(xtable(tab1.format, align=c("lr"),
             caption="Overall observed statistics"), 
      type="latex", sanitize.text.function = function(x){x}, file=here("tables","overview","overview-table.tex"), 
      floating=FALSE, tabular.environment="longtable", caption.placement = "top")


# Table: overview of observed outcomes by state
tab.overview <- d %>% group_by(state) %>% 
  summarise(N=n(), mean.anemia=mean(anemia, na.rm=TRUE), mean.underweight=mean(underweight, na.rm=TRUE), 
            mean.wasting=mean(wasting, na.rm=TRUE), mean.stunting=mean(stunting, na.rm=TRUE)) %>% mutate(state=tools::toTitleCase(state)) %>% 
  rename(State=state, Anemia=mean.anemia, Underweight=mean.underweight, Wasting=mean.wasting, Stunting=mean.stunting)
print(xtable(tab.overview, align=c("lrrrrrr"),
             caption="Overview of observed outcomes for all states"), 
      type="latex", sanitize.text.function = function(x){x}, file=here("tables","overview","overview-states-table.tex"), 
      floating=FALSE, tabular.environment="longtable", include.rownames=FALSE, caption.placement = "top")
