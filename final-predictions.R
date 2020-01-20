# This script creates a TEX and CSV files for tables containing state-stratified precision-weighted estimates for all districts
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, reshape2, tidylog, splines, pbapply, here, brms)

# Read-in models (after running "final-models.R")
lapply(c("anemia","stunting","underweight","wasting"), function(x) load(file=here("models", paste0(x,"-models.Rda")), envir=.GlobalEnv))
newdistricts <- unique(d$new_district[!is.na(d$new_district)]) # Create indicator for newly drawn district

# Functions to output model estimates
preds.fun0 <- function(x, model) {
  outcome = names(model[[1]]$data)[1]
  if( is.null(ranef(model[[x]])$district[,,"Intercept"] %>% dim) ){
    temp <- rbind(d %>% filter(state==names(model)[x]) %>% group_by(district) %>% 
                    summarise(N=n(), "Observed"=mean(eval(parse(text=outcome)), na.rm=TRUE)) %>% select(-district) %>% t(),
                  fitted(model[[x]],
                         newdata = data.frame(state=rep(states[x], length(rownames(ranef(model[[x]])$district))), 
                                              district=rownames(ranef(model[[x]])$district))) %>% t()
    )  %>% t() %>% as.data.frame() %>% rename(Predicted=Estimate) %>% select(-Est.Error)
    rownames(temp) <- tools::toTitleCase(rownames(ranef(model[[x]])$district))
    new <- tolower(rownames(temp)) %in% tolower(newdistricts) %>% as.numeric()
    temp2 <- bind_cols(temp, "New"=new)
    rownames(temp2) <- rownames(temp)
    temp2
  }
  else if( !is.null(ranef(model[[x]])$district[,,"Intercept"] %>% dim) ){
    temp <- cbind(d %>% filter(state==names(model)[x]) %>% group_by(district) %>% 
                    summarise(N=n(), "Observed"=mean(eval(parse(text=outcome)), na.rm=TRUE)) %>% select(-district),
                  fitted(model[[x]],
                         newdata = data.frame(state=rep(states[x], length(rownames(ranef(model[[x]])$district))), 
                                              district=rownames(ranef(model[[x]])$district)))) %>% 
      rename(Predicted=Estimate) %>% select(-Est.Error)
    rownames(temp) <- tools::toTitleCase(rownames(ranef(model[[x]])$district))
    new <- tolower(rownames(temp)) %in% tolower(newdistricts) %>% as.numeric()
    temp2 <- bind_cols(temp, "New"=new)
    rownames(temp2) <- rownames(temp)
    temp2
  }
}
preds.fun <- function(model) {
  pblapply(setNames(1:length(names(model)), names(model)), function(x) preds.fun0(x=x, model=model)) 
  }


preds.underweight <- preds.fun(model=m.underweight)
preds.stunting <- preds.fun(model=m.stunting)
preds.wasting <- preds.fun(model=m.wasting)
preds.anemia <- preds.fun(model=models.anemia)

# Order by district name
preds.underweight <- preds.underweight[order(names(preds.underweight))]
preds.stunting <- preds.stunting[order(names(preds.stunting))]
preds.wasting <- preds.wasting[order(names(preds.wasting))]
preds.anemia <- preds.anemia[order(names(preds.anemia))]


# Function to output to CSV
output.fun <- function(predictions, name){
out_file <- file(paste0(name,".csv"), open="a")  #creates a file in append mode
for (i in seq_along(predictions)){
  write.table(names(predictions)[i] %>% str_to_title(), file=out_file, sep=",", dec=".", 
              quote=FALSE, col.names=FALSE, row.names=FALSE)  #writes the names of list elements
  write.table(predictions[[i]], file=out_file, sep=",", dec=".", quote=FALSE, 
              col.names=NA, row.names=TRUE)  #writes the data.frames
  write.table(" ", file=out_file, sep=",", dec=".", 
              quote=FALSE, col.names=FALSE, row.names=FALSE)
}
close(out_file)
}
output.fun(predictions=preds.underweight, name="underweight")
output.fun(predictions=preds.stunting, name="stunting")
output.fun(predictions=preds.wasting, name="wasting")
output.fun(predictions=preds.anemia, name="anemia")