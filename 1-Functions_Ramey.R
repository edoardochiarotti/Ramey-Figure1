
#####################
##### FUNCTIONS #####
#####################

##### DETACH ALL PACKAGES ######
"detachAllPackages" <- function() {
  basic.packages.blank <-  c("stats", 
                             "graphics", 
                             "grDevices", 
                             "utils", 
                             "datasets", 
                             "methods", 
                             "base")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, 
                                  TRUE, 
                                  FALSE)]
  
  package.list <- setdiff(package.list, basic.packages)
  
  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}

# GRAPH
"graphIRF_RameyCEE" <- function(data, comblist, variabs,
                                type = "both", boot = FALSE, ortho = TRUE, ci=0.90, runs = 100, 
                                n.ahead, indexes, impulse, spec.name, p, state,
                                max.iter = 100, lrtest = FALSE,
                                cumulative = FALSE, seed = NULL) {
  
  # MAKE LIST OF OIRF
  oirf_list <- lapply(1:length(comblist), function(x){
    y <- data[data[,indexes[2]] >= comblist[[x]]$start_date & 
                data[,indexes[2]] <= comblist[[x]]$end_date &
                data[,indexes[1]] %in% state, comblist[[x]]$variabs] %>%
      filter(complete.cases(.)) %>%
      as.data.frame()
    
    var <- VAR(y, p = p, type = type)
    oirf <- irf(var,
                impulse = impulse, 
                response = comblist[[x]]$variabs, 
                boot = boot, ortho = ortho, ci=ci, runs = runs, n.ahead = n.ahead, 
                cumulative = cumulative, seed = seed)
  })
  
  # MAKE DATASET FOR GRAPH
  if(is.null(boot) | boot %in% FALSE){
    myvalues <- c("irf")
  } else {
    myvalues <- c("irf","Lower","Upper")  
  }
  datasetg <- lapply(myvalues, function(x1){
    tempdata <- lapply(1:length(spec.name), function(x){
      mydf <- oirf_list[[x]][[x1]]
      attributes(mydf) <- NULL
      mydf <- as.data.frame(mydf) %>%
        cbind(state = state, .) %>%
        cbind(n.ahead = 0:n.ahead, .) %>%
        reshape2::melt(., id = c("state","n.ahead")) %>%
        cbind(., spec.name = spec.name[x])
      return(mydf)
    }) %>%
      Reduce(rbind,.)
    colnames(tempdata)[4] <- x1
    return(tempdata)
  }) %>%
    Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("state","n.ahead","variable","spec.name"), all.x = TRUE), .) %>%
    arrange(., state,variable,spec.name,n.ahead)
  
  # GRAPH
  varsfactors <- c(impulse,variabs[!variabs %in% impulse])
  datasetg[,"variable"] <- factor(datasetg$variable, levels = varsfactors)
  if(n.ahead < 20){mybreak <- 1} else {mybreak <- 10}
  mygg <- ggplot(data=datasetg, aes(x=n.ahead, y=irf, colour = spec.name)) +
    scale_color_manual(values = c("black","blue","red")) +
    geom_hline(yintercept=0, color = "darkgray") +
    geom_line(aes(linetype = spec.name)) +
    scale_x_continuous(breaks=seq(0, n.ahead, mybreak)) +
    facet_wrap( ~ variable, scales = "free", ncol = 2) +
    myThemeStuff + theme(legend.position = "bottom")
  
  if (boot %in% TRUE) {
    mygg <- mygg +
      geom_ribbon(data = datasetg[datasetg$spec.name %in% spec.name[1],],
                  aes(ymin=datasetg[datasetg$spec.name %in% spec.name[1],"Lower"], 
                      ymax=datasetg[datasetg$spec.name %in% spec.name[1],"Upper"]), colour = "lightgrey", alpha=0.1)
  }
  
  # RESULT
  result <- list(graph = mygg, data.graph = datasetg)
  return(result)
}
