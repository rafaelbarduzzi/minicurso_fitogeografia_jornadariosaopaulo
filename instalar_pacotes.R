pckgs <- c("terra",
           "sf",
           "data.table",
           "betapart",
           "phangorn",
           "dendextend",
           "viridis",
           "paletteer",
           "ggdendro",
           "gridExtra",
           "naturalsort",
           "gdm")
if(!require(pckgs)) install.packages(pckgs)
