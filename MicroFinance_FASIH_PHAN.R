## ===============================================================
## Project definition
## ===============================================================

## Y: predict adoption rate / villages
## X: leader avg degree. leader avg between, leader avg closeness, etc.

## ===============================================================
## Data preparation
## ===============================================================

# install.packages("foreign")
# install.packages("igraph")
library(foreign)
library(data.table)
library(igraph)
library(foreign)

## Read demographics data from *.dta files
houseHold <- read.dta("C:/Users/Mahir/Documents/IESEG/Social Network Analysis/Data/2. Demographics and Outcomes/household_characteristics.dta")
individual <- read.dta("C:/Users/Mahir/Documents/IESEG/Social Network Analysis/Data/2. Demographics and Outcomes/individual_characteristics.dta")
village_blank <- read.delim("C:/Users/Mahir/Documents/IESEG/Social Network Analysis/Data/1. Network Data/village_table_blank.csv", header = TRUE, sep = " " )

houseHold <- data.table(houseHold)
individual <- data.table(individual)
village_blank <- data.table(village_blank)

## Loop to read adjacency matrices of all selected villages
tbl <- data.frame(village = integer(),
                  numHH = double(),
                  fractionLeaders = double(),
                  dgr = double(),
                  btw = double(),
                  cls = double(),
                  egn = double())

inputRow <- 1
for(i in 1:77) {
  filename <- paste0("C:/Users/Mahir/Documents/IESEG/Social Network Analysis/Data/1. Network Data/Selected Adjacency Matrices/adj_allVillageRelationships_HH_vilno_",i,".csv")
  if(file.exists(filename)) { ## Some filenames are missing, e.g. "13"
    
    dat <- read.csv(filename,header=FALSE)
    m <- as.matrix(dat)
    g.v <- graph.adjacency(m,mode="undirected",weighted=NULL)
    
    ## Village stats
    fractionLeaders = houseHold[village==i & leader == 1, .N]/houseHold[village==i, .N]
    numHH = ifelse(nrow(m)==houseHold[village==i,.N], houseHold[village==i,.N],0)
    
    ## Average centrality measures per vilage
    ## Only calculate on leaders' centrality values
    dgr = mean(degree(g.v,mode = "all", normalized=TRUE)[houseHold[village == i & leader ==1 , adjmatrix_key]])
    btw =  mean(betweenness(g.v, directed = FALSE, normalized =TRUE )[houseHold[village == i & leader ==1 , adjmatrix_key]])
    cls = mean(closeness(g.v, mode="all", weights=NA,normalized = TRUE)[houseHold[village == i & leader ==1 , adjmatrix_key]])
    egn = mean(eigen_centrality(g.v,directed = FALSE, weights = NA)$vector[houseHold[village == i & leader ==1 , adjmatrix_key]])
    tbl[inputRow,] = c(i,numHH,fractionLeaders, dgr,btw,cls,egn)
    
    inputRow <- inputRow+1 }   
}

## Combine the results and put in the village_blank table
microFinance <- merge(tbl,village_blank[,.(village,mf)],all.y=TRUE, by ="village")
setnames(microFinance, names(microFinance), names(village_blank))

## ===============================================================
## Multiple linear regression model
## ===============================================================

results = lm(mf ~ . - village, data=microFinance)
summary(results)

# Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(results)

## mf vs Explanatory Variables
plot(as.data.frame(microFinance)[,-10], main = "mf vs Explanatory Variables")