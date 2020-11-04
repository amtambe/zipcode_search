#Basic R script to convert stata to R
#just specify file names and locations

zip_data = "zipcode_data copy.csv"
args<-commandArgs(TRUE)

library(haven)
#set this
setwd("insert your wd here!")


zips_df <- read.csv(zip_data)
#read in zip; clean zips starting with 0
#not handling misspecified arguments for now
zip <- args[1]
while (identical(substr(zip, 0, 1), "0")) {
    zip <- substr(zip, 2, nchar(zip))
}


"Compute chi-square distance between each zip code and the specified one. 

zip: single zip code data we want to compute distances from
df: full array of zip code data

Uses a chi-square-esque distance, where the distance for each demographic value is weighted
by the inverse of the population-wide percentage of that demographic. That is, for each
demographic j (e.g. Doctorate degree), zip codes x and y, and total population size N, 
we compute Xj = (N / c_j) (x_j - y_j)^2, where c_j is the count of demographic j across 
all zip codes. We return the square root of the sum of sum Xj for each demographic 
category j."
get_nearest_zips <- function(zip, df) {
    #get data affiliated with this zip code
    zip_demogs <- df[match(zip, df$Zipcode), ]
    zip_demogs <- sapply(zip_demogs, as.numeric)

    #size of the entire population
    total_pop <- sum(df$Male + df$Female)
    #c is a row - country-wide percentage of each demographic
    c <- colSums(df[]) / total_pop
    #get the euclidean distance, segmented by demographic segment
    dists <- sweep(as.matrix(df), 2, zip_demogs, FUN='-') ^ 2
    #subset to remove zipcode, not relevant for distance computation
    dists <- subset(dists, select=-Zipcode)
    #for each demographic segment, weight it by the country-wide size of that demographic
    #remove zipcode from c as well
    dists <- sweep(dists, 2, c[2:length(c)], FUN='/')
    #then sum and square root across the demographic segments to get the final distance
    #only consider column 2 onwards since zip code number itself is irrelevant
    df$total_dists <- rowSums(dists) ^ 0.5

    #then, sort by the distance
    sorted <- df[order(df$total_dists), 'Zipcode']
    #exclude 1st because that will be the input zipcode with distance 0
    return(sorted[2:length(sorted)])
}

nearest_zips <- get_nearest_zips(zip, zips_df)
print("Nearest zips in ascending order. ")
print(nearest_zips)