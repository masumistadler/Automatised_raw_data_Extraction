#####################
## Biolog raw data ##
#####################
# Authored by: Masumi Stadler

# This script extracts the raw data from Tecan GENios (Plate reader) excel files
# make sure to create a folder called "Raw" where all the files should be stored in
# and change the path for directory accordingly
# Additionally, create a "AWCD_plot" folder at the same level as your "Raw" folder

# save samples as "SampleName_timepoint.xlsx"
# e.g. "LR145_t5.xlsx"

# this skript creates the following outputs:
# 1) a csv file with the calculated average well colour development (AWCD) per sample
# the file contains also advices on how often the plate should be read based on AWCD development
# 2) plots per sample that show the AWCD over time
# ignore the warnings at the end of the skript as it is related to the smooth fitting
# of the curve

##############
## PACKAGES ##
##############
library("XLConnect")
library("plyr")
library("dplyr")
library("reshape2")
library("ggplot2")
library("RColorBrewer")

##########################
## change wd            ## 
## until ./Raw/ ##
##########################
#path <- paste("~/CarBBAS/LaRomaine_2018/June/Biolog/Raw")
path <- paste("/path/to/your/directory/Raw")

#create vector for all substrates
sub1 <- c("Water", "b-Methyl-D-Glucoside","D-Galactonic Acid","L-Arginine")
sub2 <- c("Pyruvic Acid Methyl Ester","D-Xylose",	"D-Galacturonic Acid",
          "L-Asparagine")
sub3 <- c("Tween 40","i-Erythritol", "2-Hydroxy Benzoic Acid", "L-Phenylalanine")
sub4 <- c("Tween 80",	"D-Mannitol", "4-Hydroxy Benzoic Acid","L-Serine")
sub5 <- c("a-Cyclodextrin","N-Acetyl-D-Glucosamine", "g-Hydroxybutyric Acid",
          "L-Threonine")
sub6 <- c("Glycogen","D-Glucosaminic Acid","Itaconic Acid", "Glycyl-L-Glutamic Acid")
sub7 <- c("D-Cellobiose", "Glucose-1-Phosphate","a-Ketobutyric Acid", "Phenylethylamine")
sub8 <- c("a-D-Lactose","D,L-a-Glycerol-Phosphate","D-Malic Acid","Putrescine")

# combine into one vector to match order inside plates
substrates <- c(rep(sub1, times = 3),rep(sub2, times = 3),
                rep(sub3, times = 3),rep(sub4, times = 3),rep(sub5, times = 3),
                rep(sub6, times = 3),rep(sub7, times = 3),rep(sub8, times = 3))

#read in file names
file.names <- dir(path, pattern = "*.xlsx")
filelist <- strsplit(file.names, "[.]") # extract only sample names
file.names <- sapply(filelist, function(x) x[1]) # convert into vector

##########################################
## Loop to extract data from xlsx files ##

# create empty data frames
out.file <- data.frame()
out.subs <- data.frame()

for(i in 1:length(file.names)){
  ext <- paste0(path, "/", file.names[i], ".xlsx")
  file <- readWorksheetFromFile(ext,sheet=1, 
                                    startRow = 2,
                                    endCol = 13,header = F) #skips unnecessary rows
  
  date <- substr(file[1,6], 1, 10) #extract date of measurement
  time <- substr(file[2,6], 12, 19) #extract time of measurement
  Time <- strptime(paste(date, time, sep = " "), "%Y-%m-%d %H:%M:%S") # combine date and time
  Time <- as.POSIXct(Time) # change date/time class for calculation
  
  raw <- as.matrix(file[c(11:18),c(2:13)]) #extract absorbance values
  raw[which(raw == "OVER")] <- 2.5
  raw <- as.numeric(as.vector(t(raw))) #transpose into a vector
  
  # assign substrates to raw values
  sum <- data.frame(Substrates = substrates, Absorbance = raw, stringsAsFactors = F)
  # calculate mean per substrate
  sum <- ddply(sum,~Substrates,summarise,Mean=mean(Absorbance))
  # create dataframe with sample name and AWCD
  fin <- data.frame(Sample = file.names[i], Time = Time, AWCD = mean(sum$Mean))
  subs <- data.frame(Sample = rep(file.names[i], times = length(sum$Substrates)),
                     Time = rep(Time, times = length(sum$Substrates)),
                     Substrates = sum$Substrates, Substrate.mean = sum$Mean)
  
  # combine all excel files that are processed in the loop
  out.file <- data.frame(rbind(out.file, fin), stringsAsFactors = F)
  out.subs <- data.frame(rbind(out.subs, subs), stringsAsFactors = F)
}

# split sample name into sample ID and timepoint
out.file$Sample <- as.character(out.file$Sample)
out.subs$Sample <- as.character(out.subs$Sample)
out.file <- cbind(colsplit(out.file$Sample, pattern="_", c("Sample","Timepoint")), out.file[,2:3])
out.subs <- cbind(colsplit(out.subs$Sample, pattern="_", c("Sample","Timepoint")), out.subs[,2:4])


# create column with delta time in hours
out.file <- out.file %>% group_by(Sample) %>% dplyr::mutate(dTime = difftime(Time, min(Time), units = "hours"))
out.file$dTime <- as.numeric(out.file$dTime) # transform to numeric for plotting

out.subs <- out.subs %>% group_by(Sample) %>% dplyr::mutate(dTime = difftime(Time, min(Time), units = "hours"))
out.subs$dTime <- as.numeric(out.subs$dTime) # transform to numeric for plotting


# remove repetition of sample names
samples <- as.character(unique(out.file$Sample))

# order data frame so that newest measurements are at bottom
out.file <- out.file[order(out.file$Time, decreasing = F),]

#round numeric values
out.file$AWCD <- round(out.file$AWCD, digits = 2)
out.file$dTime <- round(out.file$dTime, digits = 2)

# decide when to measure next time
#if(out)
out.file[out.file$AWCD < 0.2, "NextMeasurement"] <- "twice a day"
out.file[out.file$AWCD >= 0.2 & out.file$AWCD <= 0.6, "NextMeasurement"] <- "three times a day"
out.file[out.file$AWCD > 0.6, "NextMeasurement"] <- "once a day"

# save data frame as csv

write.table(x = out.file,
            file = paste0(path, "/../AWCD.check.csv"),
            sep = ";", row.names = F)


# plot AWCD development over time and save
p <- out.file %>%
  group_by(Sample) %>%
  do(plots = ggplot(data=.) +
       theme_bw() +
       theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
       aes(x = dTime, y = AWCD) +
       geom_point() +
       geom_smooth(method = "loess") +
       labs(x = c("Time [h]")) +
       ggtitle(unique(.$Sample)))

# save plots
plot.list <- p$plots # save as list
names(plot.list) <- samples # give list bins sample names

dir.create(file.path(paste0(path, "/../"), "AWCD_plots"), showWarnings = FALSE)
plotpath <- paste0(path, "/../AWCD_plots/") # set a path to plot folder

# finally save plots
invisible(mapply(ggsave, file = paste0(plotpath, names(plot.list),".png"),
                 plot = plot.list))

### Plot substrate development
# plot AWCD development over time and save

n <- 32
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

p <- out.subs %>%
  group_by(Sample) %>%
  do(plots = ggplot(data=.) +
       theme_bw() +
       theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
       aes(x = dTime, y = Substrate.mean, colour = Substrates) +
       geom_point() +
       #scale_color_brewer(palette="Dark2") +
       scale_color_manual(values=col_vector) +
       geom_smooth(method = "loess", se = F) +
       labs(y = "O.D.", x = c("Time [h]")) +
       ggtitle(unique(.$Sample)))

# save plots
plot.list <- p$plots # save as list
names(plot.list) <- samples # give list bins sample names

dir.create(file.path(paste0(path, "/../"), "Substrate_plots"), showWarnings = FALSE) # go a level up in directory
plotpath <- paste0(path, "/../Substrate_plots/") # set a path to plot folder


# finally save plots
invisible(mapply(ggsave, file = paste0(plotpath, names(plot.list),".png"),
                 plot = plot.list, width = 8.93, height = 3.00))
