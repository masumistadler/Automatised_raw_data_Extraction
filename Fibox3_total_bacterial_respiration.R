##########################
## Respiration raw data ##
##########################
# Authored by: Masumi Stadler

# This script extracts the raw data of Fibox3 (PreSens) from txt files
# be aware that this is only for oxygen data
# differences in output txt files from CO2 mini has to be changed in line 61 (i.e. how many lines to skip)
# and check whether the column arrangement is different

# Default output txt file of Fibox, the script skips the first 58 rows that does not include the data
# the default column names have characters problematic in R, thus use the following vector if you need
# the column names at any point
colnames.o2 <- c("Date","Time","Logtime.min","Oxygen","Phase","Amplitude","Temperature", "ErrorMessage")
# Units: Date = DD/MM/YY, Time = hh:mm:ss, Oxygen = mg/L (ppm), Temperature = °C

############## IMPORTANT ##############
# To process the data without changing the script, arrange/save your files in the following order of folders
# Respiration / *Sample_name* / *Respiration_type* / *Replicate* / *Timepoint*.txt
# where Sample_name is a unique identifier for each of your samples
# Respiration type is either TR and/or BR in separate folders
# Replicate: folder A and B
# Timepoint as in T0, T1, ..., Tn.txt


##############
## PACKAGES ##
##############
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

##########################
## change wd            ## 
## until ./Respiration/ ##
##########################

path <- paste("/your/path/to/directory/Respiration")


# create vectors for loop
samples <- dir(path = path)
# get all sample names, this folder cannot contain ANY other folders than folders that contain raw instrument files
samples # check if you only have folders with sample names

type <- c("TR","BR")
# change this part if you have different treatments
replicate <- c("A","B")
# change this part if you have different/more/less replicates

##########################################
## Loop to extract data from text files ##
## first, loops through samples, then through types and then through replicate folders

out.file <- data.frame() # create empty data frame

for(a in 1:length(samples)){
  for(b in 1:length(type)){
    for(c in 1:length(replicate)){
      ##########################################################
      looping.path <- paste(path, "/", samples[a],"/", type[b], "/", replicate[c], sep = "")
      ##########################################################
      
      samp <- samples[a]
      res.type <- type[b]
      rep <- replicate[c]
      
      ####################################
      file.names <- dir(looping.path, pattern = "*txt") # creates vector with all txt files' names in directory
      
      for(d in 1:length(file.names)){
        file <- read.table(file = file.names[d], skip = 58, sep=";", stringsAsFactors = F)
        # reads table, skips first 58 lines

        file <- data.frame(Date = file[1,1], Time = file[1,2],
                           Oxygen = mean(file[,4]), Temperature = file[1,7])
        #colnames(file) <- colnames.o2 # assign column names
        file$ID <- paste(samp, res.type, rep, sep = "_")
        # create an unique ID with sample name + respiration type + replicate for later analysis
        file$Sample <- samp
        file$Type <- res.type
        file$Replicate <- rep
        
        file$Timepoint <- substr(file.names[d], start=1, stop=2) # add timepoint
        
        file <- file[,c("ID","Sample","Type", "Replicate",
                        "Timepoint","Date","Time", "Oxygen","Temperature")] # rearrange dataframe
        out.file <- data.frame(rbind(out.file, file), stringsAsFactors = F)
      }
    }
  }
}

# combine Date and Time -> change the setting of date and time order depending on PC settings
# check ?strptime for formats
out.file$Date <- as.character(out.file$Date)

# if you have some e.g. years that are "18" insted of "2018", change with:
#out.file[substr(out.file$Date, 7, 8) == "18", "Date"] <- paste(substr(out.file$Date[substr(out.file$Date, 7, 8) == "18"], 1, 6), "2018", sep = "")

# the "%d/%m/%Y %H:%M:%S" part is crucial, check your date and time configuration including
# what kind of separators are used e.g. "/" or "." for dates
# here we have "date/month/year[space]hour:minutes:seconds" e.g.: "24/01/2018 09:10:33"
out.file$cTime <- strptime(paste(out.file$Date, out.file$Time, sep = " "), "%d/%m/%Y %H:%M:%S")
out.file$cTime <- as.POSIXct(out.file$cTime) # change date/time class for calculation

# create column with delta time in hours
out.file <- out.file %>% group_by(ID) %>% dplyr::mutate(dTime = difftime(cTime, min(cTime), units = "hours"))

############## IMPORTANT ##############
# Check now whether you have overly weird differences in time
# rarely,it could be that the PC saved the date in a different
# format for some timepoints (observed in previous data); e.g. date/month/year -> month/date/year
# manually change wrong dates and re-run lines 86-90


# create linear models for each sample and save in list
lin.models <- dlply(out.file, ~ ID, function(df) {
  lm(Oxygen ~ dTime, data = df, na.action = na.omit)
})

# extract R2, intercept, slope, p-value for each replicate
fin <- ldply(lin.models, function(x){
  r.sq <- round(summary(x)$r.squared, digits = 2)
  intercept <- round(summary(x)$coefficients[1], digits = 2)
  beta <- round(summary(x)$coefficients[2], digits = 2)
  Pval <- round(anova(x)$`Pr(>F)`[1], digits = 4)
  data.frame(r.sq, intercept, beta, Pval)})

equation <- ldply(lin.models, function(x) {
  lm_coef <- list(a = as.numeric(round(summary(x)$coefficients[1], digits = 2)),
                  b = as.numeric(round(summary(x)$coefficients[2], digits = 4)),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
})

fin <- full_join(fin, equation, by = "ID")
colnames(fin)[6] <- "Equation"


#############################
## which are not significant?

fin[fin$Pval > 0.05,]

# do longer incubations for those samples
# seems like previously people incubated until a R2 of around 0.9 (based on excel sheet information)
# but many of them were lower (e.g. 0.7) due to the below mentioned problem in excel how it calculates R2

############## NOTE ##############
#  r-squared values are different to excel results.
# apparently, one has to force change the set intercept = 0 (by default) in excel to get a correct R2

fin <- cbind(colsplit(fin$ID, pattern="_", c("Sample","Type","Replicate")), fin[,1:6])

##################################
# merge files and plot
# plots are saved in the folder "Consumption_plots", it will be created as part of the script

out.file <- full_join(out.file, fin, by = c("ID", "Sample","Replicate", "Type"))
out.file$dTime <- as.numeric(out.file$dTime)

p <- out.file %>%
  group_by(Sample) %>%
  do(plots = ggplot(data=.) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  aes(x = dTime, y = Oxygen, label = Equation) +
  facet_grid(Replicate ~ Type, scales = "free_x") +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_text(aes(x = mean(dTime), y = max(Oxygen)), parse = T,
            position = position_dodge(width=0.9),  size=2) +
  geom_text(aes(label = paste("P value =", round(Pval, digits = 3), sep = ""),
                x = mean(dTime), y = max(Oxygen)-0.1),
            position = position_dodge(width=0.9),  size=2) +
  labs(title = samples[1], y = expression(paste("O"["2"]," [mg L"^"-1","]")), x = c("Time [h]")) +
  ggtitle(unique(.$Sample)))

# save plots
plot.list <- p$plots # save as list
names(plot.list) <- samples # give list bins sample names


dir.create(file.path(paste0(path, "/../"), "Consumption_plots"), showWarnings = FALSE)
# creates a new folder, will not show a warning if the folder already exists and will overwrite it
plotpath <- paste0(path, "/../Consumption_plots/") # set a path to plot folder

# finally save plots
invisible(mapply(ggsave, file = paste0(plotpath, names(plot.list),".png"),
                 plot = plot.list))

# save as csv if desired
#write.table(x = fin,
#            file = paste0(path.to.plots, "/file_name.csv",
#            sep = ",", row.names = F)

