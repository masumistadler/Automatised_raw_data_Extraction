######################################################
## Rename your files on your local directory from R ##
######################################################

# Find your files on your computer

file.names <- dir("/path/to/your/directory", pattern = "*fastq") # uncompressed files
# file.names <- dir("/path/to/your/directory", pattern = "*fastq.gz") # compressed files

# this function splits your file name by an "_"
# check which separator makes the most sense for your file names, you want to isolate your sample name

# our example is the following: "MI.MO3992_0286.001.FLD0097.1_DNA_R1.fastq"
# we want the final name to be: "1-DNA_R1.fastq"
# 1 is the sample name
# DNA because we have DNA and RNA samples
# and keep the _R1.fastq or _R2.fastq

t <- strsplit(file.names, "_", fixed = T)

# see how your file name was split and locate the number of element you need
t[[1]] # first sample in your folder

# so we can keep element t[[i]][3] for the "DNA"
# and element t[[i]][4] for the extension "_R1.fastq"

# to get the sample name/number, we again have to split but this time by "."
# we incorporate this in the loop

# before running the loop we have to create empty objects to store the new file names
sample.number <- vector()
new.names <- vector()

# Let's loop
for(i in 1:length(t)){
  # splits the first element of our t object because the sample number is in the second element
  # check which element you need by running t[[1]]
  # and change the x in t[[i]][x]
  t2 <- strsplit(t[[i]][2], ".", fixed = TRUE) 
  # Now, we save them into empty vectors
  sample.number[i] <- t2[[1]][4]
  new.names[i] <- paste0(t2[[1]][4], "-", t[[i]][3], "_", t[[i]][4]) # combine strings to new file name
}

# so, we end up with: "1-DNA_R1.fastq"

# as a sanity check, you can merge your file names together into a data frame and check, whether the order
# is still the same
(sanity <- data.frame(FileNames = file.names, NewNames = new.names))

# in case you do not need to separate by a second separator
# here is an example where we take the second element and last which is usually your extension
#for(i in 1:length(t)){
#  new.names[i] <- paste(t[[i]][2], t[[i]][4], sep = "_")
#}


# this loop actually renames your files
# WARNING: messing with file extensions/suffix can corrupt files (e.g. mixing "fastq.gz" and "fastq")
# try a subset (e.g. 10 files) first and also keep a copy of your non-renamed samples as safety

filepath <- "./path/to/your/directory/"

# check if you have double "//" or anything else that might be mixed up
# change your `filepath` object accordingly above

paste0(filepath, file.names[1])
paste0(filepath, new.names[1])

# Let's loop again
################# RENAMING LOOP #################
for(f in 1:length(file.names)){
  file.rename(paste0(filepath, file.names[f]), paste0(filepath, new.names[f]))  
}
#################################################



###################
# OTHER SCENARIOS #
###################

# NOT RECOMMENDED BUT, in case your file names do not contain sample names, add a step with Excel

df <- data.frame(FileNames = dir("./path/to/your/directory", pattern = "*fastq"), NewNames = NA)
write.table(df, "./path/to/your/directory/NewFileName.csv", sep = ",", dec = ".", row.names = FALSE)

# modify column NewNames in Excel etc and re-import into R
df <- read.csv( "./path/to/your/directory/NewFileName.csv", sep = ",", dec = ".", stringsAsFactors = F)

new.names <- df$NewNames
# and do the renaming loop

# OR if you have named your samples 1,2,3,4,...,n and have a table with the actual sample names separately
# you'll have to merge your table with your order of files that you get with `dir()`
# for example with
library("dplyr")
joined <- dplyr::full_join(sanity, df, by = "SampleNumber")
# where `sanity` is the data frame we created before with `dir()` and a loop, 
# you'll need to add a column "SampleNumber" in the looping to be able to merge later
# and `df` is your data frame with a "SampleNumber" column, and the actual sample names in another
# Important! both data frames need to have the same column named "SampleNumber"

# then you'll do
new.names <- joined$SampleName
# do the renaming loop
