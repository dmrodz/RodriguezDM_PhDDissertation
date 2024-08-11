################################################################################
print("Stage 1...")
################################################################################
# Se files path here
path = #your/path/here
setwd(path)
# Setting connection
print("Setting up connection...")
con <- file(paste0(path, "/logs/stage1_hp_log - ", Sys.Date(), ".log"))
print("Producing sink...")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
# Run code
print("Running script...")
source(paste0(path, "/stage1_hp_script.R"), encoding="utf-8", 
       max.deparse.length = 10000)
print("Script ran successfully!")
print("Done.")
