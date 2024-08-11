################################################################################
print("Stage 1...")
################################################################################
# File path
path = 'your/path/here'
setwd(path)
# Setting connection
print("Setting up connection...")
# Create a folder to save the logs if running from this file.
con <- file(paste0(path, "/logs/stage1_final_log - ", Sys.Date(), ".log"))
print("Producing sink...")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
# Run code
print("Running script...")
source(paste0(path, "/stage1_final_script.R"), encoding="utf-8", 
       max.deparse.length = 10000)
print("Script ran successfully!")
print("Done.")
