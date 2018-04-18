library(here)
wd <- getwd()
setwd(here::here("WindAnalysis"))

# Build the package
devtools::document()
devtools::compileAttributes()

# Build Documentation
file.remove("WindAnalysis.pdf")
path <- find.package("WindAnalysis")
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(path)))


# Clear up
setwd(wd)
rm(path, wd)
