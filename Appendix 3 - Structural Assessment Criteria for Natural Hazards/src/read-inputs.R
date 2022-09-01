
# automatic inputs
# this version of inputting data to the program guides users like an installer wizard with dialog menus
auto_inputs <- function(){
  
  # input data file
  
  dlg_message("Click OK to select a data file")
  file.name <- dlg_open(title = "Select one xlsx or csv file", 
                        filters = dlg_filters["R",], default = "inputs/*")$res
  
  if(!grepl("xls|csv", file.name, ignore.case = TRUE)){
    dlg_message("You must select an xlsx or csv file. Try again")
    file.name <- dlg_open(title = "Try again, must be an xlsx or csv file", 
                          filters = dlg_filters["R",], default = "inputs/*")$res
  }
  
  if(grepl("xls|xlsx", file.name, ignore.case = TRUE)) {
    df <- read_excel(file.name)
  } else {
    df <- read.csv(file=file.name)
  }
  
  # find right columns for MRI and y
  
  v.n <- colnames(df)
  n.n <- length(v.n)
  message <- paste0("1:",v.n[1])
  for(c in 2:n.n) { message <- paste0(message,"  ",c,":",v.n[c])}
  
  if(!("MRI" %in% v.n)){
    MRI.col <- as.numeric(dlgInput(message=paste("Which column number is MRI? \n",message))$res)
    colnames(df)[MRI.col] <- "MRI"
  }
  if(!("y" %in% v.n)){
    y.col <- as.numeric(dlgInput(message=paste("Which column number is y? \n",message))$res)
    colnames(df)[y.col] <- "y"
  }
  
  # set y-axis label text
  
  y.label <- "Exceedance"
  y.label <- dlgInput(message=paste("Enter unquoted label for y-axis or OK for default."),
                      default=y.label)$res
  
  # set title
  
  Title <- dlgInput(message=paste("Enter plot title"), default="Exceedance Projection")$res
  
  # set MRI values to be projected
  
  MRI.new <- df$MRI[which(is.na(df$y))]
  df <- df[which(!is.na(df$y)),]
  
  MRI.new.chr <- toString(MRI.new)
  
  MRI.new.chr <- dlgInput(message="Enter comma separated MRI values to be projected.",default=MRI.new.chr)$res
  
  MRI.new <- unlist(lapply(strsplit(MRI.new.chr, ",", TRUE), as.numeric))
  MRI.all <- c(df$MRI,MRI.new)
  
  # pack output list
  
  out <- list()
  out$MRI.new <- MRI.new
  out$df <- df
  out$MRI.all <- MRI.all
  out$y.label <- y.label
  out$Title <- Title
  
  return(out)
}

# manual inputs
# this version of inputting data to the program is the normal "programmer" way
manual_inputs <- function(file.name, MRI.col, y.col, y.label = "exceedance", Title = "Exceedance Projection", MRI.proj = c(1000, 2475)){
  
  # check input file
  
  if(grepl("xls|xlsx", file.name, ignore.case = TRUE)) {
    df <- read_excel(file.name)
  } else if(grepl("csv", file.name, ignore.case = TRUE)) {
    df <- read.csv(file=file.name)
  } else {
    stop("input filename not valid")
  }
  
  # set columns so they're read correctly in analysis
  
  colnames(df)[MRI.col] <- "MRI"
  colnames(df)[y.col] <- "y"
  
  MRI.new <- MRI.proj
  MRI.all <- c(df$MRI, MRI.new)
  
  # pack output list
  
  out <- list()
  out$MRI.new <- MRI.new
  out$df <- df
  out$MRI.all <- MRI.all
  out$y.label <- y.label
  out$Title <- Title
  
  return(out)
}