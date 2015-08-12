updateTimesheet <- function(st, ed, description = NULL,
                            path = "C:/Users/Andrew/Documents/R/dhstimesheet.csv") {
    require(lubridate)
    
    if (file.exists("C:/Users/Andrew/Documents/R/dhstimesheet.csv") == FALSE) {
        first <- data.frame(Start = start, End = end, Duration = (end - start))
        write.csv(first, "C:/Users/Andrew/Documents/R/dhstimesheet.csv",
                  row.names = FALSE)
    } 
    
    else {
        
        if(is.null(st) | is.null(ed)) stop("Start/End time missing!")
        
        duration <- signif(ed - st, 3)
        unit <- attributes(duration)$units
        report <- paste(duration, unit)
        
        message(paste(st, "to", ed))
        message(paste("You worked ", report, "!", sep = ""))
        
        if (is.null(description)) {
            response <- readline("Do you want to enter a description? (y/n): ")
            
            while (response %in% c("y", "n") == FALSE) {
                response <- readline("Do you want to enter a description? (y/n): ")
            } 
            
            if(response == "y") {
                description <- readline("Please enter a description: ")
            
            } else {
                description <- "No description entered."
                
            }
        }
        
        dat <- data.frame(Start = as.character(st), End = as.character(ed), 
                          Duration = as.numeric(duration), 
                          Unit = unit, Description = description)
        
        sheet <- read.csv(file = path)
        dat <- rbind(sheet, dat)
        write.csv(dat, file = path, row.names = FALSE)
    }
}

fixLastRow <- function(editOrDelete = c("delete", "edit"),
                        mypath = "C:/Users/Andrew/Documents/R/dhstimesheet.csv") {
    
    if (!(editOrDelete %in% c("delete", "edit"))) stop("Wait, what am I doing?")
    
    dat <- read.csv(mypath)
    row <- nrow(dat)
    
    message("Row being removed:")
    print(dat[row, ])
    dat <- dat[-row, ]
    write.csv(dat, file = mypath, row.names = FALSE)
    
    if (editOrDelete == "edit") {
        updateTimesheet(start, end, path = mypath
    }
}

if (exists("start", mode = "numeric") == FALSE) start <- lubridate::now()