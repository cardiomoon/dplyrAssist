# require(tidyverse)
# require(stringr)

#' Check the validity of code
#'
#'@param temp A character string to check validity
checkInValid=function(temp){
    res=c()

    for(i in 1:length(temp)){
         result<-c()
         result<-tryCatch(eval(parse(text=temp[i])),
                          error=function(e) return("error"),
                          warning=function(w) return("warning"))

         if(!is.null(result)){
             if(identical(result,"error")){
                res=i
                break
             }
         }
    }
    res
}

#' Make valid R code
#'
#' @param temp A character vector to make a valid code
makeValid=function(temp){
   no=checkInValid(temp)
   no
   if(!is.null(no)) {
       result=c()
       if(no>1) result=temp[1:(no-1)]
       if(no<length(temp)) result=c(result,paste0(temp[no],temp[no+1]))
       if(no<(length(temp)-1)) result=c(result,temp[(no+2):length(temp)])

       (temp=result)
       if(!is.null(temp)) result=makeValid(temp)
   } else{
       result=temp
   }
   result
}


#' Make valid R code
#'
#' @param codes A character vector to make a valid code
makeValidCode=function(codes){
    temp=unlist(stringr::str_split(codes,"\n"))
    temp=temp[nchar(temp)>0]
    temp
    makeValid(temp)
}


#' Differentiate the R code
#'
#' @param vcodes A character vector to differentiate
codes2kind=function(vcodes){
    result=c()
    if(!is.null(vcodes)) for(i in 1:length(vcodes)){
        temp=eval(parse(text=vcodes[i]))
        if(is.null(temp)) {
            kind="plot"
        } else if("ggplot" %in% class(temp)) {
            kind="plot"
        } else {
            kind="text"
        }
        result=c(result,kind)
    }
    result
}


#' Detect the valid data
#'
#' @param codes A character vector to detect
findData=function(codes){
    result=NULL
    (vcodes=makeValidCode(codes))
    (kind=codes2kind(vcodes))
    (textcodes=vcodes[grep("text",kind)])
    if(length(textcodes)>0){
        eval(parse(text=vcodes))
        for(i in length(textcodes):1){
            result<-eval(parse(text=textcodes[i]))
            if(any((class(result) %in% c("tbl_df","tibble","data.frame")))) break
        }
    }
    result
}



