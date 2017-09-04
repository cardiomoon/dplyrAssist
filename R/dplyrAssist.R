#' A shiny app for learn dplyr
#'
#' Interactively manipulate a \code{data.frame} or a \code{tibble}. The resulting
#' code will be emitted as an R code.
#'
#' This addin can be used to interactively manipulate a \code{data.frame} or a \code{tibble} using dplyr.
#' The intended way to use this is as follows:
#'
#' 1. Highlight a symbol naming a \code{data.frame} or a \code{tibble} in your R session,
#'    e.g. \code{mtcars},
#' 2. Execute this addin, to interactively manipulate it.
#'
#' When you're done, the code for data manipulation will be emitted
#' at the cursor position.
#'
#' @param df A tibble or a tbl_df or a data.frame to manipulate
#' @param right Optional second data(A tibble or a tbl_df or a data.frame) to join
#'
#' @return A manipulated tibble or NULL
#' @importFrom shiny div selectInput runApp fluidPage tags HTML titlePanel hr fluidRow column
#' @importFrom shiny textInput checkboxInput numericInput conditionalPanel verbatimTextOutput uiOutput h3 actionButton
#' @importFrom shiny validate need renderPrint updateTextInput updateCheckboxInput reactive renderPlot
#' @importFrom shiny updateSelectizeInput renderUI htmlOutput tagList updateNumericInput updateSelectInput imageOutput
#' @importFrom shiny observe br observeEvent renderImage stopApp plotOutput runGadget dialogViewer
#' @importFrom shinyWidgets radioGroupButtons materialSwitch pickerInput
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom stringr str_detect str_replace str_c str_replace_all str_split str_replace
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom utils capture.output
#' @importFrom magrittr "%>%"
#' @importFrom plyr "."
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(nycflights13)
#'
#' \dontrun{
#' dplyrAssist(band_members,band_instruments)
#' dplyrAssist(flights)
#' result<-dplyrAssist(iris)
#' cat(attr(result,"code"))
#' }
dplyrAssist=function(df=NULL,right=NULL){


     if(!isNamespaceLoaded("tidyverse")){
          attachNamespace("tidyverse")
     }

     selectInput3<-function(...,width=100){
        mywidth=paste(width,"px",sep="")
        div(style="display:inline-block;",selectInput(...,width=mywidth))
    }

    context <- getActiveDocumentContext()

    # Set the default data to use based on the selection.
    text <- context$selection[[1]]$text
    defaultData <- text

    if(is.null(df)) {
         if(nzchar(defaultData)) {
              df=defaultData
         } else {
              df="table1"
         }
    }
    if(any(class(df) %in% c("data.frame","tibble","tbl_df"))) {
         mydata=deparse(substitute(df))
    } else if(class(df) =="character") {

        result<-tryCatch(eval(parse(text=df)),error=function(e) "error")
        if(any(class(result) %in% c("data.frame","tibble","tbl_df"))) mydata=df
        else  return(NULL)
    }

    if(is.null(right)) {
         myright=NULL
    } else {
    if(any(class(right) %in% c("data.frame","tibble","tbl_df"))) {
        myright=deparse(substitute(right))
    } else if(class(right) =="character") {

        result<-tryCatch(eval(parse(text=right)),error=function(e) "error")
        if(any(class(result) %in% c("data.frame","tibble","tbl_df"))) myright=right
        else  return(NULL)
    }
    }

      # retValue=runApp(list(
        ui=miniPage(
            tags$head(
                tags$style(HTML("
                                .shiny-output-error-validation {
                                color: green;
                                }
                                "))
                ),

            # Application title
            gadgetTitleBar("Data Wrangling using tidyverse"),

            miniContentPanel(
            # h5("      presented by cardiomoon@gmail.com"),
            # hr(),
            # radioGroupButtons("radio1","Please Select Data or",
            #                   choices=c("flights","band_members","table1","table2","table3",
            #                             "table4a","table4b","table5","stocks","treatment","mpg","who","iris","excess"),
            #                   selected="",
            #                   status="info"),
            fluidRow(
                column(6,textInput("mydata","Enter Data Name",value=mydata)),
                column(6,
                       radioGroupButtons("showOption","Show Data",
                                         choices=c("data structure","data table","No thanks"),
                                         selected="No thanks"))),
            #materialSwitch("showData1","Show Data",status="info"),
            conditionalPanel(condition="input.showOption=='data table'",
                             dataTableOutput("data1")),
            conditionalPanel(condition="input.showOption=='data structure'",
                             verbatimTextOutput("text")
            ),
            br(),
            materialSwitch("showRelatedData","Show the 2nd Data",status="info"),
            conditionalPanel(condition='input.showRelatedData==true',
                             uiOutput("SecondData")),



            hr(),
            h3("What can I do for you ?"),
            fluidRow(
                column(3,
                       selectInput("operation","Please Select ..." ,
                                   choices=c("Make tibble",
                                             "Reshaping Data",
                                             "Subset Observation(Rows)",
                                             "Subset Variables(Columns)",
                                             "Summarise Data",
                                             "Make New Variables",
                                             "Group Data",
                                             "Combine Data Sets",
                                             "Visualize Data"),
                                   size=9,selectize=FALSE)
                ),
                column(9,
                       fluidRow(
                           column(3,
                                  selectInput("dfunction","Select Function",
                                              choices=c("gather","spread","separate","unite",
                                                        "arrange","rename"),selected="",size=9,selectize=FALSE)
                           ),
                           conditionalPanel(condition='input.dfunction=="select"',
                                            column(4,
                                                   selectInput("helper","Select Helper Function",
                                                               choices=c("contains","starts_with","ends_with",
                                                                         "everything","matches",
                                                                         "num_range","one_of"),
                                                               multiple=TRUE,
                                                               size=9,selectize=FALSE)
                                            )),
                           conditionalPanel(condition='input.operation=="Make New Variables"',
                                            column(4,
                                                   selectInput("window","Select Wndow Function",
                                                               choices=c("lead","lag",
                                                                         "dense_rank","min_rank","percent_rank",
                                                                         "row_number","ntile","between","cume_dist",
                                                                         "cumall","cumany","cummean",
                                                                         "cumsum","cummax","cummin","cumprod",
                                                                         "pmax","pmin"),
                                                               multiple=TRUE,
                                                               size=9,selectize=FALSE)
                                            )),
                           column(4,
                                  selectInput("picker1","Select Columns",
                                              choices="",selectize=FALSE,size=4),
                                  selectInput("picker2","Selected Columns",
                                              choices="",selectize=FALSE,size=4)
                                  #,verbatimTextOutput("test1")
                           ),
                           # conditionalPanel(condition='input.operation=="Combine Data Sets"',
                           #                  column(4,selectInput("relatedData","related Data",
                           #                                       choices='',selected="",size=4,selectize=FALSE))),
                           column(4,uiOutput("others"))),
                       column(8,
                              aceEditor("Rcode",value="",height="100px",showLineNumbers=FALSE)),
                       column(2,
                              actionButton("add","Add R code")
                       ),
                       column(2,
                              actionButton("reset","Reset R code")
                       ))),
            fluidRow(
                column(6,
                       h3("Data Wrangling R Code"),
                       aceEditor("wrangling",value="",height="200px"),
                       #actionButton("run","Run Code"),
                       actionButton("resetWrangling","Reset"),
                       actionButton("save","Save & Exit")
                ),
                column(5,
                       uiOutput("hint"))),
            verbatimTextOutput("result1"),
            uiOutput("result")
            #,verbatimTextOutput("test2")
            #,plotOutput("testplot")

            #,uiOutput("helpFunction")



            # Sidebar with a slider input for number of bins

        )
        )
        server=function(input,output,session){

            # help_console <- function(topic, format=c("text", "html", "latex", "Rd"),
            #                          lines=NULL, before=NULL, after=NULL) {
            #     format=match.arg(format)
            #     if (!is.character(topic)) topic <- deparse(substitute(topic))
            #     helpfile<-NULL
            #     try(helpfile <- utils:::.getHelpFile(help(topic)))
            #     if(is.null(helpfile)){
            #         cat("No help file about ",topic," is found")
            #     } else {
            #         hs <- capture.output(switch(format,
            #                                     text=tools:::Rd2txt(helpfile),
            #                                     html=tools:::Rd2HTML(helpfile),
            #                                     latex=tools:::Rd2latex(helpfile),
            #                                     Rd=tools:::prepare_Rd(helpfile)
            #         )
            #         )
            #         if(!is.null(lines)) hs <- hs[lines]
            #         hs <- c(before, hs, after)
            #         cat(hs, sep="\n")
            #         invisible(hs)
            #
            #
            #     }
            # }

            putmsg=function(msg="test message"){
                session$sendCustomMessage(type = 'testmessage',message = list( msg))
            }


            test=function(e){
                putmsg("Invalid order. Please fix.")

            }

            selected=c()
            data<-NULL

            observeEvent(input$wrangling,{

                data<<-findData(input$wrangling)
                if(!is.null(data)) updateSelectInput(session,"column",choices=colnames(data))
                if(!is.null(data)) updateSelectInput(session,"picker1",choices=colnames(data))
                if(!is.null(data)) updateSelectInput(session,"picker2",choices="")
                selected<<-c()
            })

            # observeEvent(input$radio1,{
            #     updateTextInput(session,"mydata",value=input$radio1)
            #
            # })

            observeEvent(input$mydata,{

                validate(
                    need(any(class(try(eval(parse(text=input$mydata)))) %in% c("tbl_df","tibble","data.frame")),
                         "Please enter the name of data")
                )
                data1<-get(input$mydata)
                if(!is.null(data1)) {
                    updateSelectInput(session,"column",choices=colnames(data1))
                    updateSelectInput(session,"picker1",choices=colnames(data1))
                    updateSelectInput(session,"picker2",choices="")
                    selected<<-c()
                }
                if(input$wrangling=="") {
                    updateAceEditor(session,"wrangling",value=input$mydata)
                } else if(!str_detect(input$wrangling,"[^[A-Za-z0-9\\s\\_]]")) {
                    updateAceEditor(session,"wrangling",value=input$mydata)
                } else {
                    temp=paste0(input$wrangling,"\n",input$mydata)
                    updateAceEditor(session,"wrangling",value=temp)
                }

                # if(input$mydata=="flights"){
                #     choices2=c("airlines","airports","planes","weather")
                # } else if(input$mydata=="band_members"){
                #     choices2=c("band_instruments","band_instruments2")
                # } else if(input$mydata=="who"){
                #     choices2=c("population")
                # } else choices2=""
                # updateSelectInput(session,"relatedData",choices=choices2)
            })


            # observeEvent(input$radio2,{
            #     values$data2 <-input$radio2
            # })

            observeEvent(input$add,{
                if(input$wrangling=="") updateAceEditor(session,"wrangling",value=input$mydata)
                updateAceEditor(session,"wrangling",
                                value=paste(input$wrangling,"%>%\n",input$Rcode))
                updateAceEditor(session,"Rcode",1)

            })

            observeEvent(input$picker1,{

                 selected<<-c(selected,input$picker1)
                 updateSelectInput(session,"picker1",choices=setdiff(colnames(data),selected))
                 updateSelectInput(session,"picker2",choices=selected)

            })

            observeEvent(input$picker2,{
                # if(!is.null(input$picker2)){
                 selected<<-setdiff(selected,input$picker2)
                 updateSelectInput(session,"picker1",choices=setdiff(colnames(data),selected))
                 updateSelectInput(session,"picker2",choices=selected)
                 #}
            })

            output$test1=renderPrint({

                 input$picker1
                 input$picker2
                 input$mydata
                 input$wrangling

                 # cat("data\n")
                 # str(data)
                 cat("\nselected\n")
                 selected
            })


            observeEvent(input$reset,{
                #updateSelectInput(session,"operation",selected="")

                 mytext=c("key","value","into","col")
                 for(i in 1:length(mytext)) updateTextInput(session,mytext[i],value="")
                 updateTextInput(session,"sep",value="[^[:alnum:]]+")
                 updateTextInput(session,"sep1",value="_")
                 mynumeric=c("n")
                 for(i in 1:length(mynumeric)) updateTextInput(session,mynumeric[i],value="")
                 mycheck=c("na.rm","convert","desc")
                 for(i in 1:length(mycheck)) updateCheckboxInput(session,mynumeric[i],value=FALSE)
                 if(input$dfunction=="separate"){
                      updateSelectInput("extra","extra",choices=c("warn","drop","merge"),selected="warn")
                      updateSelectInput("fill","fill",choices=c("warn","right","left"),selected="warn")
                 }
                 if(input$dfunction=="fill") updateSelectInput("direction","direction",choices=c("down","up"),selected="down")

                 myselect=c("dfunction","helper","window","picker1","picker2")
                 for(i in 1:length(mycheck)) updateSelectInput(session,myselect[i],selected="")


                updateAceEditor(session,"Rcode",1)
            })

            observeEvent(input$resetWrangling,{
                updateAceEditor(session,"wrangling",value=input$mydata)
                #updateAceEditor(session,"wrangling",value=1)
            })


            observeEvent(input$dfunction,{
                updateAceEditor(session,"Rcode",value=makeRcode())
            })

            observeEvent(input$picker1,{
                updateAceEditor(session,"Rcode",value=makeRcode())
            })
            observeEvent(input$picker2,{
                 updateAceEditor(session,"Rcode",value=makeRcode())
            })

            observeEvent(input$helper,{
                updateAceEditor(session,"Rcode",value=makeRcode())
            })
            observeEvent(input$window,{
                updateAceEditor(session,"Rcode",value=makeRcode())
            })
            observeEvent(input$desc,{
                updateAceEditor(session,"Rcode",value=makeRcode())
            })
            # observeEvent(input$relatedData,{
            #     updateAceEditor(session,"Rcode",value=makeRcode())
            # })

            observeEvent(input$geom,{
                updateAceEditor(session,"Rcode",value=makeRcode())
            })

            observeEvent(input$key,{
                temp=makeRcode()
                if(input$key!=""){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",key='",input$key,"')")
                }
                if(input$value!=""){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",value='",input$value,"')")
                }
                if(input$na.rm==TRUE){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",na.rm=TRUE)")
                }
                updateAceEditor(session,"Rcode",value=temp)

            })

            observeEvent(input$value,{
                temp=makeRcode()
                if(input$key!=""){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",key='",input$key,"')")
                }
                if(input$value!=""){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",value='",input$value,"')")
                }
                if(input$na.rm==TRUE){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",na.rm=TRUE)")
                }
                updateAceEditor(session,"Rcode",value=temp)
            })
            observeEvent(input$na.rm,{
                temp=makeRcode()
                if(input$key!=""){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",key='",input$key,"')")
                }
                if(input$value!=""){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",value='",input$value,"')")
                }
                if(input$na.rm==TRUE){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",na.rm=TRUE)")
                }
                updateAceEditor(session,"Rcode",value=temp)
            })
            observeEvent(input$n,{
                updateAceEditor(session,"Rcode",value=makeRcode())
            })

            observeEvent(input$y.b,{
                updateAceEditor(session,"Rcode",value=makeRcode())
            })

            makeRcode=reactive({

                 input$picker1
                 input$picker2

                ret=""
                if(!is.null(input$dfunction)){
                    ret=paste0(ret,input$dfunction,"(")
                    if(input$dfunction=="ggplot"){
                        ret=paste0(ret,"aes(")
                    }
                    if(input$dfunction=="top_n"){
                        ret=paste0(ret,"n=",input$n)
                    }
                    if(str_detect(input$dfunction,"join")){
                        if(!is.null(input$mydata2)) {
                            ret=paste0(ret,input$mydata2)
                        }
                    }
                    if(!is.null(selected)){
                        x<-selected
                        if(str_detect(input$dfunction,"join")) temp=paste0("'",x,"'")
                        else temp<-str_replace(x,"^[0-9]+",paste0("`",x,"`"))
                        choices<-str_c(temp,collapse=",")
                        if(input$dfunction=="arrange"){
                            if(input$desc) {
                                x<-selected
                                temp<-str_replace(x,"^[0-9]+",paste0("`",x,"`"))
                                temp=paste0("desc(",x,")")
                                choices<-str_c(temp,collapse=",")
                            }
                            ret=paste0(ret,choices)
                        }
                        else if(input$dfunction=="top_n"){
                            ret=paste0(ret,",",choices)
                        } else if(str_detect(input$dfunction,"join")){
                            if(!is.null(input$mydata2)){
                                if(input$y.b==""){
                                  ret=paste0(ret,",by=c(",choices,")")
                                } else{
                                    ret=paste0(ret,",by=c(",choices,"='",input$y.b,"')")
                                }
                            }
                        } else ret=paste0(ret,choices)
                    }
                    if((input$dfunction=="select") & (!is.null(input$helper))) {
                        x<-input$helper
                        every=FALSE
                        if("everything" %in% x) {
                            every=TRUE
                            x=x[-grep("everything",x)]
                        }
                        if(length(x)>0){
                            choices<-x %>% paste0("('')") %>% str_c(collapse=",")
                            if(!is.null(selected)) ret=paste0(ret,",",choices)
                            else ret=paste0(ret,choices)
                        }
                        if(every) ret=paste0(ret,",everything()")
                    }
                    if((input$operation=="Make New Variables") & (!is.null(input$dfunction)) &
                       (!is.null(input$window))) {
                        x<-input$window

                        choices<-x %>% paste0("()") %>% str_c(collapse=",")
                        if(!is.null(selected)) ret=paste0(ret,",",choices)
                        else ret=paste0(ret,choices)

                    }
                    if(input$dfunction=="count"){
                         if(!is.null(input$wt)) {
                              if(input$wt!="") ret=paste0(ret,",wt=",input$wt)
                         }
                    }


                    if(input$dfunction=="ggplot") ret=paste0(ret,")")
                    ret=paste0(ret,")")
                    if((input$dfunction=="ggplot")&(!is.null(input$geom))){
                        ret=paste0(ret," + ")
                        temp=paste0(input$geom,"()")
                        temp=str_c(temp,collapse=" + ")
                        ret=paste0(ret,temp)
                    }


                }
                ret
            })



            output$data1=renderDataTable({
                get(input$mydata)

            })
            output$data2=renderDataTable({
                get(input$mydata2)
            })

            output$text=renderPrint({
                #print(input$mydata)

                cat(input$mydata,"\n")

                print(get(input$mydata))

            })
            output$text2=renderPrint({
                cat(input$mydata2,"\n")
                data2<-get(input$mydata2)
                data2

            })

            observeEvent(input$operation,{
                if(input$operation=="Make tibble"){
                    updateSelectInput(session,"dfunction",
                                      choices=c("as_tibble","tbl_df","print"))
                } else if(input$operation=="Reshaping Data"){
                    updateSelectInput(session,"dfunction",
                                      choices=c("gather","spread","complete","fill","separate","unite",
                                                "arrange","rename"))
                } else if(input$operation=="Subset Observation(Rows)"){
                    updateSelectizeInput(session,"dfunction",
                                         choices=c("filter","distinct","sample_frac","sample_n",
                                                   "slice","top_n"))
                } else if(input$operation=="Subset Variables(Columns)"){
                    updateSelectInput(session,"dfunction",
                                      choices=c("select"))
                } else if(input$operation=="Summarise Data"){
                    updateSelectInput(session,"dfunction",
                                      choices=c("summarise","summarise_all",
                                                "summarise_if","summarise_at","count"))
                } else if(input$operation=="Make New Variables"){
                    updateSelectInput(session,"dfunction",
                                      choices=c("mutate","mutate_all","mutate_at","mutate_if",
                                                "transmute","transmute_all","transmute_at","transmute_if"))
                } else if(input$operation=="Group Data"){
                    updateSelectInput(session,"dfunction",
                                      choices=c("group_by","ungroup"))
                } else if(input$operation=="Combine Data Sets"){
                    updateSelectInput(session,"dfunction",
                                      choices=c("left_join","right_join",
                                                "inner_join","full_join",
                                                "semi_join","anti_join",
                                                "bind_rows","bind_cols"))

                    # if(input$mydata=="flights"){
                    #     choices2=c("airlines","airports","planes","weather")
                    # } else if(input$mydata=="band_members"){
                    #     choices2=c("band_instruments","band_instruments2")
                    # } else {
                    #     choices2=c("population")
                    # }
                    #
                    # updateSelectInput(session,"relatedData","related Data",choices=choices2)
                } else if(input$operation=="Visualize Data"){
                    updateSelectInput(session,"dfunction",
                                      choices=c("ggplot"))
                }
                # updateSelectInput(session,"column",choices=c(),selected=NULL)
                #
                # updateSelectInput(session,"column",choices=colnames(get(input$mydata)),selected=NULL)
            })

            output$helpFunction=renderUI({
                if(!is.null(input$dfunction)){
                    htmlOutput("helpFunction2")
                }
            })
            # output$helpFunction2=renderPrint({
            #     help_console(input$dfunction,"html")
            # })





            output$others=renderUI({


                if(!is.null(input$dfunction)){
                    tagList(
                        if(input$dfunction=="gather") textInput("key","key"),
                        if(input$dfunction=="gather") textInput("value","value"),
                        if(input$dfunction=="gather") checkboxInput("na.rm","na.rm"),
                        if(input$dfunction=="separate") textInput("into","into"),
                        if(input$dfunction=="separate") textInput("sep","sep",value="[^[:alnum:]]+"),
                        if(input$dfunction=="separate") checkboxInput("convert","convert"),
                        if(input$dfunction=="separate") selectInput3("extra","extra",choices=c("warn","drop","merge")),
                        if(input$dfunction=="separate") selectInput3("fill","fill",choices=c("warn","right","left")),
                        if(input$dfunction=="arrange") checkboxInput("desc","descending order"),
                        if(input$dfunction=="count")
                             selectInput3("wt","wt",choices=c("",colnames(data))),


                        if(input$dfunction %in% c("unite","rename")) textInput("col","new column name"),
                        if(input$dfunction=="unite") textInput("sep2","sep",value="_"),
                        if(input$dfunction=="fill") selectInput("direction","direction",choices=c("down","up")),
                        if(input$dfunction=="ggplot") pickerInput("geom","Select Geoms",
                                                                  choices=c("geom_point","geom_col","geom_bar","geom_line","stat_smooth"),
                                                                  options = list(`selected-text-format` = "count > 3"),
                                                                  multiple = TRUE),
                        if(input$dfunction=="top_n") numericInput("n","n",value=10),
                        if(str_detect(input$dfunction,"join")) {
                            df2=eval(parse(text=input$mydata2))
                            if(input$mydata2!=""){
                                selectInput("y.b","y.b",choices=c("",colnames(df2)))
                            }
                        }


                    )
                }

            })

            output$hint=renderUI({
                hintFunctions=c("gather","spread","separate","unite","left_join",
                                "right_join","anti_join","full_join","inner_join",
                                "semi_join","filter","select","group_by","mutate","summarise",
                                "bind_rows","bind_cols")
                if(!is.null(input$dfunction)){
                    if(input$dfunction %in% hintFunctions){
                        imageOutput("hintfig")
                    }
                }
            })

            output$hintfig=renderImage({
                temp=input$dfunction
                filename=paste0("man/figures/",temp,".png")
                list(src=filename,
                     width="100%")
            },deleteFile=FALSE)

            makeString=function(x){
                x1<-x %>%
                    str_replace_all("'","") %>%
                    str_replace_all("\"","") %>%
                    str_split(",",n=Inf) %>%
                    unlist() %>%
                    str_replace(".+",paste0("'",.,"'")) %>%
                    str_c(collapse=",")
                paste0("c(",x1,")")
            }

            makeString2=function(x){
                str_replace(x,"[^[1-9]+]",paste0("'",x,"'"))
            }

            observeEvent(input$into,{

                makeSeparateCode()
            })
            observeEvent(input$sep,{

                makeSeparateCode()
            })
            observeEvent(input$convert,{

                makeSeparateCode()
            })
            observeEvent(input$extra,{
                makeSeparateCode()
            })
            observeEvent(input$fill,{
                makeSeparateCode()
            })
            makeSeparateCode=function(){
                temp=makeRcode()
                if(input$into!=""){
                    temp=sub(")","",temp,fixed=TRUE)
                    temp=paste0(temp,",into=",makeString(input$into))
                }
                if(input$sep!="[^[:alnum:]]+"){
                    temp=paste0(temp,",sep=",makeString2(input$sep))
                }
                if(input$convert){
                    temp=paste0(temp,",convert=TRUE")
                }
                if(input$extra!="warn"){
                    temp=paste0(temp,",extra='",input$extra,"'")
                }
                if(input$fill!="warn"){
                    temp=paste0(temp,",fill='",input$fill,"'")
                }
                temp=paste0(temp,")")
                updateAceEditor(session,"Rcode",value=temp)
            }

            makeJoinCode=function(){

            }

            observeEvent(input$col,{
                temp=makeRcode()
                temp=sub(")","",temp,fixed=TRUE)
                if(input$col!=""){
                    if(input$dfunction=="rename"){
                        temp=paste0(input$dfunction,"(",input$col,"=",selected)
                    } else{
                        temp=paste0(temp,",col=",input$col)
                    }
                }
                temp=paste0(temp,")")
                updateAceEditor(session,"Rcode",value=temp)
            })
            observeEvent(input$sep2,{
                temp=makeRcode()
                temp=sub(")","",temp,fixed=TRUE)
                if(input$col!=""){
                    temp=paste0(temp,",col=",input$col)
                }
                if(input$sep2!="_") temp=paste0(temp,",sep='",input$sep2,"'")
                temp=paste0(temp,")")
                updateAceEditor(session,"Rcode",value=temp)
            })
            observeEvent(input$direction,{
                temp=makeRcode()
                temp=sub(")","",temp,fixed=TRUE)
                if(input$direction=="up"){
                    temp=paste0(temp,",.direction='",input$direction,"'")
                }
                temp=paste0(temp,")")
                updateAceEditor(session,"Rcode",value=temp)

            })
            observeEvent(input$wt,{
                 temp=makeRcode()
                 updateAceEditor(session,"Rcode",value=temp)
            })

            output$result1=renderPrint({

                #  result<-tryCatch(eval(parse(text=input$wrangling)), error=function(e) return("error"))
                # # if(!is.null(result)) {
                #      if(identical(result,"error")){
                #         putmsg("Invalid code ! Please Fix!")
                #      }

                vcodes=makeValidCode(input$wrangling)
                #cat("vcodes\n",vcodes)
                if(!is.null(vcodes)){

                    kind=codes2kind(vcodes)
                    #    cat("\n\nkind\n",kind)

                    textno=length(grep("text",kind))
                    if(textno>0){
                        plotno=length(grep("plot",kind))
                        # cat("\ntextno=",textno)
                        # cat("\nplotno=",plotno)
                        textcodes=vcodes[grep("text",kind)]
                        eval(parse(text=vcodes))
                        for(i in 1:length(textcodes)){
                            if(!is.na(textcodes[i])){
                                cat(">",textcodes[i],"\n")
                                #invisible(eval(parse(text=textcodes[i])))
                                temp=capture.output(eval(parse(text=textcodes[i])))

                                # res=c(res,codes[i])
                                # temp=capture.output(eval(parse(text=codes[i])))
                                # if(length(temp)==0) temp1=""
                                # else temp1=Reduce(pastelf,temp)
                                temp1=str_c(temp,collape="\n")

                                cat(temp1)

                            }
                        }
                    }
                }
                #}

            })
            output$result=renderUI({


                if(input$wrangling!=""){
                    vcodes=makeValidCode(input$wrangling)


                    if(!is.null(vcodes)){
                        kind=codes2kind(vcodes)
                        textno=length(grep("text",kind))
                        plotno=length(grep("plot",kind))
                        text_list<-NULL
                        plot_list<-NULL
                        plotcodes<-textcodes<-NULL
                        # if(textno>0){
                        #
                        #
                        #
                        #
                        #     # for(i in 1:length(textcodes)){
                        #     #     local({
                        #     #         j<-i
                        #     #
                        #     #         output[[paste0("text",j)]]=renderPrint({
                        #     #             cat(">",textcodes[j],"\n")
                        #     #             eval(parse(text=textcodes[j]))
                        #     #         })
                        #     #
                        #     #
                        #     #     })
                        #     # }
                        #     output$text=renderPrint({
                        #         textcodes=vcodes[grep("text",kind)]
                        #         for(i in 1:length(textcodes)){
                        #             cat("\n>",textcodes[i],"\n")
                        #             print(eval(parse(text=textcodes[i])))
                        #
                        #         }
                        #     })
                        #     text_list=list(
                        #
                        #         verbatimTextOutput("text")
                        #     )
                        # }
                        if(plotno>0) {
                            plotcodes=vcodes[grep("plot",kind)]
                            for(i in 1:length(plotcodes)){
                                local({
                                    l<-i

                                    output[[paste0("plot",l)]]=renderPlot({
                                         eval(parse(text=input$wrangling))
                                         eval(parse(text=plotcodes[l]))
                                    })


                                })
                            }
                            plot_list=lapply(1:plotno,function(m){
                                plotname<-paste0("plot",m)
                                plotOutput(plotname)
                            })
                        }

                        if(plotno>0) do.call(tagList,plot_list)
                        # }
                    }
                }
            })

            output$SecondData=renderUI({

                    tagList(
                        fluidRow(
                            column(6,
                                   # radioGroupButtons("radio2",status="info",
                                   #                   choices=choices2),
                                   textInput("mydata2","2nd data",value=myright)
                                   ),

                            column(6,
                                   radioGroupButtons("showOption2","Show Data",
                                                     choices=c("data structure","data table","No thanks"),
                                                     selected="No thanks"))),
                        conditionalPanel(condition="input.showOption2=='data table'",
                                         dataTableOutput("data2")),
                        conditionalPanel(condition="input.showOption2=='data structure'",
                                         verbatimTextOutput("text2"))
                    )

            })
            # output$test2=renderPrint({
            #     vcodes=makeValidCode(input$wrangling)
            #      for(i in 1:length(vcodes)){
            #          cat(vcodes[i])
            #          print(eval(parse(text=vcodes[i])))
            #      }
            # })
            observe({
                if(input$save >0){
                     if(nzchar(defaultData)) {
                          insertText(text=input$wrangling)
                          stopApp()
                     } else{
                          result <- eval(parse(text=input$wrangling))
                          attr(result,"code") <- input$wrangling
                          stopApp(result)
                     }
                }
            })

            observeEvent(input$done, {

                 if(nzchar(defaultData)) {
                      insertText(text=input$wrangling)
                      stopApp()
                 } else{
                    result <- eval(parse(text=input$wrangling))
                    attr(result,"code") <- input$wrangling
                    stopApp(result)
                 }

            })

        }
               #))
        viewer <- dialogViewer("dplyrAssist", width = 1000, height = 800)
        runGadget(ui, server, viewer = viewer)

}

