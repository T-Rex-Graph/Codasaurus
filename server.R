#Dr. Richard Watson, Dr. Xia Zhao, Yuanyuan Song, Nathaniel Kelley
#Author: Nathaniel Kelley
#Nathaniel Kelley is a research assistant at the University of Georgia, Terry College of Business (2020-2021) and the author of the first version of Codasaurus.
#Author email: nkelley1998@gmail.com
#Sponsor email: rwatson@uga.edu
#T-Rex Research team at the University of Georgia, Terry College of Business

#Copyright 2021 University of Georgia, Terry College of Business

#This file is part of Codasaurus.

#Codasaurus is free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#Codasaurus is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License
#along with Codasaurus.  If not, see <https://www.gnu.org/licenses/>.

############################################################################

#In this script include all the server side functions: plots, reactive objects, etc.

#Defining functions
to_null <- function(x){
  if(x == "") return("NULL")
  else return(paste0("'",x,"'"))
}


## Define a server for the Shiny app
function(input, output, session) {

  # Empty list to store parameters(ie elements, authors,..) text input values
  parameters <- reactiveValues()

  downloadStore <- reactiveValues()

  ############################################################################
  observeEvent(input$pub_save_btn, {
    parameters$pubUUID <- UUIDgenerate(TRUE, 1)
    if (input$pubTyp == "Manual Pub") {
      parameters$manuCit <- to_null(input$manuCit)
      output$pub_out <- renderText(paste("<b>CREATE (:Publication {citation: ",parameters$manuCit,", pubUUID: '",parameters$pubUUID,"'});</b>","<br/><br/>", sep = ""))
      downloadStore$pub_out <- paste("CREATE (:Publication {citation: ",parameters$manuCit,", pubUUID: '",parameters$pubUUID,"'});", sep = "")
    }
    else{
      parameters$doi <- to_null(input$doi)
      parameters$doi_cr_cn <- input$doi
      output$pub_out <- renderText(paste("<b>CREATE (:Publication {DOI: ",parameters$doi,", citation: '",cr_cn(parameters$doi_cr_cn, "text"),"', pubUUID: '",parameters$pubUUID,"'});</b>","<br/><br/>", sep = ""))
      downloadStore$pub_out <- paste("CREATE (:Publication {DOI: ",parameters$doi,", citation: '",cr_cn(parameters$doi_cr_cn, "text"),"', pubUUID: '",parameters$pubUUID,"'});", sep = "")
    }
  })

  #code to generate publication input boxes
  pubBox <- reactive({
    if (input$pubTyp == "Manual Pub") {
      # If the no. of publication boxes previously where more than zero, then
      #save the text inputs in those text boxes
      manuPubInput<- tagList(
        textInput(inputId = "manuCit",
                  label = "Appropriately cite the publication"))
    }
    else{
      autoPubInput<- tagList(
        textInput(inputId = "doi",
                  label = "Enter the publication's DOI"))
    }
  })



  #display publication input boxes
  output$pub_ui <- renderUI({ pubBox() })

###############################################################################

  # Track the number of element input boxes to render
  counterElement <- reactiveValues(n = 0)


  #Track the number of input boxes previously
  prevcountElement <-reactiveValues(n = 0)

  #On click: add element input box
  observeEvent(input$ele_add_btn, {
    counterElement$n <- counterElement$n + 1
    prevcountElement$n <- counterElement$n - 1})

  #On click: remove last element input box
  observeEvent(input$ele_rm_btn, {
    if (counterElement$n > 0) {
      counterElement$n <- counterElement$n - 1
      prevcountElement$n <- counterElement$n + 1
    }

  })

  #On click: store values from element input boxes in a list within parameters list
  observeEvent(input$ele_save_btn, {
    if (counterElement$n > 0) {
      parameters$elementName <- NA
      parameters$elementType <- NA
      parameters$elementRole <- NA
     #parameters$elementUUID <- NA
      for(i in 1:counterElement$n) {
        parameters$elementName[i] <- to_null(input[[paste0("elementName",i)]])
        parameters$elementType[i] <- input[[paste0("elementType",i)]]
        parameters$elementRole[i] <- input[[paste0("elementRole",i)]]
        parameters$elementName_select[i] <- input[[paste0("elementName",i)]]
        #parameters$elementUUID[i] <- UUIDgenerate(TRUE, 1)
      }
        output$element_list <- renderText(paste("<b>CREATE (:Element {elementName: ",parameters$elementName,", elementType: '",parameters$elementType,"', elementRole: ",parameters$elementRole,"});<b/>","<br/><br/>", sep = ""))
        downloadStore$element_list <- (paste("CREATE (:Element {elementName: ",parameters$elementName,", elementType: '",parameters$elementType,"', elementRole: ",parameters$elementRole,"});", sep = ""))
      }
    })

  #Store number of element input boxes
  output$counterElement <- renderPrint(print(counterElement$n))

  #code to generate element input boxes
  elementboxes <- reactive({

    n <- counterElement$n

    if (n > 0) {
      # If the no. of element boxes previously were more than zero, then
      #save the text inputs in those text boxes
      if(prevcountElement$n > 0){

        vals = c()
        typ = c()
        role = c()

        if(prevcountElement$n > n){
          lesscnt <- n
          isInc <- FALSE

        }else{
          lesscnt <- prevcountElement$n
          isInc <- TRUE

        }
        for(i in 1:lesscnt){
          inpid = paste0("elementName",i)
          vals[i] = input[[inpid]]
          inptyp = paste0("elementType",i)
          typ[i] = input[[inptyp]]
          inprole = paste0("elementRole",i)
          role[i] = input[[inprole]]
        }
        if(isInc){
          vals <- c(vals, input[[paste0("elementName",n)]])
          typ <- c(typ, input[[paste0("elementType",n)]])
          role <- c(role, input[[paste0("elementRole",n)]])
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("elementName", i),
                      label = paste0("Element ", i), value = vals[i]),
            selectInput(inputId = paste0("elementType",i), label = paste0("What type of element is element ",i, "?"),
                        c("Construct" = "Construct","Concept" = "Concept","Process"="Process"), selected = typ[i]),
            selectInput(inputId = paste0("elementRole",i), label = paste0("What role does element ",i, " have?"),
                        c("No Role" = "NULL","Moderator" = "'Moderator'","Mediator" = "'Mediator'"), selected = role[i]))
        })

      }else{

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("elementName", i),
                      label = paste0("Element ", i), value = "New element"),
            selectInput(inputId = paste0("elementType",i), label = paste0("What type of element is element ",i, "?"),
                        c("Construct" = "Construct","Concept" = "Concept","Process"="Process")),
            selectInput(inputId = paste0("elementRole",i), label = paste0("What role does element ",i, " have?"),
                        c("No Role" = "NULL","Moderator" = "'Moderator'","Mediator" = "'Mediator'")))
        })
      }

    }

  })



  #display element input boxes
  output$element_ui <- renderUI({ elementboxes() })

  ############################################################################

  # Track the number of input boxes to render
  counterDef <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountDef <-reactiveValues(n = 0)

  #On click: add  input box
  observeEvent(input$def_add_btn, {
    counterDef$n <- counterDef$n + 1
    prevcountDef$n <- counterDef$n - 1})


  #On click: remove last input box
  observeEvent(input$def_rm_btn, {
    if (counterDef$n > 0) {
      counterDef$n <- counterDef$n - 1
      prevcountDef$n <- counterDef$n + 1
    }

  })

  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$def_save_btn, {
    if (counterDef$n > 0) {
      parameters$defEle <- NA
      parameters$defFull <- NA
      parameters$defUUID <- NA
      for(i in 1:counterDef$n) {
#       parameters$defName[i] <- input[[paste0("defName",i)]]
        parameters$defFull[i] <- to_null(input[[paste0("defFull",i)]])
        parameters$defEle[i] <- to_null(input[[paste0("defEle",i)]])
        parameters$defUUID[i] <- UUIDgenerate(TRUE,1)
      }

  #    , modelUUID: '",parameters$modelUUID,"'});</b>"

      if(input$pubTyp == "Manual Pub"){
        output$def_list <- renderText(paste("<b>CREATE (:Definition {DefinitionName: ",parameters$defEle,", definition: ",parameters$defFull, ", defUUID: '",parameters$defUUID,"'});","<br/><br/>",
                                            "MATCH (p:Publication {citation: ",parameters$manuCit,"}), (a:Definition {DefinitionName: ",parameters$defEle,"})
                                            <br>CREATE (p)-[r:DEFINES]->(a) <br>RETURN r;","<br/><br/>",
                                            "MATCH (a:Element {elementName: ",parameters$defEle,"}), (b:Definition {DefinitionName: ",parameters$defFull,"})
                                            <br>CREATE (a)-[r:DEFINED_AS]->(b) <br>RETURN r;<br/><br/>", sep = ""))

        downloadStore$def_list <- paste("CREATE (:Definition {DefinitionName: ",parameters$defEle,", definition: ",parameters$defFull, ", defUUID: '",parameters$defUUID,"'});
MATCH (p:Publication {citation: ",parameters$manuCit,"}), (a:Definition {DefinitionName: ",parameters$defEle,"})
CREATE (p)-[r:DEFINES]->(a) RETURN r;
MATCH (a:Element {elementName: ",parameters$defEle,"}), (b:Definition {DefinitionName: ",parameters$defFull,"})
CREATE (a)-[r:DEFINED_AS]->(b) RETURN r;", sep = "")






      }
      else{
        output$def_list <- renderText(paste("<b>CREATE (:Definition {DefinitionName: ",parameters$defEle,", definition: ",parameters$defFull, ", defUUID: '",parameters$defUUID,"'});","<br/><br/>",
                                            "MATCH (p:Publication {DOI: ",parameters$doi,"}), (a:Definition {DefinitionName: ",parameters$defEle,"})
                                            <br>CREATE (p)-[r:DEFINES]->(a) <br>RETURN r;","<br/><br/>",
                                            "MATCH (a:Element {elementName: ",parameters$defEle,"}), (b:Definition {DefinitionName: ",parameters$defFull,"})
                                            <br>CREATE (a)-[r:DEFINED_AS]->(b) <br>RETURN r;<br/><br/>", sep = ""))

        downloadStore$def_list <- paste("CREATE (:Definition {DefinitionName: ",parameters$defEle,", definition: ",parameters$defFull, ", defUUID: '",parameters$defUUID,"'});
MATCH (p:Publication {DOI: ",parameters$doi,"}), (a:Definition {DefinitionName: ",parameters$defEle,"})
CREATE (p)-[r:DEFINES]->(a) RETURN r;
MATCH (a:Element {elementName: ",parameters$defEle,"}), (b:Definition {DefinitionName: ",parameters$defFull,"})
CREATE (a)-[r:DEFINED_AS]->(b) RETURN r;", sep = "")



      }
    }
  })

  #Store number of input boxes
  output$counterDef <- renderPrint(print(counterDef$n))

  #code to generate input boxes
  defboxes <- reactive({

    n <- counterDef$n

    if (n > 0) {
      # If the no. of boxes previously where more than zero, then
      #save the inputs in those boxes
      if(prevcountDef$n > 0){

#       dName = c()
        dFull = c()
        dEle = c()

        if(prevcountDef$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountDef$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
#         inpDefName = paste0("defName",i)
#         dName[i] = input[[inpDefName]]
          inpDefFull = paste0("defFull",i)
          dFull[i]=input[[inpDefFull]]
          inpDefEle = paste0("defEle",i)
          dEle[i] = input[[inpDefEle]]


        }
        if(isInc){
#         dName <- c(dName, "Definition Name")
          dFull <- c(dFull, input[[paste0("defFull",n)]])
          dEle <- c(dEle, input[[paste0("defEle",n)]])
#          dEle <- c(dEle, "Element Name")
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
#          textInput(inputId = paste0("defName", i),
#                    label = paste0("Definition ", i), value = dName[i]),
          selectInput(inputId = paste0("defEle", i),
                      label = paste0("What element does Definition ", i," define?"),
                      parameters$elementName_select, selected = dEle[i]),
            textInput(inputId = paste0("defFull", i),
                      label = paste0("What is its definition?"), value = dFull[i])

          )})

      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
#           textInput(inputId = paste0("defName", i),
#                      label = paste0("Definition ", i), value = "Definition Name"),
          selectInput(inputId = paste0("defEle", i),
                      label = paste0("What element does Definition ", i," define?"),
                      parameters$elementName_select),
          textInput(inputId = paste0("defFull", i),
                      label = paste0("What is its definition?"), value = "Definition")

          )})
      }

    }

  })



  #display input boxes
  output$def_ui <- renderUI({ defboxes() })





  ###################################################################




  # Track the number of  input boxes to render
  counterModels <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountModels <-reactiveValues(n = 0)

  #On click: add input box
  observeEvent(input$model_add_btn, {
    counterModels$n <- counterModels$n + 1
    prevcountModels$n <- counterModels$n - 1})

  #On click: remove last  input box
  observeEvent(input$model_rm_btn, {
    if (counterModels$n > 0) {
      counterModels$n <- counterModels$n - 1
      prevcountModels$n <- counterModels$n + 1
    }
  })



  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$model_save_btn, {
    if (counterModels$n > 0) {
      parameters$modelTitle <- NA
      parameters$elementDepict <- c()
      parameters$modelUUID <- NA
      parameters$modelEleOut <- c()
      for(i in 1:counterModels$n) {

        b <- 1
        while(b <= length(input[[paste0("elementDepict",i)]])){
          if(b < length(input[[paste0("elementDepict",i)]])){
            parameters$modelEleOut[i] <- paste0(na.omit(parameters$modelEleOut[i]),"'",input[[paste0("elementDepict",i)]][b],"', " )
            b <- b+1
          }else{
            parameters$modelEleOut[i] <- paste0(na.omit(parameters$modelEleOut[i]),"'",input[[paste0("elementDepict",i)]][b],"'")
            #print(paste0("parameters$modelEleOut[i] is ",parameters$modelEleOut[i]))
            b <- b+1
          }
        }

        parameters$modelTitle[i] <- to_null(input[[paste0("modelTitle",i)]])
        parameters$modelTitle_select[i] <- input[[paste0("modelTitle",i)]]
        parameters$modelUUID[i] <- UUIDgenerate(TRUE, 1)


      if(input$pubTyp == "Manual Pub"){
        output$model_list <- renderText(paste("<b>CREATE (:Model {modelTitle: ",parameters$modelTitle,", modelUUID: '",parameters$modelUUID,"'});</b>","<br/><br/>"
                                              ,"<b>MATCH (p:Publication {citation: ",parameters$manuCit,"}),(m:Model)
                                             <br>WHERE m.modelTitle IN [",parameters$modelTitle,"]
                                             <br>CREATE (p)-[r:CONTAINS]->(g)
                                             <br>RETURN r;</b>","<br/><br/>"
                                              ,"<b>MATCH (m:Model {modelTitle: ",parameters$modelTitle,"}),(e:Element)
                                             <br>WHERE e.elementName IN [",parameters$modelEleOut,"]
                                             <br>CREATE (g)-[r:DEPICTS]->(e)
                                             <br>RETURN r;</b>","<br/><br/>", sep = ""))

       downloadStore$model_list <- paste("CREATE (:Model {modelTitle: ",parameters$modelTitle,", modelUUID: '",parameters$modelUUID,"'});
MATCH (p:Publication {citation: ",parameters$manuCit,"}),(m:Model)
WHERE m.modelTitle IN [",parameters$modelTitle,"]
CREATE (p)-[r:CONTAINS]->(g)
RETURN r;
MATCH (m:Model {modelTitle: ",parameters$modelTitle,"}),(e:Element)
WHERE e.elementName IN [",parameters$modelEleOut,"]
CREATE (g)-[r:DEPICTS]->(e)
RETURN r;", sep = "")
      }
      else{
        output$model_list <- renderText(paste("<b>CREATE (:Model {modelTitle: ",parameters$modelTitle,", modelUUID: '",parameters$modelUUID,"'});</b>","<br/><br/>"
                                              ,"<b>MATCH (p:Publication {DOI: ",parameters$doi,"}),(m:Model)
     <br>WHERE m.modelTitle IN [",parameters$modelTitle,"]
     <br>CREATE (p)-[r:CONTAINS]->(g)
     <br>RETURN r;</b>","<br/><br/>"
                                              ,"<b>MATCH (m:Model {modelTitle: ",parameters$modelTitle,"}),(e:Element)
     <br>WHERE e.elementName IN [",parameters$modelEleOut,"]
     <br>CREATE (g)-[r:DEPICTS]->(e)
     <br>RETURN r;</b>","<br/><br/>", sep = ""))

       downloadStore$model_list <- paste("CREATE (:Model {modelTitle: ",parameters$modelTitle,", modelUUID: '",parameters$modelUUID,"'});
MATCH (p:Publication {DOI: ",parameters$doi,"}),(m:Model)
WHERE m.modelTitle IN [",parameters$modelTitle,"]
CREATE (p)-[r:CONTAINS]->(g)
RETURN r;
MATCH (m:Model {modelTitle: ",parameters$modelTitle,"}),(e:Element)
WHERE e.elementName IN [",parameters$modelEleOut,"]
CREATE (g)-[r:DEPICTS]->(e)
RETURN r;", sep = "")
    }

      }
    }
  })

  #Store number of model input boxes
  output$counterModels <- renderPrint(print(counterModels$n))

  #code to generate model input boxes
  modelBoxes <- reactive({

    n <- counterModels$n

    if (n > 0) {
      # If the no. of model boxes previously were more than zero, then
      #save the text inputs in those text boxes
      if(prevcountModels$n > 0){

        grTitle = c()
        depict = c()
        modLength = c()
        depict_temp = c()
        depict_vector = c()
        b = c()

        if(prevcountModels$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountModels$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpTit = paste0("modelTitle",i)
          grTitle[i] = input[[inpTit]]
          #inpDep = paste0("elementDepict",i)

          b[i] <- 1
          while(b[i] <= length(input[[paste0("elementDepict",i)]])){
            if(b[i] < length(input[[paste0("elementDepict",i)]])){
              depict[i] <- paste0(na.omit(depict[i]),input[[paste0("elementDepict",i)]][b[i]],", " )
              b[i] <- b[i]+1
            }else{
             depict[i] <- paste0(na.omit(depict[i]),input[[paste0("elementDepict",i)]][b[i]])
              b[i] <- b[i]+1
            }
          }
        }
        if(isInc){
          grTitle <- c(grTitle, input[[paste0("modelTitle",n)]])

            c <- 1

            while(c <= length(input[[paste0("elementDepict",n)]])){
              if(c < length(input[[paste0("elementDepict",n)]])){
                depict_temp[n-1] <- paste0(depict_temp[n-1],input[[paste0("elementDepict",n)]][c],", " )
                c <- c+1
                #print(paste0("193: depict_temp = ", depict_temp))
              }else{
                depict_temp[n-1] <- paste0(depict_temp[n-1],input[[paste0("elementDepict",n)]][c])
                #print(paste0("196: depict_temp[n](",n,") is ",depict_temp[n-1]))
                c <- c+1
              }
            }

          depict <- c(depict,depict_temp[n-1])
        }
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("modelTitle", i),
                      label = paste0("Model ", i), value = grTitle[i]),
            selectInput(inputId = paste0("elementDepict",i),
                      label = paste0("What elements does Model ",i, " depict?"),
                      parameters$elementName_select,
                      selected = strsplit(depict[i], ", ")[[1]],
                      multiple = TRUE))
        })

      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("modelTitle", i),
                      label = paste0("Model ", i), value = "New Model"),
            #numericInput(inputId = paste0("modelLength", i),
            #          label = paste0("How many elements are in Model ", i," ?"), value = 0),
            #modelSelectMaker(input[[paste0("modelLength",i)]],parameters$elementName,i))
              selectInput(inputId = paste0("elementDepict",i),
                        label = paste0("What elements does Model ",i, " depict?"),
                        parameters$elementName_select,
                        multiple = TRUE))
        })
      }

    }

  })



  #display model input boxes
  output$model_ui <- renderUI({ modelBoxes() })




  ###########################################################################


  # Track the number of input boxes to render
  counterRelate <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountRelate <-reactiveValues(n = 0)

  #On click: add  input box
  observeEvent(input$rel_add_btn, {
    counterRelate$n <- counterRelate$n + 1
    prevcountRelate$n <- counterRelate$n - 1})

  #On click: remove last input box
  observeEvent(input$rel_rm_btn, {
    if (counterRelate$n > 0) {
      counterRelate$n <- counterRelate$n - 1
      prevcountRelate$n <- counterRelate$n + 1
    }

  })

  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$rel_save_btn, {
    if (counterRelate$n > 0) {
      parameters$ele1 <- NA
      parameters$ele2 <- NA
      parameters$relTyp <- NA
      parameters$grOrig <- NA
      #parameters$relUUID <- NA
      for(i in 1:counterRelate$n) {
        parameters$ele1[i] <- input[[paste0("ele1",i)]]
        parameters$ele2[i] <- input[[paste0("ele2",i)]]
        parameters$desc[i] <- to_null(input[[paste0("desc",i)]])
        parameters$relTyp[i] <- input[[paste0("relTyp",i)]]
        parameters$grOrig[i] <- input[[paste0("grOrig",i)]]
        #parameters$relUUID[i] <- UUIDgenerate(TRUE, 1)
      }
      output$rel_list <- renderText(paste("<b>MATCH (a:Element {elementName: '",parameters$ele1,"'}),(b:Element {elementName: '",parameters$ele2,"'})
    <br>CREATE (a)-[r:RELATES_TO {description:",parameters$desc,", type: '",parameters$relTyp,"', model: '",parameters$grOrig,"'}]->(b)
    <br>RETURN r;</b>","<br/><br/>", sep = ""))
      downloadStore$rel_list <- paste("MATCH (a:Element {elementName: '",parameters$ele1,"'}),(b:Element {elementName: '",parameters$ele2,"'})
CREATE (a)-[r:RELATES_TO {description:",parameters$desc,", type: '",parameters$relTyp,"', model: '",parameters$grOrig,"'}]->(b)
RETURN r;", sep = "")
    }
  })

  #Store number of input boxes
  output$counterRelate <- renderPrint(print(counterRelate$n))

  #code to generate input boxes
  relBoxes <- reactive({

    n <- counterRelate$n

    if (n > 0) {
      # If the no. of boxes previously where more than zero, then
      #save the inputs in those boxes
      if(prevcountRelate$n > 0){

        el1 = c()
        el2 = c()
        descri = c()
        rel_Typ = c()
        gr_Orig = c()

        if(prevcountRelate$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountRelate$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpele1 = paste0("ele1",i)
          el1[i] = input[[inpele1]]
          inpele2 = paste0("ele2",i)
          el2[i]=input[[inpele2]]
          inpdesc = paste0("desc",i)
          descri[i] = input[[inpdesc]]
          inprelTyp = paste0("relTyp",i)
          rel_Typ[i] = input[[inprelTyp]]
          inpgrOrig = paste0("grOrig",i)
          gr_Orig[i] = input[[inpgrOrig]]

        }
        if(isInc){
          el1 <- c(el1, input[[paste0("ele1",n)]])
          el2 <- c(el2, input[[paste0("el2",n)]])
          descri <- c(descri, input[[paste0("desc",n)]])
          rel_Typ <- c(rel_Typ, input[[paste0("relTyp",n)]])
          gr_Orig <- c(gr_Orig, input[[paste0("grOrig",n)]])
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            selectInput(inputId = paste0("ele1",i), label = "Create a relationship between two elements.", parameters$elementName_select,selected = el1[i]),
            selectInput(inputId = paste0("ele2",i), label = "Relates to", parameters$elementName_select,selected = el2[i]),
            textInput(inputId = paste0("desc", i),  label = "Description of the relationship (if applicable)", value = descri[i]),
            selectInput(inputId = paste0("relTyp",i), label = paste0("What type of relationship is this?"),
                        c("Causal" = "Causal","Process [Temporal]" = "Process [Temporal]","Correlation" = "Correlation"), selected = rel_Typ[i]),
            selectInput(inputId = paste0("grOrig",i), label = "What model did this relationship come from?", parameters$modelTitle_select,
                      selected = gr_Orig[i])
          )
        })


      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            selectInput(inputId = paste0("ele1",i), label = "Create a relationship between two elements.", parameters$elementName_select),
            selectInput(inputId = paste0("ele2",i), label = "Relates to", parameters$elementName_select),
            textInput(inputId = paste0("desc", i), label = "Description of the relationship (if applicable)", value = ""),
            selectInput(inputId = paste0("relTyp",i), label = paste0("What type of relationship is this?"),
                        c("Causal" = "Causal","Process [Temporal]" = "Process [Temporal]","Correlation" = "Correlation")),
            selectInput(inputId = paste0("grOrig",i), label = "What model did this relationship come from?", parameters$modelTitle_select)
          )
        })

      }

    }
  })


  #display input boxes
  output$rel_ui <- renderUI({ relBoxes() })








  #############################################################

  # Track the number of input boxes to render
  counterAuthor <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountAuthor <-reactiveValues(n = 0)

  #On click: add  input box
  observeEvent(input$auth_add_btn, {
    counterAuthor$n <- counterAuthor$n + 1
    prevcountAuthor$n <- counterAuthor$n - 1})

  #On click: remove last input box
  observeEvent(input$auth_rm_btn, {
    if (counterAuthor$n > 0) {
      counterAuthor$n <- counterAuthor$n - 1
      prevcountAuthor$n <- counterAuthor$n + 1
    }

  })

  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$auth_save_btn, {
    if (counterAuthor$n > 0) {
      parameters$authFirst <- NA
      parameters$authMiddle <- NA
      parameters$authLast <- NA
      parameters$orcid <- NA
      parameters$authOrder <- NA
      parameters$authUUID <- NA
      for(i in 1:counterAuthor$n) {
        parameters$authFirst[i] <- to_null(input[[paste0("authFirst",i)]])
        parameters$authMiddle[i] <- to_null(input[[paste0("authMiddle",i)]])
        parameters$authLast[i] <- to_null(input[[paste0("authLast",i)]])
        parameters$orcid[i] <- to_null(input[[paste0("orcid",i)]])
        parameters$authOrder[i] <- i
        parameters$authUUID <- UUIDgenerate(TRUE, 1)
      }
      if(input$pubTyp == "Manual Pub"){
        output$auth_list <- renderText(paste("<b>CREATE (:Author {ORCID: ",parameters$orcid,", authorFirst: ",parameters$authFirst,", authorMiddle: ",parameters$authMiddle,", authorLast: ",parameters$authLast,", authUUID: '",parameters$authUUID,"'});","<br/><br/>"
                                             ,"MATCH (p:Publication {citation: ",parameters$manuCit,"}), (a:Author {ORCID: ",parameters$orcid,", authorFirst: ",parameters$authFirst,", authorMiddle: ",parameters$authMiddle,", authorLast: ",parameters$authLast,"})
                                             <br>CREATE (p)-[r:WRITTEN_BY {authorOrder: ",parameters$authOrder,"}]->(a)
                                             <br>RETURN r;</b>","<br/><br/>", sep = ""))
        downloadStore$auth_list <- paste("CREATE (:Author {ORCID: ",parameters$orcid,", authorFirst: ",parameters$authFirst,", authorMiddle: ",parameters$authMiddle,", authorLast: ",parameters$authLast,", authUUID: '",parameters$authUUID,"'});
MATCH (p:Publication {citation: ",parameters$manuCit,"}), (a:Author {ORCID: ",parameters$orcid,", authorFirst: ",parameters$authFirst,", authorMiddle: ",parameters$authMiddle,", authorLast: ",parameters$authLast,"})
CREATE (p)-[r:WRITTEN_BY {authorOrder: ",parameters$authOrder,"}]->(a)
RETURN r;", sep = "")
      }
      else{
        output$auth_list <- renderText(paste("<b>CREATE (:Author {ORCID: ",parameters$orcid,", authorFirst: ",parameters$authFirst,", authorMiddle: ",parameters$authMiddle,", authorLast: ",parameters$authLast,", authUUID: '",parameters$authUUID,"'});","<br/><br/>"
                                             ,"MATCH (p:Publication {DOI: ",parameters$doi,"}), (a:Author {ORCID: ",parameters$orcid,", authorFirst: ",parameters$authFirst,", authorMiddle: ",parameters$authMiddle,", authorLast: ",parameters$authLast,"})
                                             <br>CREATE (p)-[r:WRITTEN_BY {authorOrder: ",parameters$authOrder,"}]->(a)
                                             <br>RETURN r;</b>","<br/><br/>", sep = ""))
       downloadStore$auth_list <- paste("CREATE (:Author {ORCID: ",parameters$orcid,", authorFirst: ",parameters$authFirst,", authorMiddle: ",parameters$authMiddle,", authorLast: ",parameters$authLast,", authUUID: '",parameters$authUUID,"'});
MATCH (p:Publication {DOI: ",parameters$doi,"}), (a:Author {ORCID: ",parameters$orcid,", authorFirst: ",parameters$authFirst,", authorMiddle: ",parameters$authMiddle,", authorLast: ",parameters$authLast,"})
CREATE (p)-[r:WRITTEN_BY {authorOrder: ",parameters$authOrder,"}]->(a)
RETURN r;", sep = "")
      }
    }
  })

  #Store number of input boxes
  output$counterAuthor <- renderPrint(print(counterAuthor$n))

  #code to generate input boxes
  authBoxes <- reactive({

    n <- counterAuthor$n

    if (n > 0) {
      # If the no. of boxes previously where more than zero, then
      #save the inputs in those boxes
      if(prevcountAuthor$n > 0){

        auth_First = c()
        auth_Middle = c()
        auth_Last = c()
        auth_Orcid = c()

        if(prevcountAuthor$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountAuthor$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpAuthFirst = paste0("authFirst",i)
          auth_First[i] = input[[inpAuthFirst]]
          inpAuthMiddle = paste0("authMiddle",i)
          auth_Middle[i]=input[[inpAuthMiddle]]
          inpAuthLast = paste0("authLast",i)
          auth_Last[i] = input[[inpAuthLast]]
          inpOrcid = paste0("orcid",i)
          auth_Orcid[i]=input[[inpOrcid]]
        }
        if(isInc){
          auth_First <- c(auth_First, input[[paste0("authFirst",n)]])
          auth_Middle <- c(auth_Middle, input[[paste0("authMiddle",n)]])
          auth_Last <- c(auth_Last, input[[paste0("authLast",n)]])
          auth_Orcid <- c(auth_Orcid, input[[paste0("orcid",n)]])
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("authFirst", i),
                      label = paste0("Author ", i,"'s First Name"), value = auth_First[i]),
            textInput(inputId = paste0("authMiddle", i),
                      label = paste0("Author ", i,"'s Middle Name/Initial (if applicable)"), value = auth_Middle[i]),
            textInput(inputId = paste0("authLast", i),
                      label = paste0("Author ", i,"'s Last Name"), value = auth_Last[i]),
            textInput(inputId = paste0("orcid", i),
                      label = paste0("Author ", i,"'s ORCID (if applicable)"), value = auth_Orcid[i]),
          )
        })

      }else{
        lapply(seq_len(n), function(i) {
          elementInputs <- tagList(
            textInput(inputId = paste0("authFirst", i),
                      label = paste0("Author ", i,"'s First Name"), value = ""),
            textInput(inputId = paste0("authMiddle", i),
                      label = paste0("Author ", i,"'s Middle Name/Initial (if applicable)"), value = ""),
            textInput(inputId = paste0("authLast", i),
                      label = paste0("Author ", i,"'s Last Name"), value = ""),
            textInput(inputId = paste0("orcid", i),
                      label = paste0("Author ", i,"'s ORCID (if applicable)"), value = ""),
          )
        })
      }

    }

  })



  #display input boxes
  output$auth_ui <- renderUI({ authBoxes() })


  ##########################################################################


  # Track the number of  input boxes to render
  counterTheory <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcountTheory <-reactiveValues(n = 0)

  #On click: add input box
  observeEvent(input$theo_add_btn, {
    counterTheory$n <- counterTheory$n + 1
    prevcountTheory$n <- counterTheory$n - 1})

  #On click: remove last input box
  observeEvent(input$theo_rm_btn, {
    if (counterTheory$n > 0) {
      counterTheory$n <- counterTheory$n - 1
      prevcountTheory$n <- counterTheory$n + 1
    }

  })

  #On click: store values from input boxes in a list within parameters list
  observeEvent(input$theo_save_btn, {
    if (counterTheory$n > 0) {
      parameters$theoName <- NA
      parameters$depict <- NA
      parameters$depictPhrase <- NA
      parameters$downloadDepictPhrase <- NA
      parameters$theoryModelUUID <- NA
      b <- c()
      #parameters$theoUUID <- NA
      for(i in 1:counterTheory$n) {
        parameters$theoName[i] <- to_null(input[[paste0("theoName",i)]])
        #parameters$theoUUID <- UUIDgenerate(TRUE,1 )
        b[i] <- 1
        while(b[i] <= length(input[[paste0("theoDepict",i)]])){
          if(b[i] < length(input[[paste0("theoDepict",i)]])){
            parameters$depict[i] <- paste0(na.omit(parameters$depict[i]),input[[paste0("theoDepict",i)]][b[i]],", ")
            if(input[[paste0("theoDepict",i)]][b[i]]=="Publication"){
              if(input$pubTyp == "Manual Pub"){
                parameters$depictPhrase[i] <- paste0(na.omit(parameters$depictPhrase[i]),"<br>MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (p:Publication {citation: ",parameters$manuCit,"}) <br>CREATE (p)-[r:DRAWS_ON]->(t) <br>RETURN r; ")
                parameters$downloadDepictPhrase[i] <- paste0(na.omit(parameters$downloadDepictPhrase[i]),"MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (p:Publication {citation: ",parameters$manuCit,"})
CREATE (p)-[r:DRAWS_ON]->(t)
RETURN r; ")
                b[i] <- b[i]+1
              }
              else{
                parameters$depictPhrase[i] <- paste0(na.omit(parameters$depictPhrase[i]),"<br>MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (p:Publication {DOI: ",parameters$doi,"}) <br>CREATE (p)-[r:DRAWS_ON]->(t) <br>RETURN r; ")
                parameters$downloadDepictPhrase[i] <- paste0(na.omit(parameters$downloadDepictPhrase[i]),"MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (p:Publication {DOI: ",parameters$doi,"})
CREATE (p)-[r:DRAWS_ON]->(t)
RETURN r; ")
                b[i] <- b[i]+1
              }
            }
            else if(input[[paste0("theoDepict",i)]][b[i]] %in% parameters$modelTitle_select){
              for (c in 1:length(parameters$modelTitle_select)){
                if (input[[paste0("theoDepict",i)]][b[i]] == parameters$modelTitle_select[c]){
                  parameters$theoryModelUUID[i] <- parameters$modelUUID[c]
                  break
                }
              }
              parameters$depictPhrase[i] <- paste0(na.omit(parameters$depictPhrase[i]),"<br>MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (m:Model {id: '",parameters$theoryModelUUID[i],"'}) <br>CREATE (m)-[r:APPLIES]->(t) <br>RETURN r; ")
              parameters$downloadDepictPhrase[i] <- paste0(na.omit(parameters$downloadDepictPhrase[i]),"MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (m:Model {id: '",parameters$theoryModelUUID[i],"'})
CREATE (m)-[r:APPLIES]->(t)
RETURN r; ")
              b[i] <- b[i]+1
            }
            else{
              parameters$depictPhrase[i] <- paste0(na.omit(parameters$depictPhrase[i]),"<br>MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (e:Element {elementName: '",input[[paste0("theoDepict",i)]][b[i]],"'}) <br>CREATE (t)-[r:INFORMS]->(e) <br>RETURN r; ")
              parameters$downloadDepictPhrase[i] <- paste0(na.omit(parameters$downloadDepictPhrase[i]),"MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (e:Element {elementName: '",input[[paste0("theoDepict",i)]][b[i]],"'})
CREATE (t)-[r:INFORMS]->(e)
RETURN r; ")
              b[i] <- b[i]+1
            }
          }else{
            parameters$depict[i] <- paste0(na.omit(parameters$depict[i]),input[[paste0("theoDepict",i)]][b[i]])
            if(input[[paste0("theoDepict",i)]][b[i]]=="Publication"){
              if(input$pubTyp == "Manual Pub"){
                parameters$depictPhrase[i] <- paste0(na.omit(parameters$depictPhrase[i]),"<br>MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (p:Publication {citation: ",parameters$manuCit,"}) <br>CREATE (p)-[r:DRAWS_ON]->(t) <br>RETURN r;")
                parameters$downloadDepictPhrase[i] <- paste0(na.omit(parameters$downloadDepictPhrase[i]),"MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (p:Publication {citation: ",parameters$manuCit,"})
CREATE (p)-[r:DRAWS_ON]->(t)
RETURN r;")
                b[i] <- b[i]+1
              }
              else{
                parameters$depictPhrase[i] <- paste0(na.omit(parameters$depictPhrase[i]),"<br>MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (p:Publication {DOI: ",parameters$doi,"}) <br>CREATE (p)-[r:DRAWS_ON]->(t) <br>RETURN r;")
                parameters$downloadDepictPhrase[i] <- paste0(na.omit(parameters$downloadDepictPhrase[i]),"MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (p:Publication {DOI: ",parameters$doi,"})
CREATE (p)-[r:DRAWS_ON]->(t)
RETURN r;")
                b[i] <- b[i]+1
              }
            }
            else if(input[[paste0("theoDepict",i)]][b[i]] %in% parameters$modelTitle_select){
              for (c in 1:length(parameters$modelTitle_select)){
                if (input[[paste0("theoDepict",i)]][b[i]] == parameters$modelTitle_select[c]){
                  parameters$theoryModelUUID[i] <- parameters$modelUUID[c]
                  break
                }
              }
              parameters$depictPhrase[i] <- paste0(na.omit(parameters$depictPhrase[i]),"<br>MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (m:Model {id: '",parameters$theoryModelUUID[i],"'}) <br>CREATE (m)-[r:APPLIES]->(t) <br>RETURN r;")
              parameters$downloadDepictPhrase[i] <- paste0(na.omit(parameters$downloadDepictPhrase[i]),"MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (m:Model {id: '",parameters$theoryModelUUID[i],"'})
CREATE (m)-[r:APPLIES]->(t)
RETURN r;")
              b[i] <- b[i]+1
            }
            else{
              parameters$depictPhrase[i] <- paste0(na.omit(parameters$depictPhrase[i]),"<br>MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (e:Element {elementName: '",input[[paste0("theoDepict",i)]][b[i]],"'}) <br>CREATE (t)-[r:INFORMS]->(e) <br>RETURN r;")
              parameters$downloadDepictPhrase[i] <- paste0(na.omit(parameters$downloadDepictPhrase[i]),"MATCH (t:Theory {theoryTitle: ",parameters$theoName[i],"}), (e:Element {elementName: '",input[[paste0("theoDepict",i)]][b[i]],"'})
CREATE (t)-[r:INFORMS]->(e)
RETURN r;")
              b[i] <- b[i]+1
            }
          }
        }
      }
        output$theory_list <- renderText(paste0("<b>CREATE (:Theory {theoryTitle: ",parameters$theoName,"}); ", parameters$depictPhrase,"<b/><br/><br/>"))
        downloadStore$theory_list <- paste0("CREATE (:Theory {theoryTitle: ",parameters$theoName,"});",
parameters$downloadDepictPhrase)
    }
  })

  #Store number of input boxes
  output$counterTheory <- renderPrint(print(counterTheory$n))

  #code to generate input boxes
  theoryboxes <- reactive({

    n <- counterTheory$n

    if (n > 0) {
      # If the no. of boxes previously where more than zero, then
      #save the inputs in those boxes
      if(prevcountTheory$n > 0){
        thName = c()
        depict = c()
        depict_temp = c()
        b = c()
        if(prevcountTheory$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcountTheory$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpTheoryName = paste0("theoName",i)
          thName[i] = input[[inpTheoryName]]
          b[i] <- 1
          while(b[i] <= length(input[[paste0("theoDepict",i)]])){
            if(b[i] < length(input[[paste0("theoDepict",i)]])){
              depict[i] <- paste0(na.omit(depict[i]),input[[paste0("theoDepict",i)]][b[i]],", " )
              b[i] <- b[i]+1
            }else{
             depict[i] <- paste0(na.omit(depict[i]),input[[paste0("theoDepict",i)]][b[i]])
              b[i] <- b[i]+1
            }
          }
        }
        if(isInc){
          thName <- c(thName, input[[paste0("theoName",n)]])

            c <- 1

            while(c <= length(input[[paste0("theoDepict",n)]])){
              if(c < length(input[[paste0("theoDepict",n)]])){
                depict_temp[n-1] <- paste0(depict_temp[n-1],input[[paste0("theoDepict",n)]][c],", " )
                c <- c+1
                #print(paste0("193: depict_temp = ", depict_temp))
              }else{
                depict_temp[n-1] <- paste0(depict_temp[n-1],input[[paste0("theoDepict",n)]][c])
                #print(paste0("196: depict_temp[n](",n,") is ",depict_temp[n-1]))
                c <- c+1
              }
            }

          depict <- c(depict,depict_temp[n-1])
        }

        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("theoName", i),
                      label = paste0("Theory ", i), value = thName[i]),
            selectInput(inputId = paste0("theoDepict",i),
                      label = paste0("What item(s) does Theory ",i, " depict/ relate to?"),
                      c("Publication", parameters$modelTitle_select, parameters$elementName_select), #(Please enter each element in single quotes and seprate the elements with a comma)"),
                      selected = strsplit(depict[i], ", ")[[1]],
                      multiple = TRUE))
          })

      }else{
        lapply(seq_len(n), function(i) {
          elementInputs<- tagList(
            textInput(inputId = paste0("theoName", i),
                      label = paste0("Theory ", i), value = "Theory Name"),
            selectInput(inputId = paste0("theoDepict",i),
                      label = paste0("What item(s) does Theory ",i, " depict/ relate to?"),
                      c("Publication", parameters$modelTitle_select, parameters$elementName_select), #(Please enter each element in single quotes and seprate the elements with a comma)"),
                      multiple = TRUE))
          })
      }

    }

  })



  #display input boxes
  output$theory_ui <- renderUI({ theoryboxes() })


###################################################################################################

output$downloadData <- downloadHandler("cypherScript.txt",
content = function(file){
write(c(downloadStore$pub_out, downloadStore$element_list, downloadStore$def_list, downloadStore$model_list, downloadStore$rel_list, downloadStore$auth_list, downloadStore$theory_list), file)
}
)
}
