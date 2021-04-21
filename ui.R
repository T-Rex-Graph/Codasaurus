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

# This script includes the user-interface definition of the app.

fluidPage(

  ui <- shinyUI(fluidPage(

    sidebarLayout(
      sidebarPanel(
        ######################.
        # Pub Initialization----
        ######################.
        h4("Publication"),
        selectInput(inputId = "pubTyp", label = "Will you be citing the publication inputted manually or with DOI assistance?",
                    c("DOI"="DOI Pub","Manual" = "Manual Pub")),#,"ISBN"="ISBN Input")),
        actionButton("pub_save_btn", "Save Publication Input"),
      ),
      mainPanel(
        fluidRow(
          ####################.
          # Publication Input ----
          ####################.
          column(7,
                 h4("Publication Boxes"),
                 uiOutput("pub_ui")),
          ####################.
          # Pub Output----
          ####################.
          column(4,
                 h4("Publication Output"),
                 htmlOutput("pub_out")),
        )
      ),
    ),







    sidebarLayout(
    sidebarPanel(
      ######################.
      # Element Buttons----
      ######################.
      h4("Elements"),
      #adds a new element textbox & selection
      actionButton("ele_add_btn", "Add Element"),
      #removes last element textbox & selection
      actionButton("ele_rm_btn", "Remove Last Element"),
      #save all element textbox & selection inputs into a list
      actionButton("ele_save_btn", "Save Element List"),
      #prints the number of elements in ui sidebar
      textOutput("counterElement")

 ),
 mainPanel(
   fluidRow(
     ####################.
     # Element Input ----
     ####################.
     column(7,
            h4("Element Boxes"),
            uiOutput("element_ui")),
     ####################.
     # Element Output----
     ####################.
     column(4,
            h4("Element Output"),
            htmlOutput("element_list"))
   ))
 ),


 sidebarLayout(
   sidebarPanel(
     ######################.
     # Definition Buttons----
     ######################.
     h4("Definitions"),
     #adds a new textbox & selection
     actionButton("def_add_btn", "Add Defintion"),
     #removes last textbox & selection
     actionButton("def_rm_btn", "Remove Last Defnition"),
     #save all textbox & selection inputs into a list
     actionButton("def_save_btn", "Save Defnition List"),
     #prints the number of definitions in ui sidebar
     textOutput("counterDef")

   ),
   mainPanel(
     fluidRow(
       ####################.
       # Definition Input ----
       ####################.
       column(7,
              h4("Definition Boxes"),
              uiOutput("def_ui")),
       ####################.
       # Definition Output----
       ####################.
       column(4,
              h4("Definition Output"),
              htmlOutput("def_list"))
     ))
 ),

 sidebarLayout(
   sidebarPanel(
     ######################.
     # Model Initialization----
     ######################.
     h4("Models"),
     #adds a new model textbox
     actionButton("model_add_btn", "Add Model"),
     #Remove Lastmodel textbox
     actionButton("model_rm_btn", "Remove Last Model"),
     #save all model textbox inputs into a list
     actionButton("model_save_btn", "Save Model Input"),
     #prints the number of models in ui sidebar
     textOutput("counterModels")


   ),
   mainPanel(
     fluidRow(
       ####################.
       # Model Input ----
       ####################.
       column(7,
              h4("Model Boxes"),
              uiOutput("model_ui")),
       ####################.
       # Model Output----
       ####################.
       column(4,
              h4("Model Output"),
              htmlOutput("model_list")),
     )
   )
 ),


 sidebarLayout(
    sidebarPanel(
       ######################.
       # Relationship Initialization----
       ######################.
       h4("Element Relationships"),
       #adds a new UI
       actionButton("rel_add_btn", "Add Relationship"),
       #Remove LastUI
       actionButton("rel_rm_btn", "Remove Last Relationship"),
       #save all UI inputs into a list
       actionButton("rel_save_btn", "Save Relationship Input"),
       #prints the number of UI Outputs in ui sidebar
       textOutput("counterRelate")


    ),
    # displays the input elements
    mainPanel(
       fluidRow(
          ####################.
          # Relationship Input ----
          ####################.
          column(7,
                 h4("Element Relationship Boxes"),
                 uiOutput("rel_ui")),
          ####################.
          # Relationship Output----
          ####################.
          column(4,
                 h4("Element Relationship"),
                 htmlOutput("rel_list")),
       )
    )
 ),

 sidebarLayout(
    sidebarPanel(
       ######################.
       # Author Initialization----
       ######################.
       h4("Authors"),
       h6("Please input authors in desired order"),
       #adds a new UI
       actionButton("auth_add_btn", "Add Author"),
       #remove last UI
       actionButton("auth_rm_btn", "Remove Last Author"),
       #save all UI inputs into a list
       actionButton("auth_save_btn", "Save Author Input"),
       #prints the number of UI Outputs in ui sidebar
       textOutput("counterAuthor")


    ),
    mainPanel(
       fluidRow(
          ####################.
          # Author Input ----
          ####################.
          column(7,
                 h4("Author Boxes"),
                 uiOutput("auth_ui")),
          ####################.
          # Author Output----
          ####################.
          column(4,
                 h4("Author Output"),
                 htmlOutput("auth_list")),
       )
    )
 ),




 sidebarLayout(
   sidebarPanel(
     ######################.
     # Theory Buttons----
     ######################.
     h4("Theory Buttons"),
     actionButton("theo_add_btn", "Add Theory"),
     actionButton("theo_rm_btn", "Remove Last Theory"),
     actionButton("theo_save_btn", "Save Defnition List"),
     textOutput("counterTheory")

   ),
   mainPanel(
     fluidRow(
       ####################.
       # Theory Input ----
       ####################.
       column(7,
              h4("Theory Boxes"),
              uiOutput("theory_ui")),
       ####################.
       # Theory Output----
       ####################.
       column(4,
              h4("Theory Output"),
              htmlOutput("theory_list"))
     ))
 ),

 sidebarLayout(
   sidebarPanel(
     ######################.
     # Download Buttons----
     ######################.
     h4("Save Code"),
     downloadButton("downloadData","Download"),
   ),
   mainPanel(
     fluidRow(
     ))
 ),


  )#Close fluidpage()
  )#Close shinyUI()


)#CLOSES fluidPage
