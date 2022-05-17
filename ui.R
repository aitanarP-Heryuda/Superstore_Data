dashboardPage(
    skin = "black-light",
    #----- HEADER
    dashboardHeader(
        title = "US Superstore High-Level Dashboard",
        titleWidth = 400
    ),
    
    #----- SIDEBAR
    dashboardSidebar(
      
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("compass",lib = "font-awesome")),
            menuItem("Business Analysis", tabName = "buzzana", icon = icon("clipboard",lib = "font-awesome")),
            menuItem("Dataset", tabName = "data", icon = icon("clone",lib = "font-awesome"))
        )
    ),

    
    #------- BODY

    dashboardBody(
      
      tags$head(tags$style(HTML('
        .skin-black-light .main-header .logo {
          background-color: #FFFFFC;
        }
        .skin-black-light .main-header .logo:hover {
          background-color: #D3EB23;
        }
      '))),
      tabItems(
           #----- PAGE 1 : Overview
           tabItem(
            tabName = "overview",
            fluidRow(
                infoBox(
                  width = 3,
                  color = "green",
                  title =  tags$b("Total Current Customer"),
                  icon  = icon("studiovinari",lib = "font-awesome"),
                  value =  h3(tags$b(length(unique(superstore_data$Customer.ID))))
                  ),
                
                infoBox(
                  width = 3,
                  color = "aqua",
                  title =  tags$b("Total Transaction"),
                  icon  = icon("wpforms",lib = "font-awesome"),
                  value =  h3(tags$b(paste(format(nrow(superstore_data),nsmall = 0,big.mark = ",")))) 
                  
                ),
                infoBox(
                  width = 3,
                  color = "teal",
                  title =  tags$b("Total Sales Volume"),
                  icon  =  icon("money-bill",lib = "font-awesome"),
                  value =  h3(tags$b(paste(format(sum(superstore_data$Sales),nsmall = 0,big.mark = ","),"$"))) 
                  
                ),
                infoBox(
                  width = 3,
                  color = "navy",
                  title = tags$b("Total Profit"),
                  icon  = icon("percent",lib = "font-awesome"),
                  value = h3(tags$b(paste(format(sum(superstore_data$Profit),nsmall = 0,big.mark = ","),"$"))) 
                  
                )
              ),
            fluidRow(
              box(width = 12,
                  radioButtons(
                  "input_radio",
                  "Choose Customer Segment Category :",
                    choices = unique(superstore_data$Segment),
                    inline = TRUE,
                    width = 500
                       )),
            
                     ),
            fluidRow(
              # --- PLOT 2
              box(width = 6,
                  plotlyOutput("plot_sales")),
              
              # --- PLOT 3
              box(width = 6,
                  plotlyOutput("box_plot_cat"))
            )
            
            
           ),
           
           #PAGE 2 : channel analysis
           tabItem(
             tabName = "buzzana",
                fluidRow(
                  box(width = 12,
                      solidHeader = T,
                      h3(tags$b("Shipping Area Business Map For Each States")),
                      leafletOutput("leaflet", height = 530)),
                ),
             
             ##--- ROW 2 : Plot
             fluidRow(
               box(width = 12,
                   selectInput(
                     inputId = "input_category",
                     label = "Choose Shipping Mode Category",
                     choices = unique(superstore_data$Ship.Mode)
                   ))
             ),
             
             fluidRow(
               # --- PLOT 2
               box(width = 6,
                   plotlyOutput("plot_state_profit")),
               
               # --- PLOT 3
               box(width = 6,
                   plotlyOutput("plot_sales_time"))
             )
           ),
           
           #PAGE 3 : channel analysis
           tabItem(tabName = "data",
                   fluidRow(
                     box(
                       width = 12,
                       title = "US Superstore Detailed Data",

                       DT::dataTableOutput(outputId = "dataset_table",)
                      
                       )
                    )
                  )
                )
              )
    
  )
 

