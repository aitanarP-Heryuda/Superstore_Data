shinyServer(
  function(input, output){
    ##---PAGE 1 INFOBOX DATA
    output$info_box1 <- renderInfoBox(
      paste(format(nrow(superstore_data),nsmall = 0,big.mark = ",")) 
      
    )
    
    #----PAGE 1 GRAFIK SALES
    output$plot_sales <- renderPlotly({
      
        sales_newplot <- superstore_data %>% 
        filter(Segment == input$input_radio) %>% 
        group_by(Order.YQ) %>% 
        summarise(Sales_s = sum(Sales)) %>% 
        ungroup() 
      
        sales_newplot_p <- sales_newplot  %>% 
        mutate(label = glue(
        "Year Quarter: {Order.YQ}
        Sales Volume: {comma(Sales_s)} $"
        ))
        
        plot_segment_2 <- ggplot( data = sales_newplot_p, aes(x = Order.YQ, y = Sales_s))+
        geom_col(aes(fill = Sales_s ,text = label))+
        scale_fill_gradient(low = "black", high = "green")+
        labs(title = "Sales Volume by Customer Segment",
          x = "Year Quarterly",
               y = "Sales Volume")+
        theme_minimal()+
        theme(legend.position = "none")
        
        ggplotly(plot_segment_2,tooltip = "text")

    })
    
    ##----PAGE 1 BOXPLOT
    output$box_plot_cat <- renderPlotly({
      
      box_plot_data <- superstore_data %>% 
        filter(Segment == input$input_radio) %>% 
        group_by(Category,Segment,State) %>% 
        mutate(ProfitPerSales = Profit/Sales) %>% 
        summarise(ProfitPerSales = mean(ProfitPerSales)) %>% 
        ungroup()
      
      box_glue <-  box_plot_data %>% 
        mutate(label = glue(
          "State: {State}
    Profitability: {ProfitPerSales} "
        ))
      
      box_plot_1 <-  ggplot(data = box_glue, mapping = aes(x = Category, y = ProfitPerSales, col=Category,
      ))+
        geom_boxplot(outlier.shape = NA, col = 'blue', fill = "#2debbc")+
        geom_point(alpha = 0) +
        geom_jitter(aes(size = ProfitPerSales,col = Category, text=paste("State:",State, "\n",
                                                                         "Profitability:", format(round(ProfitPerSales,2),nsmall = 2))))+
        labs(
          title = "Profitability For Each Product Category",
          subtitle = "Defined by Jitter for State and Its Profitabilty",
          caption = "Profitability is Mean of Profit/Sales",
          x = "Product Category",
          y = "Profitability",
          col = "Category"
        )+
        theme_minimal()+
        theme(legend.position = "none")
      
      ggplotly(box_plot_1,tooltip = "text")
    })
    
    ##----PAGE 2 LEaFLET
    output$leaflet <- renderLeaflet({
      
     L <-   leaflet(sales_area) %>% 
            addTiles("Map Style") %>% 
            addProviderTiles(providers$OpenStreetMap, group = "Positron") %>%
            
            addAwesomeMarkers(lng = ~Longitude, lat = ~Lattitude,icon = store_icon,
                              clusterOptions = markerClusterOptions() ,
                              options = layersControlOptions(collapsed = TRUE),
                              popup = paste0("Shipping Area    : ",sales_area$State,
                                             "<br>Number of Customer : ",sales_area$Cust_Num,
                                             "<br>Profit Generated       : ", sales_area$Profit_A, " $")) %>% 
            
            
            addCircleMarkers(lng = ~Longitude, lat = ~Lattitude,
                             stroke = FALSE, fillOpacity = 0.1,
                             color = "blue",
                             radius = ~Cust_Num/5,
                             options = layersControlOptions(collapsed = TRUE) )  %>%        
            addMiniMap() %>%  
            addSearchOSM
     L
    })
    
    ##-- DATA BELOW LEAFLET PLOT PROFIT BY STATE
    
    output$plot_state_profit <- renderPlotly({
      
        sales_10_a <- superstore_data %>% 
        filter(Ship.Mode == input$input_category ) %>% 
        group_by(State) %>% 
        summarise(Profit_L = sum(round(Profit),4)) %>% 
        ungroup() %>% 
        arrange(-Profit_L) %>% 
        top_n(10)
        
        sales_10_a_g <- sales_10_a %>% 
          mutate(label = glue(
            "State: {State}
             Profit Volume: {comma(Profit_L)} $"
          ))
        
        plot_profit_L <- ggplot(data = sales_10_a_g, aes(x = reorder(State,Profit_L),y = Profit_L,text = label ))+
          geom_segment(aes(x = reorder(State,Profit_L),xend = reorder(State,Profit_L), y = 0, yend = Profit_L ),color="blue")+
          geom_point(color="black") +  
          coord_flip() +
          labs(title = "Top 10 State With Highest Profitability by Ship Mode",
               x = NULL,
               y = "Profit Volume") +
          theme_minimal()
        
        ggplotly(plot_profit_L, tooltip = "text")
    })
    
    ##-- DATA BELOW LEAFLET SALES BY TIME
    output$plot_sales_time <-  renderPlotly({
      
        time_10_a <- superstore_data %>% 
        filter(Ship.Mode == input$input_category) %>% 
        group_by(Order.YQ) %>% 
        summarise(Sales_L = sum(round(Sales),4)) %>% 
        ungroup() %>% 
        arrange(Order.YQ) 
        
        time_10_a_g <- time_10_a %>% 
        mutate(label = glue(
            "Year Quarter: {Order.YQ}
             Sales Volume: {comma(Sales_L)} $"
          ))
        
        plot_time_L <- ggplot(data = time_10_a_g, aes(x = Order.YQ ,y = Sales_L))+
          geom_line(col = "blue")+
          geom_point(aes(text = label),col="black")+
          labs(title = "Sales Volume per Quarter by Ship Mode",
               x = "Year Quarterly",
               y = "Sales Volume") +
          theme_minimal()
        
        ggplotly(plot_time_L, tooltip = "text")
    })
    
    ## ---DATASET
    output$dataset_table <- DT::renderDataTable(superstore_data,
                                                options = list(scrollX=T,
                                                               scrollY=T))
  }
)