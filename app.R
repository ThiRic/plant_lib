library("shiny")
library("plotly")

#wd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#data <- read.csv2(paste(getwd(),"/Extract library.csv", sep=""), sep= ",",  fileEncoding= "UTF-8-BOM")
data <- read.csv("https://raw.githubusercontent.com/ThiRic/plant_lib/main/Extract%20library.csv")

ui <- fluidPage(
  # *Input () functions,
  #selectInput(inputId = "plant_name", label= "Select a plant"),#for plant
  #radioButtons(inputId = "activity_type", label= "Select an activity type"), #for activity type
  titlePanel("Extract activity query"),
  sidebarLayout(
    sidebarPanel( width= 2,
  uiOutput("plant_name"),
  uiOutput("compound_name"),
  uiOutput("target_name")
    ),
  
  mainPanel(
    plotlyOutput("activity_heatmap_plant", height= "1100px"),
    br(),
    br(),
    plotlyOutput("activity_heatmap_compound", height= "1100px"),
    br(),
    br(),
    plotlyOutput("activity_heatmap_target", height= "1100px"),
)
)
  
  #plotOutput(outputId="activity_heatmap")
  # *Output () functions
)

server <- function(input, output, session) {
  output$plant_name <- renderUI({
    selectInput(inputId = "choose_plant", label = "Choose plants' name", selected="Passiflora edulis", sort(unique(data$Latin)), multiple= TRUE)
  })
  
  output$compound_name <- renderUI({
    selectInput(inputId = "choose_compound", label = "Choose compounds' name", sort(unique(data$Compound_Name)), multiple= TRUE)
  })
  
  output$target_name <- renderUI({
    selectInput(inputId = "choose_target", label = "Choose targets' name", sort(unique(data$Protein_Name)), multiple= TRUE)
  })
  
  data_filtered_plant <- reactive({
    data[(data$Latin %in% input$choose_plant),]
  })
  
  data_filtered_compound <- reactive({
    data[(data$Compound_Name %in% input$choose_compound),]
  })
  
  data_filtered_target <- reactive({
    data[(data$Protein_Name %in% input$choose_target),]
  })
  
  output$activity_heatmap_plant <- renderPlotly(
    {
      #plot code goes there
      #use input$plant_name and input$activity_type
      plot_ly(
        data= data_filtered_plant(),
        type="heatmap",
        x= ~Compound_Name, 
        y= ~Protein_Name, 
        z= ~scaled_Activity_Mean,
        colorscale= "Viridis",
        autocolorscale= FALSE,
        zmin= -0.05,
        zmax= 0,
        customdata= ~Activity_Type,
        text= ~paste(Activity_Mean, "<br>Protein expr. in skin: ", Protein.expression.skin, "<br>Plant (latin): ", Latin , "<br>Plant part: ", Part.for.extract),
        hovertemplate= paste(
          "Compound: %{x}<br>", 
          "Target: %{y}<br>",
          "Activity Type: %{customdata}<br>",
          "Mean activity:%{text}",
          "<extra></extra>")
        )%>%
          layout(
            title= list(text='Extract activities for selected plants', pad= list(b=200, l= 10, r= 50, t= 50)),
            dragmode= "pan",
            xaxis= list(title= list(text= "Compound", standoff= 100)),
            yaxis= list(title= list(text= "Targets", standoff= 100)),
            margin= list(pad= 20),
            height= 1000
          )
    }
  )
  
  output$activity_heatmap_compound <- renderPlotly(
    {
      #plot code goes there
      #use input$plant_name and input$activity_type
      plot_ly(
        data= data_filtered_compound(),
        type="heatmap",
        x= ~Latin, 
        y= ~Protein_Name, 
        z= ~scaled_Activity_Mean,
        colorscale= "Viridis",
        autocolorscale= FALSE,
        zmin= -0.05,
        zmax= 0,
        customdata= ~Activity_Type,
        text= ~paste(Activity_Mean, "<br>Protein expr. in skin: ", Protein.expression.skin, "<br>Compound: ", Compound_Name , "<br>Plant part: ", Part.for.extract),
        hovertemplate= paste(
          "Plant: %{x}<br>", 
          "Target: %{y}<br>",
          "Activity Type: %{customdata}<br>",
          "Mean activity:%{text}",
          "<extra></extra>")
      )%>%
        layout(
          title= list(text='Extract activities for selected compound', pad= list(b=200, l= 10, r= 50, t= 500)),
          dragmode= "pan",
          xaxis= list(title= list(text= "Plants", standoff= 100)),
          yaxis= list(title= list(text= "Targets", standoff= 100)),
          margin= list(pad= 20),
          height= 1000
        )
    }
  )
  
  output$activity_heatmap_target <- renderPlotly(
    {
      #plot code goes there
      #use input$plant_name and input$activity_type
      plot_ly(
        data= data_filtered_target(),
        type="heatmap",
        x= ~Latin, 
        y= ~Compound_Name, 
        z= ~scaled_Activity_Mean,
        colorscale= "Viridis",
        autocolorscale= FALSE,
        zmin= -0.05,
        zmax= 0,
        customdata= ~Activity_Type,
        text= ~paste(Activity_Mean, "<br>Protein expr. in skin: ", Protein.expression.skin,"<br>Protein target:", Protein_Name , "<br>Plant part: ", Part.for.extract),
        hovertemplate= paste(
          "Plant: %{x}<br>", 
          "Compound: %{y}<br>",
          "Activity Type: %{customdata}<br>",
          "Mean activity:%{text}",
          "<extra></extra>")
      )%>%
        layout(
          title= list(text='Extract activities for selected protein target', pad= list(b=200, l= 10, r= 50, t= 500)),
          dragmode= "pan",
          xaxis= list(title= list(text= "Plants", standoff= 100)),
          yaxis= list(title= list(text= "Compounds", standoff= 100)),
          margin= list(pad= 20),
          height= 1000
        )
    }
  )
  
}

shinyApp(ui = ui, server = server)