source("global.R", local = TRUE)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", icon = icon("venus"),  tabName = "intro"),
    menuItem("Charts", icon = icon("pie-chart"), startExpanded = TRUE, tabName = "charts",
             menuSubItem("Female Inclusion", tabName = "female"),
             menuSubItem("Institutional Breakdown", tabName = "course")
    ),
    menuItem("Time Series", icon = icon("area-chart"), tabName = "ts"
    ),
    menuItem("Methodology", icon = icon("cubes"), tabName = "methods"
    ),
    menuItem("Publisher Data", icon = icon("book"), tabName = "data"
    ),
    menuItem("About the Project", icon = icon("user-circle"), tabName = "project"
    ),
    menuItem("Source code for app", icon = icon("github"),badgeLabel = "soon", badgeColor = "red"
             #href = "https://github.com/rstudio/shinydashboard/blob/gh-pages/_apps/sidebar/app.R"
    ),
    menuItem("g.ciflikli@lse.ac.uk", icon = icon("envelope"),
             href = "mailto:g.ciflikli@lse.ac.uk")
  )
)

body <- dashboardBody(#includeCSS("styles.css"),
  tabItems(
    tabItem(tabName = "intro",
            h2("Time = / != Progress?"),
            fluidRow(
              column(width = 12, class = "well",
                     h3("Seeing the Big Picture"),
                     fluidRow(
                       column(width = 6,
                              plotOutput("plot2", height = 400,
                                         brush = brushOpts(
                                           id = "plot2_brush",
                                           resetOnNew = TRUE),
                                         #add the hover options
                                         hover = hoverOpts(
                                           id = "plot2_hover",
                                           nullOutside = FALSE))),
                       column(width = 6,
                              #the second plot will be hidden if the user's mouse is not on the first one
                              conditionalPanel(
                                condition = "input.plot2_hover != null",
                                plotOutput("plot3", height = 400))))
                    )
                )
            ),
    tabItem(tabName = "female",
            h2("Pathways to Female Author Inclusion"),
            h3("Which sets of conditions are more conducive?"),
            fluidRow(sunburstOutput(outputId = "sb", height = 550)),
            h5("Sequence: Decade > Article/Book > Top/Other Publisher > Single/Co-Authored > Female/Male Co-Author")
            ),
    tabItem(tabName = "course",
            fluidPage(fluidRow(
             h2("Female Author Ratio Breakdown by Cluster"),
               rbokehOutput(outputId = "plot1", width = "200%")),
             fluidRow(column(2,
             br(),
             h4("Legend"),
             br(),
               img(src = "501.png", height = 83, width = 100)),
                      column(10, br(), br(), br(),
                             h5("Core Course: LSE core course indicator"),
                             h5("F/M Ratio: Reading List Female Author Ratio"),
                             h5("Course Code: LSE Course Code"),
                             h5("Cluster: Theory, Security/Statecraft, IO/Law, IPE, Regional (Colour-coded)"),
                             h5("Level: Undergraduate, Masters, PhD"),
                             h5("Hover over course boxes for additional information. Top rows indicate higher F/M ratio.")
                                 ))
             )),
    tabItem(tabName = "ts",
             h2("Yearly Time Series Graph"),
             fluidRow(dygraphOutput(outputId = "ts"))
            ),
    tabItem(tabName = "data",
            fluidPage(fluidRow(column(12,
            h2("Gender Breakdown by Publisher"),
            fluidRow(DT::dataTableOutput(outputId = "pubtable")))),
            fluidRow(column(3,
            sliderInput("threshold2", "Minimum Number of Total Publications",
                        step = 10, min = 10, max = 100, value = 100)),
               fluidRow(column(3,
               sliderInput("female2", "Minimum Female Inclusion Ratio %",
                        step = 5, min = 0, max = 50, value = 0))))
            )),
    tabItem(tabName = "methods",
       h2("Methodology"), fluidPage(column(6,
       h4("The dataset is based on an export of Moodle data containing syllabi for each undergraduate,
       Master's and PhD level IR course on offer at the LSE in the 2015-16 academic year. A total of 43
       courses (18 undergraduate-level, 23 Master's level, and 2 PhD level)
       render 13,589 non-unique (2,574 by female authors) textual sources listed as both essential and background reading material.
       The analysis focuses on books and articles published between 1960 and 2015. Finally, 
       in order to tackle the gender bias issue as it relates to authorship in academia,
       sex of the author(s) are coded M/F. In case of multiple authors, the binary coding indicates
       at least one female scholar is involved.", align = "justify")))
       ),
    tabItem(tabName = "project",
       h2("People behind the Project"),
        h4("Long text here.")
            )
 )
)

ui <- dashboardPage(skin = "red",
      dashboardHeader(title = "LSE IR Gender Project"),
      dashboardSidebar(sidebar, width = "250px"),
      dashboardBody(body, tags$head(tags$style(HTML("
                                                   .main-sidebar, .left-side {
                                                   width: 250px;
                                                   }
                                                   @media (min-width: 768px) {
                                                   .content-wrapper,
                                                   .right-side,
                                                   .main-footer {
                                                   margin-left: 120px;
                                                   }
                                                   }
                                                   @media (max-width: 767px) {
                                                   .sidebar-open .content-wrapper,
                                                   .sidebar-open .right-side,
                                                   .sidebar-open .main-footer {
                                                   -webkit-transform: translate(250px, 0);
                                                   -ms-transform: translate(250px, 0);
                                                   -o-transform: translate(250px, 0);
                                                   transform: translate(250px, 0);
                                                   }
                                                   }
                                                   @media (max-width: 767px) {
                                                   .main-sidebar,
                                                   .left-side {
                                                   -webkit-transform: translate(-250px, 0);
                                                   -ms-transform: translate(-250px, 0);
                                                   -o-transform: translate(-250px, 0);
                                                   transform: translate(-250px, 0);
                                                   }
                                                   }
                                                   @media (min-width: 768px) {
                                                   .sidebar-collapse .main-sidebar,
                                                   .sidebar-collapse .left-side {
                                                   -webkit-transform: translate(-250px, 0);
                                                   -ms-transform: translate(-250px, 0);
                                                   -o-transform: translate(-250px, 0);
                                                   transform: translate(-250px, 0);
                                                   }
                                                   }
                                                   .js-irs-0 .irs-bar {
                                                   border-top-color: #db4c3f;
                                                   border-bottom-color: #db4c3f;
                                                   } 
                                                   .js-irs-0 .irs-bar-edge {
                                                   border-color: #db4c3f;
                                                   }
                                                   .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                   background: #db4c3f;
                                                   }
                                                   }"))))
      )

server <- function(input, output) {
  
  selectedData <- reactive({
  pub <- pub[pub$Total >= input$threshold2 & pub$Ratio >= (input$female2 / 100), ]
  })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderRbokeh({
      figure(title = "LSE IR Courses", tools = c("reset", "hover"),
                ylim = as.character(1:5),
                xlim = as.character(1:13), 
                xgrid = FALSE, ygrid = FALSE, xaxes = FALSE, yaxes = FALSE,
                ylab = "F/M Ratio", xlab = "IR Course Level",
                height = 1200, width = 3450,
                toolbar_location = NULL, h_symmetry = TRUE, v_symmetry = TRUE) %>%
      # plot rectangles
      ly_crect(xcor, ycor, data = course, width = .95, height = .95,
               fill_color = color, line_color = "#252525", fill_alpha = .6,
               hover = list(Course, Convener, Total, Ratio)) %>%
      ly_text(symx, ycor, text = Code, data = course, 
              font_style = "bold", font_size = "14pt",
              align = "left", baseline = "middle") %>%
      ly_text(symx, numbery, text = Core, data = course,
               font_style = "bold", font_size = "6pt", align = "left", baseline = "middle") %>%
      ly_text(symx, namey, text = Cluster, data = course,
              font_size = "6pt", align = "left", baseline = "middle") %>%
      ly_text(symx, massy, text = Level, data = course,
               font_size = "6pt", align = "left", baseline = "middle") %>%
      ly_text(symx2, numbery, text = Ratio, data = course,
              font_size = "8pt", align = "left", baseline = "middle")
    
    })
  
  output$plot2 <- renderPlot({
    q <- ggplot(gender[gender$Year > 1965 & gender$Year < 2017 & gender$Female==1, ],
         aes(x = Year, fill = Gender)) +
         geom_histogram(binwidth = .5, alpha = 1, position = "identity", colour = "#db4c3f") +
         scale_x_continuous(name = "Date of Publication") +
         scale_y_continuous(name = "Times Included in Reading List") +
         labs(title = "Number of Publications Included in IR Reading Lists",
             subtitle = "Female Authors Subset")
    q
    
  })
  
  output$plot3 <- renderPlot({
    
    p <- ggplot(gender[gender$Year > 1965 & gender$Year < 2017, ], aes(x = Year, fill = Gender)) +
      geom_histogram(binwidth=1, alpha=1, position = "dodge", colour = "#0f4792") +
      scale_x_continuous(name = "Date of Publication") +
      scale_y_continuous(name = "Times Included in Reading List") +
      labs(title = "Number of Publications Included in IR Reading Lists", subtitle = "Both Genders")
    p
    
  })
    
  output$sb <- renderSunburst({
    htmlwidgets::onRender(
    sunburst(patch, count = TRUE,
             legend = list(w = 150, h = 25, s = 5, t = 25),
             breadcrumb = list(w = 0, h = 25, s = 5, t = 10),
             colors = c(brewer.pal(7, "Reds"), brewer.pal(7, "Blues")),
             legendOrder = c("1960", "1970", "1980", "1990", "2000", "2010",
                             "Article", "Book",
                             "TopPublisher", "Other",
                             "SingleAuthored", "CoAuthored",
                             "MaleCoAuthor", "FemaleCoAuthor")),"
    function(el,x){
      // check legend
      d3.select(el).select('.sunburst-togglelegend').property('checked',true);
      // simulate click
      d3.select(el).select('.sunburst-togglelegend').on('click')();
    }"
    )
  })
  
  output$ts <- renderDygraph({
  
  dygraph(data = authors, main = "Reading List Inclusion Rates over Time") %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>%
    dyLimit(.2, color = "red") %>%
    dyLegend(width = 400) %>%
    dyAxis("y", label = "Percentage of All Readings",valueRange = c(0, 1.001)) %>%
    dyAxis("x", label = "Date of Publication") %>%
    dySeries("V2", label = "Female Inclusion") %>%
    dySeries("V3", label = "Male Inclusion")
  })
  
  output$pubtable <- DT::renderDataTable(selectedData())
                     options(DT.options = list(
                       pageLength = 10, language = list(search = "Enter Publisher Name:",
                                                        info = "Displaying _START_ to _END_ of _TOTAL_ publishers"),
                       order = list(list(4, "desc")),
                       class = "hover"
                       ))

}

shinyApp(ui = ui, server = server)