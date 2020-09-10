source("global.R", local = TRUE)

#####Sidebar#####

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", icon = icon("bar-chart"),  tabName = "intro"),
    menuItem("Time Series", icon = icon("area-chart"), tabName = "ts"
    ),
    menuItem("Analysis", icon = icon("sliders"), startExpanded = TRUE, tabName = "charts",
             menuSubItem("Course Breakdown", icon = icon("tasks"), tabName = "course"),
             menuSubItem("Publication Pathways", icon = icon("pie-chart"), tabName = "female"),
             menuSubItem("Co-Authorship", icon = icon("users"), tabName = "coauthor"),
             menuSubItem("Logistic Regression", icon = icon("console", lib = "glyphicon"), tabName = "logit")
    ),
    menuItem("Publisher Data", icon = icon("book"), tabName = "data"
    ),
    menuItem("Methodology", icon = icon("cubes"), tabName = "methods"
    ),
    menuItem("About the Project", icon = icon("user-circle"), tabName = "project"
    ),
    menuItem("Source code", icon = icon("github"), newtab = TRUE,
             href = "https://github.com/ciflikli/gendeR/"
    )
  )
)

#####Body#####

body <- dashboardBody(tags$head(includeCSS("www/styles.css"),
                        tags$style("@import url('//fonts.googleapis.com/css?family=Roboto+Condensed');")),
  tabItems(
    tabItem(tabName = "intro",
            h2("Time = Progress?"),
            fluidRow(
              column(width = 12,
                     h3("Seeing the Big Picture"),
                     fluidRow(
                       column(width = 6,
                              plotOutput("plot1", height = 400,
                                         hover = hoverOpts(
                                         id = "plot1_hover",
                                         nullOutside = FALSE))),
                       column(width = 6,
                              conditionalPanel(
                                condition = "input.plot1_hover != null",
                                plotOutput("plot2", height = 400)))),
                     fluidRow(br(),
                       p("Since 1965, there is an ever increasing trend in the number of publications authored by
                         female scholars included in LSE International Relations Department reading lists. However, an increase in                          absolute numbers does not necessitate relative improvement: hover/click on the left plot to reveal the                             actual trend*. Instead, we see the", a("Pareto (80/20) principle at work:",
                         href = "https://en.wikipedia.org/wiki/Pareto_principle", target = "_blank"),
                         "The rich get richer, and no real progress is made.", align = "justify")
                     )
                    )
                )
            ),
    tabItem(tabName = "female",
            fluidPage(fluidRow(column(12,
            h2("Publication Pathways"),
            p("Hover on the dial starting from the innermost circle to find out reading list female authorship trends based
              on the sequence:", br(),
              "Decade > Article/Book > Top/Other Publisher > Single/Co-Authored > Female/Male Co-Author"),
            fluidRow(sunburstOutput(outputId = "sb", height = 550)))))
            ),
    tabItem(tabName = "course",
            fluidPage(fluidRow(column(12,
                                      h2("IR Subfields and Course Breakdown"), br(),
               rbokehOutput(outputId = "bokeh", width = "210%"))),
             fluidRow(column(4, br(), br(), h4("Legend"),
                             #h5("Core: LSE core course indicator"),
                             h5("Ratio: Reading List Female Author Ratio"),
                             h5("Level: Undergraduate, Masters, PhD")),
                      column(4, br(), br(), h4("Info"),
                             h5("Hover over course boxes for additional information on:"),
                             h5("Convener rank, gender, and the total number of readings."))
                      )
             )),
    tabItem(tabName = "logit",
            fluidPage(fluidRow(
              h2("Logistic Function"),
              fluidRow(column(12,
                              plotOutput("glmplot", height = 400))), br(),
              fluidRow(column(4,
                              p("First slider creates a new dummy variable which is set to 0 if it is not met (miss)
                                in a calendar year and 1 (hit) otherwise.
                                The second slider selects the starting year for the data. For example, the default settings produce                                 a logistic link function showing which years had at least",
                                span("20%", style = "color:#468cc8"), "female authors since ",
                                span("1960", style = "color:#468cc8"), "with a 95% confidence interval.",
                                align = "justify")),
              fluidRow(column(4,
                     sliderInput("glmratio", "Select Female Author Percentage:", step = 1, min = 0, max = 40, value = 20)),
              fluidRow(column(4,
                     sliderInput("glmyear", "Select Starting Year:", step = 1, min = 1900, max = 2000, value = 1960, sep = ""))))
                )))
            ),
    tabItem(tabName = "coauthor",
            fluidPage(fluidRow(
              h2("Co-Authorship Patterns"),
              p("Explore co-authorship preferences by setting different constraints on author gender
                and/or total number of authors. The play button under the sliders animates the graph by increasing the selected                    value by one until it reaches its maximum.
                The first slider creates a subset based on the value range: The default '2-3' only shows works featuring either
                2 or 3 authors, which excludes single-authored pieces by authors from both genders. Using the other sliders,
                all gender combinations can be analysed.
                As coding conserved the first-last author sequence, 'FM' is qualitatively different than 'MF'. Note that the bubble                 circumferences are calculated using square roots: the differences look smaller than they actually are.
                Otherwise, most female author groups would be invisible.", align = "justify"),
              fluidRow(column(4,
                              sliderInput("max", "Select Maximum Number of Total Authors:",
                                          step = 1, min = 1, max = 6, value = c(2, 3), animate = TRUE)),
              fluidRow(column(4,
                              sliderInput("female", "Select Maximum Number of Female Authors:",
                                          step = 1, min = 0, max = 3, value = 3, animate = TRUE)),
              fluidRow(column(4,
                              sliderInput("male", "Select Maximum Number of Male Authors:",
                                          step = 1, min = 0, max = 5, value = 5, animate = TRUE)
                              )))
              ),
              fluidRow(column(4,
                              valueBoxOutput("total", width = NULL)),
              fluidRow(column(4,
                              valueBoxOutput("male", width = NULL)),
              fluidRow(column(4,
                              valueBoxOutput("female", width = NULL)
                              )))
              ),
              fluidRow(column(12,
                              bubblesOutput("bubbles", width = "100%", height = 600)))
              ))),                
    tabItem(tabName = "ts",
            fluidPage(
             fluidRow(column(12,
               h2("Publication-Year Author Gender Ratios"), br(),
               dygraphOutput(outputId = "ts")),
             fluidRow(column(12, br(),
               p("This interactive time series analogue of the introduction plot shows reading list breakdown based on gender.
                 Female-to-male author ratio, based on their inclusion in LSE IR reading lists, rarely hits 20% in a given                          publication year.
                 This finding is in line with that of ",
                 a("Colgan (2017).", href = "https://docs.google.com/viewer?a=v&pid=sites&srcid=ZGVmYXVsdGRvbWFpbnxqZWZmZGNvbGdhbnxneDo3NjRkNGMyODZkNDFiMTI4", target = "_blank"),
                 "Refer to the methodology tab to read about how this study was conducted.",
                 br(), br(),
                 "* Note that the data are only accurate down to individual years, even when zoomed to monthly view.",
                 align = "justify")
             ))))
            ),
    tabItem(tabName = "data",
            fluidPage(fluidRow(column(12,
            h2("Gender Breakdown by Publisher"),
            fluidRow(DT::dataTableOutput(outputId = "pubtable")))),
            fluidRow(column(4,
                            sliderInput("female2", "Minimum Female Inclusion Ratio %",
                                        step = 5, min = 0, max = 50, value = 0)),
            fluidRow(column(4,
            sliderInput("threshold2", "Minimum Number of Total Publications",
                        step = 10, min = 10, max = 100, value = 100)),
            fluidRow(column(4, br(),
            p("Play around with the sliders to filter publishers. Notice that as publishers get more 'prestigious'
            (i.e. higher number of total publications included), their F/M ratio goes down.", align = "justify"))))
            ))),
    tabItem(tabName = "methods",
       fluidPage(column(6,
       h2("Methodology"),
       p("The dataset is based on an export of", a("Moodle", href = "http://www.lse.ac.uk/internationalRelations/aboutthedepartment/forstudents/moodle.aspx", target = "_blank"), 
       "data containing syllabi for each undergraduate,
       master's and PhD level IR course on offer at the LSE in the 2015-16 academic year. A total of 43
       courses (18 Undergraduate-level, 23 Master's-level, and 2 PhD-level)
       render 12,358 non-unique (2,574 by female authors) textual sources listed as both essential and background reading material.
       The analysis focuses on books and articles published between 1960 and 2015. Finally, 
       in order to tackle the gender bias issue as it relates to authorship in academia,
       sex of the author(s) are coded M/F*. In case of multiple authors, the binary coding indicates
       at least one female scholar is involved. All coding was done manually by LSE IR PhD candidates
       on their spare time.", align = "justify"), br(),
       p("* We readily acknowledge the limitations of a binary gender indicator.")),
       column(2,
              h2("Syllabi Coders"),
              p("Sarah Bertrand"),
              p("Ilaria Carrozza"),
              p("Ida Danewid"),
              p("Andrew Delatolla"),
              p("Pilar Elizalde"),
              p("Inez Freiin von Weitershausen"),
              p("Elitsa Garnizova"),
              p("Sophie Haspelagh")),
       column(2,
              h2("< / >"),
              p("Taylor St John"),
              p("Joe Leigh"),
              p("Gustav Meibauer"),
              p("Evelyn Pauls"),
              p("Kiran Phull"),
              p("Adrian Rogstad"),
              p("William Rooke"),
              p("Joanne Yao")
              ))
       ),
    tabItem(tabName = "project",
       fluidPage(column(6,
        h2("Project Details"), 
        p("This data presentation is one component of a larger gender and diversity project that is run at the LSE International Relations Department.
          The following publications are now out:"), br(),
        p("For methodology, see", em("'Gender and bias in the International Relations curriculum: Insights from reading lists'"),
          "by Kiran Phull, Gokhan Ciflikli & Gustav Meibauer.", "Read ", a("EJIR article.",
        href = "https://journals.sagepub.com/doi/10.1177/1354066118791690",
        target = "_blank"),
          "Read LSE Impact", a("blog post.", 
        href = "http://blogs.lse.ac.uk/impactofsocialsciences/2018/01/31/male-authors-outnumber-their-female-counterparts-on-international-relations-course-reading-lists-by-more-than-five-to-one/",
        target = "_blank")), br(),
        p("For theory, see", em("'Is Your Syllabus Biased?: Analyzing Gender and Diversity in the IR Canon'"),
          "by Joanne Yao and Andrew Delatolla. Read guest", a("blog post.",
          href = "https://thedisorderofthings.com/2017/04/20/gender-and-diversity-in-the-ir-curriculum-why-should-we-care/",
          target = "_blank")), br(),
        p("This Shiny app is built by", a("Gokhan Ciflikli", icon("external-link"),
                                          href = "https://www.gokhan.io", target = "_blank"),
          "using R, utilising packages such as"),
        code("shinydashboard, shinyjs, dygraphs, sunburstR, DT, htmlwidgets, RColorBrewer, bubbles, rbokeh"), br(), br(),
        p("Contact me on ", a("Twitter", icon("twitter"), href = "https://twitter.com/gokhan_ciflikli", target = "_blank"))),
              column(6,
              h2("Additional Links"),
              p("Evans, Heather K. and A. Moulder. 2011.", a("'Reflecting on a Decade of Women’s Publications in Four Top Political Science Journals'.",
              href = "https://www.cambridge.org/core/journals/ps-political-science-and-politics/article/reflecting-on-a-decade-of-womens-publications-in-four-top-political-science-journals/4F8D8CDB080BC18877D8E1E50622B32E", target = "_blank"),
                "PS: Political Science and Politics 44(4): 793-798.", br(), br(),
                "Maliniak, Daniel, Ryan Powers, and Barbara F. Walters. 2013.", a("The Gender Citation Gap in International Relations.",
              href = "https://www.cambridge.org/core/journals/international-organization/article/the-gender-citation-gap-in-international-relations/3A769C5CFA7E24C32641CDB2FD03126A", target = "_blank"),
              "International Organization 67(4): 889-922.", br(), br(),
              "Mitchell, Sara McLaughlin, Samantha Lange, and Holly Brus. 2013.", a("Gendered Citation Patters in International Relations Journals.", href = "http://www.saramitchell.org/mlb.pdf", target = "_blank"),
              "International Studies Perspectives 14(4): 485-492.", br(), br(),
              "Nexon, Daniel. 2013.", a("The Citation Gap: Results of a Self Experiment.", href = "http://duckofminerva.com/2013/08/the-citation-gap-results-of-a-self-experiment.html", target = "_blank"), "Duck of Minerva.", br(), br(),
              "Young, Cheryl. 1995.", a("An Assessment of Articles Published by Women in 15 Top Political Science Journals.", href = "http://www.saramitchell.org/young.pdf", target = "_blank"),
              "PS: Political Science & Politics 28(3): 525–33.", br(), br(),
              a("Gender Balance Assessment Tool", icon("external-link"), href = "https://jlsumner.shinyapps.io/syllabustool/",
                target = "_blank"), br(), br(),
              a("Open Syllabus Explorer: Mapping the College Curriculum across 1M+ Syllabi", icon("external-link"),
                href = "http://explorer.opensyllabusproject.org/", target = "_blank"), br(), br(),
              a("Women Also Know, IR Edition", icon("external-link"), href = "http://duckofminerva.com/2018/02/women-also-know-international-relations-edition.html", target = "_blank"))
                     )))
 )
)

#####UI#####

ui <- dashboardPage(skin = "red", 
      dashboardHeader(title = "LSE IR Gender Project", titleWidth = "250px"),
      dashboardSidebar(sidebar, width = "250px"),
      #HTML changes: custom font; reduce valuebox height; hover over sidebar icon does not highlight;
      #get rid of whitespace between sidebar and body; make dygraph legend bg transparent
      dashboardBody(body, useShinyjs(), tags$head({tags$style(HTML("
                                                   .main-header .logo {
                                                   font-family: 'Roboto Condensed', sans-serif;
                                                   font-weight: normal;
                                                   font-size: 24px;
                                                   }
                                                  .small-box {height: 70px}
                                                  .skin-red .main-header .logo {
                                                   background-color: #db4c3f;
                                                   }
                                                   .skin-red .main-header .logo:hover {
                                                   background-color: #db4c3f;
                                                   }
                                                   .skin-red .main-header .navbar .sidebar-toggle:hover {
                                                   background-color: #db4c3f;
                                                   }
                                                   .skin-red .main-header .navbar .sidebar-toggle:hover {
                                                   color: #FFFFFF;
                                                   background: rgba(0,0,0,0);
                                                   }
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
                                                   .dygraph-legend {background: transparent !important;}"))}))
      )

#####Server#####

server <- function(input, output) {
  
  #Make sidebar collapse into icons instead of nothing
  runjs({"
        var el2 = document.querySelector('.skin-red');
        el2.className = 'skin-red sidebar-mini';
        "})
  
  #####Reactive datasets#####
  
  #XY cordinates for hover plot
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  #DT data 
  
  selectedData <- reactive({
  pub <- pub[pub$Total >= input$threshold2 & pub$Ratio >= (input$female2 / 100), ]
  })

  #Logit data
  
  gender$Female <- as.numeric(gender$Female) - 1
  gender2 <- gender %>%
    group_by(Year) %>%
    summarise(n = n(), Avg = sum(Female))
  
  plotData <- reactive({
    gender2$Avg2 <- ifelse(gender2$Avg / gender2$n >= input$glmratio/100, 1, 0)
    gender2 <- gender2[gender2$Year >= input$glmyear, ]
  })
  
  #Bubbles data
  
  coData <- reactive({
    dat2 <- na.omit(dat2)
    dat2 <- dat2[dat2$Total >= input$max[1] & dat2$Total <= input$max[2] &
                 dat2$Male <= input$male & dat2$Female <= input$female, ]
  })
  
  #ValueBox data
  
  output$total <- renderValueBox({
    valueBox(
      value = tags$p(nrow(coData()), style = "font-size: 80%;"),
      subtitle = tags$p("Unique author combinations", style = "font-size: 85%;"),
      icon = tags$i(icon("users")), color = "light-blue"
    )
  })
  
  output$male <- renderValueBox({
    valueBox(
      value = tags$p(sum(coData()$n), style = "font-size: 80%;"),
      subtitle = tags$p("Total number of readings", style = "font-size: 85%;"),
      icon = tags$i(icon("book")), color = "light-blue"
    )
  })
  
  output$female <- renderValueBox({
    valueBox(
      value = tags$p(sum(coData()$n[coData()$Female > 0]), style = "font-size: 80%;"),
      subtitle = tags$p("At least one female author involved", style = "font-size: 85%;"),
      icon = tags$i(icon("venus")), color = "light-blue"
    )
  })
  
  ######Render outputs######
  
  #First plot using static gender data
  
  output$plot1 <- renderPlot({
    
    q <- ggplot(gender[gender$Year > 1965 & gender$Year < 2017 & gender$Female == 1, ],
         aes(x = Year, fill = Gender)) +
         geom_histogram(binwidth = .5, alpha = 1, position = "identity", colour = "#232d33") +
         scale_fill_manual(values = "#1fbfc3") +
         scale_x_continuous(name = "Date of Publication") +
         scale_y_continuous(name = "Times Included in Reading List") +
         labs(title = "Number of Publications included in LSE IR Reading Lists",
              subtitle = "Female Authors Subset") +
         theme(plot.background = element_rect(fill = "#ecf0f5", linetype = "blank"),
               panel.grid = element_line(), legend.position = c(.1, .9))

    q
    
  })
  
  #Hover plot using static gender data
  
  output$plot2 <- renderPlot({
    
    p <- ggplot(gender[gender$Year > 1965 & gender$Year < 2017, ], aes(x = Year, fill = Gender)) +
         geom_histogram(binwidth = 1, alpha = .9, position = "dodge", colour = "#232d33") +
         scale_fill_manual(values = c("#1fbfc3", "#f5766f")) +
         scale_x_continuous(name = "Date of Publication") +
         scale_y_continuous(name = "Times Included in Reading List") +
         labs(title = " ", subtitle = "Both Genders") +
         theme(plot.background = element_rect(fill = "#ecf0f5", linetype = "blank"),
               panel.grid = element_line(), legend.position = c(.1, .9))
    p
    
  })
  
  #Time Series using authors data
  
  output$ts <- renderDygraph({
    
    dygraph(data = authors) %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.1, panEdgeFraction = 0.1, drawPoints = TRUE,
                strokeWidth = 1.2, drawGapEdgePoints = TRUE, drawGrid = FALSE, mobileDisableYTouch = TRUE) %>%
      dyLimit(.26, color = "black") %>%
      dyLegend(width = 400, hideOnMouseOut = TRUE) %>%
      dyAxis("y", label = "Percentage of All Readings", valueRange = c(.01, 1.005), rangePad = 1) %>%
      dyAxis("x", label = "Date of Publication") %>%
      dySeries("V2", label = "Female Author Ratio", color = "#1fbfc3", stepPlot = TRUE) %>%
      dySeries("V3", label = "Male Author Ratio", color = "#f5766f", stepPlot = TRUE, fillGraph = TRUE)
  })
  
  #Sunburst using static patch data (code at the end makes sure the legend is on by default; use with htmlwidgets)
  
  output$sb <- renderSunburst({
    htmlwidgets::onRender(
      sunburst(patch, count = TRUE,
               legend = list(w = 150, h = 25, s = 5, t = 25),
               breadcrumb = list(w = 0, h = 25, s = 5, t = 10),
               colors = c("", blues[1:8], reds[7:2]), #For some reason, had to assign a blank colour first for the palette sequences to work as intended
               legendOrder = c("1960", "1970", "1980", "1990", "2000", "2010",
                               "Book", "Article",
                               "OtherPublisher", "TopPublisher",
                               "CoAuthored", "SingleAuthored",
                               "MaleCoAuthor", "FemaleCoAuthor"), withD3 = TRUE),
    "
    function(el,x){
    d3.select(el).select('.sunburst-togglelegend').property('checked', true);
    d3.select(el).select('.sunburst-legend').style('visibility', '');
    }
    "
    )
  })
  
  #Bokeh using static course data
  
  output$bokeh <- renderRbokeh({
    figure(title = "",
           tools = c("pan", "wheel_zoom", "reset", "hover", "save"),
           font = "Roboto Condensed",
           ylim = as.character(1:6),
           xlim = as.character(0:14), 
           xgrid = FALSE, ygrid = FALSE,
           xaxes = FALSE, yaxes = FALSE,
           height = 400, width = 1050,
           h_symmetry = TRUE, v_symmetry = TRUE,
           toolbar_location = "right") %>%
      #Create cluster boxes as indicators
      ly_crect(xcor, ycor, data = indicator, width = 2.95, height = .95,
               fill_color = colors, line_color = "#252525", fill_alpha = .6,
               hover = list(Subfield, Courses)) %>%
      ly_text(symx, ycor, text = clusters, data = indicator,
              font = "Roboto Condensed",
              font_style = "normal", font_size = "14pt",
              align = "center", baseline = "middle") %>%
      #Create centered rectangles
      ly_crect(xcor, ycor, data = course, width = .95, height = .95,
               fill_color = color, line_color = "#252525", fill_alpha = .6,
               hover = list(Convener, Readings)) %>%
      #F/M ratio
      ly_text(symx, ycor, text = Ratio, data = course,
              font = "Roboto Condensed",
              font_style = "bold", font_size = "14pt",
              align = "left", baseline = "middle") %>%
      #Core course indicator
      #ly_text(symx2, numbery, text = Core, data = course, font = "Roboto Condensed",
      #        font_style = "bold", font_size = "6pt", align = "left", baseline = "middle") %>%
      #Course level
      ly_text(symx, massy, text = Level, data = course, font = "Roboto Condensed",
              font_size = "6pt", align = "left", baseline = "middle") %>%
      theme_title(text_font = "Roboto Condensed", text_font_size = "16pt",
                  background_fill_color = "#ecf0f5", text_font_style = "normal") %>%
      theme_plot(background_fill_color = "#ecf0f5", border_fill_color = "#ecf0f5", outline_line_alpha = 0)
  })
  
  #Logit using reactive plotData()
  
  output$glmplot <- renderPlot({
    r <- ggplot(plotData(), aes(x = Year, y = Avg2)) + geom_point() +
         stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE,
                     color = "#428bca", fill = "#428bca", alpha = .1) +
         scale_y_continuous(name = "Selected Female Author Ratio Hit/Missed", labels = c("Miss", "Hit"), breaks = c(0, 1)) +
         scale_x_continuous(name = "Date of Publication") +
         theme(plot.background = element_rect(fill = "#ecf0f5", linetype = "blank"),
               panel.grid = element_line())
    r
  })
  
  #Bubbles using reactive coData()
  
  output$bubbles <- renderBubbles({
    if (nrow(coData()) == 0)
      return()
    bubbles(sqrt(coData()$n), coData()$AutGen, key = coData()$AutGen,
            tooltip = coData()$n, color = "#222d32", textColor = "white")
  })
  
  #DT using reactive selectedData()
  
  output$pubtable <- DT::renderDataTable(selectedData())
                     options(DT.options = list(
                       pageLength = 10, order = list(list(4, "desc")),
                       class = "hover",
                       language = list(search = "Enter Publisher Name:",
                                       info = "Displaying _START_ to _END_ of _TOTAL_ publishers"))
                            )
}

shinyApp(ui = ui, server = server)
