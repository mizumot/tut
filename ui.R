library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title
 headerPanel("Learning by doing stats (t-test tutorial)"),


  # Sidebar
 sidebarPanel(

  p(strong("Group A:")),

    sliderInput("nx", " Sample size (n)",
                min =1, max = 500, 30),

    numericInput("mx", " Mean", 60.00),

    numericInput("sdx", " SD", 10.00),

  p(br()),

  p(strong("Group B:")),

    sliderInput("ny", " Sample size (n)",
                min =1, max = 500, 30),

    numericInput("my", " Mean", 50.00),

    numericInput("sdy", " SD", 10.00),

  p(br()),

  strong('Option:'),


  checkboxInput("varequal", "t-test with equal variances assumed", TRUE)

    ),



mainPanel(
    tabsetPanel(

    tabPanel("Main",
        h3("Checking the data"),
        tableOutput("values"),

        br(),

        h3("Histogram of Group A"),
        plotOutput("distPlot"),

        br(),

        h3("Overlayed histograms of Group A and Group B"),
        plotOutput("overPlot"),

        br(),
        br(),

        h3("Group A と Group B の n, M, SD (variance) から t 値を算出"),
        a(img(src="http://mizumot.com/files/t-value.png"), target="_blank", href="http://mizumot.com/files/t-value.png"),

        h3("t-test"),
        verbatimTextOutput("ttest.out"),
        verbatimTextOutput("vartest.out"),
        verbatimTextOutput("difference.out"),

        br(),

        h3("t distribution"),
        p('黒点線よりも左右どちらかの外側に赤線（t値）があれば p < .05 になる'),
        plotOutput("t.distPlot", width="80%"),

        br(),

        h3("Plot of means and mean of the differences [95% CI]"),
        plotOutput("ciPlot", width="80%"),

        h3("Effect size indices"),
        verbatimTextOutput("es.out"),

        br()

        ),

    tabPanel("About",

    strong('Note'),
    p('This web application is developed with',
    a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
    ''),

    br(),

    strong('Code'),
    p('Source code for this application is based on',
    a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href='http://mizumot.com/handbook/', target="_blank")),

    br(),

    strong('Recommended'),
    p('If you are a cool Mac user and want to use R with GUI,',
    a("MacR", href="http://www.urano-ken.com/blog/2013/02/25/installing-and-using-macr/", target="_blank"),
    'is defenitely the way to go!'),

    br(),

    strong('Author'),
    p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
    'Associate Professor of Applied Linguistics',br(),
    'Faculty of Foreign Language Studies /',br(),
    'Graduate School of Foreign Language Education and Research,',br(),
    'Kansai University, Osaka, Japan'),

    br(),

    a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/")
)
)
)
))