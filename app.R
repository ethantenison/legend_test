#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                QLD FED ELECTORATE MAP DB
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#packrat::init()
#Load req packages
library(rgdal);library(maptools);library(ggplot2);library(plyr);
library(dplyr);library(rgeos);library(ggrepel);library(sp);
library(raster);library(RColorBrewer);library(ggpubr);library(ggthemes);
library(scales);library(OpenStreetMap);library(rJava);library(ggmap);library(scales);
library(RJSONIO);library(osmar);library(leaflet);library(ggmap);
library(maps);library(htmlwidgets);library(htmltools);library(tidyverse); 
library(reshape2);library(ggalt); library(ggplot2); library(rsconnect); library(data.table);
library(lubridate);library(leaflet.minicharts); library(shiny);library(shinythemes);
library(plotly); library(shinydashboard); library(htmlTable); library(packrat);library(shinycssloaders);


#Load csv containing all the data we've aggreagated at the CED level
df<-read.csv("data/CED_aggregated_data_DB-works.csv")


df$X <- NULL #remove cols you dont need
is.num <- sapply(df, is.numeric)#remove the decimal points in the whole df
df[is.num] <- lapply(df[is.num], round, 1)


#----DFS for bar charts/ distributions---------
#create sep data frames with the diff cols you will use for distributions
df.age <- reshape2::melt(df[,c(1, 151:192)])
#we are taking the suffix 'females", "males" etc and creatung a separate column in each df
#we are using this separate column to create the breakdowns in the distributions repeat for each one that you want a breakdown for
setDT(df.age)[, paste0("variable",2:3):= tstrsplit(variable, "_age_")]
unique(df.age$variable2)
unique(df.age$variable3)
#set factor levels
df.age<-within(df.age,
               variable2<-factor(variable2,
                                 levels=c("CED_pop_0_4","CED_pop_5_9","CED_pop_10_14","CED_pop_15_19","CED_pop_20_24",
                                          "CED_pop_25_29","CED_pop_30_34", "CED_pop_35_39","CED_pop_40_44", 
                                          "CED_pop_45_49","CED_pop_50_54","CED_pop_55_59","CED_pop_60_64","CED_pop_65_69", 
                                          "CED_pop_70_74","CED_pop_75_79", "CED_pop_80_84","CED_pop_85_90","CED_pop_90_94","CED_pop_95_99","CED_pop_100_plus")))

df.age$variable3<-gsub(".1", "",df.age$variable3)# remove ".1" this will change it from a factor to a character
df.age$variable2<-as.factor(df.age$variable2)
#repeat above for income
df.income <- reshape2::melt(df[,c(1, 47:72)])
setDT(df.income)[, paste0("variable",2:3):= tstrsplit(variable, "_wkly_")]
df.income$variable2<-as.factor(df.income$variable2)
#highest level of education
df.edu<-reshape2::melt(df[,c(1, 102:115)])
setDT(df.edu)[, paste0("variable",2:3):= tstrsplit(variable, "_edu_")]
df.edu$variable2<-as.factor(df.edu$variable2)
#industry
df.ind<-reshape2::melt(df[,c(1, 73:94)]) 
setDT(df.ind)[, paste0("variable",2:3):= tstrsplit(variable, "_emp_")]
df.ind$variable2<-as.factor(df.ind$variable2)
df.ind$variable3<-as.factor(df.ind$variable3)
#set factor levels/order so that pie chart labels and well spaced out
df.ind<-within(df.ind,
               variable2<-factor(variable2,
                                 levels=c("CED_ind_arts_rec","CED_ind_health","CED_ind_rental_real_estate","CED_ind_finance_insur","CED_ind_logistics",
                                          "CED_ind_media_tele","CED_ind_sci_tech","CED_ind_pub_admin_safe","CED_ind_mining","CED_ind_retail","CED_ind_agri_fish")))
#DSS
df.dss<-reshape2::melt(df[,c(1, 124:133)])
#Tenure type
df.tenure<-reshape2::melt(df[,c(1, 134:136)])
df.tenure<-within(df.tenure,
                  variable<-factor(variable,
                                   levels=c("CED_ten_rented","CED_ten_owned_mortgage","CED_ten_owned_outright")))
#DWELLING
df.dwelling<-reshape2::melt(df[,c(1,137:139)])
df.dwelling<-within(df.dwelling,
                    variable<-factor(variable,
                                     levels=c("CED_dwell_flat_apt","CED_dwell_semi_detached","CED_dwell_sep_house")))
#create percentages 
df.dwell.cal<-df.dwelling%>%
    group_by(Elect_div, variable) %>% 
    summarise(sum_value=sum(value)) %>% 
    mutate(pct_value=sum_value/sum(sum_value)*100)%>%
    mutate(pos_scaled = cumsum(pct_value) - pct_value / 2,
           perc_text = paste0(round(pct_value, digits = 2), "%"))#%>%

#SELF EMPLOYMENT
df.self_emp<-reshape2::melt(df[,c(1,99:101)])
#set factor levels
df.self_emp<-within(df.self_emp,
                    variable<-factor(variable,
                                     levels=c("CED_self_emp_total","CED_self_emp_male","CED_self_emp_female")))
#MIGRATION
df.mig<-reshape2::melt(df[,c(1, 116:123)])
#set factor levels
df.mig<-within(df.mig,
               variable<-factor(variable,
                                levels=c("CED_int_mig_ACT","CED_int_mig_NSW","CED_int_mig_NT",	
                                         "CED_int_mig_SA","CED_int_mig_TAS","CED_int_mig_VIC","CED_int_mig_WA", "CED_int_mig_OVS")))
#PROJECTED POP
df.proj.pop<-reshape2::melt(df[,c(1, 41:46)])
setDT(df.proj.pop)[, paste0("variable",2:3):= tstrsplit(variable, "_pop_")]
colnames(df.proj.pop)[5]<-"Year"

#elected parties
df.elect.parties<-reshape2::melt(df[,c(1, 197:202)])

#---AEC TABLE-------
#create df with the historic AEC data for the table under map
df.table<-reshape2::melt(df[,c(1, 197:202)])
#reshape the data tabel to suit the table format we want displayed below the map
#long format is more conducive to adding in more information down the track
df.melted<-melt(df.table, idvars="Elect_div", measure.vars=c("Elected_Party_2019","Elected_Party_2016.1", "Elected_Party_2013.1",
                                                             "Elected_Party_2010.1", "Elected_Party_2007.1", "Elected_Party_2004.1"))
#we dont need the variable colum because we are using row names in the table
#be sure to update the row names in the table to reflect the order or the variables (see melt function above)
df.melted$variable <- NULL
#we want the value column first from a display point of view for the table
df.melted<-df.melted[,c(ncol(df.melted),1:(ncol(df.melted)-1))]

#---UNEMP and PARTICIPATION TABLE--
#create a df with the unemp and participation rates for table under buttons
df.workforce<-reshape2::melt(df[,c(1, 148:149)])

#we dont need the variable colum because we are using row names in the table
#be sure to update the row names in the table to reflect the order or the variables (see melt function above)
df.workforce$variable <- NULL
#we want the value column first from a display point of view for the table
df.workforce<-df.workforce[,c(ncol(df.workforce),1:(ncol(df.workforce)-1))]

#load in electorate division for QLD from AEC for 2018
#shape files
qld<-readOGR(dsn=path.expand("./data/shape_files"), layer="simplified_E_AUGEC_region")
qld<-qld[qld$Elect_div %in% c("Blair","Bonner","Bowman","Brisbane", 
                              "Capricornia","Dawson","Dickson","Fadden",
                              "Fairfax","Fisher","Flynn","Forde",
                              "Griffith","Groom","Herbert","Hinkler",
                              "Kennedy","Leichhardt","Lilley",
                              "Longman","Maranoa","Mcpherson",
                              "Moncrieff","Moreton","Oxley",
                              "Petrie","Rankin","Ryan",
                              "Wide Bay","Wright"),]
#centers for labels
qld_centre <- data.frame(gCentroid(qld)) 
centroids <- as.data.frame(coordinates(qld))
names(centroids) <- c("long", "lat")#capture the lat longs for the centres of each elctoral division
centroids$Elect_div <- factor(qld$Elect_div)#add the elect_divs to the centroids data


#merge the csv to the shape file based on elect_div
qld.stats <- merge(qld, df, by = "Elect_div")
#link for twitter and prepopulated tweet
url <- " https://twitter.com/intent/tweet?text=Dig%20deeper%20into%20your%20federal%20electorate's%20data%20using%20this%20app%20developed%20by%20RIDL&hashtags=AUSVOTES&hashtags=RIDL&url=https://regionalinnovationdatalab.shinyapps.io/Dashboard/"


# info for sharing this app on facebook/twitter
share <- list(
  title = "RIDL: 2019 Election Insights Dashboard",
  url = "https://regionalinnovationdatalab.shinyapps.io/Dashboard/",
  image = "favicon.png",
  description = "Presented in an easy-to-digest way, this dashboard allows voters to dive into important decision-making data for all Queensland electorates."
  
)


#~~~~~~~~~~~~~~~~~~~APPLICATION/ Dashboard~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ui<- shiny::fluidPage( #use fluid page so that the app adjusts to the user's screen size and device
  tags$head(
  # Facebook OpenGraph tags - image to share when social sharing app
  tags$meta(property = "og:title", content = share$title),
  tags$meta(property = "og:type", content = "website"),
  tags$meta(property = "og:url", content = share$url),
  tags$meta(property = "og:image", content = share$image),
  tags$meta(property = "og:image:width", content = "300"),
  tags$meta(property = "og:image:height", content = "400"),
  tags$meta(property = "og:description", content = share$description)),
   
#Header image                   
 titlePanel(div(img(src='DATA_DASHBOARD_V3.png', height = "auto", width = "100%"))),
  theme = "journal", #selected theme from shinythemes package
  title=" RIDL: 2019 Election App", #title of web brower tab
  br(),  
  
   column(7, tags$hr(), 
         #select input drop-down menu
         selectInput("stats_input", "",label="Select a statistic to display on the map.",
                     choices= list("**2019 RESULTS** Current sitting Party"="Elected_Party_2019", 
                                   "Sex & party of sitting member 2019"= "Sex_2019",
                                   "2016 election results"="PartyNm.1", 
                                   "2016 turnout percentage"="TurnoutPercentage",
                                   "Sex & party of sitting member 2016"="Sex_2016",
                                   "2018 Work-force participation rate"='CED_participation_rate_2018',
                                   "2018 Unemployment rate"='Unemployment_rate_2018',
                                   "2013 election results"="Elected_Party_2013")),
         tags$hr(),br(),br(),
         #share on twitter
         tags$a(href=url, "Share on Twitter", class="twitter-share-button",size="l"),
         includeScript("http://platform.twitter.com/widgets.js"),
         tags$head(
           tags$style(".selectize-input {border-radius:2px}")),
       
        #loader for map and map output
        withSpinner(leafletOutput("map",height = "1000px")),
        #table output under map
        htmlOutput("table_AEC"),

         #create a button that links to the FAQ document for the app
         tags$div(style="display:inline-block",title="Download the FAQ doc for details and data sources.", 
                  actionButton(inputId = 'pdf_button', 
                               label= "CITATIONS AND FAQs", 
                               style="color: #FFF; background-color: #021893;border-radius:0px; padding-bottom:10px;padding-right:25px; padding-left:25px; font-size:15px",
                               onclick = "window.open('ridl_app_support_doc.pdf','_blank')")),
         #create button that links to the how MPs vote website
         tags$div(style="display:inline-block",title="Check out the Regional Innovation Data Lab.", 
                  actionButton(inputId='RIDL_button', 
                               label="REGIONAL INNOVATION DATA LAB", 
                               style="color: #FFF; background-color:#0E67AD ;border-radius:0px; padding-bottom:10px;padding-right:27px; padding-left:27px; font-size:15px",
                               onclick ="window.open('https://www.griffith.edu.au/griffith-business-school/policy-innovation-hub/regional-innovation-data-lab', '_blank')")),
         #create button to link to Machinery of Gov website
         tags$div(style="display:inline-block",title="Stay informed. Read the Machinery of Government blog to see what the Griffith experts are saying.", 
                  actionButton(inputId='MOG_button', 
                               label="EXPERT ELECTION COVERAGE", 
                               style="color: #FFF; background-color: #C12525;border-radius:0px; padding-bottom:10px;padding-right:25px; padding-left:25px; font-size:15px",
                               onclick ="window.open('https://medium.com/the-machinery-of-government', '_blank')")),
          
         
         # Table output for previous election results - results are colored by party
         htmlOutput("table_unemp"), 
         
         #create a button that links to the FAQ document for the app
         tags$div(style="display:inline-block",title="Think you're enrolled? Check your 2019 enrolment now.", 
                  actionButton(inputId = 'check_button', 
                               label= "CHECK YOUR 2019 ENROLMENT", 
                               style="color: #FFF; background-color: #021893;border-radius:0px; padding-bottom:10px;padding-right:22px; padding-left:22px; font-size:15px",
                               onclick = "window.open('https://check.aec.gov.au/','_blank')")),
         
         #create button that links to the AEC website so that users can update their contact details
         tags$div(style="display:inline-block",title="Make sure you contact details are up to date with the AEC.", 
                  actionButton(inputId = 'address_button', 
                               label= "UPDATE YOUR CONTACT DETAILS", 
                               style="color: #FFF; background-color:#0E67AD;border-radius:0px; padding-bottom:10px; padding-right:22px; padding-left:22px; font-size:15px",
                               onclick = "window.open('https://www.aec.gov.au/enrol/change-address.htm', '_blank')")),
  
        
         #create a button that links to the AEC website so that users can enrol to vote in the election
         tags$div(style="display:inline-block",title="Your vote matters. Visit the AEC website and enrol online today.", 
                  actionButton(inputId='enrol_button', 
                               label="ENROL TO VOTE TODAY", 
                               style="color: #FFF; background-color:#C12525 ;border-radius:0px; padding-bottom:10px; padding-right:22px; padding-left:22px; font-size:15px",
                               onclick ="window.open('https://www.aec.gov.au/enrol/', '_blank')"))),
  #select input drop down menu for electorates       
  column(5,
         tags$hr(),
         selectInput("division_input", "",
                     label="Select an electorate, graphs will be updated.",
                     choices = qld.stats$Elect_div),  
         
         tags$hr(),
         #button to look up electorate on AEC website
         tags$div(style="display:inline-block",title="Visit the Australian Electoral Commission website to find your 2019 electorate.", 
                  actionButton(inputId='Find_button', 
                               label="LOOKUP YOUR ELECTORATE", 
                               style="color: #FFF; background-color: #C12525;border-radius:0px; padding-bottom:10px; padding-right:30px; padding-left:30px; font-size:17px",
                               onclick ="window.open('https://electorate.aec.gov.au/', '_blank')")),
         tags$hr(),
         
  #all the graph outputs
  fluidRow(
      column(5,withSpinner(plotlyOutput("agePyramid", height="300px", width = "600px")), #pop pyramid age dist
              withSpinner(plotlyOutput("projpopGraph", height = "350px",width = "600px")), #projected pop line graph
              withSpinner(plotlyOutput("eduPyramid", height = "300px",width = "600px")), #Education pyramid
              withSpinner(plotlyOutput("incomePyramid", height = "300px",width = "600px")),#Income pyramid
                 h5("The average weekly income in Queensland is $1570 which is $35 less than the Australian average of $1605. Source: ABS Average Weekly Earnings", width = "900px"),
              withSpinner(plotOutput("industryPie",height="550px", width = "600px")),#Industry of employment pie chart
              withSpinner(plotlyOutput("selfempBar", height = "300px",width = "600px")),  #Self-emp bar chart              
              withSpinner(plotlyOutput("migBar",height="300px",width = "600px")),# migration bar chart
              withSpinner(plotlyOutput("dssBar",height="300px", width = "600px")), # DSS payments bar chart              
              withSpinner(plotOutput("tenurePie", height = "300px",width = "600px")), #Tenure type pie chart
              withSpinner(plotOutput("dwellingPIe",height="300px", width = "600px")) #dwelling type pie chart
                
         ))))

br()
#~~~~~~AESTHETIC STUFF~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#colour scheme for plots
mycol <- colorRampPalette(c("#010864", "#0E67AD", "#C8C8C8", "#F1D571","#c12525"))
mycol1 <- colorRampPalette(c("#010864", "#0E67AD", "#C8C8C8", "#F1D571","#c12525"))
#pie chart colour scheme
piecol=c("#0099CC","#2359AF","#970B76","#00ccff","#99FFFF","#990623","#0033FF","#740699","#00cccc","navy","#9999CC")
#colour scheme for bar charts with male female breakdowns
MF=c("#0E67AD","#8D99AE")
#remove scientific notation from y axis labels
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
#colour scheme for elected party table and map colour map to party colours
party_cols<-c("LNP"="#021893","ALP" = "#C12525","IND" = "grey", "KAP" = "#33165F",
              "PUA"="orange", "ON"="orange", "GRN"="#339966", "LNQ"="#0066FF", "PUP"="#cc9933",
              "LP"="#0033CC", "NP"="#009999", "Electorate not established in 2007"="black", "Electorate not established in 2004"="black")

#~~~~SERVER CODE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server = function(input, output, session) {
  #add buttons created above to gain access to external sites and resources
  output$Find <- renderUI({
    actionButton("Find_button")  
  })
  output$check <- renderUI({
    actionButton("check_button")  
  })
  output$MP <- renderUI({
    actionButton("RIDL_button")  
  })
  output$pdf <- renderUI({
    actionButton("pdf_button")  
  })
  output$MOG <- renderUI({
    actionButton("MOG_button")  
  })
  output$address <- renderUI({
    actionButton("address_buttons")  
  })
  
# FIRST TABLE UNDER MAP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# table_AEC ouput  
  
    #creating a reactive table that displays the data from df.melted based on the selected electorates from the division_input dropdown
    #reactive event triggered by user selecting an electorate (division_input drop-down or map_shape_click)
    selectedData<-eventReactive(df.melted$Elect_div==input$division_input,  { 
    #store the subsetted data in an object  - we will use this later to return a list of colours
    dat <- subset(df.melted,df.melted$Elect_div==input$division_input)
    
    # Create the table (using table from htmlTables package)
    HTML(
      htmlTable(subset(df.melted,df.melted$Elect_div==input$division_input), #table only displays information for the selected division_input
                align="c", #centre aligned
                header=c("Elected party"," Electorate"), #header
                rnames= paste(c( "2019","2016","2013", "2010", "2007", "2004")), #rownames
                #return a list of colours as defined in dat object above to match the previous elected parties for the selected electorate
                #transpose the data and return the colours from our party_cols colour scheme which match Party Acronymns in the subseted data
                #vapply is like sapply but has a pre-specified type of return value - so can be safer to use
                css.cell = t(vapply(party_cols[dat$value], 
                                    function(x) rep(sprintf("color: #FFF;background-color: %s;", x), 2), #white text on coloured cells
                                    character(2))), #colour of columns change to match the colours of the elected party
                caption="Historic elected party data from the Australian Electoral Commission (AEC)",
                tfoot="&dagger;The geographic electoral boundaries change over in time in response to population shifts. The map above uses the 2018 distributions."
                
      ))
    
  })
  #output a table based on the SelectedData reactive event
  output$table_AEC <- renderUI({selectedData()})
 
#SECOND TABLE UNDER MAP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#table_unemp output
  
  selectedData2<-eventReactive(df.workforce$Elect_div==input$division_input,  { 
    
    # Create the table (using table from htmlTables package)
    HTML(
      htmlTable(subset(df.workforce,df.workforce$Elect_div==input$division_input), 
                align="c",
                header=c("2018 %s"," Electorate"),
                rnames= paste(c( "Unemployment rate %","Participation rate %")), 
                #css.cell = t(vapply(party_cols[dat$value], function(x) rep(sprintf("color: #FFF;background-color: %s;", x), 2), character(2))), #colour of columns change to match the colours of the elected party
                caption="Labourforce statistics",
                tfoot="&dagger;Participation & Unemployment rate calculated using data from Australian Government Dept of Jobs & Small Business (2018)"
                
      ))
    
  })
  output$table_unemp <- renderUI({selectedData2()})
  
  
#GRAPHS to right of map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#Create the plots for all the different bar charts we want displayed using the dfs we created above (age, income etc.)
#using ggplot and plotly to create the bar charts/ distributions
  
  #create the population per electorate by age and sex pyramid graph
  output$agePyramid <- renderPlotly({
    
    ageplot<-ggplot(
      subset(df.age,df.age$Elect_div==input$division_input),
      mapping = aes(x = variable2, fill=variable3,
                    y = ifelse(test = variable3 == "males", 
                               yes = -value, no = value), text = paste("Value:", value))) + geom_bar(stat = "identity")+
      scale_x_discrete(labels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34", "35-39","40-44", 
                                "45-49","50-54","55-59","60-64","65-69", 
                                "70-74","75-79", "80-84","85-89","90-94", "95-99", "100+")) +
      scale_y_continuous(labels = abs, limits = max(df.age$value) * c(-1,1)) +
      theme_classic() + scale_fill_manual(values = MF, guide=FALSE) +
      labs(title = "Population", subtitle = "Data from ABS", x = "", y = "", fill= "Sex")+
      coord_flip()
    
    ggplotly(ageplot, tooltip=c("text"))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) })
  
 
   #create the education pyramid graph output
  output$eduPyramid <- renderPlotly({
    
    eduplot<-ggplot(
      subset(df.edu,df.edu$Elect_div==input$division_input),
      mapping = aes(x = fct_inorder(variable2),fill=variable3,
                    y = ifelse(test = variable3 == "males", 
                               yes = -value, no = value),text = paste("Value:", value))) +
      geom_bar(stat = "identity")+
      scale_x_discrete(labels=c("Yr 9 & below","Yr 10 & above","Cert III & IV","Advanced Dip & Dip", 
                                "Bachelor's","Grad Dip & Grad Cert", "Post-Graduate")) +
     scale_y_continuous(labels = abs, limits = max(df.edu$value) * c(-1,1)) +
     theme_classic() + scale_fill_manual(values = MF, guide=FALSE) +
     labs(title = "Highest Level of Education", subtitle = "Data from ABS", x = "", y = "", fill= "Sex")+
     coord_flip()
    
    ggplotly(eduplot, tooltip=c("text"))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) })
  
  
  
  #projected population per CED 
  output$projpopGraph <- renderPlotly({
    projpopplot<-ggplot(subset(df.proj.pop, df.proj.pop$Elect_div == input$division_input),
                        aes(x=Year, y=value, colour=Year,group= 1,text = paste("Value:", value)))+
      geom_point(size = 4)+ geom_smooth(size=0.4)+
      
      scale_y_continuous(labels = point,expand = c(0, 0)) + #remove scientific notation
      theme_classic()+ scale_color_manual(values= mycol(6),guide=FALSE)+
      theme(axis.text.x = element_text(angle=45, hjust = 1, size = 8, face= 'bold'))+ coord_cartesian(ylim = c(100000, 700000))+
      
      labs(title = "Projected population", subtitle = "Data from Qld Gov", x = "", y = "")
    ggplotly(projpopplot, tooltip=c("text"))%>% config(displayModeBar = F) })
  
  
  
  #weekly income per CED by Sex 
  output$incomePyramid <- renderPlotly({
    incplot<-ggplot(
      subset(df.income,df.income$Elect_div==input$division_input),
      mapping = aes(x = fct_inorder(variable2),fill=variable3,
                    y = ifelse(test = variable3 == "males", 
                               yes = -value, no = value),text = paste("Value:", value))) + geom_bar(stat = "identity")+
      scale_x_discrete(labels=c("$1-149","$150-299","$300-399","$400-499","$500-649", "$650-799", "$800-999", "$1000-1249", "$1250-1499", "$1500-1749",
                                "$1750-1999", "$2000-2999", ">$3000"))+
      scale_y_continuous(labels = abs, limits = max(df.income$value) * c(-1,1)) +
      theme_classic() + scale_fill_manual(values = MF, guide=FALSE)+
      labs(title = "Weekly personal income",  subtitle = "Data from ABS",x = "", y = "", fill="Sex")+
      coord_flip()
    ggplotly(incplot, tooltip=c("text"))%>% config(displayModeBar = F) })
  

  #create the reactive function to calculate the percentages per industry per electorate
  #calculate the label positions and create the label text
  df.ind.calc_reactive<-reactive ({
    a<-subset(df.ind, Elect_div==input$division_input)%>% #subset based on used input
      group_by(Elect_div, variable3,variable2) %>% #group
      summarise(sum_value=sum(value)) %>% #sum
      mutate(pct_value=sum_value/sum(sum_value)*100)%>% #calculare percentages
      mutate(pos_scaled = cumsum(pct_value) - pct_value / 2,# create position for labels
             perc_text = paste0(round(pct_value), "%")) #create label text
    return(a)
  })
  
  output$industryPie <- renderPlot({
    indplot<-ggplot(df.ind.calc_reactive(), #using data from the reactive function we created to calc % above
      aes(x = "",y=pct_value, fill = variable2))+  
      geom_bar(width = 1,stat="identity",colour="white")+  
      facet_grid( ~ fct_rev(variable3))+ #two pie charts and order the factors
      coord_polar(theta = "y")+  #creating pie chart
      labs(title= "Industry of employment", color="Industries", x="", y="")+
      theme_void()+ #no axis so use theme void
      geom_text(aes(x=1.6,label = perc_text), size = 3.5,position = position_stack(vjust = 0.5))+ #label positioning
      guides(fill = guide_legend(title="",nrow=4,byrow=TRUE))+ #set legend rows
      theme(legend.position="bottom")+ #legend positon
      scale_fill_manual(values=mycol(11), #fill and manually create human readable labels
                        labels=c("Arts & Rec","Health","Real estate","Finance & Insurance","Logistics",
                                 "Media & Telecomms","Professionals, Science & Tech","Public Admin & Safety","Mining",
                                 "Retail","Agri/Forest/Fish"))+
      theme(plot.title = element_text(size = 20,hjust = 0.5),strip.text = element_text(size = 15), legend.text=element_text(size=13))
    indplot})

  #number of ppl who are self employed per CED
  output$selfempBar <- renderPlotly({
    selfplot<-ggplot(subset(df.self_emp, df.self_emp$Elect_div == input$division_input),
                    aes(variable, value,text = paste("Value:", value))) + geom_bar(stat = "identity", aes(fill = value))+
      scale_x_discrete(labels = c("Total", "Male", "Female")) +
      theme_classic() + scale_fill_gradientn(colours = mycol1(3), guide=FALSE) +
     theme(axis.text.x = element_text(angle=45, hjust = 1, size = 8, face= 'bold'))+ 
     labs(title = "Self-employment",  subtitle = "Data from ABS", x = "", y = "")
    ggplotly(selfplot, tooltip=c("text"))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) })
  
  
  #number of people who had a change of address (interstate and overseas)
  output$migBar <- renderPlotly({
    migplot<-ggplot(subset(df.mig, df.mig$Elect_div == input$division_input),
                    aes(variable, value,text = paste("Value:", value))) + geom_bar(stat = "identity", aes(fill = value))+
      scale_x_discrete(labels = c("ACT", "NSW", "NT", "SA", "Tas", "Vic", "WA", "O/S")) +
      theme_classic() + scale_fill_gradientn(colours = mycol1(8), guide=FALSE) +
      theme(axis.text.x = element_text(angle=45, hjust = 1, size = 8, face= 'bold'))+ 
      labs(title = "Interstate & overseas migration",  subtitle = "Data from ABS", x = "", y = "")
    ggplotly(migplot, tooltip=c("text"))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) })
  
  #DSS payments per CED
  output$dssBar <- renderPlotly({
    dssplot<-ggplot(subset(df.dss, df.dss$Elect_div == input$division_input),
                    aes(fct_inorder(variable), value, text = paste("Value:", value))) + geom_bar(stat = "identity", aes(fill = value))+
      scale_x_discrete(labels = c("Age Pension","Pensioner Concession","Carer Allowance",
                                  "Cwlth Seniors Health","Disability Pension","Family Tax Benifit A",
                                  "Family Tax Benifit B","Low Income","Newstart Allowance","Cwlth Rent Assist")) +
      theme_classic() + scale_fill_gradientn(colours = mycol1(10), guide=FALSE) +
      theme(axis.text.x = element_text(angle=45, hjust = 1, size = 8, face= 'bold'))+ 
      labs(title = "Dept of Social Services allowances", subtitle = "Data from DSS", x = "", y = "")
    
    ggplotly(dssplot, tooltip=c("text"))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) })
  
#calculate percentages and label positions for tenure type pie chart  
  df.tenure.calc_reactive<-reactive ({
    c<-subset(df.tenure, Elect_div==input$division_input)%>%
      group_by(Elect_div, variable) %>% 
      summarise(sum_value=sum(value)) %>% 
      mutate(pct_value=sum_value/sum(sum_value)*100)%>%
      mutate(pos_scaled = cumsum(pct_value) - pct_value / 2,
             perc_text = paste0(round(pct_value, digits =2), "%")) 
    return(c)
  })
  
#tenure type pie chart output  
  output$tenurePie <- renderPlot({
    tenureplot<-ggplot(df.tenure.calc_reactive(),
                aes(x = "",y=pct_value, fill = variable))+  
                geom_bar(width = 1,stat="identity",colour="white")+
                coord_polar(theta = "y")+  
                labs(title= "Home ownership status", color="", x="", y="")+
                theme_void()+ 
                geom_text(aes(x = 1.75, y = pos_scaled, label = perc_text), size = 3.5) +
                guides(fill = guide_legend(title=""))+
                theme(legend.position="bottom")+
                scale_fill_manual(values=mycol1(3),
                        labels=c("Owned w mortgage", "Rented","Owned outright"))+
              theme(plot.title = element_text(size = 20,hjust = 0.5),
                    strip.text = element_text(size = 15), legend.text=element_text(size=13))
    tenureplot})
  
  #dwelling structure output
  output$dwellingPIe <- renderPlot({
    dwellplot<-ggplot(
              subset(df.dwell.cal,df.dwell.cal$Elect_div==input$division_input),
              aes(x = "",y=round(pct_value, digits=2), fill = variable))+  
              geom_bar(width = 1,stat="identity",colour="white")+ 
              coord_polar(theta = "y")+  
              labs(title= "Dwelling structure", color="", x="", y="")+
              theme_void()+
              geom_text(aes(x=1.66,label = perc_text), size = 3.5,position = position_stack(vjust = 0.5))+
              guides(fill = guide_legend(reverse = TRUE,title=""))+
              theme(legend.position="bottom")+
              scale_fill_manual(values=mycol1(3),
                        labels=c("Flat/Appartment","Semi-detached", "Separate home"))+
      theme(plot.title = element_text(size = 20,hjust = 0.5),strip.text = element_text(size = 15), legend.text=element_text(size=13))
    dwellplot})
  
  
  
#MAP OUTPUT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#create the base map that will be displayed regardless of selected input
  output$map<-renderLeaflet({
    
    leaflet(qld.stats) %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
      
      # Centre the map in the middle of our co-ordinates
      fitBounds(min(137.99),max(-29.18),min(153.55),max(-9.12))
    
  }) 
  
  #we need to set up 3 separate colour schemes for the different options from the spatial stats drop down menu
  #one for current party using factor levels to match the party colours
  #one for the previous election results using same rationale
  #one for the numeric based stats for unemployment rate and participation rate
  
  observe({
      if (input$stats_input == "Elected_Party_2019") {
      pal <- colorFactor(c("#C12525","#6600CC","#021893"), domain= qld.stats[[input$stats_input]])
    } else if (input$stats_input == "Sex_2019") {
        pal <- colorFactor(c("#C12525","#CC6666", "#6600CC","#021893","#99CCFF"), domain= qld.stats[[input$stats_input]])
    } else if (input$stats_input == "PartyNm.1") {
      pal <- colorFactor(c("#C12525","#6600CC","#021893"), domain= qld.stats[[input$stats_input]])
    } else if (input$stats_input == "Sex_2016") {
      pal <- colorFactor(c("#C12525","#CC6666", "#6600CC","#021893","#99CCFF"), domain= qld.stats[[input$stats_input]])
    } else if (input$stats_input == "Elected_Party_2016") {
      pal <- colorFactor(c("#C12525","#6600CC","#021893", "yellow"), domain= qld.stats[[input$stats_input]])
    } else if (input$stats_input == "Elected_Party_2013") {
      pal <- colorFactor(c("#C12525","#6600CC","#021893", "yellow"), domain= qld.stats[[input$stats_input]])
    } else {
      pal <- colorNumeric(c("#8D99AE","#c12525"), domain = qld.stats[[input$stats_input]], reverse = FALSE)
    }
    
    
    
    #creating the lables for the pop-ups on each clickable map-shape (polygon)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      Sitting member: %s <br/><br/>
      Sitting party: %s <br/><br/>
      Margin for seat (percentage): %s<br/><br/>
      Turnout percentage: %s<br/><br/>
      Previous elected party: %s<br/><br/>
      Population: %s<br/><br/>
      Projected temp increase by 2050 (+ degrees in Celcius ): %s<br/>", 
      qld.stats$Elect_div, qld.stats$sitting_member_2019,qld.stats$Elected_Party_2019, qld.stats$margins, qld.stats$TurnoutPercentage, qld.stats$Elected_Party_2016,
      qld.stats$CED_pop_total, qld.stats$Temp_Change_2050) %>% lapply(htmltools::HTML)
   
    #creating a proxy map that displays the various stats from the stats drp down 
    leafletProxy("map", data = qld.stats) %>%
      clearShapes() %>%
      addPolygons(
        layerId = qld.stats$Elect_div,
        fillColor = ~pal(qld.stats[[input$stats_input]]), #colour map polygons based on user input
        fillOpacity = 0.6,
        weight = 0.6,
        opacity = 1,
        color = "#FFFFFF",
        dashArray = "2",
        label = labels,
        highlight = highlightOptions( # highlight the polygons when they are selected from the dropdown menu or clicked on
          weight = 4,
          color = "#FFFFFF",
          dashArray = "3",
          fillOpacity = 2,
          bringToFront = TRUE),
        labelOptions = labelOptions( #labels
          style = list("font-weight" = "normal", padding = "3px 5px"),
          textsize = "13px",
          direction = "auto")
      )
    
    #we are adding a legend to display the raw data that aligns with the spatially depicted stat from the stats_input drop-down
    #this information is also displayed in the pop-ups for each clickable electorate
    varname<-switch(input$stats_input,
                    "Elected_Party_2019"="2019 Sitting Party", 
                    "sitting_member_2019"="Sitting member 2019",
                    "Sex_2019"="Sex & party of sitting member",
                    "PartyNm.1"="2016 election results",
                    "TurnoutPercentage"="2016 turnout percentage",
                    "Electorate Population"="CED_pop_total",
                    'CED_participation_rate_2018'="Work-force participation rate %",
                    'Unemployment_rate_2018'="Unemployment rate %",
                    'Sex_2016'="Gender & party of sitting member (2016)",
                    'TurnoutPercentage'='Turn out percentage (2016)',
                    "Elected_Party_2013"="2013 election results")
   

    leafletProxy("map", data = qld.stats) %>% clearControls() %>%
      addLegend(pal = pal, opacity = 0.9, title = varname,
                values = ~qld.stats[[input$stats_input]],labels = c(min(input$stats_input), max(input$stats_input)),
                position = "topright")
  })
  
  
  #we want to create a reactivity so users can either select the division_input 
  #from the drop down menu or by clicking on the map
  
  observe({
    event <- input$map_shape_click
    if (is.null(event))
      return()
    updateSelectInput(session, "division_input", selected = event$id)
  })
  
  #we want to create reactivity so that the map to zooms in on and focus on the selected electorate
  observe({
    selectedPolygon <- subset(qld.stats, qld.stats$Elect_div == input$division_input)
    leafletProxy("map", data = qld.stats) %>%
      removeShape("highlightedPolygon") %>%
      fitBounds(selectedPolygon@bbox[1,1],
                selectedPolygon@bbox[2,1],
                selectedPolygon@bbox[1,2],
                selectedPolygon@bbox[2,2]) %>%
      addPolylines(weight = 4, color = "white",
                   data = selectedPolygon, layerId = "highlightedPolygon")
  })
  
  
  
  
}


shinyApp(ui, server)


