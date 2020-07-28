
library(dashboardthemes)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)

#library(ghyp)
library(ggplot2)
library(dplyr)
library(rlang)
library(purrr)
library(tidyr)
library(formattable)

library(lubridate)
#library(rayshader)
#library(d3heatmap)
library(tidyquant)
library(quantmod)
library(DT)




library(tidyquant)



#library(forecast)
library(dplyr)

library(dygraphs)

#library(echarts4r)

#library(plotly)

library(xts)






######

ui <- dashboardPage(

                    
                    
                    
  dashboardHeader(title = "Live FX and Random Walks with R", titleWidth = 350),
  dashboardSidebar(theme_grey_dark, width=170,
                  
                     tags$head(
                       tags$style(HTML("
                                       .sidebar { height: 90vh; overflow-y: auto; }
                                       " )
                       )),
                      
    
     
              
    
    shiny::sliderInput('riskfree', 'Simulations:',
                min = 10, max = 1000, value = 20),
    
    selectInput("variable", "Country:", 
                       selected = c('ALL'='ALL'),
                       c(
                         #'ADP'='ADP',
                         #'AED'='AED',
                         #'AFA'='AFA',
                         'AFN'='AFN',
                         #'ALK'='ALK',
                         'ALL'='ALL',
                         'AMD'='AMD',
                         'ANG'='ANG',
                         
                         'AOA'='AOA',
                        # 'AOK'='AOK',
                         #'AON'='AON',
                         #'AOR'='AOR',
                         #'ARA'='ARA',
                         #'ARP'='ARP',
                         'ARS'='ARS',
                         #'ARY'='ARY',
                         #'ATS'='ATS',
                         'AUD'='AUD',
                        
                         'AWG'='AWG',
                         #'AYM'='AYM',
                         #'AZM'='AZM',
                         'AZN'='AZN',
                         #'BAD'='BAD',
                         'BAM'='BAM',
                         'BBD'='BBD',
                         'BDT'='BDT',
                         #'BEC'='BEC',
                         #'BEF'='BEF',
                         #'BEL'='BEL',
                         #'BGJ'='BGJ',
                         #'BGK'='BGK',
                         #'BGL'='BGL',
                         'BGN'='BGN',
                         'BHD'='BHD',
                         'BIF'='BIF',
                         'BMD'='BMD',
                         'BND'='BND',
                         'BOB'='BOB',
                         #'BOP'='BOP',
                         
                         'CAD'='CAD',
                         'CDF'='CDF',
                         #'CHC'='CHC',
                         #'CHE'='CHE',
                         'CHF'='CHF',
                         'CHF'='CHF',
                         #'CHW'='CHW',
                         #'CLF'='CLF',
                         'CLP'='CLP',
                         'CNY'='CNY',
                         'COP'='COP',
                         #'COU'='COU',
                         'CRC'='CRC',
                        
                        # 'DDM'='DDM',
                         'DEM'='DEM',
                         'DJF'='DJF',
                         'DKK'='DKK',
                         'DKK'='DKK',
                         'DKK'='DKK',
                         'DOP'='DOP',
                         'DZD'='DZD',
                         'ECS'='ECS',
                         #'ECV'='ECV',
                         #'EEK'='EEK',
                         'EGP'='EGP',
                         'ERN'='ERN',
                         #'ESA'='ESA',
                         #'ESB'='ESB',
                        
                         #'ESP'='ESP',
                         'ETB'='ETB',
                        
                       
                      
                        
                         'EUR'='EUR',
                         #'FIM'='FIM',
                         #'FIM'='FIM',
                         'FJD'='FJD',
                         'FKP'='FKP',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'FRF'='FRF',
                         'GBP'='GBP',
                         'GBP'='GBP',
                         'GBP'='GBP',
                         'GBP'='GBP',
                         #'GEK'='GEK',
                         'GEL'='GEL',
                         #'GHC'='GHC',
                         #'GHP'='GHP',
                         'GHS'='GHS',
                         'GIP'='GIP',
                         'GMD'='GMD',
                         #'GNE'='GNE',
                         'GNF'='GNF',
                         #'GNS'='GNS',
                         #'GQE'='GQE',
                         #'GRD'='GRD',
                       
                         'HKD'='HKD',
                         'HNL'='HNL',
                        # 'HRD'='HRD',
                         'HRK'='HRK',
                         'HRK'='HRK',
                         'HTG'='HTG',
                         'HUF'='HUF',
                         'IDR'='IDR',
                         'IDR'='IDR',
                         'IEP'='IEP',
                         #'ILP'='ILP',
                         #'ILR'='ILR',
                         'ILS'='ILS',
                         'INR'='INR',
                         'INR'='INR',
                         'IQD'='IQD',
                         'IRR'='IRR',
                         #'ISJ'='ISJ',
                         'ISK'='ISK',
                         'ITL'='ITL',
                         'ITL'='ITL',
                         'ITL'='ITL',
                         'JMD'='JMD',
                         'JOD'='JOD',
                         'JPY'='JPY',
                         'KES'='KES',
                         'KGS'='KGS',
                         'KHR'='KHR',
                         'KMF'='KMF',
                         'KPW'='KPW',
                         'KRW'='KRW',
                         'KWD'='KWD',
                         'KYD'='KYD',
                         'KZT'='KZT',
                         #'LAJ'='LAJ',
                         'LAK'='LAK',
                         'LBP'='LBP',
                         'LKR'='LKR',
                         'LRD'='LRD',
                         'LSL'='LSL',
                         #'LSM'='LSM',
                         'LTL'='LTL',
                         #'LTT'='LTT',
                         'LUC'='LUC',
                         'LUF'='LUF',
                         'LUL'='LUL',
                         'LVL'='LVL',
                         'LVR'='LVR',
                         'LYD'='LYD',
                         'MAD'='MAD',
                         'MAD'='MAD',
                         'MDL'='MDL',
                         'MGA'='MGA',
                         'MGF'='MGF',
                         'MKD'='MKD',
                         'MLF'='MLF',
                         'MMK'='MMK',
                         'MNT'='MNT',
                         'MOP'='MOP',
                         'MRO'='MRO',
                         'MTL'='MTL',
                         'MTP'='MTP',
                         'MUR'='MUR',
                         'MVQ'='MVQ',
                         'MVR'='MVR',
                         'MWK'='MWK',
                         'MWK'='MWK',
                         'MXN'='MXN',
                         'MXP'='MXP',
                         'MXV'='MXV',
                         'MYR'='MYR',
                         'MZE'='MZE',
                         'MZM'='MZM',
                         'MZN'='MZN',
                         'NAD'='NAD',
                         'NGN'='NGN',
                         'NIC'='NIC',
                         'NIO'='NIO',
                         'NLG'='NLG',
                         'NOK'='NOK',
                         'NOK'='NOK',
                         'NOK'='NOK',
                         'NPR'='NPR',
                         'NZD'='NZD',
                         'NZD'='NZD',
                         'NZD'='NZD',
                         'NZD'='NZD',
                         'NZD'='NZD',
                         'OMR'='OMR',
                         'PAB'='PAB',
                         'PEH'='PEH',
                         'PEI'='PEI',
                         'PEN'='PEN',
                         'PEN'='PEN',
                         'PES'='PES',
                         'PGK'='PGK',
                         'PHP'='PHP',
                         'PKR'='PKR',
                         'PLN'='PLN',
                         'PLZ'='PLZ',
                         'PTE'='PTE',
                         'PYG'='PYG',
                         'QAR'='QAR',
                         'RHD'='RHD',
                         'ROK'='ROK',
                         'ROL'='ROL',
                         'RON'='RON',
                         'RON'='RON',
                         'RSD'='RSD',
                         'RUB'='RUB',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RUR'='RUR',
                         'RWF'='RWF',
                         'SAR'='SAR',
                         'SBD'='SBD',
                         'SCR'='SCR',
                         'SDD'='SDD',
                         'SDG'='SDG',
                         'SDG'='SDG',
                         'SDP'='SDP',
                         'SEK'='SEK',
                         'SGD'='SGD',
                         'SHP'='SHP',
                         'SIT'='SIT',
                         'SKK'='SKK',
                         'SLL'='SLL',
                         'SOS'='SOS',
                         'SRD'='SRD',
                         'SRG'='SRG',
                         'SSP'='SSP',
                         'STD'='STD',
                         'SUR'='SUR',
                         'SVC'='SVC',
                         'SYP'='SYP',
                         'SZL'='SZL',
                         'THB'='THB',
                         'TJR'='TJR',
                         'TJS'='TJS',
                         'TMM'='TMM',
                         'TMT'='TMT',
                         'TND'='TND',
                         'TOP'='TOP',
                         'TPE'='TPE',
                         'TRL'='TRL',
                         'TRY'='TRY',
                         'TRY'='TRY',
                         'TTD'='TTD',
                         'TWD'='TWD',
                         'TZS'='TZS',
                         'UAH'='UAH',
                         'UAK'='UAK',
                         'UGS'='UGS',
                         'UGW'='UGW',
                         'UGX'='UGX',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                         'USD'='USD',
                      
                         'VEF'='VEF',
                         'VEF'='VEF',
                         'VEF'='VEF',
                         
                         'VND'='VND',
                         
                         'WST'='WST',
                         
                         'XPT'='XPT', 
                         #'YDD'='YDD',
                         'YER'='YER',
                         #'YUD'='YUD',
                         #'YUM'='YUM',
                         #'YUN'='YUN',
                         
                         #'ZWD'='ZWD',
                         'ZWD'='ZWD',
                         'ZWL'='ZWL'
                         #'ZWN'='ZWN',
                         #'ZWR'='ZWR'
                         
                         
                         
                         
                         
                         ))
    
  
  
   
    
    
 
    
    ),
  
  
  

  
  
  dashboardBody(theme_grey_dark, 
 
    fluidRow(
                 valueBoxOutput("minbox"),
                  valueBoxOutput("riskfreebox"),
                 valueBoxOutput("maxbox")
                  
               )
    ,
  
    fluidRow(box(title="Today's Exchange", width=100,
      
            formattableOutput("table1", width="85%")))
    ,
 
   

    
    fluidRow(
    
    tabsetPanel(type = "tabs",
              
                tabPanel("Forecast", dygraphOutput("plot1",
                                                          width = "90%", height = "300px")),
                
               tabPanel("Downloads",  
                        downloadButton("downloadData2", "Historical Returns") )
    )
    
    )
    

  ) 
  
)







