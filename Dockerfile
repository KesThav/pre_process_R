FROM rocker/shiny:latest

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
    
RUN R -e "install.packages(c('shiny','DT','shinyjs','plotly','shinyWidgets','tidyverse','caret','datasets','mltools','ggcorrplot','GGally','e1071','purrr','kernlab','MASS','randomForest','lubridate'))"

RUN R -e "install.packages('shinydashboard')"

RUN mkdir /root/app2

COPY app /root/shiny_save

RUN chmod -R +r /root/shiny_save

EXPOSE 3838

CMD ["R","-e","shiny::runApp('/root/shiny_save',host='0.0.0.0',port=3838)"]