# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev pandoc 
    
    
# install plumber, GGally
RUN R -e "install.packages(c('GGally', 'leaflet', 'plumber', 'parsnip', 'ggplot2', 'yardstick', 'dplyr', 'tidymodels', 'ranger'))"

# copy myAPI.R from the current directory into the container
COPY API.R API.R
COPY diabetes_final.rds diabetes_final.rds
COPY rf_final_wkf.rds rf_final_wkf.rds
COPY rf_full_fit.rds rf_full_fit.rds

# open port to traffic
EXPOSE 8000

# when the container starts, start the API.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=8000)"]