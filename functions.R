# Randomizing and transforming the coordinates of the original data set to ensure privacy
prep_data <- function() {
  
  shapefile_path <- "./src/Rheda_Wiedebrueck_EFHZFH_Faelle.shp"
  shape_data <- st_read(shapefile_path)
  shape_data <- shape_data[shape_data$bauj>=1900,]
  shape_data <- st_transform(shape_data, "EPSG:4326")
  shape_data$lon <- 0
  shape_data$lat <- 0
  shape_data$year <- year(as.Date(shape_data$datu, format="%d.%m.%Y")) 

  for (i in c(1:length(shape_data$geometry))) {
    coord_str <- st_coordinates(shape_data[i,]$geometry)
    random_fact <- sample(.005, size = 2, replace = TRUE)
    shape_data[i,]$lat <- coord_str[2] + random_fact[1]
    shape_data[i,]$lon <- coord_str[1] + random_fact[2]
  }
  
  st_write(shape_data, "./src/randomized/Rheda_Wiedebrueck_EFHZFH_Faelle.shp",delete_layer = T, append=FALSE)
}

# Calculation of the annual mean prices
calculate_year_mean <- function() {
  
  shapefile_path <- "./src/randomized/Rheda_Wiedebrueck_EFHZFH_Faelle.shp"
  shape_data <- st_read(shapefile_path)
  
  year_avg <- shape_data %>%
    group_by(year)%>%
    summarize( mean_year = mean(prei))
  
  year_avg_gem1 <- shape_data[shape_data$gema=="052540",] %>%
    group_by(year)%>%
    summarize( mean_year = mean(prei))
  
  year_avg_gem2 <- shape_data[shape_data$gema=="052553",] %>%
    group_by(year)%>%
    summarize( mean_year = mean(prei))
  
  df <- data_frame(year_avg$year, year_avg$mean_year, year_avg_gem1$mean_year, year_avg_gem2$mean_year)
  names(df) <- c("year", "mean", "rheda_mean", "wieden_mean")
  
  return(df)
}

# Calculation of the annual median prices
calculate_year_median <- function() {
  
  shapefile_path <- "./src/randomized/Rheda_Wiedebrueck_EFHZFH_Faelle.shp"
  shape_data <- st_read(shapefile_path)
  
  year_avg <- shape_data %>%
    group_by(year)%>%
    summarize( median_year = median(prei))
  
  year_avg_gem1 <- shape_data[shape_data$gema=="052540",] %>%
    group_by(year)%>%
    summarize( median_year = median(prei))
  
  year_avg_gem2 <- shape_data[shape_data$gema=="052553",] %>%
    group_by(year)%>%
    summarize( median_year = median(prei))
  
  df <- data_frame(year_avg$year, year_avg$median_year, year_avg_gem1$median_year, year_avg_gem2$median_year)
  names(df) <- c("year", "median", "rheda_median", "wieden_median")
  
  return(df)
}

get_index_rows <- function(){
  
  consumerprice <- read.csv2("./src/Preisindex/verbraucherpreisindex.csv", sep = ";")
  consumerprice <- consumerprice[consumerprice$X>2014,]
  rent_index <- read.csv2("./src/Preisindex/nettokaltmieten.csv", sep = ";")
  rent_index <- rent_index[rent_index$X>2014,]
    
  property_index <- c(290.7, 311.5, 333.6, 357.3, 388.6, 434.1, 477.5, 505.0, 513, 515)
  
  indices <- data_frame(consumerprice$X, consumerprice$Nordrhein.Westfalen, rent_index$Nordrhein.Westfalen, property_index)
  names(indices) <- c("year", "consumerprice index NRW", "rent index NRW", "property price index Rheda-Wiedenbrück")

  return(indices)
}

adjust_prices <- function(){
  construction_index <- read.csv2("./src/Preisindex/gebaeudeindex.csv", sep = ";")
  construction_index <- data_frame(construction_index$Jahr, construction_index$Häuserpreisindex)
  names(construction_index) <-c("year", "index")
    
  shapefile_path <- "./src/randomized/Rheda_Wiedebrueck_EFHZFH_Faelle.shp"
  shape_data <- st_read(shapefile_path)
  shape_data$adjusted_price <- 0
  
  index_2023 = construction_index[construction_index$year==2023,]
    
    for (i in c(1:length(shape_data$geometry))) {
      adjusted <- 0
      case <- shape_data[i,]
      const_year <- 0
      if(case$year>2023){
         const_year <- 2023
      }
      else{
        const_year <- case$year
      }
      if(const_year == 2015){
        adjusted <- case$prei * (index_2023$index / 100)
      }else if(const_year == 2023){
        adjusted <- case$prei
      }else{
        index_const_year <- construction_index[construction_index$year==const_year,]
        price_in_2015 <- case$prei / index_const_year$index * 100
        adjusted <- price_in_2015 * (index_2023$index / 100)
      }
      shape_data[i,]$adjusted_price <- round(adjusted / 100) * 100
    }
    
    st_write(shape_data, "./src/randomized/Rheda_Wiedebrueck_EFHZFH_Faelle.shp",delete_layer = T, append=FALSE)
}

generate_copula <- function(col1, col2, district){
  
  shape_cop_data <- data_frame(shape_data$prei,
                               shape_data$flac, 
                               shape_data$bauj,
                               shape_data$wofl,
                               shape_data$stst,
                               shape_data$kpwofl)
  
  rtData <- pobs(shape_cor_data[,c(col1,col2)])
  
  sel <- BiCopSelect(rtData[,1], rtData[,2])
  cop_sel<-VC2copula::BiCop2copula(sel$family, sel$par, sel$par2) # -> t
  return(persp(cop_sel, dCopula, col="#06416b",border="lightgray", zlab="", xlab="", ylab=""))
}

fit_regression_model <- function(){
  
  shapefile_path <- "./src/randomized/Rheda_Wiedebrueck_EFHZFH_Faelle.shp"
  shape_data <- st_read(shapefile_path)
  
  # fit regression model based on "Gutachterausschuss für Grundstückswerte im Kreis Gütersloh"
  modell <- lm(prei~flac+bauj+stst+year+wofl, data = shape_data)
  
  print(summary(modell))
  
  save(modell, file = "./src/price_development_model.rda")
}

past_prices_acc_index <- function(input){
  construction_index <- read.csv2("./src/Preisindex/gebaeudeindex.csv", sep = ";")
  construction_index <- data_frame(construction_index$Jahr, construction_index$Häuserpreisindex)
  names(construction_index) <-c("year", "index")
  
  if(input$year>2023){
    const_year <- 2023
  }else{
    const_year <- input$year
  }
  
  index_const_year <- construction_index[construction_index$year==const_year,]
  indexed_prices <- tibble(const_year, input$prei)
  
  for (i in c((const_year+1):2024)) {
    if(i >= 2023){
      indexed_prices <- rbind(indexed_prices,c(i, input$adjstd_))
    }else{
      index_i = construction_index[construction_index$year==i,]
      price_in_2015 <- input$prei / index_const_year$index * 100
      adjusted <- price_in_2015 * (index_i$index / 100)
      indexed_prices <- rbind(indexed_prices, c(i, adjusted))
    }
  }
  names(indexed_prices) <- c("year", "index_price")
  return(indexed_prices)
}

predict_prices <- function(model, input){
  predicted <- tibble()
  
  for (i in c(input$year:2029)) {
    predicted <- rbind(predicted, c(i, predict.lm(model, tibble("year"=i, "bauj"=input$bauj, "stst"=input$stst, "flac"=input$flac, "wofl"=input$wofl, "adjstd_"=input$adjstd_))))
  }
  
  names(predicted) <- c("year", "pred_price")
  return(predicted)
  
}

LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}
