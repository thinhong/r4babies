library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(writexl)

# --- Generate random data
data <- list()
nrows <- 1000
cols <- c("gioitinh","ngaysinh","xa_huyen", "tinh","VGB <24", "VGB >24","VGB_1","VGB_2","VGB_3",
          "VGB_4+","HG_1","HG_2","HG_3","HG_4+","UV_1","UV_2","UV_3","UV_4+","tinhtrang")
xa_huyen <- readRDS(file.path(getwd(), "data", "xa_huyen_HCM.rds"))

for (i in 1:length(cols)){
  if (cols[i] == "gioitinh"){
    vals <- c("nữ", "nam")
  }
  else if (cols[i] == "tinhtrang"){
    vals <- c("theo dõi", "ngừng theo dõi")
  } 
  else if (cols[i] == "xa_huyen"){
    # vals <- # read xa_huyen combi fr HCDC data
    # sample from that list
    vals <- xa_huyen[[1]]
  }
  else if (cols[i] == "TCDD"){
    vals <- c(NA, "x")
  }
  else if (cols[i] == "ngaysinh"){
    # --- sample date from 01/01/2024 to 30/04/2024
    vals <- seq(as.Date("2024-01-01"), to = as.Date("2024-04-01"), by = "day")
  }else if (cols[i] == "tinh"){
    data[[ cols[i] ]] <- rep("Thành phố Hồ Chí Minh")
    next
  }
  else{
    # --- special handling while sampling date 
    if (cols[i] %in% c("VGB <24", "HG_1", "UV_1")){
      data[[ cols[i] ]] <- data[[ "ngaysinh" ]] + sample(c(0, 1), size = nrows, replace=TRUE)
    }else if (cols[i] == "VGB >24"){
      data[[ cols[i] ]] <- data[[ "VGB <24" ]] + sample(c(-1, 0, 1), prob = c(0.01, 0.09, 0.9), size = nrows, replace = TRUE)
    }else{
      # if not the 1st shot, sample by add a few days fr the prev shot
      data[[ cols[i] ]] <- data[[ cols[i-1] ]] + sample(c(-2, -1, 0, 1, 3, 4, 10, 15), prob=c(0.005, 0.005, 0.045, 0.045, 0.05, 0.05, 0.4, 0.4), size = nrows, replace = TRUE)
    }
    
    next
  }
  
  data[[ cols[i] ]] <- sample(vals, size = nrows, replace = TRUE)
}

data <- as.data.frame(data)

# mutate to bad date format
# rename to bad naming scheme
data <- data %>% 
  mutate(across(where(is.Date) , ~format(., "%d/%m/%Y"))) %>%
  separate(xa_huyen, c("huyen", "xa"), sep="_") %>% 
  rename(
  `VGB <24` = VGB..24 ,
  `VGB >24` = VGB..24.1,
  `VGB_4+` = VGB_4.,
  `HG_4+` = HG_4.,
  `UV_4+` = UV_4.
  ) 

# --- Add some NA values cuz y not
date_index <- which(str_detect(colnames(data), "[:digit:]"))
for (i in 1:nrows){
  # sample no NA cols
  sample_no_na <- sample(c(0, 1, 2, 3, 4), prob = c(0.96, 0.01, 0.01, 0.01, 0.01), 1)
  # sample cols to make it NA
  sample_col <- sample(date_index, sample_no_na)
  
  data[i,sample_col] <- NA
}

# --- Add outliers
for (i in 1:nrows){
  # sample no outiler cols
  sample_no_outlier <- sample(c(0, 1, 2, 3, 4), prob = c(0.96, 0.01, 0.01, 0.01, 0.01), 1)
  # sample cols to make it an outlier
  sample_col <- sample(date_index, sample_no_outlier)

  data[i, sample_col] <- "01/01/0001"
}

# --- Add id columns
data <- data %>% 
  mutate(id = 1:n(), .before = gioitinh)

data %>% head()

# save data 
write_xlsx(data, file.path(getwd(), "data", "vaccine_data.xlsx"))
