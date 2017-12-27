library(tseries)
library(zoo)
library(tseries)
library(forecast)
library(readxl)
library(gmp) # factorize() function
library(ggplot2)

alpha<-0.05

ar <- read_excel("C:/Users/Qubix/Desktop/GitHub/ARIMA_Simple_Predict/ar.xlsx", 
                 col_types = c("date", "numeric"))
#ar <- read_excel("./ar.xlsx", col_types = c("date","numeric"))
ncases<-nrow(ar)

# Use factorize() to figure out the sfactormallest frequency s.t. data has two periods
# and at least one extra point.
nf<-factorize(ncases)
period<-(2^length(nf[nf==2])+1)
period<-as.numeric(period[1][1])
print(paste("Default period is", period, "terms."),quote = FALSE)
temp<-readline(prompt = "Cycle Length [Enter for default]:")
if(temp!=""){period<-as.numeric(temp)}
rm(ncases)
rm(nf)

# Get the deseasoned data and store it in seas_stl
tscleaned<-tsclean(ts(zoo(ar$Count,order.by=as.character(ar$Event,format = "%Y-%m-%d")),frequency = period))
seas_stl<-seasadj(stl(tscleaned,s.window = "periodic"),s.window = "periodic")
rm(tscleaned)


# Peform the adf.test() -> Null: not stationary (stationary = mean and variance is constant over time)
adf_result_p<-adf.test(seas_stl,alternative = "stationary")$p.value

# Peform auto.arima with stationary = TRUE or FALSE given adf's test result
if(adf_result_p>0.05){
  print("Peforming nonstationary ARIMA")
  aa<-auto.arima(seas_stl,seasonal = TRUE, stationary = FALSE)
} else {
  print("Peforming stationary ARIMA")
  aa<-auto.arima(seas_stl,seasonal = TRUE, stationary = TRUE)
}

# Clean-up 
rm(adf_result_p)
rm(ar)

# Print residuals
ggtsdisplay(residuals(aa))

# Perform forecast
per<-readline(prompt = "How many periods to forecast?: ")
fcast<-forecast(aa,h = as.numeric(per))
xlabel<-paste("Term # = value *",period)
ylabel<-paste("Count")
autoplot(fcast, xlab=xlabel, ylab = ylabel)

