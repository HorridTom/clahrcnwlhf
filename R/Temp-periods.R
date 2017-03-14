c_date_str <- c('2011-01-01', '2014-03-31', '2020-05-04')
bins<- as.Date(c_date_str)
labs <- c('A', 'B')
EA_20$Discharge.period <- cut(EA_20$DischargeDate, breaks = bins, labels = labs)
