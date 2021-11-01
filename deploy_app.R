# install.packages("rsconnect")
library(rsconnect)

rsconnect::setAccountInfo(name='thais01fernandes',
                          token='704639C19B6F2F8E9C6CA316167A1F05',
                          secret='Cb6TpQeMu+r4FUsA2M//+85l9dwZRCKLHRXnKota')

deployApp()
