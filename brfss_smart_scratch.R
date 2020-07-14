library('haven')
mmsa02 <- read_xpt('~/Downloads/MMSA02.xpt')
t(mmsa02[1,])
table(mmsa02$A_MMSANA)
table(mmsa02$A_MMSA)

chi <- subset(mmsa02,grepl('Chicago',A_MMSANA))
t(summary(chi))
chi$INCOME2

cnty02 <- read_xpt('~/Downloads/CNTY02.XPT')
t(cnty02[1,])
