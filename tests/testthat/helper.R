data('testLongData')

testLongDataNamed = data.table::copy(testLongData)
data.table::setnames(testLongDataNamed, c('PatientId', 'Assessment', 'Measurement'))

testLongDataFrame = data.table::copy(testLongData)
data.table::setnames(testLongDataFrame, c('PatientId', 'Assessment', 'Measurement'))
testLongDataFrame = as.data.frame(testLongDataFrame)
