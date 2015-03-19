# parsing sites
# parse_all <- function(){
   
#  setwd("E:\\doc\\R i Big Data\\Rproject1\\wybory")

   for(i in 1:28){
      
      #do those function every 6 hours 
      parse_tvp.info()
      parse_wp.pl()
      parse_wprost.pl()
      parse_onet.pl()
      parse_newsweek.pl()
      parse_tvn24.pl()
      parse_natemat.pl()
      parse_gazeta.pl()
      parse_dziennik.pl()
      parse_wyborcza.pl()
      
      # do this every day
      if(i%%4==1) { 
         #wpolityce() 
         twitter_kandydaci()
      }
      Sys.sleep(60*60*6+60)

   }
# }


parse_all()



# parsing tvn24.pl articles
#arch_tvn()

#parsing wploityce.pl articles
#wpolityce()
   
# df <- read.table("C:\\Users\\grabarze\\Desktop\\aa.csv", header = TRUE, sep=";",dec=",", fileEncoding="windows-1250")
# write.table(df,"C:\\Users\\grabarze\\Desktop\\2015-03-16.csv", sep=";",dec=",", fileEncoding="UTF-8",
#             append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)

   
#    aa <- iconv(readLines("E:\\doc\\R i Big Data\\Rproject1\\wybory\\tweety_kandydatow_2015-03-18.csv"), 
#                from="windows-1250", to="UTF-8")
#    
#    
#    
#    writeLines(aa,"E:\\doc\\R i Big Data\\Rproject1\\wybory\\tweety_kandydatow_2015-03-18.csv")
#    
