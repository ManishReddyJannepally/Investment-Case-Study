
############################################
library(pdftools)
library(stringr)
e = pdf_text("Countries_where_English_is_an_official_language.pdf")
#str_remove_all(e,"[\r\n]")
en = str_split(e,"[\r\n]")
#en = str_remove_all(en,"")
en
en_df = data.frame(en)
names(en_df) = "English_speaking_countries"
en_df = filter(en_df,!English_speaking_countries == "")
en_df = data.frame(en_df[-c(1,2,28),])
names(en_df) = "English_speaking_countries"
en_df$English_speaking_countries = as.character(en_df$English_speaking_countries)
count_spaces = function(data){
        i = 1
        space_count = list()
        while (i <= nrow(data)){
                space_count[i] = str_count(data[i,]," ")
                print(str_count(data[i,]," "))
                #en_df[i,] = str_remove_all(en_df[i,],"     ")
                i = i+1
        }
        return(space_count)
}

split_spaces = function(data){
        i = 0
        while (i <= nrow(en_df)){
                #print(str_count(en_df[i,]," "))
                
                en_df[i,] = str_remove_all(en_df[i,],"       ")
                #print(en_df[i,])
                i = i+1
        }
        return(en_df)
}
library(dplyr)
library(plyr)
en_df$space_count = count_spaces(en_df)
en_df = split_spaces(en_df)
en_df$space_count = count_spaces(en_df)
en_df_to_be_cleaned = en_df[1:25,]
en_df_3 = filter(en_df_to_be_cleaned, space_count == 3)
en_df_2 = filter(en_df_to_be_cleaned,space_count == 2)
en_df_4 = filter(en_df_to_be_cleaned,space_count == 4)
en_df_5 = filter(en_df_to_be_cleaned,space_count == 5)
en_df_6 = filter(en_df_to_be_cleaned,space_count == 6)
en_df_1 = filter(en_df_to_be_cleaned,space_count == c(0,1))

en_df_2 = rbind(en_df_2,en_df_5[1,])
en_df_4 = rbind(en_df_4,en_df_5[c(2,6),])
en_df_5 = rbind(en_df_5,en_df_6[3,])
en_df_5 = en_df_5[-c(1,2,6),]
en_df_6 = en_df_6[-3,]

en_df_cleaned = rbind(en_df[26:nrow(en_df),],en_df_1)
library(tidyr)

en_df_6 = en_df_6 %>% separate(English_speaking_countries,c("country_","country_2"),"      ")
en_df_5 = en_df_5 %>% separate(English_speaking_countries,c("country_","country_2"),"     ")
en_df_4 = en_df_4 %>% separate(English_speaking_countries,c("country_","country_2"),"    ")             
en_df_3 = en_df_3 %>% separate(English_speaking_countries,c("country_","country_2"),"   ")             
en_df_2 = en_df_2 %>% separate(English_speaking_countries,c("country_","country_2"),"  ")           

en_df2 = rbind(en_df_2,en_df_3,en_df_4,en_df_5,en_df_6)
en_df2[6,]$country_ = sub(" ","_",en_df2[6,]$country_)
en_df2[6,] = en_df2[6,] %>% separate(country_,c("country_","country_2"),"_")            

en_df_col2 = data.frame(English_speaking_countries = en_df2$country_2)
en_df_col1 = data.frame(English_speaking_countries = en_df2$country_)
rbind(en_df_col1,en_df_col2)
English_speaking_countries = rbind(en_df_cleaned[1],en_df_col1,en_df_col2,row.names(1:55))
rm(en_df,en_df_1,en_df2,en_df_2,en_df_3,en_df_4,en_df_5,en_df_6,en_dfe,en_df_to_be_cleaned,en_df_col2,en_df_col1,en_df_cleaned,e,en,i,space_count)

#count_spaces(English_speaking_countries)

English_speaking_countries

save(English_speaking_countries, file = "English_speaking_countries.dat")






























