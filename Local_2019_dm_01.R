# Preparation of the dataset  
library(dplyr)
library(car)

Local <- read.csv("https://www.dropbox.com/s/9cwav2eq65w72vf/local_authorities_2019.csv?dl=1")

#Type of actor 

#manager versus politician
local_00 <- Local %>% 
  mutate(politician = case_when(
    TYPE %in% c(1,2,4) ~ 0,
    TYPE %in% c(3,5) ~ 1
  )) %>% 
  mutate(bureaucrat = politician %>% Recode("0=1;1=0")) %>% 

#politician, local manager present, external manager present 
mutate(Type_3cat = case_when(
  TYPE %in% c(3,5) ~ "0_Politician",
  TYPE %in% c(1,2,4) & RESIDENT_current==2 ~ "1_Manager_external",
  TYPE %in% c(1,2,4) & RESIDENT_current==1 ~ "2_Manager_local"
)) 
  


local_00 <- local_00 %>%
  mutate(Tenure_integrated = ifelse(politician==0,TENURE_municipality,
                                  ifelse(politician==1,Tenure,NA)) %>% 
           Recode("1 = '0-5';
                  2 = '06-10';
                  3 = '11-15';
                  4 = '16-20';
                  5 = '21-25';
                  6 = '26+'")) %>% 
  mutate(Tenure_integrated.g = ifelse(politician==0,TENURE_municipality,
                                      ifelse(politician==1,Tenure,NA)) %>% 
           Recode("1 = 5;
                  2 = 10;
                  3 = 15;
                  4 = 20;
                  5 = 25;
                  6 = 30")) %>%
  mutate(Tenure_integrated.g.c = Tenure_integrated.g - mean(Tenure_integrated.g,na.rm=T),
         Ideology.c = Ideology - mean(Ideology,na.rm=T)) %>% 
  mutate(female = GENDER,
         Age_cat = Age %>% 
           Recode("1 = '20-30';
                   2 = '31-40';
                   3 = '41-50';
                   4 = '51-60';
                   5 = '61-70';
                   6 = '71+'")) %>%
  mutate(Age.g = Age %>% 
           Recode("1 = 30;
                   2 = 40;
                   3 = 50;
                   4 = 60;
                   5 = 70;
                   6 = 75")) %>%
  mutate(Age.g.c = Age.g - mean(Age.g,na.rm=T)) %>%
  mutate(Profession_3cat = case_when(
    Profession %in% c(1,2,3) ~ "Planning and engineering",
    Profession %in% c(4) ~ "Legal",
    Profession %in% c(5,6) ~ "Econ or accounting",
    Profession %in% c(7) ~ "Other"),
    
    Profession_4cat = case_when(
      Profession %in% c(1,2,3) ~ "Planning and engineering",
      Profession %in% c(4) ~ "Legal",
      Profession %in% c(5) ~ "Economist",
      Profession %in% c(6) ~ "Accountant",
      Profession %in% c(7) ~ "Other")) %>% 
  mutate(Children_age.lab = Children_age %>% Recode("
  1='0-4';
  2='05-18';
  3='19-21';
  4='22-30';
  5='31+';
                                                    6=NA")) %>% 
  
  mutate(Children_age_3cat = case_when(
    Children_age %in% c(1:3) ~ "0-21",
    Children_age == 4 ~ "22-30",
    Children_age == 5 ~ "31+")) %>% 
  mutate(children.adult = case_when(
    Children_age %in% c(1:3) ~ 0,
    Children_age %in% c(4:5) ~ 1)) %>% 

 mutate(house_owner = House %>% Recode("2=0"))


#Social identification with local residents
local_00 <- local_00 %>% 
  mutate(Social_id_resident_1 = X3.1,
         Social_id_resident_2 = X3.2,
         Social_id_resident_3 = X3.3,
         Social_id_resident_4 = X3.4,
         Social_id_resident_5 = X3.5,
         Social_id_resident_6 = X3.6,
         Social_id_resident_7 = X3.7,
         Social_id_resident_8 = X3.8) %>% 
  mutate(Social_id_econ_1 = X4.1,
         Social_id_econ_2 = X4.2,
         Social_id_econ_3 = X4.3,
         Social_id_econ_4 = X4.6,
         Social_id_econ_5 = X4.9) %>% 
  mutate(Social_id_housing_1 = X4.4,
         Social_id_housing_2 = X4.5) %>% 
  mutate(pol_pref_global_1 = X1.2,
         pol_pref_global_2 = X1.3,
         pol_pref_global_3 = X1.6,
         pol_pref_global_4 = X2.2,
         pol_pref_global_5= X2.3,
         pol_pref_global_6 = X2.6,
         pol_pref_global_7 = X6.1) %>% 
  mutate(pol_pref_local_1 = X1.7,
         pol_pref_local_2 = X2.7) %>% 
  mutate(pol_pref_central_gov_1 = X7.1,
         pol_pref_central_gov_2 = X7.2,
         pol_pref_central_gov_3 = X7.3)


cities <- read.csv("https://www.dropbox.com/s/l9po55eguchnyk0/cities.csv?dl=1",encoding = "UTF-8") %>% 
  mutate(Prec_renting_3year = (Prec_renting_2015+Prec_renting_2016+Prec_renting_2017)/3) %>% 
mutate(Prec_renting_grand_mean_centered_2015 = Prec_renting_2015 - mean(Prec_renting_2015),
       Prec_renting_grand_mean_centered_2016 = Prec_renting_2016 - mean(Prec_renting_2017),
       Prec_renting_grand_mean_centered_2017 = Prec_renting_2017 - mean(Prec_renting_2017)) %>% 
  mutate(Prec_renting3year_grand_mean_centered = Prec_renting_3year - mean(Prec_renting_3year))




local_00 <- local_00 %>% 
  left_join(cities,by="CITY")


#Creating indices 

#social identification_resident 
local_00 <- local_00 %>%
  mutate(
    Social_id_resident = (
      Social_id_resident_1 +
        Social_id_resident_2 +
        Social_id_resident_3 +
        Social_id_resident_4 +
        Social_id_resident_5 +
        Social_id_resident_6 +
        Social_id_resident_7 + Social_id_resident_8
    ) / 8
  ) %>% 
  mutate(Social_id_resident.c = Social_id_resident - mean(Social_id_resident,na.rm=T)) %>% 
  
mutate(social_id_res_binary = case_when(
  Social_id_resident<4 ~ 0,
  Social_id_resident>=4 ~ 1)) %>% 

  mutate(Social_id_resident_01 = (Social_id_resident-1)/(5-1))


#social identification_economic hardship without housing 
local_00 <- local_00 %>% 
  mutate(Social_id_econ = (Social_id_econ_1+
                             Social_id_econ_2+
                             Social_id_econ_3+
                             Social_id_econ_4+
                             Social_id_econ_5)/5) %>% 
  
  mutate(Social_id_econ_01 = (Social_id_econ-1)/(5-1))


# social identification_economic hardship with housing 
local_00 <- local_00 %>% 
  mutate(Social_id_econ_and_housing = (Social_id_econ_1+
                                         Social_id_econ_2+
                                         Social_id_econ_3+
                                         Social_id_econ_4+
                                         Social_id_econ_5+
                                         Social_id_housing_1+
                                         Social_id_housing_2)/7) %>% 
  mutate(Social_id_econ_and_housing.c = Social_id_econ_and_housing -  mean(Social_id_econ_and_housing,na.rm=T)) %>% 
  
  mutate(Social_id_econ_and_housing_01 = (Social_id_econ_and_housing-1)/(5-1))
  



#Social identification only housing
local_00 <- local_00 %>% 
  mutate(Social_id_housing = (Social_id_housing_1+
                                Social_id_housing_2)/2)


# policy preference global

local_00 <- local_00 %>% 
  mutate(Policy_pref_global= (pol_pref_global_1+
                                pol_pref_global_2+
                                pol_pref_global_3+
                                pol_pref_global_4+
                                pol_pref_global_5+
                                pol_pref_global_6+
                                pol_pref_global_7)/7) %>% 
  mutate(Policy_pref_global_01 = (Policy_pref_global-1)/(5-1))

# policy preference local 
local_00 <- local_00 %>% 
  mutate(Policy_pref_local = (pol_pref_local_1+
                                pol_pref_local_2)/2)%>% 
  mutate(Policy_pref_local_01 = (Policy_pref_local-1)/(5-1))

#policy preference econ. stability
Local <- local_00 %>% 
  mutate(Pref_econ_stab = (X1.1+
                             X2.1)/2)


#Policy preference vis-a-vis gov responsibility
local_00 <- local_00 %>% 
  mutate(Policy_pref_central_gov = (pol_pref_central_gov_1+
                                      pol_pref_central_gov_2+
                                      pol_pref_central_gov_3)/3)



#group-meaned indexes 
local_00 <- local_00 %>%
  group_by(CITY) %>%
  mutate(GroupCenteredSocial_id_res = Social_id_resident - mean(Social_id_resident, na.rm=TRUE)) %>% 
  mutate(GroupCenteredSocial_id_econ = Social_id_econ_and_housing - mean(Social_id_econ_and_housing, na.rm=TRUE)) %>% 
  mutate(GroupCenteredCentral_Gov_responsibility = Policy_pref_central_gov - mean(Policy_pref_central_gov, na.rm=TRUE)) %>% 
  ungroup()



local_00 <- local_00 %>% 
  data.frame() %>% 
  mutate(Type.lab = Type_3cat %>%
           Recode("'1_Manager_external'='2_Manager_external';
                '2_Manager_local'='1_Manager_local'"))
