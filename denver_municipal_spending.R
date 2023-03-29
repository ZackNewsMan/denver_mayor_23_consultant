library(tidyverse)

#### Used a series of nested joins to categorize the "purpose" data column into broader buckets. ########
  # See the Excel sheet: denver_2023_municipal_election_spending_030123_4.xlsx 

library(readr)
purpose_refine_1 <- read_csv("purpose_refine_1.csv")
purpose_refine_2 <- read_csv("purpose_refine_2.csv")
purpose_refine_3 <- read_csv("purpose_refine_3.csv")
purpose_refine_4 <- read_csv("purpose_refine_4.csv")
purpose_refine_5 <- read_csv("purpose_refine_5.csv")
purpose_refine_6 <- read_csv("purpose_refine_6.csv")
purpose_refine_7 <- read_csv("purpose_refine_7.csv")
purpose_refine_8 <- read_csv("purpose_refine_8.csv")
purpose_refine_9 <- read_csv("purpose_refine_9.csv")
purpose_refine_10 <- read_csv("purpose_refine_10.csv")
purpose_refine_11 <- read_csv("purpose_refine_11.csv")

### Now let's full join them ### 

    purpose_refine_1_2 <- purpose_refine_1 %>% full_join(purpose_refine_2)
    
    purpose_refine_1_3 <- purpose_refine_3 %>% full_join(purpose_refine_1_2)
    
    purpose_refine_1_4 <- purpose_refine_4 %>% full_join(purpose_refine_1_3)
    
    purpose_refine_1_5 <- purpose_refine_5 %>% full_join(purpose_refine_1_4)
    
    purpose_refine_1_6 <- purpose_refine_6 %>% full_join(purpose_refine_1_5)
    
    purpose_refine_1_7 <- purpose_refine_7 %>% full_join(purpose_refine_1_6)
    
        # Running into difference of character type when try to merge 6 and 7, so going to export and addit manually 
    
                purpose_refine_1_6 %>% write_csv("purpose_refine_1_6.csv")
                
                # ALso - Shannon Hoffman spending was in the wrong place so re-ordered that too
                
                purpose_refine_1_7 <- read_csv("purpose_refine_1_7.csv")
                
   # Back to merging 
      
    purpose_refine_1_8 <- purpose_refine_8 %>% full_join(purpose_refine_1_7)

    purpose_refine_1_9 <- purpose_refine_9 %>% full_join(purpose_refine_1_8)

    purpose_refine_1_10 <- purpose_refine_10 %>% full_join(purpose_refine_1_9)
    
    purpose_refine_1_11 <- purpose_refine_11 %>% full_join(purpose_refine_1_10)
    
    # I keep running into similar errors where R resisters the columns as different data types even though they are the same.   
    # So, I manually copied and pasted the sections that didn't join and saved each one. 
    # I also added a year column using the =TEXT(A2,"YYYY") formula and saved it as version 2

    library(readr)
    purpose_refine_1_11_2 <- read_csv("purpose_refine_1_11_2.csv")
    View(purpose_refine_1_11_2)
    
      # Number of rows is 23,072 after cleaning, which is the same as the number of columns in the raw data. 

 spend <- purpose_refine_1_11_2

 # Went in and folded some categories into others, like printing into supplies, food into events and stamps into supplies
 
     spend <- purpose_refine_1_11_3

  # Alright and folded "phone calls" into Supplies          
     
     library(readr)
     purpose_refine_1_11_4 <- read_csv("purpose_refine_1_11_4.csv")
     View(purpose_refine_1_11_4)
     
     spend <- purpose_refine_1_11_4
     
     # Need to put Payroll into Staffing
      # Rolled Marketing and Advertisment into one column, Marketing and advertising just in case I put one in Marketing when it should have been in Advertisment and vice versa.
 
     library(readr)
     purpose_refine_1_11_6 <- read_csv("purpose_refine_1_11_6.csv")
     View(purpose_refine_1_11_6)
     
     spend <- purpose_refine_1_11_6
     
     # Noticed some Mail categories when it should be Email. Fixed and reimported below
     
         library(readr)
         purpose_refine_1_11_7 <- read_csv("purpose_refine_1_11_7.csv")
         View(purpose_refine_1_11_7)
         
         spend <- purpose_refine_1_11_7
     
     # Need to roll "website" into "Online expense"  
         
         library(readr)
         purpose_refine_1_11_8 <- read_csv("purpose_refine_1_11_8.csv")
         View(purpose_refine_1_11_8)
         
         spend <- purpose_refine_1_11_8
         
     
############## Who's spending the most often? ######### 
 
     spend %>% 
       group_by(candidate_name) %>% 
       summarize(count = n()) %>% 
       arrange(desc(count)) %>% 
       View()
 
    # Hella blanks through candidate name, let's try by committee name instead
     
     spend %>% 
       group_by(committee_name) %>% 
       summarize(count = n()) %>% 
       arrange(desc(count)) %>% 
       View()
    
     # Hancock for Denver has the most expenditures which is wild considering he's term-limited. I think old campaign activity is getting into 2023 municipal election data for some reaspm

############## fef spending ################
     
     spend %>%
       filter(fef_expenditure == "Yes") %>% 
       View()
     
     # 565 rows
     
###### filter out by year ##########
     
     # We just want 2022 and 2023 expenditures. 
      # According to texts from Denver Elections (specifically FEF funds) PIO Lucille Wenegieme, the first Fair Elections Fund disbursement from the Fund was on August 15,2022.
     
     spend %>%
     filter(expenditure_year == "2022"|expenditure_year == "2023") %>% 
     View()
     
      # 4,104 rows
        # Same in SQL, 4,104 rows
           # SELECT *
           #   from spend
           # where expenditure_year = "2022"
           # or expenditure_year = "2023"
    
     spend %>%
       filter(expenditure_year == "2022") 
     
        # 3,188 rows in 2022
        
     spend %>%
       filter(expenditure_year == "2023") 
     
        # 916 rows in 2023
     
    spend_22_23 <- spend %>%
       filter(expenditure_year == "2022"|expenditure_year == "2023")
     
    spend_22_23 %>% 
      filter(fef_expenditure == "Yes")
    
          # Still 565 rows    

### by office type #########
      
      spend_22_23 %>% 
        group_by(office_sought) %>% 
        summarize(count = n()) %>% 
        arrange(desc(count)) %>% 
        View()
    
      # No blanks:
     
     spend_22_23 %>% 
        filter(!is.na(office_sought)) %>% 
        View()
     
          # 3675 rows
            # Same as in SQL, 3675 rows
               # CREATE TABLE "filtered_spend_22_23" as 
               # SELECT *
               # FROM spend_22_23
               # WHERE office_sought != "NA"
      
      # Just blank office sought field:
      
      spend_22_23 %>% 
        filter(is.na(office_sought)) %>% 
        View()
      
        # Apartment Association of Metro Denver Denver Small Donor Committee gave a lot of money to candidates and did not list the offices they were running for
      
      spend_22_23 <- spend_22_23 %>% 
        filter(!is.na(office_sought))
 
      
 ############## Who's spending the most? On what? #########      
 
       library(tidyverse)
      
      spend_22_23 %>% 
        group_by(committee_name) %>% 
        summarize(count = n()) %>% 
        arrange(desc(count)) %>% 
        View()
          
          # Most number of expenditures:
          # committee_name                                      count
          # Sarah Parady for Denver                               271
          # Tafoya for Mayor                                      212
          # Travis Leiker for Denver                              208
      
      # By candidate:
          
          spend_22_23 %>% 
            group_by(candidate_name) %>% 
            summarize(count = n()) %>% 
            arrange(desc(count)) %>% 
            View()
      
      
      # By amount of money:  
      
      spend_22_23 %>% 
        group_by(candidate_name) %>% 
        summarize(total = sum(amount)) %>% 
        View() 
      
        # R is throwing a fit when I try to sort it by most money spent, so sort in View
      
        # SQL gets same results in a format that's easier to read:
          # SELECT candidate_name, sum(amount), count(candidate_name) 
          # FROM filtered_spend_22_23
          # group by candidate_name
          # order by sum(amount) DESC
      
          # Kelly Brough's campaign and independent expenditure committees supporting her is spending the most money across all Denver municipal campaigns -- $552,914.45 in 130 expenditures from 2022 to ____ 2023. 
            # Leslie Herod is the next closest with $377,502.28 through 163 purchases. 
     
      # Top 5: 
      # candidate name amount_spent   number of expenditures
      # Kelly Brough	552914.45	      130
      # Leslie Herod	377502.28	      163
      # Andre Rougeot	172270.92	      84
      # Sarah Parady	153296.22	      271
      # Deborah Ortega	124876.14	     125

     candidate_total <- spend_22_23 %>% 
        group_by(candidate_name) %>% 
        summarize(candidate_total_spend = sum(amount))
      
######## What are they spending money on? ###########################
      
      # Overall: 
      
          spend_22_23 %>% 
            group_by(purpose_cleaned) %>% 
            summarize(count = n()) %>% 
            arrange(desc(count)) %>% 
            View()
      
        # Most common expense is on fees
      
      spend_22_23 %>% 
        group_by(purpose_cleaned) %>% 
        summarize(count = n(), total_spent = sum(amount)) %>% 
        View()
      
          # Advertising and marketing is the largest expenditure
      
      spend_22_23 %>% 
        group_by(purpose_cleaned) %>% 
        summarize(count = n(), total_spent = sum(amount))
      arrange(desc(count)) %>% 
      
      # Mayoral total:
        
        mayor %>% 
        group_by(purpose_cleaned) %>% 
        summarize(count = n(), total_spent = sum(amount)) %>% 
        View()
        
          # Most of the spending is on Marketing, advertising and consulting. 
            # Echo'd in SQL:  
              # SELECT purpose_cleaned, sum(amount)
              # from mayor
              # group by purpose_cleaned
              # order by sum(amount) DESC
      
     
############ percentage of spending on what category?###########
      
      spend_22_23 %>% 
        group_by(candidate_name, purpose_cleaned) %>% 
        summarize(total_spent = sum(amount)) %>% 
        View()
      
      # slight name change of column
            
      candidate_categories <- spend_22_23 %>% 
        group_by(candidate_name, purpose_cleaned) %>% 
        summarize(category_spent = sum(amount))
      
      # Going to full join candidate categories with candidate totals spent
      
      candidate_categories %>% full_join(candidate_total) %>% 
        View()
      
        # This work so well!! I am stoked :) 
      
            candidate_cat_total <- candidate_categories %>% full_join(candidate_total)
      
       # Now to get the percentages:
            
            candidate_cat_total %>% 
             mutate(spend_perc = ((category_spent/candidate_total_spend)*100)) %>% 
              View()
            
            
        # This worked well but I need to re-work it to only look at mayoral and city council 
            
            library(dplyr)
            
            mayor <- spend_22_23 %>% 
              filter(office_sought == "Mayor")
            
              # SQL has 1223 rows too:
               # CREATE TABLE "mayor" as 
               # SELECT *
               #   FROM filtered_spend_22_23
               # WHERE office_sought = "Mayor"
                
            
            mayor_candidate_categories <- mayor %>% 
              group_by(candidate_name, purpose_cleaned) %>% 
              summarize(category_spent = sum(amount))
            
                # Spot checked totals between R and SQL and they are the same:
            
                   # SELECT candidate_name, purpose_cleaned, sum(amount)
                   # FROM mayor
                   # group by candidate_name, purpose_cleaned
                   # order by candidate_name
            
            mayor_candidate_total <- mayor %>% 
              group_by(candidate_name) %>% 
              summarize(candidate_total_spend = sum(amount))
            
                  # Same as SQL:
                   # SELECT candidate_name, sum(amount), count(candidate_name) 
                   # FROM mayor
                   # group by candidate_name
                   # order by sum(amount) DESC 
                  
            
            mayor_candidate_cat_total <- mayor_candidate_categories %>% full_join(mayor_candidate_total)
            
            mayor_spend_perc <- mayor_candidate_cat_total %>% 
              mutate(spend_perc = ((category_spent/candidate_total_spend)*100))
            
            # Mutate stopped working for reasons that aren't clear, so going to restart and hope for the best 
           
            #  When trying to mutate that column, the percentage gets all sorts of messed up for no reason despite working above. going to try to export 
            
            mayor_spend_perc %>% write_csv("mayor_spend_perc.csv", na = "")  
            
              # Looks right on export but not when I try to bring it back in, R can shove it 
            
                 # Graphic lives here: https://public.flourish.studio/visualisation/13079541/ 
            
            
 ######### Removing IE spending from calculations ##########           
           
            non_ie_mayor <- mayor %>% 
              filter(independent_expenditure == "No") 
          
          # In SQL:  
            # CREATE TABLE "non_ie_mayor" as 
            # SELECT *
            #  FROM mayor
            # WHERE independent_expenditure = "No"
            
            ### need to change to non-IE: 
            non_ie_mayor_categories <- non_ie_mayor %>% 
              group_by(candidate_name, purpose_cleaned) %>% 
              summarize(category_spent = sum(amount))
            
            # Spot checked totals between R and SQL and they are the same:
            
            # SELECT candidate_name, purpose_cleaned, sum(amount)
            # FROM non_ie_mayor
            # group by candidate_name, purpose_cleaned
            # order by candidate_name
            
            non_ie_mayor_candidate_total <- non_ie_mayor %>% 
              group_by(candidate_name) %>% 
              summarize(candidate_total_spend = sum(amount))
            
            # Same as SQL:
            # SELECT candidate_name, sum(amount), count(candidate_name) 
            # FROM mayor
            # group by candidate_name
            # order by sum(amount) DESC 
            
            
            non_ie_mayor_candidate_cat_total <- non_ie_mayor_categories %>% full_join(non_ie_mayor_candidate_total)
            
            non_ie_mayor_spend_perc <- non_ie_mayor_candidate_cat_total %>% 
              mutate(spend_perc = ((category_spent/candidate_total_spend)*100))
            
            # Viewing the mutate still not working for reasons that aren't clear. Going to export to view
            
            non_ie_mayor_spend_perc %>% write_csv("non_ie_mayor_spend_perc.csv", na = "")  
            
            
            
############ non-IE totals ##################            
        
      # # By amount of money:  
            
            non_ie_mayor %>% 
              group_by(candidate_name) %>% 
              summarize(total = sum(amount)) %>% 
              View()       
            
            # To get full total:
            
            non_ie_mayor %>% 
              summarize(total = sum(amount))
            
              # $1,273,321 is total but let's pull out Hancock because he's not running for mayor again.
                # SQL confirmed, 1273321.49:
                  # SELECT sum(amount)
                  # FROM non_ie_mayor
                  # order by sum(amount) DESC
                  
            
            1273321-8843.57
            
              # New grand total for all candidates vying for Denver mayor: $1,264,477
            
      # Categories:
        # Want to pull Hancock spending so the focus can just be on people currently running for mayor    
            
            non_hancock_ie_mayor <- non_ie_mayor %>% 
              filter(candidate_name != "Michael Hancock") 
            
              # 1,162 rows.
                # Same in SQL: 1,1162 rows
            
                 # CREATE TABLE "non_hancock_ie_mayor" as 
                 # SELECT *
                 #   FROM non_ie_mayor
                 # WHERE candidate_name != "Michael Hancock"
                  
            non_hancock_ie_mayor %>% 
              group_by(purpose_cleaned) %>% 
              summarize(count = n(), total_spent = sum(amount)) %>% 
              View()
            
              # SQL is the same, also 28 rows.
            
                 # SELECT purpose_cleaned, sum(amount)
                 # FROM non_hancock_ie_mayor
                 # group by purpose_cleaned
                 # order by sum(amount) DESC
            
            # Most of the money has been spent on consultants and staffing.
              # Consultant: 379282.36
              # Staffing: 309899.92
            
            ((379282.36+309899.92)/1264477)*100
            
              # 54.5% of mayoral campaign spending has been on consultants and staffing so far. 
            
          # Percent just on consultants:
            
            (379282.36/1264477)*100
            
              # 29.995% 
                # So one third of all campaign spending has been on consultants. 

            
            
######## What is Independent Expenditure committee (IE) spending on? #####
      
            ie_mayor <- mayor %>% 
              filter(independent_expenditure == "Yes") 
         
   # categories for IE spending  
            
             ie_mayor_categories <- ie_mayor %>% 
              group_by(candidate_name, purpose_cleaned) %>% 
              summarize(category_spent = sum(amount))
             
             # What the IE spending was on:
             
             # candidate_name purpose_cleaned           category_spent
             # Deborah Ortega Marketing and advertising         10,000 
             # Kelly Brough   Canvassing                        54,375 
             # Kelly Brough   Marketing and advertising        262,368.
             # Leslie Herod   Marketing and advertising        122,350 
             
 
    # Total
             
           ie_mayor_candidate_total <- ie_mayor %>% 
             group_by(candidate_name) %>% 
             summarize(candidate_total_spend = sum(amount))    
           
   # Merge 
           
           ie_mayor_candidate_cat_total <- ie_mayor_categories %>% full_join(ie_mayor_candidate_total)
   
  # Mutate with perc
           
           ie_mayor_spend_perc <- ie_mayor_candidate_cat_total %>% 
             mutate(spend_perc = ((category_spent/candidate_total_spend)*100))
           
           # Viewing the mutate still not working for reasons that aren't clear. Going to export to view
           
           ie_mayor_spend_perc %>% write_csv("ie_mayor_spend_perc.csv", na = "")           
    
           
  # Total IE spending
    
           ie_mayor %>% 
             summarize(total = sum(amount))           
           
          # At least $449,093 in Independent Expenditure committee spending 
           
           
######## Biggest consultant paydays? ############# 

 # Let's start with just consultants because that's the lion's share of the data   
       
           library(tidyverse)
               
           non_ie_mayor %>% 
             filter(purpose_cleaned == "Consultant") 
           
           # 97 rows
            # Same with SQL, 97 rows
           
              # CREATE TABLE "consultant_non_ie_mayor" as 
              # SELECT *
              #   FROM non_ie_mayor
              # WHERE purpose_cleaned = "Consultant"
           
           consultant_non_ie_mayor  <- non_ie_mayor %>% 
             filter(purpose_cleaned == "Consultant") 
           
           consultant_non_ie_mayor %>% write_csv("consultant_non_ie_mayor.csv", na = "")
           
           consultant_non_ie_mayor %>% 
             group_by(organization_name) %>% 
             summarize(organization_total_earned = sum(amount)) %>% 
             View()
           
                # Most of money to individuals, not orgs
                  # To get the columns together: https://www.marsja.se/how-to-concatenate-two-columns-or-more-in-r-stringr-tidyr/         
           
           consultant_non_ie_mayor$full_name <- paste(consultant_non_ie_mayor$payee_first_name,consultant_non_ie_mayor$payee_last_name)
             
           # Money towards individuals:
           
           consultant_non_ie_mayor %>% 
             group_by(full_name) %>% 
             summarize(person_total_earned = sum(amount)) %>% 
             View()
           
           # Individual earners:
           
            #   Alvertis Simmons     26250.00
            # Shawn Werner  20000.00
            #   Emily Dowd           17375.00
           
           
           consultant_non_ie_mayor %>% 
             group_by(full_name, committee_name) %>% 
             summarize(person_total_earned = sum(amount)) %>% 
             View()
           
              # Breaks this down into which consultants were paid the most and by whom
           
                payday_individual_consultant <- consultant_non_ie_mayor %>% 
                  group_by(full_name, committee_name) %>% 
                  summarize(person_total_earned = sum(amount))
           
                payday_individual_consultant %>% write_csv("payday_individual_consultant.csv", na = "")
           
           # Totals to organizations 
           
           consultant_non_ie_mayor %>% 
             group_by(organization_name, committee_name) %>% 
             summarize(organization_total_earned = sum(amount)) %>% 
             View()
           
              # That worked great, super interesting orgs getting cash 
           
           payday_org_consultant <-  consultant_non_ie_mayor %>% 
             group_by(organization_name, committee_name) %>% 
             summarize(organization_total_earned = sum(amount)) 
           
           payday_org_consultant %>% write_csv("payday_org_consultant.csv", na = "")
           
           
      # consultants paid through fef funding?
           
           consultant_non_ie_mayor %>%  
             filter(fef_expenditure == "Yes")
           
            # There are 17 instances of consultants paid by candidates using FEF money. 
              # Confirmed via SQL, 17 rows
           
                # CREATE TABLE "fef_consultant_non_ie_mayor" as 
                # SELECT *
                #   FROM consultant_non_ie_mayor
                # WHERE fef_expenditure = "Yes"
                  
           fef_consultant_non_ie_mayor <- consultant_non_ie_mayor %>%  
             filter(fef_expenditure == "Yes")
           
        # Total from FEF spent on consultants?
           
           fef_consultant_non_ie_mayor %>% 
             summarize(total = sum(amount))
           
           # $95,400 from FEF coffers.
            # Confirmed via SQL, $95,400
           
               # SELECT sum(amount)
               # from fef_consultant_non_ie_mayor
           
           # Percent of FEF spending within all consultant spending:
           
           (95400/379282.36)*100
           
              # 25%, or a quarter, of all consultant spending drew from tax payers. 
           
    # Group by campaign
           
           fef_consultant_non_ie_mayor %>% 
             group_by(candidate_name) %>% 
             summarize(candidate_total_spend = sum(amount))
           
           # Leslie Herod’s campaign spent $64,700 from the FEF on consultants. Debbie Ortega spent $30,700.
            # Echo'd in SQL
               # select candidate_name, sum(amount)
               # from fef_consultant_non_ie_mayor
               # group by candidate_name
               
           
           
    # Total FEF org recieved 
           
           fef_consultant_non_ie_mayor %>% 
           group_by(organization_name, committee_name) %>% 
             summarize(organization_total_earned = sum(amount)) %>% 
             View()
           
  ###### Where is most of the FEF money going? ############                      
           
           
  


