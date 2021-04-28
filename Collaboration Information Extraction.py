#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 10:56:10 2020

@author: yitongli
"""

import pandas as pd
import numpy as np
from itertools import combinations
import spacy
from spacy.lang.en import English
from spacy.pipeline import EntityRuler
import en_core_web_sm

#Load english tokenizer, tagger, parser, NER and word vectors
nlp = spacy.load('en_core_web_sm')

def clean_sow(text):
    """
    Parameter: text with new lines
    
    The function removes all types of new line character, and returns the cleaned text
    """
    text=text.replace('\r\n', ' ')
    text=text.replace('\n\n', ' ')
    text=text.replace('\n', ' ')
    text=text.lower()
    return text

def ner_spec(df):
    """
    Parameter: dataframe that stores sow

    The function collects the dataframe. Returns specified entities using specialize pipeline in SpaCy
    """
    cleaned_sow = []
    ner_spec = []
    # For each sow in the table
    for i in range(len(df)):
        # extract raw string
        sow = df['statementOfWork'][i]
        
        # clean the raw string
        sow = clean_sow(sow)
        cleaned_sow.append(sow)
       
        # get entities
        doc = nlp(sow)
        ner_spec.append([(ent.ent_id_) for ent in doc.ents])
    return [cleaned_sow, ner_spec]  

def col_info(ner_name):
    """
    Parameters: NER in spaCy with specialized pipeline (specified_ner)
    The function collectes the specialized NER, and outputs stakeholders and stakeholder interactions  
    """
    specified_entity = ner_name[1]
    stakeholder = []
    stakeholder_inter = []
    for i in range(len(specified_entity)):
        entity = specified_entity[i]
        entity_uniq = list(set(entity)) # Unique stakeholder in each mission description
        stakeholder.append(entity_uniq)
        stakeholder_inter.append(list(combinations(entity_uniq,2))) # Stakeholder interaction
    return [stakeholder, stakeholder_inter]

def date(df,dat):
    """
    Parameter: list of extracted stakeholder/stakeholder interaction

    The function collects the list, 
    reformat stakeholder and stakeholder interaction, 
    and add dates to them
    dates are obtained from data 'dat'
    """
    information = []
    date = []
    date_all = []
    for i in df:
        for j in i:
            information.append(j)
    df_information = pd.DataFrame(information)
    # Add date
    for i in range(len(dat)):
        d = dat["dateRequested"][i]
        date.append([d]*len(df[i]))
    for i in date:
        for j in i:
            date_all.append(j)
    df_date = pd.DataFrame(date_all)
    df = pd.concat([df_information, df_date], axis=1)
    return df

# Import statement of work descriptions
dat = pd.read_csv("~/CleanedMissionAssignments.csv")
agency = pd.read_csv("~/Stakeholder Dictionary.csv", encoding= 'unicode_escape')

ner_label = agency.drop_duplicates()
p1 = ner_label['agency'].str.lower()
p2 = ner_label['id']
ner_label = pd.concat([p1, p2], axis=1)

# Using Entity Ruler
nlp = English()
ruler = EntityRuler(nlp)

# Specify the entity label
patterns = []        
for i in range(len(ner_label)):
    patterns.append({"label": "Agency", "pattern": str(ner_label.iloc[i,0]), "id": str(ner_label.iloc[i,1])})

ruler.add_patterns(patterns)
nlp.add_pipe(ruler) # specialized pipeline

#######################
#### All Disasters ####
#######################
specified_ner_all = ner_spec(dat)

def col_info_sh(ner_name):
    """
    Parameters: NER in spaCy with specialized pipeline (specified_ner)
    The function collectes the specialized NER, and outputs stakeholders and stakeholder interactions  
    """
    specified_entity = ner_name[1]
    stakeholder = []
    stakeholder_inter = []
    for i in range(len(specified_entity)):
        entity = specified_entity[i]
        entity_uniq = list(set(entity)) # Unique stakeholder in each mission description
        stakeholder.append(entity_uniq)
    return stakeholder

stakeholder = col_info_sh(specified_ner_all)

# Check external stakeholders extracted from the SOWs
external_agency_coll = []
for i in range(len(dat)):
    external_agency_coll.append(dat['agency'][i] in stakeholder[i])
df_external_agency_coll = pd.DataFrame(external_agency_coll)

# Combine the Columns
dat_final = pd.concat([dat['maId'], dat['agency'], dat['city'], df_external_agency_coll], axis=1) 
dat_final['city'] = dat_final['city'].astype('object') # for storing the list object into one dataframe column
for i in range(len(dat_final)):
    dat_final['city'][i] = stakeholder[i]
dat_final.columns = ['maId', 'main', 'agency_name','external_agency_coll']

###############################
###### Four Disasters #########
###############################
# NER using SpaCy with specialized pipeline    
dat_HU = dat[(dat.disasterDescription == "Hurricane")].reset_index()
dat_SS = dat[(dat.disasterDescription == "Severe Storm(s)")].reset_index()
dat_FL = dat[(dat.disasterDescription == "Flood")].reset_index()
dat_FI = dat[(dat.disasterDescription == "Fire")].reset_index() 
   
specified_ner_HU = ner_spec(dat_HU)
specified_ner_SS = ner_spec(dat_SS)
specified_ner_FL = ner_spec(dat_FL)
specified_ner_FI = ner_spec(dat_FI)

def col_info(ner_name):
    """
    Parameters: NER in spaCy with specialized pipeline (specified_ner)
    The function collectes the specialized NER, and outputs stakeholders and stakeholder interactions  
    """
    specified_entity = ner_name[1]
    stakeholder = []
    stakeholder_inter = []
    for i in range(len(specified_entity)):
        entity = specified_entity[i]
        entity_uniq = list(set(entity)) # Unique stakeholder in each mission description
        stakeholder.append(entity_uniq)
        stakeholder_inter.append(list(combinations(entity_uniq,2))) # Stakeholder interaction
    return [stakeholder, stakeholder_inter]

#### Extract stakeholder collaboration information ####
stakeholder_HU, stakeholder_inter_HU = col_info(specified_ner_HU)
stakeholder_SS, stakeholder_inter_SS = col_info(specified_ner_SS)
stakeholder_FL, stakeholder_inter_FL = col_info(specified_ner_FL)
stakeholder_FI, stakeholder_inter_FI = col_info(specified_ner_FI)

# Add date to the extracted collaboration information 
interaction_date_HU = date(stakeholder_inter_HU, dat_HU)
stakeholder_date_HU = date(stakeholder_HU, dat_HU)
interaction_date_SS = date(stakeholder_inter_SS, dat_SS)
stakeholder_date_SS = date(stakeholder_SS, dat_SS)
interaction_date_FL = date(stakeholder_inter_FL, dat_FL)
stakeholder_date_FL = date(stakeholder_FL, dat_FL)
interaction_date_FI = date(stakeholder_inter_FI, dat_FI)
stakeholder_date_FI = date(stakeholder_FI, dat_FI)

# Export the extracted collaboration information for further quantitiative analysis (plot in R)
stakeholder_date_HU.to_csv('/Volumes/GoogleDrive/My Drive/GMU Material/GMU Course/Spring 2021/DAEN 690/Code and Data/Collaboration Information/HU_stakeholder.csv')
interaction_date_HU.to_csv('/Volumes/GoogleDrive/My Drive/GMU Material/GMU Course/Spring 2021/DAEN 690/Code and Data/Collaboration Information/HU_interaction.csv')
stakeholder_date_SS.to_csv('/Volumes/GoogleDrive/My Drive/GMU Material/GMU Course/Spring 2021/DAEN 690/Code and Data/Collaboration Information/SS_stakeholder.csv')
interaction_date_SS.to_csv('/Volumes/GoogleDrive/My Drive/GMU Material/GMU Course/Spring 2021/DAEN 690/Code and Data/Collaboration Information/SS_interaction.csv')
stakeholder_date_FL.to_csv('/Volumes/GoogleDrive/My Drive/GMU Material/GMU Course/Spring 2021/DAEN 690/Code and Data/Collaboration Information/FL_stakeholder.csv')
interaction_date_FL.to_csv('/Volumes/GoogleDrive/My Drive/GMU Material/GMU Course/Spring 2021/DAEN 690/Code and Data/Collaboration Information/FL_interaction.csv')
stakeholder_date_FI.to_csv('/Volumes/GoogleDrive/My Drive/GMU Material/GMU Course/Spring 2021/DAEN 690/Code and Data/Collaboration Information/FI_stakeholder.csv')
interaction_date_FI.to_csv('/Volumes/GoogleDrive/My Drive/GMU Material/GMU Course/Spring 2021/DAEN 690/Code and Data/Collaboration Information/FI_interaction.csv')









