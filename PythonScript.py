# -*- coding: utf-8 -*-
"""
Created on Tue May  8 15:01:54 2018


"""

# -*- coding: utf-8 -*-
"""
Éditeur de Spyder
Ceci est un script temporaire."""
#import numpy as np
import pandas as pd
dictionnaire = {}
fichier = open("dictionnaire.txt", 'r')
for ligne in fichier.readlines() :
    element=ligne.split(":")
    cle=element[0]
    element[1]=element[1].lower()
    valeurs=element[1].split(",")
    dictionnaire[cle]=valeurs
def Frequencedesmots(listofwords):
    i=0   
    j=0
    lesmots=[]
    frequence={}
    listofwords=pd.DataFrame(listofwords)
    message=listofwords['message']
    #df.loc[:,'_']
    for msg in message:
        msg = msg.lower()
        words=msg.split(" ")
        lesmots.extend(words)
    clef=list(dictionnaire.keys())
    while i < len(lesmots):
        nb=0
        j = 0
        while j < len(clef):
            if lesmots[i] in (dictionnaire[clef[j]]):
                nb+=1
                if (clef[j] in frequence.keys()):
                    frequence[clef[j]] += nb
                else:
                    frequence[clef[j]] = nb
                    
            j+=1  
        
        i+=1
            
      #frequence.setdefault(clef[1],dictionnaire[clef[1]].count(listofwords[j]))
         
         
          #frequence[clef]=valeur[i].count(word)
         
           #frequence[clef]=valeur.count(word)
    
    return(frequence)
              
     
 
        
        #if valeur in listofwords:
            #print (clef)
