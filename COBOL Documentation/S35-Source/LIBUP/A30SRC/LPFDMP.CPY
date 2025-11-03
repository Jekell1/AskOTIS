      ******************************************************************
      ******************************************************************
      ******************************************************************
      ****  A32 OVERRIDE - NEED TO USE LP01LP.CPY TO REPLACE
      ****                 "LP" FOR "MP"; THAT IS WHAT IT IS STANDARD.
      ******************************************************************
      ******************************************************************
      ******************************************************************
      * COPYMEMBER: LIBLP/LPFDMP
      *************************************************************
      *    LOAN MEMO PAYMENT FILE:   (200 BYTES)
      ** MUST STAY IDENTICAL TO THE LOAN PAYMENT FILE
      *  HOLDS CONVERTED PAYMENT RECORDS FOR VIEWING ONLY
      *
      *         KEY 1 - LOAN #, SEQUENCE #  (ASCENDING)
      *
      *  REV:
      *   070711 BAH NEW REGACC PR# 154
      *************************************************************
       FD  OMP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01LP.CPY"
                          REPLACING LEADING "LP" BY "OMP".
