      * COPYMEMBER: LIBSP/SPFSII
      *******************************************************************
      *  IRS IDENTIFICATION FILE
      *  REV:
      *  BLV 010125 CHANGE ASSIGN FROM HARD-CODED PATH TO II-PATH
      *             NOTE, SOMEHOW IN A15 THIS HARD-CODED PATH WAS CHANGED 
      *             FROM GLOBAL GB TO LOCAL GB, I AM CHANGING IT BACK TO
      *             GLOBAL GB NOW
      *******************************************************************
           SELECT II-FILE ASSIGN II-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY II-KEY
                  FILE STATUS FILE-STAT.
