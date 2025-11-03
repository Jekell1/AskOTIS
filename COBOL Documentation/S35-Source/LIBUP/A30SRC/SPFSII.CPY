      * COPYMEMBER: LIBSP/SPFSII
      ******************************************************************
      *
      * DESCRIPTION: IRS IDENTIFICATION FILE
      *
      *=================================================================
      * REV:
      * BLV 010125 CHANGE ASSIGN FROM HARD-CODED PATH TO II-PATH
      *            NOTE, SOMEHOW IN A15 THIS HARD-CODED PATH WAS CHANGED
      *            FROM GLOBAL GB TO LOCAL GB, I AM CHANGING IT BACK TO
      *            GLOBAL GB NOW
      *
      * KEC 2016.1223 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED II-FILE FROM RELATIVE TO INDEXED.
      *
      ******************************************************************
           SELECT II-FILE ASSIGN II-PATH
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY II-KEY
                  FILE STATUS FILE-STAT.
