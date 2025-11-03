      * COPYMEMBER: LIBGL/GL01GLLOGS
      ***************************************************
      *  GENERAL LEDGER LOG FOR ENDMTH WHICH PURGES GT
      *  PREVIOUSLY CREATED TAG FILES FOR EACH RUN
      *
      *  TAG = PURGE050118-05311800(PURGE,BEG-DATE,END-DATE,SEQUENCE)
      *
      *  KEY 1 - GLLOGS-ID, GLLOGS-SYSTEM-DATE, GLLOGS-SEQNO             
      *  KEY 2 - GLLOGS-ID, GLLOGS-BEG-DATE, GLLOGS-END-DATE
      *
      *  CS 190718 ADDED A 2ND KEY SO YOU COULD USE PURGE RANGE 
      ***************************************************
       78  GLLOGS-SIZE                 VALUE 60.
       01  GLLOGS-REC.
           03  GLLOGS1-KEY.
               05  GLLOGS-ID           PIC X(6).
      $XFD DATE=YYYYMMDD
               05  GLLOGS-SYSTEM-DATE  PIC 9(8).            
               05  GLLOGS-SEQNO        PIC 9(2).
      *BEG-DATE/END-DATE. IN GL/ENDMTH DATES ARE USED BY KEY2 AND
      *ARE THE PURGE DATE RANGES.
      *WHEN RUNNING GL/ENDYER THESE DATES ARE THE BEGINNING FISCAL
      *PERIOD12 FISCAL THAT END OF YEAR IS BEING PROCESSED FOR
      $XFD DATE=YYYYMMDD
           03  GLLOGS-BEG-DATE         PIC 9(8).  
      $XFD DATE=YYYYMMDD
           03  GLLOGS-END-DATE         PIC 9(8).  
           03  GLLOGS-BRNO-RANGE       PIC 9(4) COMP-3.
           03  GLLOGS-TIME             PIC 9(6) COMP-3.
           03  GLLOGS-RECS-PURGED      PIC 9(11) COMP-3.
           03  GLLOGS-USERID           PIC X(10).
                       
      $XFD DATE=YYYYMMDD
           03  GLLOGS-LTOUCH-DATE      PIC 9(8) COMP-3.
