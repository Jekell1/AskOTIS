      * COPYMEMBER: LIBLP/LPFDFDB
      ******************************************************************
      *
      * IDENTIFICATION:  FORM DATA BASE FILE
      *
      * PRIMARY KEY   :  DCODE, DBNO                         
      *
      * DESCRIPTION   :  FORM DATA BASE FILE
      *                  CONTAINS DATA BASE FOR 2 FILES      
      *                  100 FIELDS PER RECORD                       
      *                  FDB-DBNO 1-99         
      *
      *=================================================================
      * REV:                                            
      * KEC 2016.0114 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ***NEW*** CREATED FROM REDEF WITHIN FD-FILE (LP01FD).
      *
      ******************************************************************
       FD  OFDB-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01FDB.CPY" REPLACING LEADING
                                                       "FDB" BY "OFDB".
