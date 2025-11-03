      * COPYMEMBER: LIBGL/GLGLLOGSCL.CPY

      ******************************************************************
      *    GLLOGSCL.CPY CLEAR GLLOGS ROUTINE                           
      *
      * REV:
      * CLS 190723 *NEW*                                      
      *            CLEAR GLLOGS FILE, THIS FILE IS USED TO HOLD THE
      *            MONTH END/YEAR END TAG DATES WERE PREVIOUSLY
      *            TAGGED IN THE /USR/XXXX/RX/GL
      ******************************************************************
       CLEAR-GLLOGS-FILE SECTION.
           MOVE SPACES TO GLLOGS-REC.
           MOVE 0      TO GLLOGS-SYSTEM-DATE
                          GLLOGS-SEQNO 
                          GLLOGS-BEG-DATE
                          GLLOGS-END-DATE
                          GLLOGS-TIME
                          GLLOGS-RECS-PURGED
                          GLLOGS-BRNO-RANGE 
                          GLLOGS-LTOUCH-DATE.
