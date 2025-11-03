      * COPYMEMBER: LIBSP/SP01BH
      *****************************************************************
      *                         (BH)
      *             BULLETIN BOARD HISTORY FILE
      *
      *    BH1-KEY = BH-USERID, BH-DATE-REVERSED, BH-TIME
      *
      * REV:
      *  190219 BAH REMOVED BH-RECORD; CHANGED BH-GROUP TO BE THE PRIMARY
      *             FIELD AS ALPHA, AND BH-BRNO IS NOW THE REDEFINE
      *  190516  CS REMOVE ENDING FILLER
      *  2020.0610 BAH ADDED BH-LTOUCH-DATE
      *                REMOVED XFD DATE FROM THE BH-DATE IN THE KEY
      *                CHANGED KEY TO HAVEV BH-DATE-REVERSED WITH BH-DATE
      *                AS A REDEFINES LIKE WE DID WITH THE MEMO FILES
      * BAH 20230612 ADDED FILLER
      *****************************************************************
       78  BH-SIZE                       VALUE 120.
       01  BH-REC.
           03  BH1-KEY.
               05  BH-USERID            PIC X(10).
      * CCYYMMDD SUBTRACTED FROM 99999999
               05  BH-DATE-REVERSED     PIC 9(8).
               05  BH-DATE REDEFINES BH-DATE-REVERSED 
                                        PIC 9(8).
               05  BH-TIME              PIC 9(4).

           03  BH-GROUP                 PIC X(4).
           03  BH-BRNO REDEFINES BH-GROUP      
                                        PIC 9(4).
           03  BH-ACTION                PIC X(40).
      $XFD DATE=YYYYMMDD
           03  BH-LTOUCH-DATE           PIC 9(8)   COMP-3.
           03  FILLER                   PIC X(49).
