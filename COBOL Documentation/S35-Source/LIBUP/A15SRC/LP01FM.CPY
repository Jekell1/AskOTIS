      * COPYMEMBER: LIBLP/LP01FM
      **************************************************************************
      *                 (FM)
      *       FORM REQUEST FILE   RSZ 52
      *       KEY 1 - INITKEY, FMNO
      *       KEY 2 - FMNO, ZIP
      * REV:
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  BLV 991206 ADDED REDEFINE OF FM-INITKEY FOR GLOBAL FORM PRINTING:
      *             NEEDED BR # IN PRIMARY KEY TO KEEP UNIQUE - DIDN'T
      *             CHANGE RECORD FORMAT - SHOULDN'T AFFECT ANYTHING
      **************************************************************************
       01  FM-REC.
           03  FM-KEYA.
               05  FM1-KEY.
                   07  FM-INITKEY          PIC X(20).
                   07  FM-GLOBAL-INITKEY  REDEFINES FM-INITKEY.
                       09  FILLER          PIC X(16).
                       09  FM-BRNO2        PIC 9(4).
                   07  FM-FMNO             PIC 9(4).
               05  FM-ZIP                  PIC X(10).
           03  FM-KEYB                    REDEFINES FM-KEYA.
               05  FILLER                  PIC X(20).
               05  FM2-KEY                 PIC X(14).
           03  FM-PRINT                    PIC X.
           03  FM-BRNO                     PIC 9(4).
      * USED TO HOLD LAST PMT PRINTED ON STMT FOR REPRINT
           03  FM-STMT-COUNTER             PIC 9(3).
           03  FM-TRDATE                   PIC 9(6) COMP-3.
           03  FILLER                      PIC X(6).
