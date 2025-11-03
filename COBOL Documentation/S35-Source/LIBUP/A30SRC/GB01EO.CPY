      * COPYMEMBER: LIBGB/GB01EO
      ******************************************************************
      *
      *   BATCH REPORT PROCESSING RECORD
      *
      *=================================================================
      * REV:
      * KEC 2016.0225 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASE FILE SIZE; ADDED EO-SIZE = 800
      *         ADDED EO-LTOUCH-DATE
      *         DATES TO CCYYMMDD FORMAT
      * BLM 170310 INCREASED EO-PARAMETERS FROM 400 TO 600, 
      *            CHANGED UNUSED TO 100, PD0006
      ******************************************************************
       78  EO-SIZE                             VALUE 854.
       01  EO-REC.
           03  EO1-KEY.
               05  EO-PCODE                    PIC X(3).
               05  EO-STEP                     PIC 9999.
      *>>XDD DATE=YYYYMMDD
           03  EO-LTOUCH-DATE                  PIC 9(08).
           03  EO-DIR                          PIC XX.
           03  EO-PROGRAM                      PIC X(6).
           03  EO-COPIES                       PIC 9.
           03  EO-COMMENT                      PIC X(30).
           03  EO-MESS                         PIC X(60).
      *>>XDD DATE=YYYYMMDD
           03  EO-START-DATE                   PIC 9(8).
           03  EO-START-TIME                   PIC 9(6).
      *>>XDD DATE=YYYYMMDD
           03  EO-COMPLETE-DATE                PIC 9(8).
           03  EO-COMPLETE-TIME                PIC 9(6).
           03  EO-COMPLETE-STAT                PIC X(6).
           03  EO-PARAMETERS                   PIC X(600).

           03  EO-ERROR-STEP                   PIC 9999.
           03  EO-ERROR-STEP-X       REDEFINES EO-ERROR-STEP
                                               PIC XXXX.

           03  EO-TRANSFER                     PIC X.

           03  EO-LEVEL                        PIC 9.
           03  EO-LEVEL-X            REDEFINES EO-LEVEL
                                               PIC X.

           03  FILLER                          PIC X(100).
