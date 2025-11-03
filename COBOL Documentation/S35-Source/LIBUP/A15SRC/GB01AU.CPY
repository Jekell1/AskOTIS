      * COPYMEMBER: LIBGB/GB01AU
      **************************************************************************
      *        AUDIT CHANGE FILE BY FILE         BYTES = 335
      *
      *        KEY 1 - FILE CODE, DATE, SEQ
      * REV:
      *  20220207 BAH NEW #1542
      **************************************************************************
       01  AU-REC.
           03  AU1-KEY.
               05  AU-FILE-NAME          PIC X(7).
               05  AU-DATE               PIC 9(8).
               05  AU-SEQ                PIC 9(8).

           03  AU-ACTION                 PIC X.
               88  AU-ACTION-DELETE           VALUE "D".
               88  AU-ACTION-ADD              VALUE "A".
               88  AU-ACTION-CHANGE           VALUE "C".
      * FOR WHEN NEEDED WITH FILES LIKE THE CDFILE
           03  AU-FILE-TYPE              PIC X(2).
           03  AU-BRANCH                 PIC 9(4).
           03  AU-FILE-KEY               PIC X(50).
           03  AU-FIELD-DESC             PIC X(30).
           03  AU-ORIG-VALUE             PIC X(80).
           03  AU-NEW-VALUE              PIC X(80).
           03  AU-TICKET                 PIC X(20).
           03  AU-PROGRAM                PIC X(8).
           03  AU-TIME                   PIC 9(6).
           03  AU-USERID                 PIC X(8).
           03  FILLER                    PIC X(23).
