      * COPYMEMBER: LIBGB/GB01PW_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - USER SECURITY PASSWORD FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  PW1-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  PW1-CURSOR-STAT-GOOD               VALUE "00".
           88  PW1-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  PW1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QPW1-WHERE-BEG-KEY.
           03  QPW1-WBEG-PGM            PIC X(10).
           03  QPW1-WBEG-SEQ            PIC 9(03).

       01  QPW1-WHERE-END-KEY.
           03  QPW1-WEND-PGM            PIC X(10).
           03  QPW1-WEND-SEQ            PIC 9(03).

       01  QPW-REC.
           03  QPW-PGM                      PIC X(10).
           03  QPW-SEQ                      PIC 9(03).
           03  QPW-LTOUCH-DATE                  PIC X(10).
           03  QPW-PASSYN                       PIC X.
           03  QPW-PASSWORD                     PIC X(14).
           03  QPW-DESC                         PIC X(30).
           03  QPW-USER-PROFILE                 PIC X.
           03  QPW-ASK-FG                       PIC X.
           03  QPW-EXCEPTION-FG                 PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
