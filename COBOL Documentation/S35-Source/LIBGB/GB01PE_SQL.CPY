      * COPYMEMBER: LIBGB/GB01PE_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - PASSWORD EXCEPTION FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  PE1-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  PE1-CURSOR-STAT-GOOD               VALUE "00".
           88  PE1-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  PE1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QPE1-WHERE-BEG-KEY.
           03  QPE1-WBEG-PGM            PIC X(10).
           03  QPE1-WBEG-SEQ            PIC 9(03).
           03  QPE1-WBEG-LOGIN          PIC X(08).

       01  QPE1-WHERE-END-KEY.
           03  QPE1-WEND-PGM            PIC X(10).
           03  QPE1-WEND-SEQ            PIC 9(03).
           03  QPE1-WEND-LOGIN          PIC X(08).

       01  QPE-REC.
           03  QPE-PGM                  PIC X(10).
           03  QPE-SEQ                  PIC 9(03).
           03  QPE-LOGIN                PIC X(8).
           03  QPE-LTOUCH-DATE          PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
