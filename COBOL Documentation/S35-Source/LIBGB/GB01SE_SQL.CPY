      * COPYMEMBER: LIBGB/GB01SE_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - USER SECURITY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  SE1-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  SE1-CURSOR-STAT-GOOD               VALUE "00".
           88  SE1-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  SE1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QSE1-WHERE-BEG-KEY.
           03  QSE1-WBEG-FORM           PIC X(6).

       01  QSE1-WHERE-END-KEY.
           03  QSE1-WEND-FORM           PIC X(6).

       01  QSE-REC.
           03  QSE-FORM                 PIC X(06).
           03  QSE-LTOUCH-DATE          PIC X(10).
           03  QSE-EXP-DATE             PIC X(10).
           03  QSE-PURCHASABLE          PIC X(01).
           03  QSE-PROF-1               PIC X.
           03  QSE-PROF-2               PIC X.
           03  QSE-PROF-3               PIC X.
           03  QSE-PROF-4               PIC X.
           03  QSE-PROF-5               PIC X.
           03  QSE-PROF-6               PIC X.
           03  QSE-PROF-7               PIC X.
           03  QSE-PROF-8               PIC X.
           03  QSE-PROF-9               PIC X.
           03  QSE-PROF-10              PIC X.
           03  QSE-PROF-11               PIC X.
           03  QSE-PROF-12               PIC X.
           03  QSE-PROF-13               PIC X.
           03  QSE-PROF-14               PIC X.
           03  QSE-PROF-15               PIC X.
           03  QSE-PROF-16               PIC X.
           03  QSE-PROF-17               PIC X.
           03  QSE-PROF-18               PIC X.
           03  QSE-PROF-19               PIC X.
           03  QSE-PROF-20               PIC X.
           03  QSE-PROF-21               PIC X.
           03  QSE-PROF-22               PIC X.
           03  QSE-PROF-23               PIC X.
           03  QSE-PROF-24               PIC X.
           03  QSE-PROF-25               PIC X.
           03  QSE-PROF-26               PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
