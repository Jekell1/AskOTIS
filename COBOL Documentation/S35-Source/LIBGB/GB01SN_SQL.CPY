      * COPYMEMBER: LIBGB/GB01SN_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - SECURITY PROGRAM NAME FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  SN1-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  SN1-CURSOR-STAT-GOOD               VALUE "00".
           88  SN1-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".
       01  SN2-CURSOR-STAT              PIC X(2)  VALUE SPACES.
           88  SN2-CURSOR-STAT-GOOD               VALUE "00".
           88  SN2-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  SN1-RETURN-CODE           PIC S9(4)   COMP-5.
       01  SN2-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QSN1-WHERE-BEG-KEY.
           03  QSN1-WBEG-FORM           PIC X(6).

       01  QSN1-WHERE-END-KEY.
           03  QSN1-WEND-FORM           PIC X(6).

       01  QSN2-WHERE-BEG-KEY.
           03  QSN2-WBEG-SEQ            PIC 9(8).
           03  QSN2-WBEG-FORM           PIC X(6).

       01  QSN2-WHERE-END-KEY.
           03  QSN2-WEND-SEQ            PIC 9(8).
           03  QSN2-WEND-FORM           PIC X(6).


       01  QSN-REC.
           03  QSN-SEQ              PIC 9(8).
           03  QSN-FORM             PIC X(6).
           03  QSN-LTOUCH-DATE          PIC X(10).
           03  QSN-DESC                 PIC X(30).
           03  QSN-PURCHASABLE          PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
