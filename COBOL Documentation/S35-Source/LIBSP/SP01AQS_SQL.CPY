      * COPYMEMBER: LIBSP/SP01AQS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - AUDITOR INQUIRY SUMMARY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  AQS1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  AQS1-CURSOR-STAT-GOOD             VALUE "00".
           88  AQS1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  AQS1-RETURN-CODE            PIC S9(4)   COMP-5.

       01  QAQS1-WHERE-BEG-KEY.
           03  QAQS1-WBEG-BRNO         PIC 9(04).
 
       01  QAQS1-WHERE-END-KEY.
           03  QAQS1-WEND-BRNO         PIC 9(04).
 
       01  QAQS-REC.
           03  QAQS-BRNO               PIC 9(4).
           03  QAQS-NEXT-SEQNO         PIC 9(6).
           03  QAQS-TOT-REC            PIC 9(6).
           03  QAQS-WORKED-REC         PIC 9(6).
           03  QAQS-LTOUCH-DATE        PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
