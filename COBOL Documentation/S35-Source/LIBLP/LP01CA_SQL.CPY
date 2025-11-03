      * COPYMEMBER: LIBLP/LP01CA_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - CASH DRAWER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  CA1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CA1-CURSOR-STAT-GOOD              VALUE "00".
           88  CA1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CA1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QCA1-WHERE-BEG-KEY.
           03  QCA1-WBEG-BRNO             PIC 9(04).
           03  QCA1-WBEG-DATE             PIC X(10).
           03  QCA1-WBEG-TILLNO           PIC 9(2). 
 
       01  QCA1-WHERE-END-KEY.
           03  QCA1-WEND-BRNO             PIC 9(04).
           03  QCA1-WEND-DATE             PIC X(10).
           03  QCA1-WEND-TILLNO           PIC 9(2).
 
       01  QCA-REC.
           03  QCA-BRNO             PIC 9(4).
           03  QCA-DATE             PIC X(10).
           03  QCA-TILLNO           PIC 99.
           03  QCA-LTOUCH-DATE      PIC X(10).
           03  QCA-DESC             PIC X(30).
           03  QCA-BEGBAL           PIC S9(7)V99.
           03  QCA-OPENED-HHMM      PIC 9(4).
           03  QCA-CLOSED-HHMM      PIC 9(4).
           03  QCA-ENDBAL           PIC S9(7)V99.
           03  QCA-BALBY            PIC X(10).
           03  QCA-RUNNING          PIC S9(7)V99.
           03  QCA-DEPOSIT          PIC S9(7)V99.
           03  QCA-OPEN-USERID      PIC X(8).
           03  QCA-NET-CASH-NOTES   PIC S9(7)V99.
           03  QCA-PASSWORD         PIC X(4).
           03  QCA-CHECK-DEPOSIT    PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
