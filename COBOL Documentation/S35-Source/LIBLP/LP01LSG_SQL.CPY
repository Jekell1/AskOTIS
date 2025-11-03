      * COPYMEMBER: LIBLP/LP01LSG_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN STATUS -  GPS STARTER
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LSG1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LSG1-CURSOR-STAT-GOOD             VALUE "00".
           88  LSG1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LSG1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLSG1-WHERE-BEG-KEY.
           03  QLSG1-WBEG-BRNO             PIC 9(04).
           03  QLSG1-WBEG-ACCTNO           PIC 9(08).
           03  QLSG1-WBEG-SEQNO            PIC 9(02).

       01  QLSG1-WHERE-END-KEY.
           03  QLSG1-WEND-BRNO             PIC 9(04).
           03  QLSG1-WEND-ACCTNO           PIC 9(08).
           03  QLSG1-WEND-SEQNO            PIC 9(02).

       01  QLSG-REC.
           03  QLSG-BRNO               PIC 9(4).
           03  QLSG-ACCTNO             PIC 9(8).
           03  QLSG-SEQNO              PIC 99.
           03  QLSG-LTOUCH-DATE        PIC X(10).
           03  QLSG-SERIAL-NO             PIC X(12).
           03  QLSG-STARTER-STAT          PIC X.
           03  QLSG-WARNING-STAT          PIC X.
           03  QLSG-RECOVERY-STAT         PIC X.
           03  QLSG-LAST-COMMAND-DATE     PIC X(10).
           03  QLSG-LAST-ADDRESS          PIC X(75).
           03  QLSG-LAST-AUTO-REPORT-DATE PIC X(10).
           03  QLSG-GPS-INSTALLER         PIC X(3).

       EXEC SQL END DECLARE SECTION END-EXEC.
