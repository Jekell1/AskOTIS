      * COPYMEMBER: LIBLP/LP01CH_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY CHANGES
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  CH-CURSOR-STAT               PIC X(2) VALUE SPACES.
           88  CH-CURSOR-STAT-GOOD               VALUE "00".
           88  CH-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CH1-RETURN-CODE               PIC S9(4)   COMP-5.

       01  QCH-WHERE-BEG-KEY.
           03  QCH-WBEG-BRNO             PIC 9(04).
           03  QCH-WBEG-DATE             PIC X(10).
           03  QCH-WBEG-CODE             PIC X(03).
           03  QCH-WBEG-USERID           PIC X(10).
           03  QCH-WBEG-TIME             PIC 9(06).
           03  QCH-WBEG-SEQ              PIC 9(03).
 
       01  QCH-WHERE-END-KEY.
           03  QCH-WEND-BRNO             PIC 9(04).
           03  QCH-WEND-DATE             PIC X(10).
           03  QCH-WEND-CODE             PIC X(03).
           03  QCH-WEND-USERID           PIC X(10).
           03  QCH-WEND-TIME             PIC 9(06).
           03  QCH-WEND-SEQ              PIC 9(03).
 
       01  QCH-REC.
           03  QCH-BRNO                            PIC 9(4).
           03  QCH-DATE                            PIC X(10).
           03  QCH-CODE                            PIC X(3).
           03  QCH-USERID                          PIC X(10).
           03  QCH-TIME                            PIC 9(6).
           03  QCH-SEQ                             PIC 9(3).
           03  QCH-LTOUCH-DATE                     PIC X(10).
           03  QCH-DESCRIPT                        PIC X(25).
           03  QCH-ACCTNO                          PIC 9(8).
           03  QCH-SSNO                            PIC 9(9).
           03  QCH-TYPE                            PIC 99.
           03  QCH-ORG-CHAR                        PIC X(50).
           03  QCH-NEW-CHAR                        PIC X(50).

       EXEC SQL END DECLARE SECTION END-EXEC.
