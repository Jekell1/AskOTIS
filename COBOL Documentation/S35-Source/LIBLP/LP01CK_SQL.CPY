      * COPYMEMBER: LIBLP/LP01CK_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - CHECK MASTER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  CK1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CK1-CURSOR-STAT-GOOD              VALUE "00".
           88  CK1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CK1-RETURN-CODE                PIC S9(4)   COMP-5. 

       01  QCK1-WHERE-BEG-KEY.
           03  QCK1-WBEG-BRNO             PIC 9(04).
           03  QCK1-WBEG-CKNO             PIC 9(06).
 
       01  QCK1-WHERE-END-KEY.
           03  QCK1-WEND-BRNO             PIC 9(04).
           03  QCK1-WEND-CKNO             PIC 9(06).
 
       01  QCK-REC.
           03  QCK-BRNO                PIC 9(4).
           03  QCK-NO                  PIC 9(6).
           03  QCK-LTOUCH-DATE         PIC X(10).
           03  QCK-TYPE                PIC X(2).
           03  QCK-DATE                PIC X(10).
           03  QCK-AMT                 PIC S9(7)V99.
           03  QCK-PAYEE               PIC X(60).
           03  QCK-ADR1                PIC X(30).
           03  QCK-ADR2                PIC X(30).
           03  QCK-CITY                PIC X(30).
           03  QCK-STATE               PIC X(2).
           03  QCK-ZIP                 PIC X(10).
           03  QCK-REF                 PIC 9(6).
           03  QCK-REASON              PIC X(30).
           03  QCK-GLNO-1              PIC 9(11).
           03  QCK-GLAMT-1             PIC S9(7)V99.
           03  QCK-GLNO-2              PIC 9(11).
           03  QCK-GLAMT-2             PIC S9(7)V99.
           03  QCK-GLNO-3              PIC 9(11).
           03  QCK-GLAMT-3             PIC S9(7)V99.
           03  QCK-GLNO-4              PIC 9(11).
           03  QCK-GLAMT-4             PIC S9(7)V99.
           03  QCK-GLNO-5              PIC 9(11).
           03  QCK-GLAMT-5             PIC S9(7)V99.
           03  QCK-VOID                PIC X.
           03  QCK-PAID                PIC X.
           03  QCK-CLASS               PIC 9(3).
           03  QCK-DLNO                PIC 9(4)V99.
           03  QCK-PGM                 PIC XX.
           03  QCK-NC-SEQNO            PIC 99.
           03  QCK-VOID-DATE           PIC X(10).
           03  QCK-ACCTNO              PIC 9(8).
           03  QCK-DLRVNO              PIC 9.
           03  QCK-CLEARED-FG          PIC X.
           03  QCK-SSNO                PIC 9(9).
           03  QCK-BATCHID             PIC 9(10).
           03  QCK-UNPAID              PIC X.
           03  QCK-CLEARED-DATE        PIC X(10).
           03  QCK-TILLNO              PIC 99.
           03  QCK-EVENT-CD            PIC X(2).
           03  QCK-PAYEE-NO            PIC 9(6).
           03  QCK-FREE1               PIC X(70).
           03  QCK-FREE2               PIC X(70).
           03  QCK-BWPROC              PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
