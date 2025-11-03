      * COPYMEMBER: LIBLP/LP01CX_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - CHECK TEMPORARY FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  CX1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CX1-CURSOR-STAT-GOOD              VALUE "00".
           88  CX1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  CX2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CX2-CURSOR-STAT-GOOD              VALUE "00".
           88  CX2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CX1-RETURN-CODE           PIC S9(4)   COMP-5.
       01  CX2-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QCX1-WHERE-BEG-KEY.
           03  QCX1-WBEG-BRNO           PIC 9(04).
           03  QCX1-WBEG-TYPE           PIC X(02).
           03  QCX1-WBEG-SEQNO          PIC 9(04).
 
       01  QCX1-WHERE-END-KEY.
           03  QCX1-WEND-BRNO           PIC 9(04).
           03  QCX1-WEND-TYPE           PIC X(02).
           03  QCX1-WEND-SEQNO          PIC 9(04).
 
       01  QCX2-WHERE-BEG-KEY.
           03  QCX2-WBEG-BRNO           PIC 9(04).
           03  QCX2-WBEG-PAYEE          PIC X(60).
 
       01  QCX2-WHERE-END-KEY.
           03  QCX2-WEND-BRNO           PIC 9(04).
           03  QCX2-WEND-PAYEE          PIC X(60).
 
       01  QCX-REC.
           03  QCX-BRNO             PIC 9(4).
           03  QCX-TYPE             PIC X(2).
           03  QCX-SEQNO            PIC 9(4).
           03  QCX-PAYEE            PIC X(60).
           03  QCX-LTOUCH-DATE          PIC X(10).
           03  QCX-ADR1                 PIC X(30).
           03  QCX-ADR2                 PIC X(30).
           03  QCX-CITY                 PIC X(30).
           03  QCX-STATE                PIC X(2).
           03  QCX-ZIP                  PIC X(10).
           03  QCX-AMT                  PIC S9(7)V99.
           03  QCX-REF                  PIC 9(6).
           03  QCX-REASON               PIC X(30).
           03  QCX-GLNO-1             PIC 9(11).
           03  QCX-GLAMT-1            PIC S9(7)V99.
           03  QCX-GLNO-2             PIC 9(11).
           03  QCX-GLAMT-2            PIC S9(7)V99.
           03  QCX-GLNO-3             PIC 9(11).
           03  QCX-GLAMT-3            PIC S9(7)V99.
           03  QCX-GLNO-4             PIC 9(11).
           03  QCX-GLAMT-4            PIC S9(7)V99.
           03  QCX-GLNO-5             PIC 9(11).
           03  QCX-GLAMT-5            PIC S9(7)V99.
           03  QCX-FREE1                PIC X(70).
           03  QCX-FREE2                PIC X(70).
           03  QCX-VOID                 PIC X.
           03  QCX-CLASS                PIC 9(3).
           03  QCX-CKNO                 PIC 9(7).
           03  QCX-DLNO                 PIC 9(4)V99.
           03  QCX-DATE                 PIC X(10).
           03  QCX-PAID                 PIC X.
           03  QCX-PGM                  PIC XX.
           03  QCX-BATCHID              PIC 9(10).
           03  QCX-BWPROC               PIC X.
           03  QCX-CKNO-NEW             PIC 9(7).
           03  QCX-DLRVNO               PIC 9.
           03  QCX-DT-SEQ               PIC 9(4).
           03  QCX-RESCISSION           PIC X.
           03  QCX-TILLNO               PIC 99.
           03  QCX-ACCTNO               PIC 9(8).
           03  QCX-SERVACCT             PIC X.
           03  QCX-DT-DATE              PIC X(10).
           03  QCX-SSNO                 PIC 9(9).
           03  QCX-DT-PAID              PIC X.
           03  QCX-DT-UNPAID            PIC X.
           03  QCX-CKPAID-NEW           PIC X.
           03  QCX-EVENT-CD             PIC X(2).
           03  QCX-PAYEE-NO             PIC 9(6).
           03  QCX-DEPOSIT-WITHDR       PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
