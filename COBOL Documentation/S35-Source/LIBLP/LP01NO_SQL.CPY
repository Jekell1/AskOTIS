      * COPYMEMBER: LIBLP/LP01NO_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - NOTICE FILE
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      * BAH 2024.0201 REMOVED NO-FMNO, NO-PRINTED AND NO-WAGE-ASSIGN-DELINQ
      *               ADDED QNO-AVCR-RESTRICT-FG, QNO-INDU-PENALTY  
      *               QNO-CPOT30, QNO-RDPOT30, QNO-CPOTCHGOFF, QNO-RDPOTCHGOFF,
      *               QNO-CSUB, AND QNO-RSUB.
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  NO1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  NO1-CURSOR-STAT-GOOD              VALUE "00".
           88  NO1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  NO1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QNO1-WHERE-BEG-KEY.
           03  QNO1-WBEG-BRNO             PIC 9(04).
           03  QNO1-WBEG-ACCTNO           PIC 9(08).
 
       01  QNO1-WHERE-END-KEY.
           03  QNO1-WEND-BRNO             PIC 9(04).
           03  QNO1-WEND-ACCTNO           PIC 9(08).
 
       01  QNO-REC.
           03  QNO-BRNO           PIC 9(4).
           03  QNO-ACCTNO         PIC 9(8).
           03  QNO-LTOUCH-DATE        PIC X(10).
           03  QNO-TDUE-DATE          PIC X(10).
           03  QNO-TDUELC             PIC S9(7)V99.
           03  QNO-TDUEAMT            PIC S9(7)V99.
           03  QNO-TDUEPRIN           PIC S9(7)V99.
           03  QNO-TDUETBAL           PIC S9(7)V99.
           03  QNO-TDUEPOFF           PIC S9(7)V99.
           03  QNO-LDUE-DATE          PIC X(10).
           03  QNO-LDUELC             PIC S9(7)V99.
           03  QNO-LDUEAMT            PIC S9(7)V99.
           03  QNO-DATE-PAID-LAST     PIC X(10).
           03  QNO-AMT-PAID-LAST      PIC S9(7)V99.
           03  QNO-LSTLPKEY           PIC X(17).
           03  QNO-NXDUE-DATE         PIC X(10).
           03  QNO-NXDUEAMT           PIC S9(7)V99.
           03  QNO-NXDUELC            PIC S9(7)V99.
           03  QNO-TRUCONTR           PIC 9(4).
           03  QNO-RECENCY            PIC 9(4).
           03  QNO-PDTH-DATE          PIC X(10).
           03  QNO-LCPDTH-DATE        PIC X(10).
           03  QNO-LCHG-INTDUE        PIC S9(7)V99.
           03  QNO-AVAILCREDIT        PIC S9(7)V99.
           03  QNO-TDUEDELINQ         PIC S9(7)V99.
           03  QNO-DEFERMENT          PIC S9(7)V99.
           03  QNO-TRUE-DUE-DATE      PIC X(10).
           03  QNO-SCHED-PAYMENT      PIC S9(7)V99.
           03  QNO-TDUECONTR          PIC S9(7)V99.
           03  QNO-LDUECONTR          PIC S9(7)V99.
           03  QNO-NXDUECONTR         PIC S9(7)V99.
           03  QNO-CURRENT-AVAIL      PIC S9(7)V99.
           03  QNO-LATE-CHG           PIC S9(7)V99.
           03  QNO-PAST-DUE-DAYS      PIC 9(4).
           03  QNO-RECDEL             PIC S9(5).
           03  QNO-AVCR-RESTRICT-FG   PIC X.
           03  QNO-INDU-PENALTY       PIC S9(7)V99.
           03  QNO-CPOT30             PIC X.
           03  QNO-RDPOT30            PIC X.
           03  QNO-CPOTCHGOFF         PIC X.
           03  QNO-RDPOTCHGOFF        PIC X.
           03  QNO-CSUB               PIC S9(5).
           03  QNO-RSUB               PIC S9(5).

       EXEC SQL END DECLARE SECTION END-EXEC.
