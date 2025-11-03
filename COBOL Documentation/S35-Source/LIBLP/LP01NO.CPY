      * COPYMEMBER: LIBLP/LP01NO
      ******************************************************************
      *                 NOFILE    NOTICE FILE
      *
      *    NO1-KEY = NO-BRNO, NO-ACCTNO
      *
      * REV:
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  000726 BAH ADDED NO-PAST-DUE-DAYS, PR# 1170 SUPPORT
      *  160318 BAH MOVED NO-BRNO, INCREASED ALL DATES TO 9(8), CHANGED
      *             NO-LSTLPKEY TO X(13), FMNO TO 4, MORE FILLER, LTOUCH
      *             PD0003
      *  171120 BAH REMOVED COMP-3 FROM NO-BRNO
      *  180717 BAH ADDED NO-RECDEL SO WE CAN CALL CRNOR2, EMPOIQ
      *  190107 BAH MOVED NO-BRNO UNDER THE NO1-KEY AND CHANGED
      *             NO-LSTLPKEY FROM 13 TO 17
      *  190507  CS ADD FUTURE FIELDS
      *  BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS, AND
      *               FUTURE-DATE WAS MISSING XFD DATE
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      * BAH 2024.0201 REMOVED NO-FMNO, NO-PRINTED AND NO-WAGE-ASSIGN-DELINQ
      *               ADDED NO-AVCR-RESTRICT-FG, NO-INDU-PENALTY
      *               NO-CPOT30, NO-RDPOT30, NO-CPOTCHGOFF, NO-RDPOTCHGOFF,
      *               NO-CSUB, AND NO-RSUB.
      ******************************************************************
       78  NO-SIZE                 VALUE 430.
       01  NO-REC.
           03  NO1-KEY.
               05  NO-BRNO           PIC 9(4).
               05  NO-ACCTNO         PIC 9(8).
      $XFD DATE=YYYYMMDD
           03  NO-LTOUCH-DATE        PIC 9(8)     COMP-3.

      *----TO DATE DUE FIGURES-----------------------
      $XFD DATE=YYYYMMDD
           03  NO-TDUE-DATE          PIC 9(8)     COMP-3.
           03  NO-TDUELC             PIC S9(7)V99 COMP-3.
           03  NO-TDUEAMT            PIC S9(7)V99 COMP-3.
           03  NO-TDUEPRIN           PIC S9(7)V99 COMP-3.
           03  NO-TDUETBAL           PIC S9(7)V99 COMP-3.
           03  NO-TDUEPOFF           PIC S9(7)V99 COMP-3.

      *----LAST DUE FIGURES--------------------------
      $XFD DATE=YYYYMMDD
           03  NO-LDUE-DATE          PIC 9(8)     COMP-3.
           03  NO-LDUELC             PIC S9(7)V99 COMP-3.
           03  NO-LDUEAMT            PIC S9(7)V99 COMP-3.
      $XFD DATE=YYYYMMDD
           03  NO-DATE-PAID-LAST     PIC 9(8)     COMP-3.
           03  NO-AMT-PAID-LAST      PIC S9(7)V99 COMP-3.
           03  NO-LSTLPKEY           PIC X(17).

      *----NEXT DUE FIGURES--------------------------
      $XFD DATE=YYYYMMDD
           03  NO-NXDUE-DATE         PIC 9(8)     COMP-3.
           03  NO-NXDUEAMT           PIC S9(7)V99 COMP-3.
           03  NO-NXDUELC            PIC S9(7)V99 COMP-3.
      
           03  NO-TRUCONTR           PIC 9(4)     COMP-3.
           03  NO-RECENCY            PIC 9(4)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  NO-PDTH-DATE          PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  NO-LCPDTH-DATE        PIC 9(8)     COMP-3.
           03  NO-LCHG-INTDUE        PIC S9(7)V99 COMP-3.
           03  NO-AVAILCREDIT        PIC S9(7)V99 COMP-3.
           03  NO-TDUEDELINQ         PIC S9(7)V99 COMP-3.
           03  NO-DEFERMENT          PIC S9(7)V99 COMP-3.
      $XFD DATE=YYYYMMDD
           03  NO-TRUE-DUE-DATE      PIC 9(8)     COMP-3.
           03  NO-SCHED-PAYMENT      PIC S9(7)V99 COMP-3.

      * TO DATE DUE CONTR
           03  NO-TDUECONTR          PIC S9(7)V99 COMP-3.
      * LAST DUE CONTR
           03  NO-LDUECONTR          PIC S9(7)V99 COMP-3.
      * NEXT DUE CONTR
           03  NO-NXDUECONTR         PIC S9(7)V99 COMP-3.

           03  NO-CURRENT-AVAIL      PIC S9(7)V99 COMP-3.
           03  NO-LATE-CHG           PIC S9(7)V99 COMP-3.

           03  NO-PAST-DUE-DAYS      PIC 9(4)     COMP-3.
           03  NO-RECDEL             PIC S9(5)    COMP-3.

           03  NO-AVCR-RESTRICT-FG   PIC X.
           03  NO-INDU-PENALTY       PIC S9(7)V99 COMP-3.
           03  NO-CPOT30             PIC X.
           03  NO-RDPOT30            PIC X.
           03  NO-CPOTCHGOFF         PIC X.
           03  NO-RDPOTCHGOFF        PIC X.
           03  NO-CSUB               PIC S9(5)    COMP-3.
           03  NO-RSUB               PIC S9(5)    COMP-3.

           03  FILLER                 PIC X(233).
