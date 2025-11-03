      * COPYMEMBER: LIBLP/LPNOCL
      ******************************************************************
      *
      *    LPNOCL    NOTICE FILE CLEAR ROUTINE
      *
      * REV:
      *  050597 BAH ADDED NO-CURRENT-AVAIL NO-LATE-CHG AND RENAMED
      *             NO-LATECHG TO NO-LCHG-INTDUE
      *  072897 JTG ADDED CLEAR OF NO-WAGE-ASSIGN-DELINQ
      *  000726 BAH ADDED CLEAR OF NO-PAST-DUE-DAYS
      *  2018.07.16 BAH ADDED CLEAR OF NO-RECDEL
      *    190508   CS CLEAR FUTURE FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20220118 MOVE ALL "0" TO NO-LSTLPKEY, IT'S ALPHA
      * BAH 2024.0201 REMOVED NO-WAGE-ASSIGN-DELINQ
      *               ADDED NO-INDU-PENALTY, NO-CSUB, AND NO-RSUB.
      ******************************************************************
       CLEAR-NO-FILE SECTION.
           MOVE SPACES TO NO-REC.

           MOVE 0 TO NO-TDUE-DATE      NO-TDUELC        NO-TDUEAMT
                     NO-TDUEPRIN       NO-TDUETBAL      NO-TDUEPOFF
                     NO-LDUE-DATE      NO-LDUELC        NO-LDUEAMT
                     NO-DATE-PAID-LAST NO-AMT-PAID-LAST
                     NO-NXDUE-DATE     NO-NXDUEAMT      NO-NXDUELC
                     NO-TRUCONTR       NO-RECENCY       NO-PDTH-DATE
                     NO-LCPDTH-DATE    NO-LCHG-INTDUE   NO-AVAILCREDIT
                     NO-TDUEDELINQ     NO-TRUE-DUE-DATE NO-DEFERMENT
                     NO-TDUECONTR      NO-LDUECONTR     NO-NXDUECONTR
                     NO-CURRENT-AVAIL  NO-LATE-CHG      NO-PAST-DUE-DAYS
                     NO-BRNO           NO-INDU-PENALTY  NO-CSUB
                     NO-LTOUCH-DATE    NO-SCHED-PAYMENT NO-RSUB
                     NO-RECDEL.
          
           MOVE ALL "0" TO NO-LSTLPKEY.

       CLEAR-NO-FILE-EXIT.
           EXIT.
