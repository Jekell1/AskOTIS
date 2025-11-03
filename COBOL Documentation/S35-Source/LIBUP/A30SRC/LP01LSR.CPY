      * COPYMEMBER: LIBLP/LP01LSR
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *         RESCHEDULE/SCRA INFORMATION
      *                     ^ SERVICE MEMBER CIVIL RELIEF ACT
      *
      * REV:
      *  BAH 110328 ADDED "R" RESCHEDULE/SCRA INFO REGACC PR# 289/290
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ******************************************************************
       78  LSR-SIZE                       VALUE 450.
       01  LSR-REC.
           03  LSR1-KEY.
               05  LSR-BRNO               PIC 9(4).
               05  LSR-ACCTNO             PIC 9(8).
               05  LSR-SEQNO              PIC 99.

      *>>XDD DATE=YYYYMMDD
           03  LSR-LTOUCH-DATE               PIC 9(8)     COMP-3.

           03  LSR-RESCHEDULE-AREA.
               05  LSR-SCRA-ACTIVE           PIC X.
      *>>XDD DATE=YYYYMMDD
               05  LSR-SCRA-START-DATE       PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSR-SCRA-END-DATE         PIC 9(8)      COMP-3.
               05  LSR-SCRA-BORROWER         PIC X.
      *>>XDD DATE=YYYYMMDD
               05  LSR-CURRENT-RESCHED-DATE  PIC 9(8)      COMP-3.
               05  LSR-CURRENT-REASON-CODE   PIC X(5).
      *>>XDD DATE=YYYYMMDD
               05  LSR-INITIAL-RESCHED-DATE  PIC 9(8)      COMP-3.
               05  LSR-INITIAL-REASON-CODE   PIC X(5).
      *>>XDD DATE=YYYYMMDD
               05  LSR-INITIAL-TDR-DATE      PIC 9(8)      COMP-3.
               05  LSR-INITIAL-TDR-REASON    PIC X(5).
      *>>XDD DATE=YYYYMMDD
               05  LSR-CURRENT-TDR-DATE      PIC 9(8)      COMP-3.
               05  LSR-CURRENT-TDR-REASON    PIC X(5).

               05  FILLER                    PIC X(379).

