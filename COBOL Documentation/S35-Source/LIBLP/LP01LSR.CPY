      * COPYMEMBER: LIBLP/LP01LSR
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *         RESCHEDULE/SCRA INFORMATION
      *                     ^ SERVICE MEMBER CIVIL RELIEF ACT
      *
      *    LSR1-KEY = LSR-BRNO, LSR-ACCTNO, LSR-SEQNO
      *
      * REV:
      *  BAH 110328 ADDED "R" RESCHEDULE/SCRA INFO REGACC PR# 289/290
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 190506 ADD FUTURE FIELDS
      *  BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  LSR-SIZE                       VALUE 450.
       01  LSR-REC.
           03  LSR1-KEY.
               05  LSR-BRNO                PIC 9(4).
               05  LSR-ACCTNO              PIC 9(8).
               05  LSR-SEQNO               PIC 99.

      $XFD DATE=YYYYMMDD
           03  LSR-LTOUCH-DATE            PIC 9(8)      COMP-3.

           03  LSR-SCRA-ACTIVE            PIC X.
      $XFD DATE=YYYYMMDD
           03  LSR-SCRA-START-DATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSR-SCRA-END-DATE          PIC 9(8)      COMP-3.
           03  LSR-SCRA-BORROWER          PIC X.
      $XFD DATE=YYYYMMDD
           03  LSR-CURRENT-RESCHED-DATE   PIC 9(8)      COMP-3.
           03  LSR-CURRENT-REASON-CODE    PIC X(5).
      $XFD DATE=YYYYMMDD
           03  LSR-INITIAL-RESCHED-DATE   PIC 9(8)      COMP-3.
           03  LSR-INITIAL-REASON-CODE    PIC X(5).
      $XFD DATE=YYYYMMDD
           03  LSR-INITIAL-TDR-DATE       PIC 9(8)      COMP-3.
           03  LSR-INITIAL-TDR-REASON     PIC X(5).
      $XFD DATE=YYYYMMDD
           03  LSR-CURRENT-TDR-DATE       PIC 9(8)      COMP-3.
           03  LSR-CURRENT-TDR-REASON     PIC X(5).

           03  FILLER                 PIC X(379).
