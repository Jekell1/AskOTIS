      * COPYMEMBER: LIBLP/LPFDTRC.CPY
      ******************************************************************
      *
      * DESCRIPTION:  TRANSACTION FILE [CHECKS/CASH]
      *
      * FILE INFO  :  CHECKS/CASH
      *               LP/TRCFILE
      *
      * TRC1-KEY = TC-BRNO,TC-KEY-DATE,TC-CKNO,TC-CLASS,TC-SEQ
      * TRC2-KEY = TC-KEY-DATE,TC-BRNO,TC-CKNO,TC-CLASS,TC-SEQ
      *
      *
      *=================================================================
      * HERE IS SOME DOCUMENTATION ON TC-TYPE, AS OF 3/19/12:
      *
      * CHECK TRANSACTIONS:
      *
      * TC-TYPE = "LN"  LOAN CHECK
      * TC-TYPE = "MS"  MISC. CHECK
      * TC-TYPE = "DL"  DEALER CHECK (DEALER PROCEEDS)
      * TC-TYPE = "DC"  DEALER CHECK (DEALER RESERVE FROM CHARGES)
      * TC-TYPE = "DR"  DEALER CHECK (DEALER RESERVE FROM PRINCIPAL)
      * TC-TYPE = "DP"  DEALER CHECK (DEALER PARTICIPATION HELD)
      * TC-TYPE = "OV"  OVERAGE CHECK
      * TC-TYPE = "DD"  DRAWER DEPOSIT CHECK
      *
      * CASH/NON-CHECK TRANSACTIONS:
      *
      * TC-TYPE = "AC"  DEALER ACH, DIRECT DEPOSIT OF DEALER PROCEEDS
      * TC-TYPE = "BK"  BANK ADJUSTMENT
      * TC-TYPE = "CA"  CASH RECEIPT/DISBURSEMENT
      * TC-TYPE = "ID"  INTERIM DEPOSIT (WORLD ONLY)
      * TC-TYPE = "IW"  INTERIM WITHDRAWAL (WORLD ONLY)
      * TC-TYPE = "RD"  RECEIPT (DEALER PROCEEDS)
      * TC-TYPE = "RC"  RECEIPT (DEALER RESERVE FROM CHARGES)
      * TC-TYPE = "RR"  RESERVE (DEALER RESERVE FROM PRINCIPAL)
      * TC-TYPE = "RP"  RESERVE (DEALER PARTICIPATION HELD)
      *
      *=================================================================
      * REV:
      * JTG 122988 CHANGED KEY TO COMP FIELDS
      * MJD 062795 ADDDED TC-USERID, TC-POSTDATE, TC-POSTTIME
      * BAH 062598 ADDED TC-TILLNO, WAS USING TC-ACCTNO BUT NOW THE
      *            TILLNO IS ALPHA
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      * BAH 060824 ADDED TC-ADR1 FOR THE EXTENDED PAYEE NAME FOR
      *             LENDMARK PR# 2809
      * BLF 071015 ADDED TC-PAYEE-NO, HOME PR# 3078
      * BLF 071015 ADDED TC-ACH-FG, REGACC PR# 185
      *  CS 160912 CHANGED TC-CLASS FROM 9(2) TO 9(3)   
      *  CS 160914 TC-PAYEE FROM PIC X(30) TO X(60) TO MATCH CK
      *            CHANGED TC-SPRCLASS FROM 9(2) TO 9(3)
      *            TC-POSTDATE FROM 9(6) TO 9(8), TILLNO FROM PIC X
      *            TO 9(2), TC-ADR1 FROM X(25) TO X(24) AND ADDED
      *            TC-ADR2,TC-CITY,TC-STATE,TC-ZIP
      *            SAVED VERSION LIBLP/LPFDTRC.160921
      * BAH 160926 PUT TC-PAYEE-NO BACK IN, REMOVED IN ERROR???
      * BAH 180124 CHANGED ADR1, ADR2 AND CITY TO X(30)
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      * BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210112 ADDED TC-EXTRACT-FG AS TC-FUTURE-FLAG(1) FOR WELLS
      *              FARGO POSITIVE PAY MULTIPLE RUNS (CHKWF4) #1486
      * BAH 20210305 ADDED TC-EXTRACT-FG, TC-REFCD-DEBIT AND
      *              TC-FUTURE-ALPHA5 OCCURS 5, #1490
      * BAH 20210426 REMOVED FUTURE FIELDS
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  TRC-SIZE                    VALUE 650.
       01  TRC-REC.
           03  TRC1-KEY.
               05  TC-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  TC-KEY-DATE         PIC 9(8).
               05  TC-CKNO             PIC 9(6).
               05  TC-CLASS            PIC 9(3). 
               05  TC-SEQ              PIC 9(5).

      *-------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  TC-LTOUCH-DATE      PIC 9(8)     COMP-3.
           03  TC-TYPE             PIC X(2).
      $XFD DATE=YYYYMMDD
           03  TC-DATE             PIC 9(8)     COMP-3.
           03  TC-AMT              PIC S9(7)V99 COMP-3.

           03  TC-PAYEE            PIC X(60).
           03  TC-REF              PIC 9(6)     COMP-3.
           03  TC-REASON           PIC X(30).
           03  TC-GLDIST           OCCURS 5.
               05  TC-GLNO         PIC 9(11)    COMP-3.
               05  TC-GLAMT        PIC S9(7)V99 COMP-3.
           03  TC-VOID             PIC X.
           03  TC-ACCTNO           PIC 9(8) COMP-3.
           03  TC-PAID             PIC X.
           03  TC-PGM              PIC XX.
           03  TC-DLRVNO           PIC 9.
      *-------------------------------------------------

           03  TC-SPRCLASS         PIC 999     COMP-3.
           03  TC-DLNO             PIC 9(4)V99 COMP-3.
           03  TC-USERID           PIC X(10).

      $XFD DATE=YYYYMMDD
           03  TC-POSTDATE         PIC 9(8)    COMP-3.
           03  TC-POSTTIME         PIC 9(6)    COMP-3.

           03  TC-TILLNO           PIC 9(2).

           03  TC-ADR1             PIC X(30).
           03  TC-ADR2             PIC X(30).
           03  TC-CITY             PIC X(30).
           03  TC-STATE            PIC X(2).
           03  TC-ZIP              PIC X(10).
           03  TC-PAYEE-NO         PIC 9(6)    COMP-3.
           03  TC-ACH-FG           PIC X.
           03  TC-EXTRACT-FG       PIC X.
           03  TC-DEBIT-REFCD      PIC X(5).
           03  FILLER              PIC X(308).
