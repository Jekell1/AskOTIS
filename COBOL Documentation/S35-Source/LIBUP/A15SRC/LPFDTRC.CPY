      * COPYMEMBER: LIBLP/LPFDTRC
      *******************************************************************
      *             TR-CODE  =  "C" 
      *               CHECKS/CASH
      *
      *******************************************************************
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
      *******************************************************************
      * REV: JTG 122988 CHANGED KEY TO COMP FIELDS
      *      MJD 062795 ADDDED TC-USERID, TC-POSTDATE, TC-POSTTIME
      *      BAH 062598 ADDED TC-TILLNO, WAS USING TC-ACCTNO BUT NOW THE
      *                 TILLNO IS ALPHA
      *      MJD 990127 ----- UPDATED IN FLEXGEN -----
      *      BAH 060824 ADDED TC-ADR1 FOR THE EXTENDED PAYEE NAME FOR
      *                 LENDMARK PR# 2809
      *      BLF 071015 ADDED TC-PAYEE-NO, HOME PR# 3078
      *      BLF 071015 ADDED TC-ACH-FG, REGACC PR# 185
      *******************************************************************
       01  TC-REC.
           03  TC-DAY              PIC 99       COMP.
           03  TC-BRNO             PIC 9(4)     COMP.
           03  TC-CODE             PIC X.
           03  TC-CKNO             PIC 9(6)     COMP.
           03  TC-CLASS            PIC 99       COMP.
           03  TC-SEQ              PIC 9(4)     COMP.
           03  TC-FILLER2          PIC X(4).
      *-------------------------------------------------
           03  TC-TYPE             PIC X(2).
           03  TC-DATE             PIC 9(6)     COMP-3.
           03  TC-AMT              PIC S9(7)V99 COMP-3.
           03  TC-PAYEE            PIC X(30).
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
           03  TC-SPRCLASS         PIC 99.
           03  TC-DLNO             PIC 9(4)V99 COMP-3.
           03  TC-USERID           PIC X(10).
           03  TC-POSTDATE         PIC 9(6)    COMP-3.
           03  TC-POSTTIME         PIC 9(6)    COMP-3.
           03  TC-TILLNO           PIC X.
           03  TC-ADR1             PIC X(25).
           03  TC-PAYEE-NO         PIC 9(6)    COMP-6.
           03  TC-ACH-FG           PIC X.
           03  TC-EXTRACTED-FG     PIC X.
           03  TC-DEBIT-REFCD      PIC X(5).
           03  FILLER              PIC X(336).
