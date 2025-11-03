      * COPYMEMBER: LIBLP/LP01MP
      **************************************************************************
      *    LOAN MEMO PAYMENT FILE:   (240 BYTES)
      * 
      *         KEY 1 - LOAN #, SEQUENCE #  (ASCENDING)    
      *
      *  REV:
      *   070711 BAH MEMO PAYMENT FILE FOR VIEWING ONLY - CONVERTED PAYMENT
      *              RECORDS FOR REGACC PR# 154
      *             THESE PAYMENTS WERE BEING CONVERTED AFTER REAL MORE
      *             CURRENT PAYMENTS HAD ALREADY BEEN POSTED.
      **************************************************************************
       78  MP-SIZE                     VALUE 239.
       01  MP-REC.
           03  MP1-KEY.
               05  MP-ACCTNO           PIC  9(8).
               05  MP-SEQNO            PIC  9(4).
           03  MP1-KEY9               REDEFINES MP1-KEY
                                       PIC  9(12).   
      *-----------------------------------------------------
           03  MP-USERID               PIC X(10).
           03  MP-POSTDATE             PIC  9(6)     COMP-3.
           03  MP-POSTTIME             PIC  9(6)     COMP-3.
           03  MP-TRDATE               PIC  9(6)     COMP-3.
           03  MP-PAYDATE              PIC  9(6)     COMP-3.
           03  MP-TRCD                 PIC XX.
           03  MP-REFNO                PIC X(5).
           03  MP-POOLID              REDEFINES MP-REFNO
                                       PIC  9(5).
           03  MP-TFRNO               REDEFINES MP-REFNO.
               05  MP-FROMCL           PIC  99.
               05  MP-T                PIC X.
               05  MP-TOCL             PIC  99.
           03  MP-DDNO                REDEFINES MP-REFNO.
               05  MP-FROMDD           PIC  99.
               05  FILLER              PIC X.
               05  MP-TODD             PIC  99.
           03  MP-ADDON               REDEFINES MP-REFNO.
               05  MP-ADDON-TEXT       PIC X(3).
               05  MP-ADDON-TYPE       PIC X(2).
           03  FILLER                 REDEFINES MP-REFNO.
               05  MP-DLRVNO           PIC 9.
               05  FILLER              PIC X(4).
           03  FILLER                 REDEFINES MP-REFNO.
               05  MP-P4P5-TRCD        PIC XX.
               05  MP-P4P5-DLRVNO      PIC 9.
               05  FILLER              PIC X(2).
      *-----------------------------------------------------
           03  MP-TRAMT                PIC S9(7)V99  COMP-3.
           03  MP-APOTH                PIC S9(7)V99  COMP-3. 
           03  MP-APOT2                PIC S9(7)V99  COMP-3. 
           03  MP-APINT                PIC S9(7)V99  COMP-3.
           03  MP-APLC                 PIC S9(7)V99  COMP-3. 
           03  MP-APCUR                PIC S9(7)V99  COMP-3.
      *-----------------------------------------------------
           03  MP-OTHBAL               PIC S9(7)V99  COMP-3.
           03  MP-OT2BAL               PIC S9(7)V99  COMP-3.
           03  MP-INTBAL               PIC S9(7)V99  COMP-3.
           03  MP-LCBAL                PIC S9(7)V99  COMP-3.
           03  MP-CURBAL               PIC S9(7)V99  COMP-3.
      *-----------------------------------------------------
           03  MP-INTDUE               PIC S9(7)V99  COMP-3.
           03  MP-DLPROC               PIC S9(7)V99  COMP-3.
           03  MP-EARNED               OCCURS 3
                                       PIC S9(7)V99  COMP-3.
           03  MP-INTPAID              PIC S9(7)V99  COMP-3.
           03  MP-SSNO                 PIC  9(10)    COMP-3.
           03  MP-PDTH                 PIC  9(6)     COMP-3.
           03  MP-LCPDTH               PIC  9(6)     COMP-3.
           03  MP-TILLNO               PIC  X.
           03  MP-APINTOWE             PIC S9(7)V99  COMP-3.
           03  MP-PRIOR-POOLID        REDEFINES MP-APINTOWE
                                       PIC S9(9)     COMP-3.
           03  MP-PRIOR-DUEDAY        REDEFINES MP-APINTOWE
                                       PIC S9(9)     COMP-3.
           03  MP-ADDON-INTPDTH       REDEFINES MP-APINTOWE
                                       PIC S9(9)     COMP-3.

           03  MP-DLPARVHELD-ADJ       PIC S9(7)V99  COMP-3.
           03  MP-LT-SEQNO            REDEFINES MP-DLPARVHELD-ADJ
                                       PIC S9(9)     COMP-3.
           03  MP-IBPC                 PIC X.
           03  MP-MANUAL-RB            PIC X.
           03  MP-INSTYPES            REDEFINES MP-MANUAL-RB
                                       PIC X.
           03  MP-REV                  PIC X.
           03  MP-REVTRCD              PIC XX.
           03  MP-REVSEQNO             PIC 9(4)      COMP-3.
           03  MP-STATUSFG             PIC X.
           03  MP-NEWSTAT-RATE         PIC 999V999   COMP-3.
           03  MP-LCWORK1              PIC S9(7)V99  COMP-3.
           03  MP-NEW1ST-PYDATE       REDEFINES MP-LCWORK1
                                       PIC S9(9)     COMP-3.
           03  MP-DEF-DUEDATE         REDEFINES MP-LCWORK1
                                       PIC S9(9)     COMP-3.

           03  MP-LCWORK2              PIC S9(7)V99  COMP-3.
           03  MP-OLD1ST-PYDATE       REDEFINES MP-LCWORK2
                                       PIC S9(9)     COMP-3.
           03  MP-DEF-TERM            REDEFINES MP-LCWORK2
                                       PIC S9(9)     COMP-3.
           03  MP-DLRVCHGS-ADJ        OCCURS 3
                                       PIC S9(7)V99  COMP-3.
           03  MP-DLPARVPAID-ADJ       PIC S9(7)V99  COMP-3.
           03  MP-RT-SEQNO            REDEFINES MP-DLPARVPAID-ADJ
                                       PIC S9(9)     COMP-3.
      * MP-DLNO MUST BE '0' UNLESS A PAYMENT FROM RESERVE
      * WAS POSTED FROM A DEALER OTHER THAN THE DEALER WHO
      * THE CONTRACT WAS PURCHASED FROM.
           03  MP-DLNO                  PIC 9(4)V99   COMP-3.
      *    WORKAMT1 HOLDS CONTAINS THE AMOUNT OF PAYOFF PENALTY.
           03  MP-WORKAMT1              PIC S9(7)V99  COMP-3.
      *    WORKAMT2 HOLDS THE PRORATED SALES TAX PORTION OF THE INSURANCE REFUND
           03  MP-WORKAMT2              PIC S9(7)V99  COMP-3.
           03  MP-POSTING-BR            PIC 9(4)      COMP-3.
           03  MP-NOLCHG                PIC S9(3)     COMP-3.
           03  MP-COLLECTOR             PIC X(3).
           03  MP-EXTRA-PRIN-LFP        PIC S9(7)V99  COMP-3.
           03  MP-REMAIN-INT-SINCE-LFP  PIC S9(7)V99  COMP-3.
           03  MP-REMAIN-PRIN-SINCE-LFP PIC S9(7)V99  COMP-3.
           03  FILLER                   PIC X(4).
