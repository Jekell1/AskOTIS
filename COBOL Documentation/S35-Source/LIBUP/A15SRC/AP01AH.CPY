      * COPYMEMBER: LIBAP/AP01AH
      ***************************************************
      *  AP TRANSACTION HISTORY FILE
      *  KEY 1 - VENDOR/DATE/TYPE/CHECK#/INVOICE/SUF/SEQ
      *  KEY 2 - TYPE/CHECK#/INVOICE/SUF/SEQ
      *  KEY 3 - DATE
      *  REV:
      *  BLM 120105 ADDED AHP-POS-PAY-FG, THRIFT PR# 3943
      ***************************************************
       01  AH-REC.
           03  AH-KEY-AREA.
               05  AH-VENDOR         PIC 9(6).
               05  AH-DATE           PIC 9(6).
               05  AH-TRANS-TYPE     PIC XX.
      *
      *               AH-TRANS-TYPE - "P" = PAYMENT
      *                               "I" = INVOICE
      *
               05  AH-CHECKNO        PIC 9(6).
               05  AH-INVOICE        PIC X(16).
               05  AH-SUFFIX         PIC 99.
               05  AH-SEQ            PIC 9(3).
           03  AH1-KEY-AREA REDEFINES AH-KEY-AREA.
               05  AH1-KEY           PIC X(41).
           03  AH2-KEY-AREA REDEFINES AH-KEY-AREA.
               05  FILLER            PIC X(12).
               05  AH2-KEY           PIC X(29).
           03  AH3-KEY-AREA REDEFINES AH-KEY-AREA.
               05  FILLER            PIC X(6).
               05  AH3-KEY           PIC X(6).
               05  FILLER            PIC X(29).
           03  AH-LOC                PIC 9(4).
           03  AH-COMMON-REC            PIC X(248).
           03  AHI-INVOICE-REC REDEFINES AH-COMMON-REC.
               05  AHI-SEQ-NO           PIC 9.
               05  AHI-TYPE             PIC X.
      *
      *                AHI-TYPE - "I" = NORMAL INVOICE
      *                           "S" = SPLIT INVOICE
      *                           "R" = RECURRING INVOICE
      *                           "D" = DIRECT PAYMENT
      *
               05  AHI-INVDATE          PIC 9(6)     COMP-3.
               05  AHI-TRANSDATE        PIC 9(6)     COMP-3.
               05  AHI-DUEDATE          PIC 9(6)     COMP-3.
               05  AHI-DISC-DATE        PIC 9(6)     COMP-3.
               05  AHI-REFNO            PIC X(15).
               05  AHI-ORIGAMT          PIC S9(7)V99 COMP-3.
               05  AHI-NONDISC          PIC S9(7)V99 COMP-3.
               05  AHI-ORIGDISC         PIC S9(5)V99 COMP-3.
               05  AHI-TERMS            PIC 99       COMP-3.
               05  AHI-GL-TABLE.
                   07  AHI-GLNO         PIC 9(11)    COMP-3 OCCURS 10.
                   07  AHI-GLAMT        PIC S9(7)V99 COMP-3 OCCURS 10.
               05  AHI-PURORD           PIC X(12).
               05  AHI-TOT-PAYS         PIC 99.
               05  AHI-MONTHLY-CYCLE    PIC 99.
               05  AHI-POST-FLAG        PIC X.
               05  AHI-VENDOR-NAME      PIC X(25).
               05  AHI-REMIT-TO-FLAG    PIC X.
               05  AHI-REMIT-TO         PIC 9(6) COMP-3.
               05  AHI-POST-PER         PIC 99   COMP-3.
      *  GLB  05/18/04 ADD GT-REFERENCE
               05  AHI-GT-REF           PIC X(14).
               05  AHI-UNUSED           PIC X(26).
      *        05  AHI-UNUSED           PIC X(40).
           03  AHP-PAYMENT-REC REDEFINES AH-COMMON-REC.
               05  AHP-CHECKTYPE        PIC X.
      *
      *                AHP-CHECKTYPE - "M" = MANUAL CHECK
      *                AHP-CHECKTYPE - "L" = DUMMY CHECK
      *                                LINKED TO REMIT TO
      *
               05  AHP-CHECKDATE        PIC 9(6)     COMP-3.
               05  AHP-CHECKNO          PIC 9(6)     COMP-3.
               05  AHP-TRANS-CHECKDATE  PIC 9(6)     COMP-3.
               05  AHP-TOTAL-AP         PIC S9(7)V99 COMP-3.
               05  AHP-TOTAL-PAY        PIC S9(7)V99 COMP-3.
               05  AHP-TOTAL-DISC       PIC S9(5)V99 COMP-3.
               05  AHP-VENDOR-NAME      PIC X(25).
               05  AHP-ADR1             PIC X(25).
               05  AHP-ADR2             PIC X(25).
               05  AHP-CITY             PIC X(15).
               05  AHP-STATE            PIC X(4).
               05  AHP-ZIP              PIC X(10).
               05  AHP-POST-FLAG        PIC X.
               05  AHP-VOID             PIC XX.
               05  AHP-REMIT-TO         PIC 9(6).
               05  AHP-REMIT-TO-FLAG    PIC X(1).
               05  AHP-POST-PER         PIC 99   COMP-3.
      *  GLB  05/18/04 ADD GT-REFERENCE
               05  AHP-GT-REF           PIC X(14).
               05  AHP-POS-PAY-FG       PIC X.
               05  FILLER               PIC X(88).
      *        05  FILLER               PIC X(103).
               05  AHP-EXTRA-PER        PIC 99   COMP-3.
