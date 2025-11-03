      * COPYMEMBER: LIBAP/AP01AP
      **************************************************
      *    ACCOUNTS PAYABLE INVOICE FILE               *
      *    KEY1 = VENDOR NO. / INVOICE NO.             *
      *    KEY2 = LOC / VENDOR NO. / INVOICE NO.       *
      *    KEY3 = VENDOR NO. / DATE                    *
      **************************************************
       78  AP-SIZE                VALUE 340.
       01  AP-REC.
           03  AP2-KEY.
               05  AP-LOC         PIC 9(4).
               05  AP1-KEY.
                   07  AP-VENDOR  PIC 9(6).
                   07  AP-INVOICE PIC X(16).
                   07  AP-SUFFIX  PIC 99.
           03  AP3-KEY.
               05  AP-VENDOR2     PIC 9(6).
      * AP-REV-DATE IS 99999999 - YYYYMMDD
               05  AP-REV-DATE    PIC 9(8).
           03  AP-TYPE            PIC X.
      *           I = INVOICE
      *           S = SPLIT INVOICE
      *           R = RECURRING INVOICE
      *           D = DIRECT PAY
      *>>XDD DATE=YYYYMMDD
           03  AP-LTOUCH-DATE     PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  AP-INVDATE         PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  AP-TRANSDATE       PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  AP-DUEDATE         PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  AP-DISC-DATE       PIC 9(8)     COMP-3.
           03  AP-REFNO           PIC X(15).
           03  AP-ORIGAMT         PIC S9(7)V99 COMP-3.
           03  AP-SPLIT-AMT       PIC S9(7)V99 COMP-3.
           03  AP-PAIDAMT         PIC S9(7)V99 COMP-3.
           03  AP-NONDISC         PIC S9(7)V99 COMP-3.
           03  AP-ORIGDISC        PIC S9(7)V99 COMP-3.
           03  AP-SPLIT-DISC      PIC S9(7)V99 COMP-3.
           03  AP-DISC-TAKEN      PIC S9(7)V99 COMP-3.
           03  AP-TERMS           PIC 99       COMP-3.
           03  AP-CKNO            PIC 9(6)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  AP-CHECKDATE       PIC 9(8)     COMP-3.
           03  AP-GL-TABLE.
               05  AP-GLNO        PIC 9(11)    COMP-3 OCCURS 10.
               05  AP-GLAMT       PIC S9(7)V99 COMP-3 OCCURS 10.
           03  AP-STOP            PIC X.
           03  AP-PURORD          PIC X(12).
           03  AP-AUTH-FLAG       PIC X.
           03  AP-TOT-PAYS        PIC 99.
           03  AP-MONTHLY-CYCLE   PIC 99.
           03  AP-AUTH            PIC X(10).
      *>>XDD DATE=YYYYMMDD
           03  AP-SYSDATE         PIC 9(8).
           03  AP-MISC-VENDOR     PIC 9(6).
           03  AP-MISC-ALP REDEFINES AP-MISC-VENDOR PIC X(6).
           03  FILLER             PIC X(59).
