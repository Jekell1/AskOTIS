      * COPYMEMBER: LIBLP/LPFDCLAS

      ************************************************************************
      *           (LPFDCLAS)
      *    FILE DESCRIPTOR FOR (CLASTR.C) (CLAST1.C) (CLAST2.C)
      * REV:
      *  JTG 091896 ADDED WK-PL-ONLY TO PASS TO CLAST2.C
      *  JTG 031097 ADDED WK-ONHAND, WK-SOLD & WK-REDEEM TO PASS TO CLAST2.C
      *  BAH 991230 ADDED OPTION TO I,S,O ZERO BALANCE ACCOUNTS, TFC PR# 62
      ************************************************************************
       FD  WK-FILE
           LABEL RECORD IS STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-TYPE         PIC X.
               05  WK-ACCTNO       PIC 9(8)      COMP-3.
           03  WK-NOSEL            PIC 9(8)      COMP.
           03  WK-AMTSEL           PIC 9(8)V99   COMP.
           03  WK-TO-DLNO          PIC 9(4)V99   COMP.
           03  WK-PL-ONLY          PIC X.
           03  WK-ONHAND           PIC X.
           03  WK-SOLD             PIC X.
           03  WK-REDEEM           PIC X.
           03  WK-CHG-SPR-LC       PIC X.
           03  WK-BUILD-SPR        PIC X.
           03  WK-ZBAL             PIC X.

