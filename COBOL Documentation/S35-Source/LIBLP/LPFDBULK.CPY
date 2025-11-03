      * COPYMEMBER: LIBLP/LPFDBULK

      **************************************************************************
      *           (LPFDBULK)
      *    FILE DESCRIPTOR FOR (BULKTR.C) (BULKT1.C) (BULKT2.C)
      *                        (BULKGR.C) (BULKG1.C) (BULKG2.C)
      * REV:
      *  JTG 091896 ADDED WK-PL-ONLY TO PASS TO BULKT2.C
      *  JTG 031097 ADDED WK-ONHAND, WK-SOLD & WK-REDEEM TO PASS TO CLAST2.C
      *  JTG 031898 ADDED OPTION TO I,S,O ZERO BALANCE ACCOUNTS, WORLD #184.
      *             ALSO CHANGED WK-KEY TO INCLUDE LN-CLASS, SO REPORTS ARE
      *             CORRECTLY ORDERED.
      **************************************************************************
       FD  WK-FILE
           LABEL RECORD IS STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-TYPE         PIC X.
               05  WK-BRNO         PIC 9(4)      COMP-3.
               05  WK-CLASS        PIC 9(3)      COMP-3.
               05  WK-ACCTNO       PIC 9(8)      COMP-3.
           03  WK-NOSEL            PIC 9(8)      COMP.
           03  WK-AMTSEL           PIC 9(8)V99   COMP.
           03  WK-TO-DLNO          PIC 9(4)V99   COMP.
           03  WK-PL-ONLY          PIC X.
           03  WK-ONHAND           PIC X.
           03  WK-SOLD             PIC X.
           03  WK-REDEEM           PIC X.
           03  WK-ZBAL             PIC X.

