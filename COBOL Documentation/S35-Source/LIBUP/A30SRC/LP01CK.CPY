      * COPYMEMBER: LIBLP/LP01CK
      **************************************************
      *                ( CHECK FILE )    
      *    KEY - BRANCH #, CHECK NO.
      * REV:
      *  JTG 112097 EXPANDED BY 30 BYTES
      *  MJD 990125 ----- UPDATED IN FLEXGEN -----
      *  CS  1999.0315 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  GLB 2000.0808 MAKE CK-DLNO 9(6) MATCH OTHER WORKERS 9(4)V99
      *  GLB 2000.0920 ADD CK-BATCHID
      *  GLB 2000.1016 ADD CK-UNPAID (FOR DS-UNPAIDCK)
      *  BAH 020408 ADDED CK-CLEARED-DATE FOR REGMGM, PR# 1726
      *  GLB 030326 ADD CK-TILLNO FOR PAYEE TYPE "DD" WORLD, PR#320
      *             CASH DRAWER DEPOSIT CHECK
      *   CS 060606 ADDED CK-EVENT-CD SECURITY AUTO PR#2503
      *  BLF 060712 ADDED CK-PAYEE-NO,  DEVELOPMENT PR# 2633
      *  BAH 060824 ADDED CK-ADR1-PARTIAL FOR 10 BYTES OF THE EXTENDED
      *             PAYEE NAME WHICH IS STORED IN CX-ADR1, DIDNT WANT TO TAKE
      *             ALL THE FILLER BUT I REALLY NEEDED 25! LENDMARK  PR# 2809
      *  BAH 160317 EXPANDED CLASS, TILLNO NUMERIC, ADDED ADDRESS INFO, EXPANDED
      *             PAYEE TO 60 PD0003
      *  BAH 180215 ADDED CK-BWPROC
      **************************************************
       78  CK-SIZE                 VALUE 520.
       01  CK-REC.
           03  CK1-KEY.
               05  CK-BRNO         PIC 9(4).
               05  CK-NO           PIC 9(6).
      *>>XDD DATE=YYYYMMDD
           03  CK-LTOUCH-DATE      PIC 9(8)     COMP-3.

           03  CK-TYPE             PIC X(2).
      *>>XDD DATE=YYYYMMDD
           03  CK-DATE             PIC 9(8)     COMP-3.
           03  CK-AMT              PIC S9(7)V99 COMP-3.

           03  CK-PAYEE            PIC X(60).
           03  CK-ADR1             PIC X(30).
           03  CK-ADR2             PIC X(30).
           03  CK-CITY             PIC X(30).
           03  CK-STATE            PIC X(2).
           03  CK-ZIP              PIC X(10).
           03  CK-REF              PIC 9(6)     COMP-3.
           03  CK-REASON           PIC X(30).
           03  CK-GLDIST           OCCURS 5.
               05  CK-GLNO         PIC 9(11)    COMP-3.
               05  CK-GLAMT        PIC S9(7)V99 COMP-3.
           03  CK-VOID             PIC X.
           03  CK-PAID             PIC X.
           03  CK-CLASS            PIC 9(3)     COMP-3.
           03  CK-DLNO             PIC 9(4)V99  COMP-3.
           03  CK-PGM              PIC XX.
           03  CK-NC-SEQNO         PIC 99.
      *>>XDD DATE=YYYYMMDD
           03  CK-VOID-DATE        PIC 9(8)     COMP-3.
           03  CK-ACCTNO           PIC 9(8)     COMP-3.
           03  CK-DLRVNO           PIC 9.
           03  CK-CLEARED-FG       PIC X.
           03  CK-SSNO             PIC 9(9)     COMP-3.
           03  CK-BATCHID          PIC 9(10)    COMP-3.
           03  CK-UNPAID           PIC X.
      *>>XDD DATE=YYYYMMDD
           03  CK-CLEARED-DATE     PIC 9(8)     COMP-3.
           03  CK-TILLNO           PIC 99.
           03  CK-EVENT-CD         PIC X(2).
           03  CK-PAYEE-NO         PIC 9(6)     COMP-3.
           03  CK-FREE1            PIC X(70).
           03  CK-FREE2            PIC X(70).
      * "Y" PRESSED F3 CHECK TO BORROWER
           03  CK-BWPROC            PIC X.

           03  FILLER              PIC X(52).
