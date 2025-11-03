      * COPYMEMBER: LIBLP/LP01CA
      **************************************************
      *             ( CASH DRAWER FILE ) 
      *    KEY 1 - BRANCH, DATE, TILL # 
      * REV:
      *  JTG 112097 EXPANDED BY 15 BYTES
      *   CS 112497 ADDED LOGIN ID FOR OPENING OF CASH DRAWER
      *  BAH 062498 CHANGED CA-TILLNO TO ALPHANUMERIC, AND MADE THE FIRST
      *             CHARACTER CA-TILLNO-ZERO SO THE OLD TILLNO'S WOULD
      *             STILL BE IN THE CORRECT POSITION, THE OLD TILLNO WAS 99.
      *             MUST ALWAYS MOVE 0 TO CA-TILLNO-ZERO SO THE OLD RECORDS
      *             MATCH THESE NEW RECORDS.
      *  BLV 980923 ADDED CA-NET-CASH-NOTES TO TRACK CASH DRAWER TRANS
      *             FROM NOTES PAYABLE
      *  MJD 990125 ----- UPDATED IN FLEXGEN -----
      *  CS  1999.0312 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  GLB 990715 ADD CA-PASSWORD-DRAWER PIC X(3), REGMGM PR# 901
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BLM 160915 ADDED CA-CHECK-DEPOSIT FOR SCANNED CKS, WORLD PR# 1188
      *********************************************************************
       01  CA-REC.
           03  CA1-KEY.
               05  CA-BRNO             PIC 9(4).
               05  CA-DATE             PIC 9(6).
               05  CA-TILLNO-ZERO      PIC 9.
               05  CA-TILLNO           PIC X.
           03  CA-DESC                 PIC X(30).
           03  CA-BEGBAL               PIC S9(7)V99 COMP-3.
           03  CA-OPENED               PIC 9(4)     COMP-3.
           03  CA-CLOSED               PIC 9(4)     COMP-3.
           03  CA-ENDBAL               PIC S9(7)V99 COMP-3.
           03  CA-BALBY                PIC X(10).
           03  CA-RUNNING              PIC S9(7)V99 COMP-3.
           03  CA-DEPOSIT              PIC S9(7)V99 COMP-3.
           03  CA-OPENID               PIC X(8).
           03  CA-NET-CASH-NOTES       PIC S9(7)V99 COMP-3.
           03  CA-PASSWORD             PIC X(4).
           03  CA-CHECK-DEPOSIT        PIC S9(7)V99 COMP-3.
           03  FILLER                  PIC X(8).
