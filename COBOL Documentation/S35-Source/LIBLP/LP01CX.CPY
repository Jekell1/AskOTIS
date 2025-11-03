      * COPYMEMBER: LIBLP/LP01CX
      ******************************************************************
      *
      * DESCRIPTION: CHECK TEMPORARY FILE (LOAN PROCESSING)
      *
      * CX1-KEY: CX-BRNO, CX-TYPE, CX-SEQNO
      * CX2-KEY: NON CONTIGUOUS; CX-BRNO, CX-PAYEE
      *
      *=================================================================
      * REV:
      * BAH 112596 ADDED CX-RESCISSION, ADVANCE MTG (#401)
      * JTG 112097 EXPANDED BY 30 BYTES
      * BAH 062598 ADDED CX-TILLNO SO IT DOESNT USE CX-ACCTNO
      *             ANYMORE, TILLNO IS NOW ALPHA..
      * KEC 1999.0510 <<<<<<<< 'A60-TO-A15' CONVERSION UPDATED >>>>>>>>
      * BLF 050323 ADDED CX-CK-PAID-NEW TO TRACK TYPE OF
      *         REPLACEMENT CHECK
      * ??? 060608 ADDED CX-EVENT-CD  PR#2503 SECURIT
      * BLF 060712 ADDED CX-PAYEE-NO, DEVELOPMENT PR# 2633
      * BAH 061107 ADDED 88 CX-UPDATE-RESCISSION TO IDENTIFY THAT THE
      *            LOAN WAS UPDATED OUT OF RESCISSION AND THESE CHECKS
      *            NEED TO BE PRINTED BEFORE THE DAY CLOSES.
      *            REGWAN PL# 556
      * BAH 160315 CHANGED DATES TO 9(8), TILLNO TO 99, CLASS TO 9(3),
      *            ADDED FILLER, PD0003
      * BAH 180124 CHANGED FILLER FROM 137 TO 87, SIZE FROM 615 TO 565
      * BAH 181009 GLOBAL CXFILE
      *  CS 190422 ADD FUTURE FIELDS.
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210427 REMOVED FUTURE FIELDS
      * BAH 20230600 ADDED FILLER
      ******************************************************************
       78  CX-SIZE                 VALUE 560.
       01  CX-REC.
           03  CX1-KEY.
               05  CX-BRNO             PIC 9(4).
               05  CX-TYPE             PIC X(2).
               05  CX-SEQNO            PIC 9(4).   
           03  CX-PAYEE                PIC X(60).
      $XFD DATE=YYYYMMDD
           03  CX-LTOUCH-DATE          PIC 9(8)     COMP-3.
           03  CX-ADR1                 PIC X(30).
           03  CX-ADR2                 PIC X(30).
           03  CX-CITY                 PIC X(30).
           03  CX-STATE                PIC X(2).
           03  CX-ZIP                  PIC X(10).
           03  CX-AMT                  PIC S9(7)V99 COMP-3.
           03  CX-REF                  PIC 9(6)     COMP-3.
           03  CX-REASON               PIC X(30).
           03  CX-GLDIST           OCCURS 5.
               05  CX-GLNO             PIC 9(11)    COMP-3.
               05  CX-GLAMT            PIC S9(7)V99 COMP-3.
           03  CX-FREE1                PIC X(70).
           03  CX-FREE2                PIC X(70).
           03  CX-VOID                 PIC X.
           03  CX-CLASS                PIC 9(3).
           03  CX-CKNO                 PIC 9(7)     COMP-3.
           03  CX-DLNO                 PIC 9(4)V99  COMP-3.
      $XFD DATE=YYYYMMDD
           03  CX-DATE                 PIC 9(8)     COMP-3.
           03  CX-PAID                 PIC X.
           03  CX-PGM                  PIC XX.
           03  CX-BATCHID              PIC 9(10)    COMP-3.
           03  CX-BWPROC               PIC X.
           03  CX-CKNO-NEW             PIC 9(7)     COMP-3.
           03  CX-DLRVNO               PIC 9.
           03  CX-DT-SEQ               PIC 9(4)     COMP-3.
           03  CX-RESCISSION           PIC X.
               88  CX-RESCISSION-VALID        VALUE " " "Y".
               88  CX-IN-RESCISSION           VALUE "Y".
      * THIS IS WHEN THE CHECK WAS RELEASED FROM RESCISSION AND IS
      * READY TO PRINT. WE NEEDED TO IDENTIFY THEM IN LONPL0 FOR CLOSING
               88  CX-UPDATE-RESCISSION       VALUE "U".

           03  CX-TILLNO               PIC 99.
           03  CX-ACCTNO               PIC 9(8).
           03  CX-SERVACCT             PIC X.
      $XFD DATE=YYYYMMDD
           03  CX-DT-DATE              PIC 9(8)     COMP-3.
           03  CX-SSNO                 PIC 9(9)     COMP-3.
           03  CX-DT-PAID              PIC X.
           03  CX-DT-UNPAID            PIC X.
           03  CX-CKPAID-NEW           PIC X.
           03  CX-EVENT-CD             PIC X(2).
           03  CX-PAYEE-NO             PIC 9(6)     COMP-3.
           03  CX-DEPOSIT-WITHDR       PIC X.
               88  CX-BANK-DEPOSIT              VALUE "D".
               88  CX-BANK-WITHDRAW             VALUE "W".

           03  FILLER                   PIC X(82).
