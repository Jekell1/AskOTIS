      * COPYMEMBER: LIBLP/LPLOANW
      ******************************************************************
      *            (LPLOANW)                   SIZE = 356
      *     C O M M O N  LONP  M E M O R Y
      *
      *=================================================================
      * REV:
      *  JTG 021106 BATCH PAYMENT REVISIONS ADDED BATCH-REFCD       REGACC #0006
      *  JTG 021114 BATCH PAYMENT REVISIONS ADDED BATCH-REFCD-LC-FG REGACC #0006
      *  JTG 021121 BATCH PAYMENT REVISIONS ADDED BATCH-REFCD-FILE-TYPE
      *                                                             REGACC #0006
      *  BLM 120725 CHANGED FILLER UNDER USERID TO USER-LETTER1, REGACC PR# 316
      *  BAH 130417 ADDED PRLOAN-RESTRICTED FOR LONPC0 TO PASS LONPC2
      *  BAH 130418 ADDED GN-GIFTNET-INFO
      *
      * KEC 2016.0427 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *          IN A15, LOAN APP NUMBER (MA-APPNO) WAS STORED IN
      *           LOAN-KEY.
      *          IN A30, LOAN APP NUMBER (MA-APP-NUMBER) IS STORED IN
      *           LOAN-KEY BUT WAS INCREASED TO 9 BYTES ALONG WITH
      *           ADDING REDEFINES FOR APPLICATION-KEY.  COULD NOT
      *           USE BORROWER-KEY SINCE PROGRAMS WERE ALREADY USING
      *           LOAN-KEY FOR LOAN APPLICATIONS.
      * BAH 160818 REDEFINED LOAN-KEY FOR CHECK WRITING, USED TO USE THE
      *            LN-SEQNO TO DETERMINE TYPE OF CHECK AND TO FIND IT IN 
      *            THE FM-FILE AND CX-FILE BY USING LN-ACCTNO/LN-SEQNO, SO
      *            NOW IT WILL USE LOAN-KEY REDEF.
      * BLM 170213 CHANGE TRANS-DATE TO BE CCYYMMDD, PD0006
      * KEC 180823 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 2024.0409 REMOVED GN-GIFTNET-INFO 
      ******************************************************************
       01  PROG-BUF.
           03  ERRCD                      PIC X(1).
           03  NEW-PROG-FORM.
               05  NEW-PROG.
                   07  FILLER             PIC X.
                   07  NEW-PROG-SUB       PIC X.
               05  NEW-FORM               PIC X(1).
           03  OLD-PROG-FORM.
               05  OLD-PROG               PIC X(2).
               05  OLD-FORM               PIC X(1).
           03  ROUTE-CD                   PIC X(1).

           03  TRANS-DATE                 PIC 9(8).
           03  FILLER                     REDEFINES TRANS-DATE.
               05  TRANS-DATE-YYYYMM      PIC 9(6).
               05  FILLER REDEFINES TRANS-DATE-YYYYMM.
                   07  TRANS-CC           PIC 99.
                   07  TRANS-YEAR         PIC 99.
                   07  TRANS-MONTH        PIC 99.
               05  TRANS-DAY              PIC 99.

           03  BRANCH                     PIC 9(4).

           03  LOAN-KEY                   PIC 9(9).
           03  LOAN-KEYX               REDEFINES LOAN-KEY.
               05  LOAN-KEY-SEQNO         PIC 9(7).
               05  LOAN-KEY-TYPE          PIC 99.

           03  BORROWER-KEY               PIC 9(9).
           03  LPLOAN-CONAME              PIC X(20).
           03  LPLOAN-SYSDATE             PIC X(8).
           03  LPLOAN-USERID.
               05  LPLOAN-USER-LETTER1    PIC X.
               05  LPLOAN-USER-BRANCH     PIC 9(4).
               05  LPLOAN-USER-NAME       PIC X(5).
           03  USER-PID                   PIC X(7).
           03  BATCH                      PIC 999.
           03  TRANS-CODES                PIC 99.
           03  NEW-SUB-MENU-FORM          PIC XXX.
           03  BATCH-REFCD                PIC X(5).
           03  BATCH-REFCD-LC-FG          PIC X.
           03  BATCH-REFCD-FILE-TYPE      PIC X(2).
           03  PRLOAN-RESTRICTED          PIC X.

           03  ADDON-LN-ACCTNO         PIC 9(8) COMP.
           03  LPLOAN-CALLED-FROM      PIC X(6).
      * ADDED THIS FOR LNCREA TO CALL LONPB2
           03  LPLOAN-RETURN-ERR-BUF   PIC X(55).

           03  FILLER                  PIC X(50).
