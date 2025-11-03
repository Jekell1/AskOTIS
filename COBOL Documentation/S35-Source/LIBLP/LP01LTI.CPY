      * COPYMEMBER: LIBLP/LP01LT
      ******************************************************************
      *    INSURANCE LOAN TRAILER FILE 
      *
      *     CREATED BY ADDON INSURANCE
      *
      *       STORES A RECORD FOR EACH TYPE OF ADDON INSURANCE
      *
      *    LTI1-KEY = LTI-BRNO, LTI-ACCTNO, LTI-SEQNO
      *
      *
      *  BAH 06/19/2015 CREATED NEW FROM LP01LT
      *  BAH 02/18/2019 REMOVED LTI-INSTRM
      *   CS 190507 ADD FUTURE FIELDS
      *  BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      *  BAH 20230600 ADDED FILLER
      **************************************************************************
       78  LTI-SIZE                      VALUE 370.
       01  LTI-REC.
           03  LTI1-KEY.
               05  LTI-BRNO           PIC 9(4).
               05  LTI-ACCTNO         PIC 9(8).
      * THIS MAY BE THE LP-LT-SEQNO FOR WHICH ADDON INSURANCE RECORD TO GET
               05  LTI-SEQNO          PIC 999.

      $XFD DATE=YYYYMMDD
           03  LTI-LTOUCH-DATE        PIC 9(8)     COMP-3.

           03  LTI-INS-TYPE           PIC 99.
               88  LTI-INS-TYPE-CL    VALUE 01.
               88  LTI-INS-TYPE-AH    VALUE 02.
               88  LTI-INS-TYPE-PP    VALUE 03.
               88  LTI-INS-TYPE-O1    VALUE 04.

      $XFD DATE=YYYYMMDD
           03  LTI-ORIG-1STPYDATE     PIC 9(8)      COMP-3.
           03  LTI-PRIOR-NODEF        PIC 9(3).

           03  LTI-FEES               OCCURS 2 TIMES.
               05  LTI-INSFEECD       PIC XX.
               05  LTI-INSFEE         PIC S9(7)V99   COMP-3.

           03  LTI-TRCD               PIC X(2).

           03  LTI-INSCOMP            PIC 99.
           03  LTI-INSOURS            PIC X.
      $XFD DATE=YYYYMMDD
           03  LTI-INSEFF-DATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LTI-INSEXP-DATE        PIC 9(8)      COMP-3.
           03  LTI-INSPREM            PIC S9(7)V99  COMP-3.
           03  LTI-INSCOVR            PIC S9(7)V99  COMP-3.
           03  LTI-INSCOMM            PIC S9(7)V99  COMP-3.

           03  LTI-ANTICTERM          PIC 9(3).
      * 1 INSURANCE INTEREST (USED IN ACCRUAL PROGRAMS)
      * 2 NOT USED
      * 3 LTI-ANTICERN(3) HOLDS ORIGINAL ADDON FINCHG
           03  LTI-ANTICTBL          OCCURS 3 TIMES.
               05  LTI-ANTICERN       PIC S9(7)V99  COMP-3.
               05  LTI-ANTICADJ       PIC S9(7)V99  COMP-3.
               05  LTI-UFERN          PIC S9(7)V99  COMP-3.
               05  LTI-ACCERN         PIC S9(7)V99  COMP-3.

           03  LTI-INT-REBATE         PIC X.
           03  LTI-INS-PAYMNT-INCR    PIC S9(7)V99  COMP-3.
           03  LTI-INS-TOT-ADDON      PIC S9(7)V99  COMP-3.
           03  LTI-INT-LAST-EARNED    PIC S9(7)V99  COMP-3.
           03  LTI-INS-REBATE         PIC X.
               88  LTI-INS-REBATE-VALID     VALUE " " "Y" "R".
           03  LTI-RB-WORKER          PIC S9(7)V99  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LTI-ANTIC-START-DATE   PIC 9(8)      COMP-3.
           03  LTI-CANCEL-REASON-CD   PIC X(3).

           03  FILLER                 PIC X(203).
