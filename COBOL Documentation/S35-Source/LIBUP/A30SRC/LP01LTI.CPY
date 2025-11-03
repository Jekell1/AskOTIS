      * COPYMEMBER: LIBLP/LP01LT
      ******************************************************************
      *    INSURANCE LOAN TRAILER FILE :  (400) BYTES
      *
      *     CREATED BY ADDON INSURANCE
      *
      *       STORES A RECORD FOR EACH TYPE OF ADDON INSURANCE
      *
      *
      *  BAH 06/19/2015 CREATED NEW FROM LP01LT
      **************************************************************************
       78  LTI-SIZE                      VALUE 400.
       01  LTI-REC.
           03  LTI1-KEY.
               05  LTI-BRNO              PIC 9(4).
               05  LTI-ACCTNO            PIC 9(8).
      * THIS MAY BE THE LP-LT-SEQNO FOR WHICH ADDON INSURANCE RECORD TO GET
               05  LTI-SEQNO             PIC 999.

      *>>XDD DATE=YYYYMMDD
           03  LTI-LTOUCH-DATE           PIC 9(8)     COMP-3.

           03  LTI-INSURANCE-AREA.
               05  LTI-INS-TYPE           PIC 99.
                   88  LTI-INS-TYPE-CL    VALUE 01.
                   88  LTI-INS-TYPE-AH    VALUE 02.
                   88  LTI-INS-TYPE-PP    VALUE 03.
                   88  LTI-INS-TYPE-O1    VALUE 04.
      *>>XDD DATE=YYYYMMDD
               05  LTI-ORIG-1STPYDATE     PIC 9(8)      COMP-3.
               05  LTI-PRIOR-NODEF        PIC 9(3).

               05  LTI-FEES               OCCURS 2 TIMES.
                   07  LTI-INSFEECD       PIC XX.
                   07  LTI-INSFEE         PIC S9(7)V99   COMP-3.

               05  LTI-TRCD               PIC X(2).
               05  LTI-INSTBL.
                   07  LTI-INSCOMP        PIC 99.
                   07  LTI-INSOURS        PIC X.
      *>>XDD DATE=YYYYMMDD
                   07  LTI-INSEFF-DATE    PIC 9(8)      COMP-3.
                   07  LTI-INSTRM REDEFINES LTI-INSEFF-DATE
                                          PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
                   07  LTI-INSEXP-DATE    PIC 9(8)      COMP-3.
                   07  LTI-INSPREM        PIC S9(7)V99  COMP-3.
                   07  LTI-INSCOVR        PIC S9(7)V99  COMP-3.
                   07  LTI-INSCOMM        PIC S9(7)V99  COMP-3.

               05  LTI-ANTICTERM          PIC 9(3).
      * 1 INSURANCE INTEREST (USED IN ACCRUAL PROGRAMS)
      * 2 NOT USED
      * 3 LTI-ANTICERN(3) HOLDS ORIGINAL ADDON FINCHG
               05  LTI-ANTICTBL           OCCURS 3 TIMES.
                   07  LTI-ANTICERN       PIC S9(7)V99  COMP-3.
                   07  LTI-ANTICADJ       PIC S9(7)V99  COMP-3.
                   07  LTI-UFERN          PIC S9(7)V99  COMP-3.
                   07  LTI-ACCERN         PIC S9(7)V99  COMP-3.
               05  LTI-INT-REBATE         PIC X.
               05  LTI-INS-PAYMNT-INCR    PIC S9(7)V99  COMP-3.
               05  LTI-INS-TOT-ADDON      PIC S9(7)V99  COMP-3.
               05  LTI-INT-LAST-EARNED    PIC S9(7)V99  COMP-3.
               05  LTI-INS-REBATE         PIC X.
382                88  LTI-INS-REBATE-VALID   VALUE " " "Y" "R".
               05  LTI-RB-WORKER          PIC S9(7)V99  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LTI-ANTIC-START-DATE   PIC 9(8)      COMP-3.
               05  LTI-CANCEL-REASON-CD   PIC X(3).

               05  FILLER                PIC X(233).
