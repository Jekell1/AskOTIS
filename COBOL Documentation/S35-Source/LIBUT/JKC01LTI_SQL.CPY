      * COPYMEMBER: LIBLP/LP01LTI_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER FILE - INSURANCE
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QLTI1-WHERE-BEG-KEY.
           03  QLTI1-WBEG-BRNO             PIC 9(04).
           03  QLTI1-WBEG-ACCTNO           PIC 9(08).
           03  QLTI1-WBEG-SEQNO            PIC 9(03).

       01  QLTI1-WHERE-END-KEY.
           03  QLTI1-WEND-BRNO             PIC 9(04).
           03  QLTI1-WEND-ACCTNO           PIC 9(08).
           03  QLTI1-WEND-SEQNO            PIC 9(03).

       01  QLTI-REC.
           03  QLTI-BRNO           PIC 9(4).
           03  QLTI-ACCTNO         PIC 9(8).
           03  QLTI-SEQNO          PIC 999.
           03  QLTI-LTOUCH-DATE    PIC X(10).
           03  QLTI-INS-TYPE           PIC 99.
           03  QLTI-ORIG-1STPYDATE     PIC X(10).
           03  QLTI-PRIOR-NODEF        PIC 9(3).
           03  QLTI-INSFEECD-1         PIC XX.
           03  QLTI-INSFEE-1           PIC S9(7)V99.
           03  QLTI-INSFEECD-2         PIC XX.
           03  QLTI-INSFEE-2           PIC S9(7)V99.
           03  QLTI-TRCD               PIC X(2).
           03  QLTI-INSCOMP            PIC 99.
           03  QLTI-INSOURS            PIC X.
           03  QLTI-INSEFF-DATE        PIC X(10).
           03  QLTI-INSEXP-DATE        PIC X(10).
           03  QLTI-INSPREM            PIC S9(7)V99.
           03  QLTI-INSCOVR            PIC S9(7)V99.
           03  QLTI-INSCOMM            PIC S9(7)V99.
           03  QLTI-ANTICTERM          PIC 9(3).
           03  QLTI-ANTICERN-1       PIC S9(7)V99.
           03  QLTI-ANTICADJ-1       PIC S9(7)V99.
           03  QLTI-UFERN-1          PIC S9(7)V99.
           03  QLTI-ACCERN-1         PIC S9(7)V99.
           03  QLTI-ANTICERN-2       PIC S9(7)V99.
           03  QLTI-ANTICADJ-2       PIC S9(7)V99.
           03  QLTI-UFERN-2          PIC S9(7)V99.
           03  QLTI-ACCERN-2         PIC S9(7)V99.
           03  QLTI-ANTICERN-3       PIC S9(7)V99.
           03  QLTI-ANTICADJ-3       PIC S9(7)V99.
           03  QLTI-UFERN-3          PIC S9(7)V99.
           03  QLTI-ACCERN-3         PIC S9(7)V99.
           03  QLTI-INT-REBATE         PIC X.
           03  QLTI-INS-PAYMNT-INCR    PIC S9(7)V99.
           03  QLTI-INS-TOT-ADDON      PIC S9(7)V99.
           03  QLTI-INT-LAST-EARNED    PIC S9(7)V99.
           03  QLTI-INS-REBATE         PIC X.
           03  QLTI-RB-WORKER          PIC S9(7)V99.
           03  QLTI-ANTIC-START-DATE   PIC X(10).
           03  QLTI-CANCEL-REASON-CD   PIC X(3).

       EXEC SQL END DECLARE SECTION END-EXEC.
