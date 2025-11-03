      * COPYMEMBER: LIBLP/LP01LSM_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN STATUS -  MISC.
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      * BAH 20220923 ADDED QLSM-MISPAY-90-DATE VERITEC "IL" #1570 
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LSM1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LSM1-CURSOR-STAT-GOOD             VALUE "00".
           88  LSM1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LSM1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLSM1-WHERE-BEG-KEY.
           03  QLSM1-WBEG-BRNO             PIC 9(04).
           03  QLSM1-WBEG-ACCTNO           PIC 9(08).
           03  QLSM1-WBEG-SEQNO            PIC 9(02).

       01  QLSM1-WHERE-END-KEY.
           03  QLSM1-WEND-BRNO             PIC 9(04).
           03  QLSM1-WEND-ACCTNO           PIC 9(08).
           03  QLSM1-WEND-SEQNO            PIC 9(02).

       01  QLSM-REC.
           03  QLSM-BRNO               PIC 9(4).
           03  QLSM-ACCTNO             PIC 9(8).
           03  QLSM-SEQNO              PIC 99.
           03  QLSM-LTOUCH-DATE        PIC X(10).
           03  QLSM-MISC-FIELD-1       PIC X(20).
           03  QLSM-MISC-FIELD-2       PIC X(20).
           03  QLSM-MISC-FIELD-3       PIC X(20).
           03  QLSM-MISC-FIELD-4       PIC X(20).
           03  QLSM-MISC-FIELD-5       PIC X(20).
           03  QLSM-MISC-FIELD-6       PIC X(20).
           03  QLSM-MISC-FIELD-7       PIC X(20).
           03  QLSM-MISC-FIELD-8       PIC X(20).
           03  QLSM-MISC-FIELD-9       PIC X(20).
           03  QLSM-MISC-FIELD-10      PIC X(20).
           03  QLSM-MISC-FIELD-11       PIC X(20).
           03  QLSM-MISC-FIELD-12       PIC X(20).
           03  QLSM-MISC-FIELD-13       PIC X(20).
           03  QLSM-MISC-FIELD-14       PIC X(20).
           03  QLSM-MISC-FIELD-15       PIC X(20).
           03  QLSM-MISC-FIELD-16       PIC X(20).
           03  QLSM-MISC-FIELD-17       PIC X(20).
           03  QLSM-MISC-FIELD-18       PIC X(20).
           03  QLSM-MISC-FIELD-19       PIC X(20).
           03  QLSM-MISC-FIELD-20       PIC X(20).
           03  QLSM-MISC-FIELD-21       PIC X(20).
           03  QLSM-MISC-FIELD-22       PIC X(20).
           03  QLSM-MISC-FIELD-23       PIC X(20).
           03  QLSM-MISC-FIELD-24       PIC X(20).
           03  QLSM-MISC-FIELD-25       PIC X(20).
           03  QLSM-MISC-FIELD-26       PIC X(20).
           03  QLSM-MISC-FIELD-27       PIC X(20).
           03  QLSM-MISC-FIELD-28       PIC X(20).
           03  QLSM-MISC-FIELD-29       PIC X(20).
           03  QLSM-MISC-FIELD-30       PIC X(20).
           03  QLSM-MISC-FIELD-31       PIC X(20).
           03  QLSM-MISC-FIELD-32       PIC X(20).
           03  QLSM-TRANS-FROM-BRNO     PIC 9(04).
           03  QLSM-TRANS-FROM-ACCTNO   PIC 9(08).
           03  QLSM-TRANS-FROM-DATE     PIC X(10).
           03  QLSM-MISPAY-90-DATE      PIC X(10).
           03  QLSM-SECOND-BEN-NAME     PIC X(32).
           03  QLSM-SECOND-BEN-ADR      PIC X(32).
           03  QLSM-SECOND-BEN-CITY     PIC X(16).
           03  QLSM-SECOND-BEN-STATE    PIC X(2).
           03  QLSM-SECOND-BEN-ZIP      PIC X(10).
           03  QLSM-SECOND-BEN-PHONE    PIC 9(10).
           03  QLSM-SECOND-BEN-RELATION PIC X(15).

       EXEC SQL END DECLARE SECTION END-EXEC.
