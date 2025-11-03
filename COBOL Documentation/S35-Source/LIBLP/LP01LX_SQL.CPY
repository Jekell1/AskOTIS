      * COPYMEMBER: LIBLP/LP01LX_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN PAYMENT TRAILER FILE - STANDARD
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LX1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LX1-CURSOR-STAT-GOOD              VALUE "00".
           88  LX1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LX1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLX1-WHERE-BEG-KEY.
           03  QLX1-WBEG-BRNO             PIC 9(04).
           03  QLX1-WBEG-ACCTNO           PIC 9(08).
           03  QLX1-WBEG-SEQNO            PIC 9(05).

       01  QLX1-WHERE-END-KEY.
           03  QLX1-WEND-BRNO             PIC 9(04).
           03  QLX1-WEND-ACCTNO           PIC 9(08).
           03  QLX1-WEND-SEQNO            PIC 9(05).

       01  QLX-REC.
           03  QLX-BRNO             PIC 9(4).
           03  QLX-ACCTNO           PIC 9(8).
           03  QLX-SEQNO            PIC 9(5).
           03  QLX-LTOUCH-DATE      PIC X(10).
           03  QLXT-CODE            PIC X.
           03  QLXT-EARN-1          PIC S9(7)V99.
           03  QLXT-EARN-2          PIC S9(7)V99.
           03  QLXT-EARN-3          PIC S9(7)V99.
           03  QLXT-EARN-4          PIC S9(7)V99.
           03  QLXT-EARN-5          PIC S9(7)V99.
           03  QLXT-EARN-6          PIC S9(7)V99.
           03  QLXT-EARN-7          PIC S9(7)V99.
           03  QLXT-EARN-8          PIC S9(7)V99.
           03  QLXT-EARN-9          PIC S9(7)V99.
           03  QLXT-EARN-10         PIC S9(7)V99.
           03  QLXT-EARN-11          PIC S9(7)V99.
           03  QLXT-EARN-12          PIC S9(7)V99.
           03  QLXT-EARN-13          PIC S9(7)V99.
           03  QLXT-EARN-14          PIC S9(7)V99.
           03  QLXT-EARN-15          PIC S9(7)V99.
           03  QLXT-EARN-16          PIC S9(7)V99.
           03  QLXT-EARN-17          PIC S9(7)V99.
           03  QLXT-EARN-18          PIC S9(7)V99.
           03  QLXT-EARN-19          PIC S9(7)V99.
           03  QLXT-EARN-20          PIC S9(7)V99.
           03  QLXP-CODE            PIC X.
           03  QLXP-EARN-1          PIC S9(7)V99.
           03  QLXP-EARN-2          PIC S9(7)V99.
           03  QLXP-EARN-3          PIC S9(7)V99.
           03  QLXP-EARN-4          PIC S9(7)V99.
           03  QLXP-EARN-5          PIC S9(7)V99.
           03  QLXP-EARN-6          PIC S9(7)V99.
           03  QLXP-EARN-7          PIC S9(7)V99.
           03  QLXP-EARN-8          PIC S9(7)V99.
           03  QLXP-EARN-9          PIC S9(7)V99.
           03  QLXP-EARN-10         PIC S9(7)V99.
           03  QLXP-EARN-11          PIC S9(7)V99.
           03  QLXP-EARN-12          PIC S9(7)V99.
           03  QLXP-EARN-13          PIC S9(7)V99.
           03  QLXP-EARN-14          PIC S9(7)V99.
           03  QLXP-EARN-15          PIC S9(7)V99.
           03  QLXP-EARN-16          PIC S9(7)V99.
           03  QLXP-EARN-17          PIC S9(7)V99.
           03  QLXP-EARN-18          PIC S9(7)V99.
           03  QLXP-EARN-19          PIC S9(7)V99.
           03  QLXP-EARN-20          PIC S9(7)V99.
           03  QLXA-CODE            PIC X.
           03  QLXA-SRCD                PIC XX.
           03  QLXA-START-PMT-1       PIC 99999.
           03  QLXA-END-PMT-1         PIC 99999.
           03  QLXA-NET-CHANGE-1      PIC S9(7)V99.
           03  QLXA-START-PMT-2       PIC 99999.
           03  QLXA-END-PMT-2         PIC 99999.
           03  QLXA-NET-CHANGE-2      PIC S9(7)V99.
           03  QLXA-START-PMT-3       PIC 99999.
           03  QLXA-END-PMT-3         PIC 99999.
           03  QLXA-NET-CHANGE-3      PIC S9(7)V99.
           03  QLXA-START-PMT-4       PIC 99999.
           03  QLXA-END-PMT-4         PIC 99999.
           03  QLXA-NET-CHANGE-4      PIC S9(7)V99.
           03  QLXA-START-PMT-5       PIC 99999.
           03  QLXA-END-PMT-5         PIC 99999.
           03  QLXA-NET-CHANGE-5      PIC S9(7)V99.
           03  QLXA-START-PMT-6       PIC 99999.
           03  QLXA-END-PMT-6         PIC 99999.
           03  QLXA-NET-CHANGE-6      PIC S9(7)V99.
           03  QLXA-START-PMT-7       PIC 99999.
           03  QLXA-END-PMT-7         PIC 99999.
           03  QLXA-NET-CHANGE-7      PIC S9(7)V99.
           03  QLXA-START-PMT-8       PIC 99999.
           03  QLXA-END-PMT-8         PIC 99999.
           03  QLXA-NET-CHANGE-8      PIC S9(7)V99.
           03  QLXA-START-PMT-9       PIC 99999.
           03  QLXA-END-PMT-9         PIC 99999.
           03  QLXA-NET-CHANGE-9      PIC S9(7)V99.
           03  QLXA-LN-INSCOMP        PIC 99.
           03  QLXA-LN-INSOURS        PIC X.
           03  QLXA-LN-INSEFF-DATE    PIC X(10).
           03  QLXA-LN-INSEXP-DATE    PIC X(10).
           03  QLXA-LN-INSPREM        PIC S9(7)V99.
           03  QLXA-LN-INSCOVR        PIC S9(7)V99.
           03  QLXA-LN-INSCOMM        PIC S9(7)V99.
           03  QLXA-LN-INS-REBATE     PIC X.
           03  QLXA-LN-INSCODES       PIC X(3).
           03  QLXZ-CODE             PIC X.
           03  QLXZ-POST-REGPY       PIC S9(7)V99.
           03  QLXZ-POST-EXTRA-PRIN  PIC S9(7)V99.
           03  QLXZ-POST-OTH         PIC S9(7)V99.
           03  QLXZ-POST-LCHG        PIC S9(7)V99.
           03  QLXZ-PDUE-APINT       PIC S9(7)V99.
           03  QLXZ-PDUE-APPRIN      PIC S9(7)V99.
           03  QLXZ-CUR-APINT        PIC S9(7)V99.
           03  QLXZ-CUR-APPRIN       PIC S9(7)V99.
           03  QLXR-CODE             PIC X.
           03  QLXR-REB-AMT          PIC S9(7)V99.
           03  QLXX-CODE               PIC X.
           03  QLXX-INS-TAX-COUNTY-CD  PIC X(3).
           03  QLXX-INS-TAX-CITY-CD    PIC X(3).
           03  QLXM-CODE               PIC X.
           03  QLXM-MIP-CLPAID         PIC S9(7)V99.
           03  QLXM-MIP-CLPDTH-DATE    PIC X(10).
           03  QLXM-MIP-AHPAID         PIC S9(7)V99.
           03  QLXM-MIP-AHPDTH-DATE    PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
