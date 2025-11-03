      * COPYMEMBER: LIBEX/EX01XLTL
      **************************************************************************
      * LIENHOLDER ADDITIONAL INFO FILE      
      *  LT FILE CODE "L", P2 COLLATERAL
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *  040414 BAH ADDED SEQ 5 AND 6 FOR PAYOFF TITLE PROCESSING REGACC PR# 53
      *  080227 BAH ADDED XLTL-TITLE-RCV-DATE, XLTL-TITLE-LIEN-DATE,
      *             XLTL-TITLE-NO, XLTL-LIEN-LETTER-SENT, XLTL-SEND-ASAP
      *  100820 BAH ADDED XLTL-TITLE-5-ADR2 AND 23 MORE TO FILLER, REGACC PR#270
      *  120925 BAH ADDED XLTL-LIEN-4-STATUS-CODE, REGACC PR# 321
      *  121001 BAH ADDED XLTL-CARRIER-TYPE,XLTL-CARRIER-ACCTNO, REGACC PR# 322
      *  160202 BAH REMOVED XLTL-CODE XLTL-TYPE AND UNUSED
      *  180108 BAH PUT BACK XLTL-CODE XLTL-TYPE AND UNUSED
      *  180509 CS  PUT BACK XLTL-UNUSED TO MATCH A15
      **************************************************************************
       78  XLTL-SIZE                    VALUE 700.
       01  XLTL-REC.
           03  XLTL-BRACCTNO            PIC 9(10).
           03  XLTL-ACCTNO              PIC 9(08).
           03  XLTL-CODE                PIC X.
           03  XLTL-SEQNO               PIC 9(03).
           03  XLTL-TYPE                PIC 9(02).

           03  XLTL-LIEN-1-TYPE        PIC 9.
           03  XLTL-LIEN-1-NAME        PIC X(30).
           03  XLTL-LIEN-1-ADR         PIC X(24).
           03  XLTL-LIEN-1-CITY        PIC X(15).
           03  XLTL-LIEN-1-STATE       PIC X(2).
           03  XLTL-LIEN-1-ZIP         PIC X(10).
           03  XLTL-LIEN-1-DATE        PIC 9(8).

           03  XLTL-LIEN-2-TYPE        PIC 9.
           03  XLTL-LIEN-2-NAME        PIC X(30).
           03  XLTL-LIEN-2-ADR         PIC X(24).
           03  XLTL-LIEN-2-CITY        PIC X(15).
           03  XLTL-LIEN-2-STATE       PIC X(2).
           03  XLTL-LIEN-2-ZIP         PIC X(10).
           03  XLTL-LIEN-2-DATE        PIC 9(8).

           03  XLTL-LIEN-3-TYPE        PIC 9.
           03  XLTL-LIEN-3-NAME        PIC X(30).
           03  XLTL-LIEN-3-ADR         PIC X(24).
           03  XLTL-LIEN-3-CITY        PIC X(15).
           03  XLTL-LIEN-3-STATE       PIC X(2).
           03  XLTL-LIEN-3-ZIP         PIC X(10).
           03  XLTL-LIEN-3-DATE        PIC 9(8).

           03  XLTL-LIEN-4-COMMENT1     PIC X(60).
           03  XLTL-LIEN-4-COMMENT2     PIC X(60).

           03  XLTL-TITLE-5-SEND-FLAG   PIC X.
           03  XLTL-TITLE-5-NAME        PIC X(25).
           03  XLTL-TITLE-5-ADR         PIC X(25).
           03  XLTL-TITLE-5-CITY        PIC X(15).
           03  XLTL-TITLE-5-STATE       PIC X(2).
           03  XLTL-TITLE-5-ZIP         PIC X(10).
           03  XLTL-TITLE-5-DATE        PIC 9(6).
           03  XLTL-TITLE-5-SENT-BY     PIC X(25).

           03  XLTL-TITLE-6-COMMENT1     PIC X(40).
           03  XLTL-TITLE-6-COMMENT2     PIC X(40).

           03  XLTL-REGION-NUMBER       PIC 9(2).

           03  XLTL-TITLE-RCV-DATE      PIC 9(8).
           03  XLTL-TITLE-LIEN-DATE     PIC 9(8).
           03  XLTL-TITLE-NO            PIC X(20).
           03  XLTL-LIEN-LETTER-SENT    PIC X.
           03  XLTL-SEND-ASAP           PIC X.

           03  XLTL-TITLE-5-ADR2        PIC X(22).

           03  XLTL-LIEN-4-TITLE-STATE   PIC X(2).
           03  XLTL-TITLE-6-RELEASE-TYPE PIC X(2).
           03  XLTL-LIEN-4-STATUS-CODE   PIC X(2).
           03  XLTL-CARRIER-TYPE         PIC X(4).
           03  XLTL-CARRIER-ACCTNO       PIC X(20).
           03  XLTL-UNUSED               PIC X(3).
           03  XLTL-CR                  PIC X.
           03  XLTL-LF                  PIC X.
