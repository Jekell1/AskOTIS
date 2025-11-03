      * COPYMEMBER: LIBLP/LP01LTL
      ******************************************************************
      *    LOAN LIENHOLDER TRAILER FILE : (900) BYTES
      *      F9-COLLATERAL - PAGE 2
      *
      *---------------------------------------------------------------
      *
      * BAH 06/19/2015 CREATED NEW FROM LP01LT
      **************************************************************************
       78  LTL-SIZE                      VALUE 900.
       01  LTL-REC.
           03  LTL1-KEY.
               05  LTL-BRNO              PIC 9(4).
               05  LTL-ACCTNO            PIC 9(8).
               05  LTL-SEQNO             PIC 999.

      *>>XDD DATE=YYYYMMDD
           03  LTL-LTOUCH-DATE           PIC 9(8)     COMP-3.

           03  LTL-LIEN-AREA.
      *---------------------------------------------------------------
      *    LIENHOLDER INFO RECORD "L":
      *---------------------------------------------------------------
      * 3 X 87 = 261
               05  LTL-LIEN               OCCURS 3.
                   07  LTL-LIEN-TYPE        PIC 9.
                   07  LTL-LIEN-NAME        PIC X(30).
                   07  LTL-LIEN-ADR         PIC X(24).
                   07  LTL-LIEN-CITY        PIC X(15).
                   07  LTL-LIEN-STATE       PIC X(2).
                   07  LTL-LIEN-ZIP         PIC X(10).
      *>>XDD DATE=YYYYMMDD
                   07  LTL-LIEN-DATE        PIC 9(8)      COMP-3.

           03  LTL-LIEN-AREA-REST.
      *>>XDD DATE=YYYYMMDD
               05  LTL-TITLE-RCV-DATE       PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LTL-TITLE-LIEN-DATE      PIC 9(8)      COMP-3.
               05  LTL-TITLE-NO             PIC X(20).
               05  LTL-LIEN-LETTER-SENT     PIC X.

               05  LTL-LIEN-COMMENT1    PIC X(60).
               05  LTL-LIEN-COMMENT2    PIC X(60).
               05  LTL-LIEN-TITLE-STATE  PIC XX.
               05  LTL-TITLE-STATUS-CODE PIC XX.
               05  LTL-TITLE-SEND-FLAG   PIC X.
               05  LTL-TITLE-NAME        PIC X(25).
               05  LTL-TITLE-ADR         PIC X(25).
               05  LTL-TITLE-CITY        PIC X(15).
               05  LTL-TITLE-STATE       PIC X(2).
               05  LTL-TITLE-ZIP         PIC X(10).
      *>>XDD DATE=YYYYMMDD
               05  LTL-TITLE-DATE        PIC 9(8)     COMP-3.
               05  LTL-TITLE-SENT-BY     PIC X(25).
               05  LTL-SEND-ASAP         PIC X.
               05  LTL-TITLE-SEND-FMNO   PIC 9(4).
               05  LTL-TITLE-ADR2        PIC X(22).

               05  LTL-TITLE-COMMENT1    PIC X(40).
               05  LTL-TITLE-COMMENT2    PIC X(40).

               05  LTL-RELEASE-TYPE      PIC XX.
               05  LTL-CARRIER-TYPE      PIC X(4).
               05  LTL-CARRIER-ACCTNO    PIC X(20).

               05  FILLER                PIC X(223).
