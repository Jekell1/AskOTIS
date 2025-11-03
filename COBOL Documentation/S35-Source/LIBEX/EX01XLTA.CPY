      * COPYMEMBER: LIBEX/EX01XLTA
      **************************************************************************
      * AUTO INSURANCE TRAILER EXTRACTION FILE       
      * /USR/EXTRACT/AUTO_INS
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *    040903 ADD XLTA-REGION-NUMBER [PR#76 REGACC]
      *    070504 ADDED XLTA-TRACK-ONLY, XLTA-INSURANCE, XLTA-TRACK-MONTHS,
      *           XLTA-INS-MONTHS, XLTA-CPP-MONTHS, REGACC PR# 173
      *    160202 REMOVED XLTA-CODE, XLTA-TYPE AND UNUSED PD0003
      *    180108 PUT BACK XLTA-CODE, XLTA-TYPE AND UNUSED PD0003
      *    180509 PUT BACK XLTA-UNUSED, NEED TO MATCH A15 OUTPUT
      **************************************************************************
       78  XLTA-SIZE                    VALUE 442.
       01  XLTA-REC.
           03  XLTA-BRACCTNO            PIC 9(10).
           03  XLTA-ACCTNO              PIC 9(08).
           03  XLTA-CODE                PIC X.
           03  XLTA-SEQNO               PIC 9(03).
           03  XLTA-TYPE                PIC 9(02).
           03  XLTA-DLR-REQ             PIC X(01).
           03  XLTA-INS-TYPE            PIC X(01).
           03  XLTA-INS-CO              PIC X(25).
           03  XLTA-POLICY-NO           PIC X(25).
           03  XLTA-AGENCY              PIC X(25).
           03  XLTA-AGENT               PIC X(25).
           03  XLTA-ADR1                PIC X(25).
           03  XLTA-ADR2                PIC X(25).
           03  XLTA-CITY                PIC X(15).
           03  XLTA-STATE               PIC X(02).
           03  XLTA-ZIP                 PIC X(10).
           03  XLTA-PHONE               PIC 9(10).
           03  XLTA-EFF-DATE            PIC 9(08).
           03  XLTA-EXP-DATE            PIC 9(08).
           03  XLTA-CAN-DATE            PIC 9(08).
           03  XLTA-CONF-DATE           PIC 9(08).
           03  XLTA-LAST-FORM           PIC 9(03).
           03  XLTA-COV-TYPE            PIC X(01).
           03  XLTA-BIL-COV             PIC +9(07).99.
           03  XLTA-PDL-COV             PIC +9(07).99.
           03  XLTA-PIP-COV             PIC +9(07).99.
           03  XLTA-UMB-COV             PIC +9(07).99.
           03  XLTA-COL-COV             PIC +9(07).99.
           03  XLTA-LOSS-PAYEE          PIC X(25).
           03  XLTA-COM-COV             PIC +9(07).99.
           03  XLTA-REGION-NUMBER       PIC 9(2).
           03  XLTA-TRACK-ONLY          PIC X.
           03  XLTA-INSURANCE           PIC X.
           03  XLTA-TRACK-MONTHS        PIC X(12).
           03  XLTA-INS-MONTHS          PIC X(12).
           03  XLTA-CPP-MONTHS          PIC X(12).
           03  XLTA-UNUSED              PIC X(60).
           03  XLTA-CR                  PIC X(01).
           03  XLTA-LF                  PIC X(01).
