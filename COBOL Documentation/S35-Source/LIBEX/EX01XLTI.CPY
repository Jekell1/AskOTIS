      * COPYMEMBER: LIBEX/EX01XLTI
      **************************************************************************
      * "ADDON" INSURANCE TRAILER EXTRACTION FILE        
      * /USR/EXTRACT/ADDON
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *   CS  040907 ADD XLTI-REGION-NUMBER  [PR#76 REGACC]
      *   CS  180508 ADD BACK XLTI-CODE, NEED TO MATCH A15 OUTPUT
      *   CS  180509 ADD BACK XLTI-UNUSED TO MATCH A15 OUTPUT
      **************************************************************************
       78  XLTI-SIZE                    VALUE 371.
       01  XLTI-REC.
           03  XLTI-BRACCTNO            PIC 9(10).
           03  XLTI-ACCTNO              PIC 9(08).
           03  XLTI-CODE                PIC X.
           03  XLTI-SEQNO               PIC 9(03).
           03  XLTI-TYPE                PIC 9(02).
           03  XLTI-ORIG-1STPAY         PIC 9(08).
           03  XLTI-PRIOR-NODEF         PIC +9(02).
           03  XLTI-INSFEECD1           PIC X(02).
           03  XLTI-INSFEE1             PIC 9(04).99.
           03  XLTI-INSFEECD2           PIC X(02).
           03  XLTI-INSFEE2             PIC 9(04).99.
           03  XLTI-TRCD                PIC X(02).
           03  XLTI-INSCOMP             PIC 9(02).
           03  XLTI-INSOURS             PIC X(01).
           03  XLTI-INSEFF              PIC 9(08).
           03  XLTI-INSEXP              PIC 9(08).
           03  XLTI-INSPREM             PIC +9(04).99.
           03  XLTI-INSCOVR             PIC +9(06).99.
           03  XLTI-INSCOMM             PIC +9(04).99.
           03  XLTI-ANTICTERM           PIC 9(03).
      * 1 INSURANCE INTEREST (USED IN ACR PROGRAMS)
      * 2 NOT USED
      * 3XLTI-ANTICERN(3) HOLDS ORIGINAL ADDON FINCHG
           03  XLTI-ANTICERN1           PIC +9(06).99.
           03  XLTI-ANTICADJ1           PIC +9(06).99.
           03  XLTI-UFERN1              PIC +9(06).99.
           03  XLTI-ACCERN1             PIC +9(06).99.
           03  XLTI-ANTICERN2           PIC +9(06).99.
           03  XLTI-ANTICADJ2           PIC +9(06).99.
           03  XLTI-UFERN2              PIC +9(06).99.
           03  XLTI-ACCERN2             PIC +9(06).99.
           03  XLTI-ANTICERN3           PIC +9(06).99.
           03  XLTI-ANTICADJ3           PIC +9(06).99.
           03  XLTI-UFERN3              PIC +9(06).99.
           03  XLTI-ACCERN3             PIC +9(06).99.
           03  XLTI-INT-REBATE          PIC X(01).
           03  XLTI-INS-PAYMNT-INCR     PIC +9(05).99.
           03  XLTI-INS-TOT-ADDON       PIC +9(05).99.
           03  XLTI-INT-LAST-EARNED     PIC +9(05).99.
           03  XLTI-INS-REBATE          PIC X(01).
           03  XLTI-RB-WORKER           PIC +9(05).99.
           03  XLTI-ANTIC-START-DATE    PIC 9(08).
           03  XLTI-REGION-NUMBER       PIC 9(2).
           03  XLTI-UNUSED              PIC X(98).
           03  XLTI-CR                  PIC X(01).
           03  XLTI-LF                  PIC X(01).
