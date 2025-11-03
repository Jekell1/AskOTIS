      * COPYMEMBER: LIBWI/CDSCANW
      ******************************************************************
      * REV:
      * MJD 071497 ADDED CDSCAN-CL-KEY FOR CLFILE (CAN LOANS)
      * JTG 081397 ADDED CDSCAN-FEE-GIFTNET-ISO WORLD A90
      * KEC 092198 ADDED CDSCAN-PR-KEY
      * JTG 120798 ADDED CDSCAN-RO-KEY
      * MJD 990303 ADDED CDSCAN-BS-KEY
      * JTG 990729 ADDED CD-RO-NUM TO PROVIDE AN ALPHA KEY RE: CD-RO-CODE
      * BLV 120600 ADDED CD-EA-GQ-FG TO SCAN FOR GLOBAL OR LOCAL
      *            QUEUING GROUPS, TFC PR# 60
      * BLV 010924 ADDED CD-FU-KEY, PR# 1487
      * JTG 021011 ADDED CDSCAN-BR-KEY
      *            ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      *  JTG 051006 ADDED LN-POCD-UNWIND "*U" A LN-POCD-NONLOAN
      *                   AND LN-UNWIND-REASON-CODE                 REGACC #0105
      * BLF 060908 ADDED CD-PA-KEY, WORLD PR# 488
      * MJD 070816 ADDED CD-ID-KEY, REGENCY PR# 3032
      * BAH 110201 ADDED CD-JT-KEY, JOB TYPE, WORLD PR# 239
      * BAH 110411 ADDED CD-PS-KEY, PAYMENT RESCHEDULE REASON, REGACC PR# 290
      * BLM 121203 ADDED CD-DR-KEY, NOTES DORMANT REASON, REGWANT PR# 4150
      * BLM 130710 CHANGE CDSCAN-UW-CODE FROM 9(2) TO X(2)
      * KEC 2016.0309 ADDED CDSCAN-LO-KEY
      * KEC 2016.0311 NO LONGER USING HASH ON CD-RC-CODE BUT STILL
      *               USING CDSCAN-RC-CODE OF X(6) AND USING
      *               CDSCAN-RC-CODE-ALPHA OF X(8) TO MATCH CD-RC-CODE.
      * KEC 2016.0323 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      * BAH 2019.0122 SPLIT OUT CDFILE AND CDBFILE, CDBFILE CONTAINS "GC"
      *               AND "CN" AND IS IN CDBSCANW.CPY
      *  2024.0503 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  CDSCAN-BUF.
           03  CDSCAN-BEGIN-X              PIC 9(02)  VALUE 37.
           03  CDSCAN-BEGIN-Y              PIC 9(02)  VALUE 6.

           03  CDSCAN-WINDOW-BUF.
      *  CDSCAN-CD1-KEY WITH REDEFINES FOR EACH CODE TYPE (CD-TYPE)
               05  CDSCAN-CD1-KEY              PIC X(10).
               05  FILLER            REDEFINES CDSCAN-CD1-KEY.
                   07  CDSCAN-TYPE             PIC X(2).
                   07  CDSCAN-CODE             PIC X(8).

               05  CDSCAN-CT-KEY     REDEFINES CDSCAN-CD1-KEY.
                   07  CDSCAN-CT-TYPE          PIC X(2).
                   07  CDSCAN-CT-STATE         PIC X(2).
                   07  CDSCAN-CT-COUNTY        PIC X(3).
                   07  CDSCAN-CT-FILLER        PIC X(3).

               05  CDSCAN-CY-KEY     REDEFINES CDSCAN-CD1-KEY.
                   07  CDSCAN-CY-TYPE          PIC X(2).
                   07  CDSCAN-CY-STATE         PIC X(2).
                   07  CDSCAN-CY-CITY          PIC X(3).
                   07  CDSCAN-CY-FILLER        PIC X(3).

               05  CDSCAN-LV-KEY     REDEFINES CDSCAN-CD1-KEY.
                   07  CDSCAN-LV-TYPE          PIC X(2).
                   07  CDSCAN-LV-LEVEL         PIC 9(1).
                   07  CDSCAN-LV-FILLER        PIC X(7).


               05  CDSCAN-PL-KEY     REDEFINES CDSCAN-CD1-KEY.
                   07  CDSCAN-PL-TYPE          PIC X(2).
                   07  CDSCAN-PL-POOLID        PIC 9(5).
                   07  CDSCAN-PL-FILLER        PIC X(3).

      * CDSCAN-EA-CQ-FG = " " WILL SCAN FOR ALL EA AND CQ.
               05  CDSCAN-EA-CQ-FG             PIC X(01).
      * CDSCAN-EA-GQ-FG (G=GLOBAL QUEUING, " "=LOCAL QUEUEING)
               05  CDSCAN-EA-GQ-FG             PIC X(01).
               05  CDSCAN-STATUS               PIC XX.

