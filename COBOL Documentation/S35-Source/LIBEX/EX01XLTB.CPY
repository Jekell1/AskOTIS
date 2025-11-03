      * COPYMEMBER: LIBEX/EX01XLTB
      **************************************************************************
      * BANKRUPT TRAILER EXTRACTION FILE          
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *  020923 ADDED LT-B-BDDATE, BNKRPT DISMISSAL, REGACC PR# 1901
      *  040907 ADD XLTB-REGION-NUMBER PR# 76 REGACC
      *  080303 ADDED XLTB-WHO-FILED, XLTB-ENDORSER-FILED, XLTB-CLAIM-AMT
      *  081016 ADDED XLTB-ACTIVE-INACTIVE REGACC PR# 223
      *  160201 REMOVED XLTB-CODE XLTB-TYPE AND UNUSED PD0003
      *  171016 ADDED XLTB-INT-LC-DUE XLTB-OTH-DUE XLTB-TDUEAMT, WORLD PR#1282  
      *  180108 PUT BACK XLTB-CODE XLTB-TYPE
      *  280509 CS PUT BACK XLTB-UNUSED, NEEDS TO MATCH A15
      **************************************************************************
       78  XLTB-SIZE                    VALUE 592.
       01  XLTB-REC.
           03  XLTB-BRACCTNO            PIC 9(10).
           03  XLTB-ACCTNO              PIC 9(08).
           03  XLTB-CODE                PIC X.
           03  XLTB-SEQNO               PIC 9(03).
           03  XLTB-TYPE                PIC 9(02).
           03  XLTB-FILEDATE            PIC 9(08).
           03  XLTB-CASE-NO             PIC X(09).
           03  XLTB-CHAPTER             PIC X(02).
           03  XLTB-MISC1               PIC X(30).
           03  XLTB-MISC2               PIC X(30).
           03  XLTB-MISC3               PIC X(30).
           03  XLTB-ATTORNEY-NAME       PIC X(24).
           03  XLTB-ATTORNEY-ADR1       PIC X(24).
           03  XLTB-ATTORNEY-ADR2       PIC X(24).
           03  XLTB-ATTORNEY-CITY       PIC X(15).
           03  XLTB-ATTORNEY-STATE      PIC X(02).
           03  XLTB-ATTORNEY-ZIP        PIC X(10).
           03  XLTB-ATTORNEY-PHONE      PIC 9(10).
           03  XLTB-CLERK-NAME          PIC X(24).
           03  XLTB-CLERK-ADR1          PIC X(24).
           03  XLTB-CLERK-ADR2          PIC X(24).
           03  XLTB-CLERK-CITY          PIC X(15).
           03  XLTB-CLERK-STATE         PIC X(02).
           03  XLTB-CLERK-ZIP           PIC X(10).
           03  XLTB-CLERK-PHONE         PIC 9(10).
           03  XLTB-TRUSTEE-NAME        PIC X(24).
           03  XLTB-TRUSTEE-ADR1        PIC X(24).
           03  XLTB-TRUSTEE-ADR2        PIC X(24).
           03  XLTB-TRUSTEE-CITY        PIC X(15).
           03  XLTB-TRUSTEE-STATE       PIC X(02).
           03  XLTB-TRUSTEE-ZIP         PIC X(10).
           03  XLTB-TRUSTEE-PHONE       PIC 9(10).
           03  XLTB-BDDATE              PIC 9(8).
           03  XLTB-REGION-NUMBER       PIC 9(2).
           03  XLTB-WHO-FILED           PIC X.
           03  XLTB-ENDORSER-FILED      PIC X.
           03  XLTB-CLAIM-AMT           PIC 9(7).99.
           03  XLTB-ACTIVE-INACTIVE     PIC X.
           03  XLTB-REAFF-DATE          PIC 9(8).
           03  XLTB-REAFF-SYSDATE       PIC 9(8).
           03  XLTB-REAFF-BALANCE       PIC 9(6).99.
           03  XLTB-REAFF-PAYAMT        PIC 9(6).99.       
           03  XLTB-REAFF-NO-PAYS       PIC 9(3).
           03  XLTB-INTRATE             PIC 9(3).9(3).
           03  XLTB-RESCIND-DATE        PIC 9(8).
           03  XLTB-PARTIAL-FULL        PIC X.
           03  XLTB-DISCHARGE-DATE      PIC 9(8).
           03  XLTB-DISCHARGE-SYSDATE   PIC 9(8).   
           03  XLTB-INT-LC-DUE          PIC 9(7).99.  
           03  XLTB-OTH-DUE             PIC 9(7).99.  
           03  XLTB-TDUEAMT             PIC 9(7).99.  
           03  XLTB-UNUSED              PIC X(8).

           03  XLTB-CR                  PIC X(01).
           03  XLTB-LF                  PIC X(01).
