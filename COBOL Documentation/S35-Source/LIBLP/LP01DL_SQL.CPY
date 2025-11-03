      * COPYMEMBER: LIBLP/LP01DL_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DEALER MASTER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  DL1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  DL1-CURSOR-STAT-GOOD              VALUE "00".
           88  DL1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  DL2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  DL2-CURSOR-STAT-GOOD              VALUE "00".
           88  DL2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DL1-RETURN-CODE           PIC S9(4)   COMP-5.
       01  DL2-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QDL1-WHERE-BEG-KEY.
           03  QDL1-WBEG-BRNO             PIC 9(4).
           03  QDL1-WBEG-NUM             PIC 9(6).
 
       01  QDL1-WHERE-END-KEY.
           03  QDL1-WEND-BRNO             PIC 9(4).
           03  QDL1-WEND-NUM             PIC 9(6).

       01  QDL2-WHERE-BEG-KEY.
           03  QDL2-WBEG-BRNO             PIC 9(4).
           03  QDL2-WBEG-SHORT             PIC X(16).
 
       01  QDL2-WHERE-END-KEY.
           03  QDL2-WEND-BRNO             PIC 9(4).
           03  QDL2-WEND-SHORT             PIC X(16).
 
 
       01  QDL-REC.
           03  QDL-BRNO                    PIC 9(4).
           03  QDL-NUM                     PIC 9(6).
           03  QDL-LTOUCH-DATE             PIC X(10).
           03  QDL-NAME                    PIC X(25).
           03  QDL-ADR1                    PIC X(25).
           03  QDL-ADR2                    PIC X(25).
           03  QDL-CITY                    PIC X(15).
           03  QDL-STATE                   PIC XX.
           03  QDL-ZIP                     PIC X(10).
           03  QDL-SHORT                   PIC X(16).
           03  QDL-PHONE                   PIC 9(10).
           03  QDL-PURPOSE                 PIC X(3).
           03  QDL-MFGCODE                 PIC X(3).
           03  QDL-ORIGDATE                PIC X(10).
           03  QDL-TERMDATE                PIC X(10).
           03  QDL-LC-CODE                 PIC 99.
           03  QDL-FULLCEIL                PIC S9(7)V99.
           03  QDL-PARTCEIL                PIC S9(7)V99.
           03  QDL-NONCEIL                 PIC S9(7)V99.
           03  QDL-SIGN1                   PIC X(25).
           03  QDL-SIGN2                   PIC X(25).
           03  QDL-SIGN3                   PIC X(25).
           03  QDL-MAXPURTRM               PIC 9(4).
           03  QDL-MAXPURPER               PIC 9(4).
           03  QDL-EARNCD                  PIC 99.
           03  QDL-EARNFAC                 PIC 9(4).
           03  QDL-DISC-FRMLA              PIC 99.
           03  QDL-DISC-CODE               PIC X.
           03  QDL-DISC-TERM-1             PIC 9(3).
           03  QDL-DISC-RATE-1             PIC 99V9(4).
           03  QDL-DISC-TERM-2             PIC 9(3).
           03  QDL-DISC-RATE-2             PIC 99V9(4).
           03  QDL-DISC-TERM-3             PIC 9(3).
           03  QDL-DISC-RATE-3             PIC 99V9(4).
           03  QDL-DISC-TERM-4             PIC 9(3).
           03  QDL-DISC-RATE-4             PIC 99V9(4).
           03  QDL-DISC-TERM-5             PIC 9(3).
           03  QDL-DISC-RATE-5             PIC 99V9(4).
           03  QDL-DISC-TERM-6             PIC 9(3).
           03  QDL-DISC-RATE-6             PIC 99V9(4).
           03  QDL-RVPRIN-FRMLA-1     PIC 99.
           03  QDL-RVPRIN-CODE-1      PIC X.
           03  QDL-RVPRIN-ADDON-1     PIC S9(5)V99.
           03  QDL-RVPRIN-TERM-1-1    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-1    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-2    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-2    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-3    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-3    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-4    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-4    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-5    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-5    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-6    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-6    PIC 99V9(4).
           03  QDL-RVPRIN-FRMLA-2     PIC 99.
           03  QDL-RVPRIN-CODE-2      PIC X.
           03  QDL-RVPRIN-ADDON-2     PIC S9(5)V99.
           03  QDL-RVPRIN-TERM-2-1    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-1    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-2    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-2    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-3    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-3    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-4    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-4    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-5    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-5    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-6    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-6    PIC 99V9(4).
           03  QDL-RVPRIN-FRMLA-3     PIC 99.
           03  QDL-RVPRIN-CODE-3      PIC X.
           03  QDL-RVPRIN-ADDON-3     PIC S9(5)V99.
           03  QDL-RVPRIN-TERM-3-1    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-1    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-2    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-2    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-3    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-3    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-4    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-4    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-5    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-5    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-6    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-6    PIC 99V9(4).
           03  QDL-RVCHGS-FRMLA-1     PIC 99.
           03  QDL-RVCHGS-CODE-1      PIC X.
           03  QDL-RVCHGS-ADDON-1     PIC S9(5)V99.
           03  QDL-RVCHGS-TERM-1-1    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-1    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-2    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-2    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-3    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-3    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-4    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-4    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-5    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-5    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-6    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-6    PIC 99V9(4).
           03  QDL-RVCHGS-FRMLA-2     PIC 99.
           03  QDL-RVCHGS-CODE-2      PIC X.
           03  QDL-RVCHGS-ADDON-2     PIC S9(5)V99.
           03  QDL-RVCHGS-TERM-2-1    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-1    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-2    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-2    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-3    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-3    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-4    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-4    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-5    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-5    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-6    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-6    PIC 99V9(4).
           03  QDL-RVCHGS-FRMLA-3     PIC 99.
           03  QDL-RVCHGS-CODE-3      PIC X.
           03  QDL-RVCHGS-ADDON-3     PIC S9(5)V99.
           03  QDL-RVCHGS-TERM-3-1    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-1    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-2    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-2    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-3    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-3    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-4    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-4    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-5    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-5    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-6    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-6    PIC 99V9(4).
           03  QDL-PARVHELD-FRMLA      PIC 99.
           03  QDL-PARVHELD-CODE       PIC X.
           03  QDL-PARVHELD-ADDON      PIC S9(5)V99.
           03  QDL-PARVHELD-TERM-1    PIC 9(3).
           03  QDL-PARVHELD-RATE-1    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-2    PIC 9(3).
           03  QDL-PARVHELD-RATE-2    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-3    PIC 9(3).
           03  QDL-PARVHELD-RATE-3    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-4    PIC 9(3).
           03  QDL-PARVHELD-RATE-4    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-5    PIC 9(3).
           03  QDL-PARVHELD-RATE-5    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-6    PIC 9(3).
           03  QDL-PARVHELD-RATE-6    PIC 99V9(4).
           03  QDL-PARVPAID-FRMLA     PIC 99.
           03  QDL-PARVPAID-CODE      PIC X.
           03  QDL-PARVPAID-ADDON     PIC S9(5)V99.
           03  QDL-PARVPAID-TERM-1    PIC 9(3).
           03  QDL-PARVPAID-RATE-1    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-2    PIC 9(3).
           03  QDL-PARVPAID-RATE-2    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-3    PIC 9(3).
           03  QDL-PARVPAID-RATE-3    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-4    PIC 9(3).
           03  QDL-PARVPAID-RATE-4    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-5    PIC 9(3).
           03  QDL-PARVPAID-RATE-5    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-6    PIC 9(3).
           03  QDL-PARVPAID-RATE-6    PIC 99V9(4).
           03  QDL-SVACCT-FRMLA            PIC 99.
           03  QDL-SVCODE                  PIC X.
           03  QDL-SVACCT-AMT              PIC S9(5)V99.
           03  QDL-SVACCT-RATE             PIC 999V99.
           03  QDL-SVACCT-LCHGPER          PIC 999.
           03  QDL-SVACCT-INTPER           PIC 999.
           03  QDL-SVACCT-DEFPER           PIC 999.
           03  QDL-FULLCEIL-BAL            PIC S9(9)V99.
           03  QDL-PARTCEIL-BAL            PIC S9(9)V99.
           03  QDL-NONCEIL-BAL             PIC S9(9)V99.
           03  QDL-AFP-DEALER              PIC X.
           03  QDL-RECOURSE-FRMLA          PIC X.
           03  QDL-FAX-NUMBER              PIC 9(10).
           03  QDL-PARVPAID-FRMLA-IB       PIC X.
           03  QDL-PARVPAID-CODE-IB        PIC X.
           03  QDL-LPO                     PIC X(4).
           03  QDL-ACH-FG                  PIC X.
           03  QDL-BANK-ROUTING-NO         PIC 9(9).
           03  QDL-BANK-ACCTNO             PIC X(17).
           03  QDL-ACH-PRENOTE-FG          PIC X.
           03  QDL-SAL-MISC-PCT            PIC 99V99.
           03  QDL-SAL-MISC-PCT2           PIC 99V99.
           03  QDL-WAIVE-ACH-FEE           PIC X.
           03  QDL-TRACK-CHANGE-FLAG       PIC X.
           03  QDL-DBA-NAME                PIC X(40).

       EXEC SQL END DECLARE SECTION END-EXEC.
