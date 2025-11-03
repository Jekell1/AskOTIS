      * COPYMEMBER: LIBEX/EX01XDL
      **************************************************************************
      * DEALER EXTRACTION FILE          1142 BYTES
      * 
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675 
      * 
      * REV:
      *     CS  040903 ADDED XDL-REGION-NUMBER PR#76 REGACC    
      *    BAH  100917 ADDED XDL-WAIVE-ACH-FEE PR#3650 MIDATL
      *    BAH  140310 ADDED XDL-TRACK-CHANGE-FLAG  PR#350 REGACC
      **************************************************************************
       78  XDL-SIZE                        VALUE 1142.
       01  XDL-REC.
           03  XDL-BRNO                    PIC 9(04).
           03  XDL-NUM                     PIC 9(06).
           03  XDL-NAME                    PIC X(25).
           03  XDL-ADR1                    PIC X(25).
           03  XDL-ADR2                    PIC X(25).
           03  XDL-CITY                    PIC X(15).
           03  XDL-STATE                   PIC X(02).
           03  XDL-ZIP                     PIC X(10).
           03  XDL-SHORT                   PIC X(16).
           03  XDL-PHONE                   PIC 9(10).
           03  XDL-BRANCH                  PIC 9(04).
           03  XDL-PURPOSE                 PIC X(03).
           03  XDL-MFGCODE                 PIC X(03).
           03  XDL-ORIGDATE                PIC 9(08).
           03  XDL-TERMDATE                PIC 9(08).
           03  XDL-LC-CODE                 PIC 9(02).
           03  XDL-FULLCEIL                PIC +9(07).99.
           03  XDL-PARTCEIL                PIC +9(07).99.
           03  XDL-NONCEIL                 PIC +9(07).99.
           03  XDL-SIGN1                   PIC X(25).
           03  XDL-SIGN2                   PIC X(25).
           03  XDL-SIGN3                   PIC X(25).
           03  XDL-MAXPURTRM               PIC 9(04).
           03  XDL-MAXPURPER               PIC 9(04).
           03  XDL-EARNCD                  PIC 9(02).
           03  XDL-EARNFAC                 PIC 9(04).
           03  XDL-DISC-FRMLA              PIC 9(02).
           03  XDL-DISC-CODE               PIC X(01).
           03  XDL-DISC-TAB.
               05  XDL-DISC-TERM1          PIC 9(03).
               05  XDL-DISC-RATE1          PIC 9(02).9(04).
               05  XDL-DISC-TERM2          PIC 9(03).
               05  XDL-DISC-RATE2          PIC 9(02).9(04).
               05  XDL-DISC-TERM3          PIC 9(03).
               05  XDL-DISC-RATE3          PIC 9(02).9(04).
               05  XDL-DISC-TERM4          PIC 9(03).
               05  XDL-DISC-RATE4          PIC 9(02).9(04).
               05  XDL-DISC-TERM5          PIC 9(03).
               05  XDL-DISC-RATE5          PIC 9(02).9(04).
               05  XDL-DISC-TERM6          PIC 9(03).
               05  XDL-DISC-RATE6          PIC 9(02).9(04).
           03  XDL-DISC-TABLE REDEFINES XDL-DISC-TAB OCCURS 6.
               05  XDL-DISC-TERM           PIC 9(03).
               05  XDL-DISC-RATE           PIC 9(02).9(04).
           03  XDL-RVPRIN-TAB.
               05  XDL-RVPRIN-FRMLA1       PIC 9(02).
               05  XDL-RVPRIN-CODE1        PIC X(01).
               05  XDL-RVPRIN-ADDON1       PIC +9(05).99.
               05  XDL-RVPRIN-RATE-TAB1.
                   07 XDL-RVPRIN-TERM1-1   PIC 9(03).
                   07 XDL-RVPRIN-RATE1-1   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM1-2   PIC 9(03).
                   07 XDL-RVPRIN-RATE1-2   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM1-3   PIC 9(03).
                   07 XDL-RVPRIN-RATE1-3   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM1-4   PIC 9(03).
                   07 XDL-RVPRIN-RATE1-4   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM1-5   PIC 9(03).
                   07 XDL-RVPRIN-RATE1-5   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM1-6   PIC 9(03).
                   07 XDL-RVPRIN-RATE1-6   PIC 9(02).9(04).
               05  XDL-RVPRIN-RATE-TABLE1 REDEFINES XDL-RVPRIN-RATE-TAB1
                                           OCCURS 6.
                   07 XDL-RVPRIN-TERM1     PIC 9(03).
                   07 XDL-RVPRIN-RATE1     PIC 9(02).9(04).
               05  XDL-RVPRIN-FRMLA2       PIC 9(02).
               05  XDL-RVPRIN-CODE2        PIC X(01).
               05  XDL-RVPRIN-ADDON2       PIC +9(05).99.
               05  XDL-RVPRIN-RATE-TAB2.
                   07 XDL-RVPRIN-TERM2-1   PIC 9(03).
                   07 XDL-RVPRIN-RATE2-1   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM2-2   PIC 9(03).
                   07 XDL-RVPRIN-RATE2-2   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM2-3   PIC 9(03).
                   07 XDL-RVPRIN-RATE2-3   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM2-4   PIC 9(03).
                   07 XDL-RVPRIN-RATE2-4   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM2-5   PIC 9(03).
                   07 XDL-RVPRIN-RATE2-5   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM2-6   PIC 9(03).
                   07 XDL-RVPRIN-RATE2-6   PIC 9(02).9(04).
               05  XDL-RVPRIN-RATE-TABLE2 REDEFINES XDL-RVPRIN-RATE-TAB2
                                           OCCURS 6.
                   07 XDL-RVPRIN-TERM2     PIC 9(03).
                   07 XDL-RVPRIN-RATE2     PIC 9(02).9(04).
               05  XDL-RVPRIN-FRMLA3       PIC 9(02).
               05  XDL-RVPRIN-CODE3        PIC X(01).
               05  XDL-RVPRIN-ADDON3       PIC +9(05).99.
               05  XDL-RVPRIN-RATE-TAB3.
                   07 XDL-RVPRIN-TERM3-1   PIC 9(03).
                   07 XDL-RVPRIN-RATE3-1   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM3-2   PIC 9(03).
                   07 XDL-RVPRIN-RATE3-2   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM3-3   PIC 9(03).
                   07 XDL-RVPRIN-RATE3-3   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM3-4   PIC 9(03).
                   07 XDL-RVPRIN-RATE3-4   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM3-5   PIC 9(03).
                   07 XDL-RVPRIN-RATE3-5   PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM3-6   PIC 9(03).
                   07 XDL-RVPRIN-RATE3-6   PIC 9(02).9(04).
               05  XDL-RVPRIN-RATE-TABLE3 REDEFINES XDL-RVPRIN-RATE-TAB3
                                           OCCURS 6.
                   07 XDL-RVPRIN-TERM3     PIC 9(03).
                   07 XDL-RVPRIN-RATE3     PIC 9(02).9(04).
           03  XDL-RVPRIN-TABLE REDEFINES XDL-RVPRIN-TAB OCCURS 3.
               05  XDL-RVPRIN-FRMLA        PIC 9(02).
               05  XDL-RVPRIN-CODE         PIC X(01).
               05  XDL-RVPRIN-ADDON        PIC +9(05).99.
               05  XDL-RVPRIN-RATE-TAB.
                   07 XDL-RVPRIN-TERM1     PIC 9(03).
                   07 XDL-RVPRIN-RATE1     PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM2     PIC 9(03).
                   07 XDL-RVPRIN-RATE2     PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM3     PIC 9(03).
                   07 XDL-RVPRIN-RATE3     PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM4     PIC 9(03).
                   07 XDL-RVPRIN-RATE4     PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM5     PIC 9(03).
                   07 XDL-RVPRIN-RATE5     PIC 9(02).9(04).
                   07 XDL-RVPRIN-TERM6     PIC 9(03).
                   07 XDL-RVPRIN-RATE6     PIC 9(02).9(04).
               05  XDL-RVPRIN-RATE-TABLE REDEFINES XDL-RVPRIN-RATE-TAB
                                           OCCURS 6.
                   07 XDL-RVPRIN-TERM      PIC 9(03).
                   07 XDL-RVPRIN-RATE      PIC 9(02).9(04).
           03  XDL-RVCHGS-TAB.
               05  XDL-RVCHGS-FRMLA1       PIC 9(02).
               05  XDL-RVCHGS-CODE1        PIC X(01).
               05  XDL-RVCHGS-ADDON1       PIC +9(05).99.
               05  XDL-RVCHGS-RATE-TAB1.
                   07 XDL-RVCHGS-TERM1-1   PIC 9(03).
                   07 XDL-RVCHGS-RATE1-1   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM1-2   PIC 9(03).
                   07 XDL-RVCHGS-RATE1-2   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM1-3   PIC 9(03).
                   07 XDL-RVCHGS-RATE1-3   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM1-4   PIC 9(03).
                   07 XDL-RVCHGS-RATE1-4   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM1-5   PIC 9(03).
                   07 XDL-RVCHGS-RATE1-5   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM1-6   PIC 9(03).
                   07 XDL-RVCHGS-RATE1-6   PIC 9(02).9(04).
               05  XDL-RVCHGS-RATE-TABLE1 REDEFINES 
                      XDL-RVCHGS-RATE-TAB1 OCCURS 6.
                   07 XDL-RVCHGS-TERM1     PIC 9(03).
                   07 XDL-RVCHGS-RATE1     PIC 9(02).9(04).
               05  XDL-RVCHGS-FRMLA2       PIC 9(02).
               05  XDL-RVCHGS-CODE2        PIC X(01).
               05  XDL-RVCHGS-ADDON2       PIC +9(05).99.
               05  XDL-RVCHGS-RATE-TAB2.
                   07 XDL-RVCHGS-TERM2-1   PIC 9(03).
                   07 XDL-RVCHGS-RATE2-1   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM2-2   PIC 9(03).
                   07 XDL-RVCHGS-RATE2-2   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM2-3   PIC 9(03).
                   07 XDL-RVCHGS-RATE2-3   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM2-4   PIC 9(03).
                   07 XDL-RVCHGS-RATE2-4   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM2-5   PIC 9(03).
                   07 XDL-RVCHGS-RATE2-5   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM2-6   PIC 9(03).
                   07 XDL-RVCHGS-RATE2-6   PIC 9(02).9(04).
               05  XDL-RVCHGS-RATE-TABLE2 REDEFINES 
                     XDL-RVCHGS-RATE-TAB2 OCCURS 6.
                   07 XDL-RVCHGS-TERM2     PIC 9(03).
                   07 XDL-RVCHGS-RATE2     PIC 9(02).9(04).
               05  XDL-RVCHGS-FRMLA3       PIC 9(02).
               05  XDL-RVCHGS-CODE3        PIC X(01).
               05  XDL-RVCHGS-ADDON3       PIC +9(05).99.
               05  XDL-RVCHGS-RATE-TAB3.
                   07 XDL-RVCHGS-TERM3-1   PIC 9(03).
                   07 XDL-RVCHGS-RATE3-1   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM3-2   PIC 9(03).
                   07 XDL-RVCHGS-RATE3-2   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM3-3   PIC 9(03).
                   07 XDL-RVCHGS-RATE3-3   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM3-4   PIC 9(03).
                   07 XDL-RVCHGS-RATE3-4   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM3-5   PIC 9(03).
                   07 XDL-RVCHGS-RATE3-5   PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM3-6   PIC 9(03).
                   07 XDL-RVCHGS-RATE3-6   PIC 9(02).9(04).
               05  XDL-RVCHGS-RATE-TABLE3 REDEFINES 
                     XDL-RVCHGS-RATE-TAB3 OCCURS 6.
                   07 XDL-RVCHGS-TERM3     PIC 9(03).
                   07 XDL-RVCHGS-RATE3     PIC 9(02).9(04).
           03  XDL-RVCHGS-TABLE REDEFINES XDL-RVCHGS-TAB OCCURS 3.
               05  XDL-RVCHGS-FRMLA        PIC 9(02).
               05  XDL-RVCHGS-CODE         PIC X(01).
               05  XDL-RVCHGS-ADDON        PIC +9(05).99.
               05  XDL-RVCHGS-RATE-TAB.
                   07 XDL-RVCHGS-TERM1     PIC 9(03).
                   07 XDL-RVCHGS-RATE1     PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM2     PIC 9(03).
                   07 XDL-RVCHGS-RATE2     PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM3     PIC 9(03).
                   07 XDL-RVCHGS-RATE3     PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM4     PIC 9(03).
                   07 XDL-RVCHGS-RATE4     PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM5     PIC 9(03).
                   07 XDL-RVCHGS-RATE5     PIC 9(02).9(04).
                   07 XDL-RVCHGS-TERM6     PIC 9(03).
                   07 XDL-RVCHGS-RATE6     PIC 9(02).9(04).
               05  XDL-RVCHGS-RATE-TABLE REDEFINES XDL-RVCHGS-RATE-TAB
                                           OCCURS 6.
                   07 XDL-RVCHGS-TERM      PIC 9(03).
                   07 XDL-RVCHGS-RATE      PIC 9(02).9(04).
           03  XDL-PARVHELD-FRMLA          PIC 9(02).
           03  XDL-PARVHELD-CODE           PIC X(01).
           03  XDL-PARVHELD-ADDON          PIC +9(05).99.
           03  XDL-PARVHELD-TAB.
               05  XDL-PARVHELD-TERM1      PIC 9(03).
               05  XDL-PARVHELD-RATE1      PIC 9(02).9(04).
               05  XDL-PARVHELD-TERM2      PIC 9(03).
               05  XDL-PARVHELD-RATE2      PIC 9(02).9(04).
               05  XDL-PARVHELD-TERM3      PIC 9(03).
               05  XDL-PARVHELD-RATE3      PIC 9(02).9(04).
               05  XDL-PARVHELD-TERM4      PIC 9(03).
               05  XDL-PARVHELD-RATE4      PIC 9(02).9(04).
               05  XDL-PARVHELD-TERM5      PIC 9(03).
               05  XDL-PARVHELD-RATE5      PIC 9(02).9(04).
               05  XDL-PARVHELD-TERM6      PIC 9(03).
               05  XDL-PARVHELD-RATE6      PIC 9(02).9(04).
           03  XDL-PARVHELD-TABLE REDEFINES XDL-PARVHELD-TAB 
                                           OCCURS 6.
               05  XDL-PARVHELD-TERM       PIC 9(03).
               05  XDL-PARVHELD-RATE       PIC 9(02).9(04).
           03  XDL-PARVPAID-FRMLA          PIC 9(02).
           03  XDL-PARVPAID-CODE           PIC X(01).
           03  XDL-PARVPAID-ADDON          PIC +9(05).99.
           03  XDL-PARVPAID-TAB.
               05  XDL-PARVPAID-TERM1      PIC 9(03).
               05  XDL-PARVPAID-RATE1      PIC 9(02).9(04).
               05  XDL-PARVPAID-TERM2      PIC 9(03).
               05  XDL-PARVPAID-RATE2      PIC 9(02).9(04).
               05  XDL-PARVPAID-TERM3      PIC 9(03).
               05  XDL-PARVPAID-RATE3      PIC 9(02).9(04).
               05  XDL-PARVPAID-TERM4      PIC 9(03).
               05  XDL-PARVPAID-RATE4      PIC 9(02).9(04).
               05  XDL-PARVPAID-TERM5      PIC 9(03).
               05  XDL-PARVPAID-RATE5      PIC 9(02).9(04).
               05  XDL-PARVPAID-TERM6      PIC 9(03).
               05  XDL-PARVPAID-RATE6      PIC 9(02).9(04).
           03  XDL-PARVPAID-TABLE REDEFINES XDL-PARVPAID-TAB OCCURS 6.
               05  XDL-PARVPAID-TERM       PIC 9(03).
               05  XDL-PARVPAID-RATE       PIC 9(02).9(04).
           03  XDL-SVACCT-FRMLA            PIC 9(02).
           03  XDL-SVCODE                  PIC X(01).
           03  XDL-SVACCT-AMT              PIC +9(05).99.
           03  XDL-SVACCT-RATE             PIC 9(03).99.
           03  XDL-SVACCT-LCHGPER          PIC 9(03).
           03  XDL-SVACCT-INTPER           PIC 9(03).
           03  XDL-SVACCT-DEFPER           PIC 9(03).
           03  XDL-FULLCEIL-BAL            PIC +9(09).99.
           03  XDL-PARTCEIL-BAL            PIC +9(09).99.
           03  XDL-NONCEIL-BAL             PIC +9(09).99.
           03  XDL-AFP-DEALER              PIC X(01).
           03  XDL-RECOURSE-FRMLA          PIC X(01).
           03  XDL-FAX-NUMBER              PIC 9(10).
           03  XDL-PARVPAID-FRMLA-IB       PIC X(01).
           03  XDL-PARVPAID-CODE-IB        PIC X(01).
           03  XDL-REVOLVING               PIC X(01).
           03  XDL-LPO                     PIC X(04).
           03  XDL-ACH-FG                  PIC X(01).
           03  XDL-BANK-ROUTING-NO         PIC 9(09).
           03  XDL-BANK-ACCTNO             PIC X(17).
           03  XDL-ACH-PRENOTE-FG          PIC X(01).
           03  XDL-REGION-NUMBER           PIC 9(2).
           03  XDL-WAIVE-ACH-FEE           PIC X.
           03  XDL-TRACK-CHANGE-FLAG       PIC X.
           03  XDL-UNUSED                  PIC X(96).
           03  XDL-CR                      PIC X(01).
           03  XDL-LF                      PIC X(01).
