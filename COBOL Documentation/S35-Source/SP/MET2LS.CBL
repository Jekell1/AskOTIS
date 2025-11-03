       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MET2LS.
       DATE-WRITTEN.  01-09-2004.
      ******************************************************************
      *
      *  IDENTIFICATION:  SP/METLS2.C
      *
      *  DESCRIPTION   :  METRO 2 FILE LISTING
      *
      *=================================================================
      * REV:
      * BDM 2004.0109 {PR#00000} PARADATA <A15> ***NEW***
      *
      * KEC 2004.1216 {DR#00012} PARADATA <A15> [BETH]
      *               FINISHING UP WHERE THIS WAS LEFT OFF.
      * BAH 051216 ALLOW FOR THE OTHER CREDIT BUREAU IDS, METRO2.2, ETC
      *            ADDED OPTION ON THE SCREEN SO YOU DONT HAVE TO MOVE THE
      *            FILE BEFORE RUNNING THE LISTING
      * BAH 151118 FIXED ACCOUNT NUMBER RANGE TEST, WAS NOT WORKING.
      *            ADDED BRANCH, SINGLE OR F3 ALL, CITIZENS PL#910
      * BAH 2025.0311 ADDED TRW-H-INNOVIS-ID #1702 S35Q-169
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSTW.CPY".

      *-----------------------------------------------------------------
           SELECT TRW-E-FILE ASSIGN TO TRW-E-PATH
                           ORGANIZATION RELATIVE
                           ACCESS DYNAMIC
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           RELATIVE KEY TRW-E-KEY
                           FILE STATUS FILE-STAT.

      *=================================================================
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBSP/SPFDMETRO2.CPY".

      ******************************************************************
      *
      *    T R W - E - F I L E
      *
      ******************************************************************
       FD  TRW-E-FILE
           LABEL RECORDS ARE STANDARD.
      *=================================================================
       01  TRW-E-REC.
           03  FILLER                  PIC X(1364).
           03  TRW-E-CR                PIC X(01).
           03  TRW-E-LF                PIC X(01).
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/MET2LS S35 01/18/24-12:47:58 >".

       COPY "LIBSP/SP01TRWINFO.CPY".
       COPY "LIBSP/SP01TRWINFO_SQL.CPY".
       COPY "LIBSP/SPWSTRWINFO.CPY".
       COPY "LIBSP/SPWSMETRO2.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN                      PIC X(03)     VALUE "PR ".
       01  TRW-IFN                     PIC X(03)     VALUE "TRW".
       01  TRW-KEY                     PIC 9(06)     VALUE ZEROES.
       01  TRW-E-KEY                   PIC 9(06)     VALUE ZEROES.


      ******************************************************************
      *
      *    F I L E - P R I N T - W O R K E R S                     {FPW}
      *
      ******************************************************************
       78  FPW---TYPE-HEADER                         VALUE "HEADER    ".
       78  FPW---TYPE-TRAILER                        VALUE "TRAILER   ".
       78  FPW---TYPE-BASE                           VALUE "BASE      ".
      *-----------------------------------------------------------------
       77  FPW-TYPE                    PIC X(10)     VALUE SPACES.
           88  FPW-TYPE-HEADER                       VALUE "HEADER    ".
           88  FPW-TYPE-TRAILER                      VALUE "TRAILER   ".
           88  FPW-TYPE-BASE                         VALUE "BASE      ".

      *=================================================================
       01  FPW-BYTE                    PIC 9(04)     VALUE ZEROES.
       01  FPW-BYTE-BEG                PIC 9(04)     VALUE ZEROES.
       01  FPW-BYTE-END                PIC 9(04)     VALUE ZEROES.
       01  FPW-SIZE                    PIC 9(04)     VALUE ZEROES.

      *=================================================================
       77  FPW-ELE                     PIC 9(03)     VALUE ZEROES.
       78  FPW-MAX                                   VALUE 200.
      *-----------------------------------------------------------------
       01  FPW-REC.
           03  FPASSWDW-OCCURS                    OCCURS FPW-MAX.
               05  FPASSWDW-TEXT                  PIC X(50).
               05  FPASSWDW-FIELD                 PIC X(30).
               05  FPASSWDW-SIZE                  PIC X(07).
               05  FILLER                    REDEFINES FPASSWDW-SIZE.
                   07  FPASSWDW-SIZE-TYPE         PIC X(01).
                       88  FPASSWDW-SIZE-TYPE-ALPHA   VALUE "X".
                       88  FPASSWDW-SIZE-TYPE-NUMERIC VALUE "9".
                   07  FILLER                    PIC X(01).
                   07  FPASSWDW-SIZE-NUMBER       PIC 9(04).
                   07  FILLER                    PIC X(01).

      *=================================================================
       01  FPW-HEADER-REC.

           03  FILLER PIC X(50) VALUE "RECORD DESCRIPTOR WORD (RDW)  ".
           03  FILLER PIC X(30) VALUE "TRW-H-RDW                     ".
           03  FILLER PIC X(07) VALUE "9(0004)".

           03  FILLER PIC X(50) VALUE "RECORD IDENTIFIER             ".
           03  FILLER PIC X(30) VALUE "TRW-H-ID                      ".
           03  FILLER PIC X(07) VALUE "X(0006)".

           03  FILLER PIC X(50) VALUE "CYCLE NUMBER                  ".
           03  FILLER PIC X(30) VALUE "TRW-H-CYCLE-NO                ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "INNOVIS PROGRAM IDENTIFIER    ".
           03  FILLER PIC X(30) VALUE "TRW-H-INNOVIS-ID              ".
           03  FILLER PIC X(07) VALUE "X(0010)".

           03  FILLER PIC X(50) VALUE "EQUIFAX PROGRAM IDENTIFIER    ".
           03  FILLER PIC X(30) VALUE "TRW-H-EQUIFAX-ID              ".
           03  FILLER PIC X(07) VALUE "X(0010)".

           03  FILLER PIC X(50) VALUE "EXPERIAN PROGRAM IDENTIFIER   ".
           03  FILLER PIC X(30) VALUE "TRW-H-EXPERIAN-ID             ".
           03  FILLER PIC X(07) VALUE "X(0005)".

           03  FILLER PIC X(50) VALUE "TRANS UNION PROGRAM IDENTIFIER".
           03  FILLER PIC X(30) VALUE "TRW-H-TRANSUNION-ID           ".
           03  FILLER PIC X(07) VALUE "X(0010)".

           03  FILLER PIC X(50) VALUE "ACTIVITY DATE                 ".
           03  FILLER PIC X(30) VALUE "TRW-H-ACTIVITY-DATE           ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "DATE CREATED                  ".
           03  FILLER PIC X(30) VALUE "TRW-H-DATE-CREATED            ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "PROGRAM DATE                  ".
           03  FILLER PIC X(30) VALUE "TRW-H-PROGRAM-DATE            ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "PROGRAM REVISION DATE         ".
           03  FILLER PIC X(30) VALUE "TRW-H-REVISION-DATE           ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "REPORTER NAME                 ".
           03  FILLER PIC X(30) VALUE "TRW-H-REPORTER-NAME           ".
           03  FILLER PIC X(07) VALUE "X(0040)".

           03  FILLER PIC X(50) VALUE "REPORTER ADDRESS              ".
           03  FILLER PIC X(30) VALUE "TRW-H-REPORTER-ADR            ".
           03  FILLER PIC X(07) VALUE "X(0096)".

           03  FILLER PIC X(50) VALUE "REPORTER TELEPHONE NUMBER     ".
           03  FILLER PIC X(30) VALUE "TRW-H-REPORTER-PHONE          ".
           03  FILLER PIC X(07) VALUE "9(0010)".

           03  FILLER PIC X(50) VALUE "SOFTWARE VENDOR NAME          ".
           03  FILLER PIC X(30) VALUE "TRW-H-SOFTWARE-VENDOR         ".
           03  FILLER PIC X(07) VALUE "X(0040)".

           03  FILLER PIC X(50) VALUE "SOFTWARE VERSION NUMBER       ".
           03  FILLER PIC X(30) VALUE "TRW-H-SOFTWARE-VERSION        ".
           03  FILLER PIC X(07) VALUE "X(0005)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(1094)".

      *=================================================================
       01  FPW-BASE-REC.

           03  FILLER PIC X(50) VALUE "RECORD DESCRIPTOR WORD (RDW)  ".
           03  FILLER PIC X(30) VALUE "TRW-B-RDW                     ".
           03  FILLER PIC X(07) VALUE "9(0004)".

           03  FILLER PIC X(50) VALUE "PROCESSING INDICATOR          ".
           03  FILLER PIC X(30) VALUE "TRW-B-PROC-INDICATOR          ".
           03  FILLER PIC X(07) VALUE "9(0001)".

           03  FILLER PIC X(50) VALUE "DATE STAMP                    ".
           03  FILLER PIC X(30) VALUE "TRW-B-DATE-STAMP              ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "TIME STAMP                    ".
           03  FILLER PIC X(30) VALUE "TRW-B-TIME-STAMP              ".
           03  FILLER PIC X(07) VALUE "9(0006)".

           03  FILLER PIC X(50) VALUE "CORRECTION INDICATOR          ".
           03  FILLER PIC X(30) VALUE "TRW-B-CORRECTION              ".
           03  FILLER PIC X(07) VALUE "9(0001)".

           03  FILLER PIC X(50) VALUE "IDENTIFICATION NUMBER         ".
           03  FILLER PIC X(30) VALUE "TRW-B-IDENT-NO                ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "CYCLE IDENTIFIER              ".
           03  FILLER PIC X(30) VALUE "TRW-B-CYCLE                   ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "CONSUMER ACCOUNT NUMBERX      ".
           03  FILLER PIC X(30) VALUE "TRW-B-ACCTNOX                 ".
           03  FILLER PIC X(07) VALUE "X(0015)".

           03  FILLER PIC X(50) VALUE "CONSUMER ACCOUNT NUMBER       ".
           03  FILLER PIC X(30) VALUE "TRW-B-ACCTNO                  ".
           03  FILLER PIC X(07) VALUE "9(0015)".

           03  FILLER PIC X(50) VALUE "PORTFOLIO TYPE                ".
           03  FILLER PIC X(30) VALUE "TRW-B-PTYPE                   ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "ACCOUNT TYPE                  ".
           03  FILLER PIC X(30) VALUE "TRW-B-ATYPE                   ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "DATE OPENED                   ".
           03  FILLER PIC X(30) VALUE "TRW-B-DATE-OPENED             ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "CREDIT LIMIT                  ".
           03  FILLER PIC X(30) VALUE "TRW-B-CREDITLIMIT             ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "HIGHEST CREDIT OR ORIGINAL LOAN AMOUNT            ".
           03  FILLER PIC X(30) VALUE "TRW-B-AMT                     ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TERMS DURATION                ".
           03  FILLER PIC X(30) VALUE "TRW-B-TERMS                   ".
           03  FILLER PIC X(07) VALUE "9(0003)".

           03  FILLER PIC X(50) VALUE "TERMS FREQUENCY               ".
           03  FILLER PIC X(30) VALUE "TRW-B-TERMS-FREQ              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE
                  "SCHEDULED MONTHLY PAYMENT AMOUNT                  ".
           03  FILLER PIC X(30) VALUE "TRW-B-REGPYAMT                ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "ACTUAL PAYMENT AMOUNT         ".
           03  FILLER PIC X(30) VALUE "TRW-B-ACTUAL-PAID             ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "ACCOUNT STATUS                ".
           03  FILLER PIC X(30) VALUE "TRW-B-STATUS                  ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "PAYMENT RATING                ".
           03  FILLER PIC X(30) VALUE "TRW-B-PAY-RATING              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "PAYMENT HISTORY PROFILE       ".
           03  FILLER PIC X(30) VALUE "TRW-B-HIST-PROFILE            ".
           03  FILLER PIC X(07) VALUE "X(0024)".

           03  FILLER PIC X(50) VALUE "SPECIAL COMMENT               ".
           03  FILLER PIC X(30) VALUE "TRW-B-COMMENT                 ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "COMPLIANCE CONDITION CODE     ".
           03  FILLER PIC X(30) VALUE "TRW-B-CONDITION               ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "CURRENT BALANCE               ".
           03  FILLER PIC X(30) VALUE "TRW-B-CURBAL                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "AMOUNT PAST DUE               ".
           03  FILLER PIC X(30) VALUE "TRW-B-AMT-PASTDUE             ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "ORIGINAL CHARGE-OFF AMOUNT    ".
           03  FILLER PIC X(30) VALUE "TRW-B-ORIG-PLAMT              ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "BILLING DATE                  ".
           03  FILLER PIC X(30) VALUE "TRW-B-BILL-DATE               ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE
                  "FCRA COMPLIANCE/DATE OF FIRST DELINQUENCY         ".
           03  FILLER PIC X(30) VALUE "TRW-B-DATE-DELINQ             ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "DATE CLOSED                   ".
           03  FILLER PIC X(30) VALUE "TRW-B-DATE-CLOSED             ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "DATE OF LAST PAYMENT          ".
           03  FILLER PIC X(30) VALUE "TRW-B-DATE-LAST-PAY           ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0017)".

           03  FILLER PIC X(50) VALUE "CONSUMER TRANSACTION TYPE     ".
           03  FILLER PIC X(30) VALUE "TRW-B-CONSUMER-TRAN-TYPE      ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SURNAME                       ".
           03  FILLER PIC X(30) VALUE "TRW-B-LNAME                   ".
           03  FILLER PIC X(07) VALUE "X(0025)".

           03  FILLER PIC X(50) VALUE "FIRST NAME                    ".
           03  FILLER PIC X(30) VALUE "TRW-B-FNAME                   ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "MIDDLE NAME                   ".
           03  FILLER PIC X(30) VALUE "TRW-B-MNAME                   ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "GENERATION CODE               ".
           03  FILLER PIC X(30) VALUE "TRW-B-GENERATION              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SOCIAL SECURITY NUMBER        ".
           03  FILLER PIC X(30) VALUE "TRW-B-SSNO                    ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "DATE OF BIRTH                 ".
           03  FILLER PIC X(30) VALUE "TRW-B-BDATE                   ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "TELEPHONE NUMBER              ".
           03  FILLER PIC X(30) VALUE "TRW-B-PHONE                   ".
           03  FILLER PIC X(07) VALUE "9(0010)".

           03  FILLER PIC X(50) VALUE "ECOA CODE                     ".
           03  FILLER PIC X(30) VALUE "TRW-B-ECOA-CODE               ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "CONSUMER INFORMATION INDICATOR".
           03  FILLER PIC X(30) VALUE "TRW-B-CONSUMER-INFO-IND       ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "COUNTRY CODE                  ".
           03  FILLER PIC X(30) VALUE "TRW-B-COUNTRY                 ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "FIRST LINE OF ADDRESS         ".
           03  FILLER PIC X(30) VALUE "TRW-B-ADR1                    ".
           03  FILLER PIC X(07) VALUE "X(0032)".

           03  FILLER PIC X(50) VALUE "SECOND LINE OF ADDRESS        ".
           03  FILLER PIC X(30) VALUE "TRW-B-ADR2                    ".
           03  FILLER PIC X(07) VALUE "X(0032)".

           03  FILLER PIC X(50) VALUE "CITY                          ".
           03  FILLER PIC X(30) VALUE "TRW-B-CITY                    ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "STATE                         ".
           03  FILLER PIC X(30) VALUE "TRW-B-STATE                   ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "POSTAL/ZIP CODE               ".
           03  FILLER PIC X(30) VALUE "TRW-B-ZIP                     ".
           03  FILLER PIC X(07) VALUE "X(0009)".

           03  FILLER PIC X(50) VALUE "ADDRESS INDICATOR             ".
           03  FILLER PIC X(30) VALUE "TRW-B-ADD-IND                 ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "RESIDENCE CODE                ".
           03  FILLER PIC X(30) VALUE "TRW-B-RESIDENCE               ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-J1-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "CONSUMER TRANSACTION TYPE     ".
           03  FILLER PIC X(30) VALUE "TRW-J1-CONSUMER-TRAN-TYPE     ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SURNAME                       ".
           03  FILLER PIC X(30) VALUE "TRW-J1-LNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0025)".

           03  FILLER PIC X(50) VALUE "FIRST NAME                    ".
           03  FILLER PIC X(30) VALUE "TRW-J1-FNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "MIDDLE NAME                   ".
           03  FILLER PIC X(30) VALUE "TRW-J1-MNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "GENERATION CODE               ".
           03  FILLER PIC X(30) VALUE "TRW-J1-GENERATION             ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SOCIAL SECURITY NUMBER        ".
           03  FILLER PIC X(30) VALUE "TRW-J1-SOCSEC                 ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "DATE OF BIRTH                 ".
           03  FILLER PIC X(30) VALUE "TRW-J1-BDATE                  ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "PHONE                         ".
           03  FILLER PIC X(30) VALUE "TRW-J1-PHONE                  ".
           03  FILLER PIC X(07) VALUE "9(0010)".

           03  FILLER PIC X(50) VALUE "ECOA CODE                     ".
           03  FILLER PIC X(30) VALUE "TRW-J1-ECOA-CODE              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "CONSUMER INFORMATION INDICATOR".
           03  FILLER PIC X(30) VALUE "TRW-J1-CONSUMER-INFO-IND      ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-J1-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "CONSUMER TRANSACTION TYPE     ".
           03  FILLER PIC X(30) VALUE "TRW-J1-CONSUMER-TRAN-TYPE     ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SURNAME                       ".
           03  FILLER PIC X(30) VALUE "TRW-J1-LNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0025)".

           03  FILLER PIC X(50) VALUE "FIRST NAME                    ".
           03  FILLER PIC X(30) VALUE "TRW-J1-FNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "MIDDLE NAME                   ".
           03  FILLER PIC X(30) VALUE "TRW-J1-MNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "GENERATION CODE               ".
           03  FILLER PIC X(30) VALUE "TRW-J1-GENERATION             ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SOCIAL SECURITY NUMBER        ".
           03  FILLER PIC X(30) VALUE "TRW-J1-SOCSEC                 ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "DATE OF BIRTH                 ".
           03  FILLER PIC X(30) VALUE "TRW-J1-BDATE                  ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "PHONE                         ".
           03  FILLER PIC X(30) VALUE "TRW-J1-PHONE                  ".
           03  FILLER PIC X(07) VALUE "9(0010)".

           03  FILLER PIC X(50) VALUE "ECOA CODE                     ".
           03  FILLER PIC X(30) VALUE "TRW-J1-ECOA-CODE              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "CONSUMER INFORMATION INDICATOR".
           03  FILLER PIC X(30) VALUE "TRW-J1-CONSUMER-INFO-IND      ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "CONSUMER TRANSACTION TYPE     ".
           03  FILLER PIC X(30) VALUE "TRW-J2-CONSUMER-TRAN-TYPE     ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SURNAME                       ".
           03  FILLER PIC X(30) VALUE "TRW-J2-LNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0025)".

           03  FILLER PIC X(50) VALUE "FIRST NAME                    ".
           03  FILLER PIC X(30) VALUE "TRW-J2-FNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "MIDDLE NAME                   ".
           03  FILLER PIC X(30) VALUE "TRW-J2-MNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "GENERATION CODE               ".
           03  FILLER PIC X(30) VALUE "TRW-J2-GENERATION             ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SOCIAL SECURITY NUMBER        ".
           03  FILLER PIC X(30) VALUE "TRW-J2-SOCSEC                 ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "DATE OF BIRTH                 ".
           03  FILLER PIC X(30) VALUE "TRW-J2-BDATE                  ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "PHONE                         ".
           03  FILLER PIC X(30) VALUE "TRW-J2-PHONE                  ".
           03  FILLER PIC X(07) VALUE "9(0010)".

           03  FILLER PIC X(50) VALUE "ECOA CODE                     ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ECOA-CODE              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "CONSUMER INFORMATION INDICATOR".
           03  FILLER PIC X(30) VALUE "TRW-J2-CONSUMER-INFO-IND      ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "COUNTRY                       ".
           03  FILLER PIC X(30) VALUE "TRW-J2-COUNTRY                ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "FIRST LINE OF ADDRESS         ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ADR1                   ".
           03  FILLER PIC X(07) VALUE "X(0032)".

           03  FILLER PIC X(50) VALUE "SECOND LINE OF ADDRESS        ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ADR2                   ".
           03  FILLER PIC X(07) VALUE "X(0032)".

           03  FILLER PIC X(50) VALUE "CITY                          ".
           03  FILLER PIC X(30) VALUE "TRW-J2-CITY                   ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "STATE                         ".
           03  FILLER PIC X(30) VALUE "TRW-J2-STATE                  ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "POSTAL/ZIP CODE               ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ZIP                    ".
           03  FILLER PIC X(07) VALUE "X(0009)".

           03  FILLER PIC X(50) VALUE "ADDRESS INDICATOR             ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ADD-IND                ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "RESIDENCE CODE                ".
           03  FILLER PIC X(30) VALUE "TRW-J2-RESIDENCE              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "CONSUMER TRANSACTION TYPE     ".
           03  FILLER PIC X(30) VALUE "TRW-J2-CONSUMER-TRAN-TYPE     ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SURNAME                       ".
           03  FILLER PIC X(30) VALUE "TRW-J2-LNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0025)".

           03  FILLER PIC X(50) VALUE "FIRST NAME                    ".
           03  FILLER PIC X(30) VALUE "TRW-J2-FNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "MIDDLE NAME                   ".
           03  FILLER PIC X(30) VALUE "TRW-J2-MNAME                  ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "GENERATION CODE               ".
           03  FILLER PIC X(30) VALUE "TRW-J2-GENERATION             ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SOCIAL SECURITY NUMBER        ".
           03  FILLER PIC X(30) VALUE "TRW-J2-SOCSEC                 ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "DATE OF BIRTH                 ".
           03  FILLER PIC X(30) VALUE "TRW-J2-BDATE                  ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "PHONE                         ".
           03  FILLER PIC X(30) VALUE "TRW-J2-PHONE                  ".
           03  FILLER PIC X(07) VALUE "9(0010)".

           03  FILLER PIC X(50) VALUE "ECOA CODE                     ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ECOA-CODE              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "CONSUMER INFORMATION INDICATOR".
           03  FILLER PIC X(30) VALUE "TRW-J2-CONSUMER-INFO-IND      ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "COUNTRY                       ".
           03  FILLER PIC X(30) VALUE "TRW-J2-COUNTRY                ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "FIRST LINE OF ADDRESS         ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ADR1                   ".
           03  FILLER PIC X(07) VALUE "X(0032)".

           03  FILLER PIC X(50) VALUE "SECOND LINE OF ADDRESS        ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ADR2                   ".
           03  FILLER PIC X(07) VALUE "X(0032)".

           03  FILLER PIC X(50) VALUE "CITY                          ".
           03  FILLER PIC X(30) VALUE "TRW-J2-CITY                   ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "STATE                         ".
           03  FILLER PIC X(30) VALUE "TRW-J2-STATE                  ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "POSTAL/ZIP CODE               ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ZIP                    ".
           03  FILLER PIC X(07) VALUE "X(0009)".

           03  FILLER PIC X(50) VALUE "ADDRESS INDICATOR             ".
           03  FILLER PIC X(30) VALUE "TRW-J2-ADD-IND                ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "RESIDENCE CODE                ".
           03  FILLER PIC X(30) VALUE "TRW-J2-RESIDENCE              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-K1-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "ORIGINAL CREDITOR NAME        ".
           03  FILLER PIC X(30) VALUE "TRW-K1-CREDITOR               ".
           03  FILLER PIC X(07) VALUE "X(0030)".

           03  FILLER PIC X(50) VALUE "CREDITOR CLASSIFICATION       ".
           03  FILLER PIC X(30) VALUE "TRW-K1-CLASSIFICATION         ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-K2-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "PORTFOLIO INDICATOR           ".
           03  FILLER PIC X(30) VALUE "TRW-K2-INDICATOR              ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE
                  "PURCHASED PORTFOLIO OR SOLD TO NAME               ".
           03  FILLER PIC X(30) VALUE "TRW-K2-NAME                   ".
           03  FILLER PIC X(07) VALUE "X(0030)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-K3-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "AGENCY IDENTIFIER             ".
           03  FILLER PIC X(30) VALUE "TRW-K3-IDENT                  ".
           03  FILLER PIC X(07) VALUE "9(0002)".

           03  FILLER PIC X(50) VALUE "ACCOUNT NUMBER                ".
           03  FILLER PIC X(30) VALUE "TRW-K3-ACCOUNT                ".
           03  FILLER PIC X(07) VALUE "X(0018)".

           03  FILLER PIC X(50) VALUE
                  "MORTGAGE INDENTIFICATION NUMBER (MIN)             ".
           03  FILLER PIC X(30) VALUE "TRW-K3-MIN                    ".
           03  FILLER PIC X(07) VALUE "X(0018)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-K4-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "SPECIALIZED PAYMENT INDICATOR ".
           03  FILLER PIC X(30) VALUE "TRW-K4-INDICATOR              ".
           03  FILLER PIC X(07) VALUE "9(0002)".

           03  FILLER PIC X(50) VALUE "DEFERRED PAYMENT START DATE   ".
           03  FILLER PIC X(30) VALUE "TRW-K4-START-DATE             ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "PAYMENT DUE DATE              ".
           03  FILLER PIC X(30) VALUE "TRW-K4-DUE-DATE               ".
           03  FILLER PIC X(07) VALUE "9(0008)".

           03  FILLER PIC X(50) VALUE "PAYMENT AMOUNT                ".
           03  FILLER PIC X(30) VALUE "TRW-K4-PAY-AMT                ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-L1-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "CHANGE INDICATOR              ".
           03  FILLER PIC X(30) VALUE "TRW-L1-CHANGE-IND             ".
           03  FILLER PIC X(07) VALUE "9(0001)".

           03  FILLER PIC X(50) VALUE
                  "NEW CONSUMER ACCOUNT NUMBER {1}                   ".
           03  FILLER PIC X(30) VALUE "TRW-L1-NEW-ACCTNO1            ".
           03  FILLER PIC X(07) VALUE "9(0015)".

           03  FILLER PIC X(50) VALUE
                  "NEW CONSUMER ACCOUNT NUMBER {2}                   ".
           03  FILLER PIC X(30) VALUE "TRW-L1-NEW-ACCTNO2            ".
           03  FILLER PIC X(07) VALUE "9(0015)".

           03  FILLER PIC X(50) VALUE "NEW IDENTIFICATION NUMBER     ".
           03  FILLER PIC X(30) VALUE "TRW-L1-NEW-IDENT              ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0001)".

           03  FILLER PIC X(50) VALUE "SEGMENT IDENTIFIER            ".
           03  FILLER PIC X(30) VALUE "TRW-N1-ID                     ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "EMPLOYER NAME                 ".
           03  FILLER PIC X(30) VALUE "TRW-N1-EMP-NAME               ".
           03  FILLER PIC X(07) VALUE "X(0030)".

           03  FILLER PIC X(50) VALUE "FIRST LINE OF EMPLOYER ADDRESS".
           03  FILLER PIC X(30) VALUE "TRW-N1-EMP-ADR1               ".
           03  FILLER PIC X(07) VALUE "X(0032)".

           03  FILLER PIC X(50) VALUE
                  "SECOND LINE OF EMPLOYER ADDRESS                   ".
           03  FILLER PIC X(30) VALUE "TRW-N1-EMP-ADR2               ".
           03  FILLER PIC X(07) VALUE "X(0032)".

           03  FILLER PIC X(50) VALUE "EMPLOYER CITY                 ".
           03  FILLER PIC X(30) VALUE "TRW-N1-EMP-CITY               ".
           03  FILLER PIC X(07) VALUE "X(0020)".

           03  FILLER PIC X(50) VALUE "EMPLOYER STATE                ".
           03  FILLER PIC X(30) VALUE "TRW-N1-EMP-STATE              ".
           03  FILLER PIC X(07) VALUE "X(0002)".

           03  FILLER PIC X(50) VALUE "EMPLOYER ZIPCODE              ".
           03  FILLER PIC X(30) VALUE "TRW-N1-EMP-ZIPCODE            ".
           03  FILLER PIC X(07) VALUE "X(0009)".

           03  FILLER PIC X(50) VALUE "OCCUPATION                    ".
           03  FILLER PIC X(30) VALUE "TRW-N1-OCCUPATION             ".
           03  FILLER PIC X(07) VALUE "X(0018)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0001)".

      *=================================================================
       01  FPW-TRAILER-REC.

           03  FILLER PIC X(50) VALUE "RECORD DESCRIPTOR WORD (RDW)  ".
           03  FILLER PIC X(30) VALUE "TRW-T-RDW                     ".
           03  FILLER PIC X(07) VALUE "9(0004)".

           03  FILLER PIC X(50) VALUE "RECORD IDENTIFIER             ".
           03  FILLER PIC X(30) VALUE "TRW-T-ID                      ".
           03  FILLER PIC X(07) VALUE "X(0007)".

           03  FILLER PIC X(50) VALUE "TOTAL BASE RECORDS            ".
           03  FILLER PIC X(30) VALUE "TRW-T-RECORDS                 ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0009)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL ASSOCIATED CONSUMER SEGMENTS (J1)           ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-J1                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL ASSOCIATED CONSUMER SEGMENTS (J2)           ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-J2                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "BLOCK COUNT                   ".
           03  FILLER PIC X(30) VALUE "TRW-T-BLOCKS                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE 'DA'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-DA                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '05'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-05                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '11'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-11                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '13'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-13                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '61'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-61                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '62'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-62                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '63'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-63                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '64'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-64                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '65'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-65                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '71'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-71                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '78'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-78                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '80'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-80                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '82'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-82                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '83'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-83                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '84'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-84                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '88'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-88                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '89'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-89                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '93'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-93                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '94'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-94                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '95'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-95                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '96'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-96                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF STATUS CODE '97'     ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-97                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL OF ECOA CODE 'Z'        ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-ECOA-Z              ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL OF EMPLOYMENT SEGMENTS  'N1'                ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-N1                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL ORIGINAL CREDITOR SEGMENTS 'K1'             ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-K1                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL PURCHASED PORTFOLIO/SOLD TO SEGMENTS 'K2'   ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-K2                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL MORTGAGE INFORMATION SEGMENTS 'K3'          ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-K3                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL SPECIALIZED PAYMENT INFO SEGMENTS 'K4'      ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-K4                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "TOTAL CHANGE SEGMENTS 'L1'    ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-L1                  ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL SOCIAL SECURITY NUMBERS ALL SEGMENTS        ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-SSNO-ALL            ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL SOCIAL SECURITY NUMBERS BASE SEGMENTS       ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-SSNO-BASE           ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL SOCIAL SECURITY NUMBERS J1 SEGMENTS         ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-SSNO-J1             ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL SOCIAL SECURITY NUMBERS J2 SEGMENTS         ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-SSNO-J2             ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL DATES OF BIRTH ALL SEGMENTS                 ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-BDATE-ALL           ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL DATES OF BIRTH BASE SEGMENTS                ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-BDATE-BASE          ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL DATES OF BIRTH J1 SEGMENTS                  ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-BDATE-J1            ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL DATES OF BIRTH J2 SEGMENTS                  ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-BDATE-J2            ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE
                  "TOTAL TELEPHONE NUMBERS ALL SEGMENTS              ".
           03  FILLER PIC X(30) VALUE "TRW-T-TOT-PHONE               ".
           03  FILLER PIC X(07) VALUE "9(0009)".

           03  FILLER PIC X(50) VALUE "RESERVED                      ".
           03  FILLER PIC X(30) VALUE "FILLER                        ".
           03  FILLER PIC X(07) VALUE "X(0957)".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  FIRST-TIME                  PIC X(01)     VALUE "Y".

       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

       01  ACP-ACCTNO                  PIC 9(8).
       01  FILLER                      REDEFINES ACP-ACCTNO.
           03  ACP-NUMBER              PIC 9(06).
           03  ACP-SEQNO               PIC 9(02).

       01  ACP-TRW-B-ACCTNO            PIC 9(15).
       01  ACP-TRW-B-WORLD-REDEF       REDEFINES ACP-TRW-B-ACCTNO.
           03  FILLER                  PIC 9(03).
           03  ACP-TRW-B-W-BRNO        PIC 9(04).
           03  ACP-TRW-B-W-ACCT        PIC 9(08).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  ACP-STAT                    PIC X(02)   VALUE SPACES.
           88  ACP-STAT-GOOD                       VALUE "00".
           88  ACP-STAT-BAD                        VALUE "  " THRU "//"
                                                         "01" THRU "~~".

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND-ENTER-PRT-BASE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-KEY, ENT".
           03  FILLER PIC X(30) VALUE "ER (Y)ES OR (N)O FOR PRINT OF ".
           03  FILLER PIC X(20) VALUE "BASE RECORDS        ".

       01  LEGEND-ENTER-PRT-HEAD.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-KEY, ENT".
           03  FILLER PIC X(30) VALUE "ER (Y)ES OR (N)O FOR PRINT OF ".
           03  FILLER PIC X(20) VALUE "HEADER RECORD       ".

       01  LEGEND-ENTER-PRT-TAIL.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-KEY, ENT".
           03  FILLER PIC X(30) VALUE "ER (Y)ES OR (N)O FOR PRINT OF ".
           03  FILLER PIC X(20) VALUE "TRAILER RECORD      ".

       01  LEGEND-ENTER-BRNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BRANCH NUMBER              ".
           03  FILLER PIC X(20) VALUE "                    ".


       01  LEGEND-ENTER-BEG-ACCT.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING ACCOUNT NUMBER   ".
           03  FILLER PIC X(20) VALUE "                    ".

       01  LEGEND-ENTER-END-ACCT.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-KEY, ENT".
           03  FILLER PIC X(30) VALUE "ER ENDING ACCOUNT NUMBER      ".
           03  FILLER PIC X(20) VALUE "                    ".

       01  LEGEND-ENTER-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-KEY, ENT".
           03  FILLER PIC X(30) VALUE "ER (Y)ES OR (N)O TO ACCEPT PAR".
           03  FILLER PIC X(20) VALUE "AMETERS             ".

       01  BUREAU-ID               PIC 9         VALUE 1.
       01  PRT-BASE                PIC X(01)     VALUE SPACES.
       01  PRT-HEAD                PIC X(01)     VALUE SPACES.
       01  PRT-TAIL                PIC X(01)     VALUE SPACES.
       01  BEG-ACCT                PIC 9(06)     VALUE ZEROES.
       01  END-ACCT                PIC 9(06)     VALUE ZEROES.
       01  BRNO                    PIC 9(4)      VALUE ZEROES.

      ******************************************************************
      *
      *    S C R E E N - F I E L D S
      *
      ******************************************************************
       01  ERRCD                   PIC X(01).

      *-----------------------------------------------------------------
      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT               PIC 9(03)     VALUE 99.
           03  LN-COUNT-ESTIMATE      PIC 9(03)     VALUE ZEROES.

           03  LN-FEED                PIC 9(02)     VALUE ZEROES.

           03  PAGE-NUMBER            PIC 9(04)     VALUE ZEROES.

      *=================================================================
      *
      *    H E A D E R - W O R K E R S
      *
      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H01-HR                  PIC 9(02)     VALUE 0.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H01-MIN                 PIC 9(02)     VALUE 0.
           03  FILLER                  PIC X(05)     VALUE SPACES.
           03  H01-NAME                PIC X(92)     VALUE SPACES.
           03  FILLER                  PIC X(10)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H01-PAGE-NO             PIC ZZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10)     VALUE "USER ID :".
           03  H02-USERID              PIC X(10)     VALUE SPACES.
           03  FILLER PIC X(30) VALUE "                ----- TRW (MET".
           03  FILLER PIC X(30) VALUE "RO 2) TAPE FILE LISTING ----- ".
           03  FILLER PIC X(30) VALUE "    *** C O N F I D E N T I A ".
           03  FILLER PIC X(22) VALUE "L ***                 ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER                  PIC X(16)     VALUE SPACES.
           03  H03-TYPE                PIC X(100)    VALUE SPACES.
           03  FILLER                  PIC X(16)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-04.
           03  FILLER PIC X(30) VALUE "NO.  BEG-END  SIZE FIELD      ".
           03  FILLER PIC X(30) VALUE "                    DESCRIPTIO".
           03  FILLER PIC X(30) VALUE "N                             ".
           03  FILLER PIC X(30) VALUE "           INFORMATION        ".
           03  FILLER PIC X(12) VALUE "            ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-05.
           03  FILLER PIC X(30) VALUE "==============================".
           03  FILLER PIC X(30) VALUE "==============================".
           03  FILLER PIC X(30) VALUE "==============================".
           03  FILLER PIC X(30) VALUE "=========[ .........1.........".
           03  FILLER PIC X(12) VALUE "2.........3 ".

      *=================================================================
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      *=================================================================
       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01              REDEFINES DETAIL-LINE.
           03  D01-NUMBER              PIC 9(03).
           03  FILLER                  PIC X(01).
           03  D01-BEG                 PIC 9(04).
           03  D01-DASH                PIC X(01).
           03  D01-END                 PIC 9(04).
           03  FILLER                  PIC X(01).
           03  D01-SIZE                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  D01-FIELD               PIC X(30).
           03  FILLER                  PIC X(01).
           03  D01-TEXT                PIC X(50).
           03  FILLER                  PIC X(01).
           03  D01-INFO                PIC X(30).
           03  FILLER                  PIC X(01).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02              REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(32).
           03  D02-INFO                PIC X(100).

      *-----------------------------------------------------------------
       01  RANGE-LINE                  REDEFINES DETAIL-LINE.
           03  R01-PROMPT              PIC X(35).
           03  R01-TEXT                PIC X(97).

      *=================================================================
       01  DETAIL-LINE-INFO            PIC X(100).
       01  FILLER                      REDEFINES DETAIL-LINE-INFO.
           03  DETAIL-LINE-INFO-30     PIC X(30).
           03  DETAIL-LINE-INFO-70     PIC X(70).

       01  DETAIL-LINE-GUIDE.
           03  FILLER PIC X(30) VALUE ".........1.........2.........3".
           03  FILLER PIC X(30) VALUE ".........4.........5.........6".
           03  FILLER PIC X(30) VALUE ".........7.........8.........9".
           03  FILLER PIC X(10) VALUE ".........*".

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       01  MET2LS-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/MET2LS_DEF.CPY".
       COPY "LIBSP/MET2LS_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/MET2LS_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 TRW-FILE TRW-E-FILE PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           CLOSE TRW-FILE TRW-E-FILE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.
      D    STOP MESS.

           IF ( ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           PERFORM EXECUTION-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MET2LS" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS MET2LS-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM ENTER-PARAMETERS.
           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-END.

           MOVE "LISTING IN PROGRESS........................" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.

           MOVE "X" TO ERRCD.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           MOVE SPACES TO ERRCD.

       ENTER-PRINT-OPTIONS.

           MOVE MET2LS-QUEUE-POS TO Q-POS.
           MOVE MET2LS-COPIES-POS TO C-POS.
           MOVE MET2LS-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.

           IF ( NOT STAT-VALID )
              GO TO ENTER-PARAMETERS-END.

           MOVE 1 TO BUREAU-ID.
           GO TO ENTER-PRT-HEAD.

       ENTER-BUREAU-ID.
           MOVE "EN N010IDNO." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) GO TO ENTER-PRINT-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BUREAU-ID.

           IF VDUNUM0 = 0
              GO TO ENTER-BUREAU-ID.

           MOVE VDUNUM0 TO BUREAU-ID.

      *=================================================================
       ENTER-PRT-HEAD.

           MOVE LEGEND-ENTER-PRT-HEAD TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-HEAD." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PRINT-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        UP-KEY ) GO TO ENTER-BUREAU-ID.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRT-HEAD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRT-HEAD.

           IF NOT ( PRT-HEAD = "Y" OR "N" )
              MOVE "INVALID: (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRT-HEAD.

      *=================================================================
       ENTER-PRT-TAIL.

           MOVE LEGEND-ENTER-PRT-TAIL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-TAIL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRT-HEAD.
           IF (        F1-KEY ) GO TO ENTER-PRINT-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRT-TAIL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRT-TAIL.

           IF NOT ( PRT-TAIL = "Y" OR "N" )
              MOVE "INVALID: (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRT-TAIL.

      *=================================================================
       ENTER-PRT-BASE.

           MOVE LEGEND-ENTER-PRT-BASE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-BASE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRT-TAIL.
           IF (        F1-KEY ) GO TO ENTER-PRINT-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRT-BASE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRT-BASE.

           IF NOT ( PRT-BASE = "Y" OR "N" )
              MOVE "INVALID: (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRT-BASE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PRT-HEAD = "N" ) AND
              ( PRT-TAIL = "N" ) AND
              ( PRT-BASE = "N" )
              MOVE "MUST SELECT AT LEAST ONE OPTION TO PRINT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRT-HEAD.

           IF ( PRT-BASE = "N" )
              GO TO ENTER-ACCEPT-YN.

      *=================================================================
       ENTER-BRANCH.

           MOVE LEGEND-ENTER-BRNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRT-BASE.
           IF (        F1-KEY ) GO TO ENTER-PRINT-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BRNO.

           GO TO ENTER-BEG-ACCT.

       ENTER-ALL-BRNO.

           MOVE "SNNN040BRNO." TO VDU.
           MOVE ZEROES         TO VDUNUM0 BRNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCT.

           MOVE LEGEND-ENTER-BEG-ACCT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-ACCT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BRANCH
           IF (        F1-KEY ) GO TO ENTER-PRINT-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCT.

      *-----------------------------------------------------------------
       ENTER-END-ACCT.

           MOVE LEGEND-ENTER-END-ACCT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-ACCT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCT.
           IF (        F1-KEY ) GO TO ENTER-PRINT-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-ACCT.

           IF ( BEG-ACCT > END-ACCT )
              MOVE "INVALID ACCOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCT.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCT.

           MOVE "SNNN060BEG-ACCT." TO VDU.
           MOVE ZEROES             TO VDUNUM0 BEG-ACCT.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCT." TO VDU.
           MOVE 999999             TO VDUNUM0 END-ACCT.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ENTER-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRT-BASE.
           IF (        F1-KEY ) GO TO ENTER-PRINT-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = "Y" ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( VDUBUF = "N" ) GO TO ENTER-PRINT-OPTIONS.

           GO TO ENTER-ACCEPT-YN.

      *=================================================================
       ENTER-PARAMETERS-END.

           MOVE "X" TO ERRCD.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       EXECUTION-MODULE SECTION.
      ******************************************************************

           MOVE "Y"    TO FIRST-TIME.
           MOVE 9999   TO PROGID-SLEEP-COUNTER.

           MOVE BUREAU-ID TO TRW-INFO-ID.
           PERFORM OPEN-TRW-INFO-FILE.
           PERFORM READ-TRW-INFO-FILE.
           PERFORM CLOSE-TRW-INFO-FILE.
           IF IO-FG NOT = "0"
               MOVE SPACES TO TRW-INFO-CR-LF.

           MOVE ZEROES TO TRW-KEY
                          TRW-E-KEY.

           MOVE EXT-FILPATH-BASE TO TRW-PATH-BASE
                                    TRW-PATH-2-BASE
                                    TRW-E-PATH-BASE
                                    TRW-E2-PATH-BASE
                                    TRW-WORLD-I-PATH-BASE.

           MOVE EXT-FILPATH-MACHINE TO TRW-PATH-MACHINE
                                       TRW-PATH-2-MACHINE
                                       TRW-E-PATH-MACHINE
                                       TRW-E2-PATH-MACHINE
                                       TRW-WORLD-I-PATH-MACHINE.

           IF BUREAU-ID NOT = 1
              MOVE BUREAU-ID TO TRW-E2-PATH-ID TRW-PATH-ID
              IF TRW-INFO-CR-LF = "Y"
                 MOVE TRW-E2-PATH TO TRW-E-PATH
              ELSE
                 MOVE TRW-PATH-2 TO TRW-PATH.

           MOVE TRW-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE TRW-E-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 GO TO EXECUTION-MODULE-END.

           IF TRW-INFO-CR-LF = "Y" 
              PERFORM OPEN-TRW-E-FILE
           ELSE
              PERFORM OPEN-TRW-FILE.

       EXECUTION-NEXT-TRW.

           ADD 1 TO TRW-KEY
                    TRW-E-KEY.

           IF TRW-INFO-CR-LF = "Y"
              PERFORM READ-TRW-E-FILE
           ELSE
              PERFORM READ-TRW-FILE.

           IF ( IO-BAD )
              GO TO EXECUTION-MODULE-END.

           PERFORM SEND-PROGRESS-INDICATOR.

           IF TRW-INFO-CR-LF = "Y"
              MOVE TRW-E-REC TO TRW-REC.

      *- - - - - - - - - - - - - - - - - - - -{ ANALYZE CURRENT RECORD }
           PERFORM ACP-DRIVER.

           IF ( ACP-STAT-BAD )
              GO TO EXECUTION-NEXT-TRW.

           MOVE "N" TO FIRST-TIME.
           PERFORM FPW-DRIVER.

           GO TO EXECUTION-NEXT-TRW.

       EXECUTION-MODULE-END.

           IF ( FIRST-TIME = "Y" )
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99     TO LN-FEED
              PERFORM WRITE-PR-REC.

           MOVE 99 TO LN-FEED.
           PERFORM PRINT-RANGE-LINES.

           IF TRW-INFO-CR-LF = "Y"
              PERFORM CLOSE-TRW-E-FILE
           ELSE
              PERFORM CLOSE-TRW-FILE.

       EXECUTION-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    A C P - D R I V E R                                     {ACP}
      *
      *=================================================================
      *    IN  : BEG-ACCT
      *          END-ACCT
      *          BRNO
      *          TRW-REC
      *
      *    OUT : ACP-ACCTNO
      *          ACP-STAT  {BAD/GOOD}
      *
      *    DESC: 'ANALYZE-CURRENT-RECORD'
      *          ANALYZE CURRENT RECORD TO SEE IF IT IS GOOD TO PRINT.
      *
      ******************************************************************
       ACP-DRIVER SECTION.

           MOVE STAT---GOOD                TO ACP-STAT.

           IF (( TRW-H-ID = "HEADER"  ) AND ( PRT-HEAD = "N" )) OR
              (( TRW-T-ID = "TRAILER" ) AND ( PRT-TAIL = "N" ))
              MOVE STAT---BAD              TO ACP-STAT.

      * BASE RECORD

           IF ( TRW-H-ID = "HEADER"  ) OR
              ( TRW-T-ID = "TRAILER" )
              GO TO ACP-DRIVER-EXIT.

           IF ( PRT-BASE = "N" )
              MOVE STAT---BAD              TO ACP-STAT
              GO TO ACP-DRIVER-EXIT.

      * NOW CHECK IF THE BRANCH IS INCLUDED
           IF TRW-B-W-BRNO NOT = 0
              MOVE TRW-B-W-ACCT TO ACP-ACCTNO
           ELSE
              MOVE TRW-B-ACCTNO TO ACP-ACCTNO.

           IF ( ACP-NUMBER < BEG-ACCT ) OR
              ( ACP-NUMBER > END-ACCT )
              MOVE STAT---BAD              TO ACP-STAT.

           IF BRNO NOT = 0
              IF TRW-B-W-BRNO NOT = BRNO
                 MOVE STAT---BAD           TO ACP-STAT.

       ACP-DRIVER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F P W - D R I V E R
      *
      *=================================================================
      *    IN  :  TRW-REC
      *
      *    OUT :  NONE
      *
      *    DESC:  'FILE-PRINT-WORKERS'.
      *           USED TO PRINT A LISTING ON A GIVEN TRW-REC.
      *
      ******************************************************************
       FPW-DRIVER SECTION.

           MOVE 99                         TO LN-COUNT.

           MOVE ZEROES                     TO FPW-BYTE
                                              FPW-BYTE-BEG
                                              FPW-BYTE-END.

           IF ( TRW-H-ID = "HEADER" )
              MOVE FPW---TYPE-HEADER       TO FPW-TYPE
              MOVE FPW-HEADER-REC          TO FPW-REC
           ELSE
           IF ( TRW-T-ID = "TRAILER" )
              MOVE FPW---TYPE-TRAILER      TO FPW-TYPE
              MOVE FPW-TRAILER-REC         TO FPW-REC
           ELSE
              MOVE FPW---TYPE-BASE         TO FPW-TYPE
              MOVE FPW-BASE-REC            TO FPW-REC.

           MOVE ZEROES                     TO FPW-ELE.

       FPW-DRIVER-LOOP.

           ADD 01 TO FPW-ELE.

           IF ( FPW-ELE > FPW-MAX )
              GO TO FPW-DRIVER-END.

           IF ( FPASSWDW-TEXT  (FPW-ELE) = SPACES ) OR
              ( FPASSWDW-FIELD (FPW-ELE) = SPACES ) OR
              ( FPASSWDW-SIZE  (FPW-ELE) = SPACES )
              GO TO FPW-DRIVER-END.

           IF ( FPASSWDW-SIZE-NUMBER(FPW-ELE) NOT NUMERIC )
              GO TO FPW-DRIVER-END.

           MOVE FPASSWDW-SIZE-NUMBER(FPW-ELE) TO FPW-SIZE.

           COMPUTE FPW-BYTE-BEG = FPW-BYTE + 1.
           COMPUTE FPW-BYTE-END = FPW-BYTE + FPW-SIZE.

           MOVE FPW-BYTE-END               TO FPW-BYTE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE FPW-ELE                    TO D01-NUMBER.
           MOVE FPW-BYTE-BEG               TO D01-BEG.
           MOVE "-"                        TO D01-DASH.
           MOVE FPW-BYTE-END               TO D01-END.
           MOVE FPW-SIZE                   TO D01-SIZE.
           MOVE FPASSWDW-FIELD    (FPW-ELE) TO D01-FIELD.
           MOVE FPASSWDW-TEXT     (FPW-ELE) TO D01-TEXT.

           MOVE TRW-REC(FPW-BYTE-BEG:FPW-SIZE)
                                           TO DETAIL-LINE-INFO.

           IF ( DETAIL-LINE-INFO-70 = SPACES )
              MOVE DETAIL-LINE-INFO        TO D01-INFO.

           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - -{ CHECK TO SKIP LINE 02  }
           IF ( DETAIL-LINE-INFO-70 = SPACES )
              GO TO FPW-DRIVER-LOOP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           ADD  02  LN-COUNT              TO LN-COUNT-ESTIMATE.

           IF ( LN-COUNT-ESTIMATE > EXT-CONAME-RPT-LINES )
              MOVE 99                      TO LN-COUNT.

           MOVE DETAIL-LINE-INFO           TO D02-INFO.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL-LINE-GUIDE          TO D02-INFO.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO FPW-DRIVER-LOOP.

      *=================================================================
       FPW-DRIVER-END.

           MOVE SPACES                     TO FPW-TYPE.

       FPW-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE 99 TO LN-COUNT.

           MOVE SPACES                                TO DETAIL-LINE.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BUREAU ID NUMBER                 : " TO R01-PROMPT.
           MOVE BUREAU-ID                             TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT 'HEADER'  RECORD      (Y/N): " TO R01-PROMPT.
           MOVE PRT-HEAD                              TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT 'TRAILER' RECORD      (Y/N): " TO R01-PROMPT.
           MOVE PRT-TAIL                              TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT 'BASE'    RECORD      (Y/N): " TO R01-PROMPT.
           MOVE PRT-BASE                              TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BRANCH NUMBER                    : " TO R01-PROMPT.
           MOVE BRNO                                  TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING ACCOUNT NUMBER         : " TO R01-PROMPT.
           MOVE BEG-ACCT                              TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCOUNT NUMBER         : " TO R01-PROMPT.
           MOVE END-ACCT                              TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-RANGE-LINES-EXIT.
           EXIT.

      ******************************************************************
       SEND-PROGRESS-INDICATOR SECTION.
      ******************************************************************

           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE SPACES                 TO PROGID-ROUTE-BUF.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "S"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

           PERFORM PROGID-DISPLAY.

       SEND-PROGID-INDICATOR-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD  LN-FEED                   TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-PAGE-HEADER SECTION.
      ******************************************************************

           ADD  01                         TO PAGE-NUMBER.
           PERFORM GET-TIME.

           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE T-HH                       TO H01-HR.
           MOVE T-MM                       TO H01-MIN.
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           CALL "C$JUSTIFY"             USING H01-NAME "C".
           MOVE PAGE-NUMBER                TO H01-PAGE-NO.
           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID         TO H02-USERID.
           MOVE HEADER-LINE-02             TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES                     TO H03-TYPE.

           IF ( FPW-TYPE-HEADER )
              STRING "********************",
                     " H E A D E R - R E C O R D ",
                     "********************"
                     DELIMITED BY "  "   INTO H03-TYPE
           ELSE
           IF ( FPW-TYPE-TRAILER )
              STRING "********************",
                     " T R A I L E R - R E C O R D ",
                     "********************"
                     DELIMITED BY "  "   INTO H03-TYPE
           ELSE
           IF ( FPW-TYPE-BASE )
              STRING "********************",
                     " B A S E - ", ACP-NUMBER,
                     " ********************"
                     DELIMITED BY "  "   INTO H03-TYPE
           ELSE
              STRING "********************",
                     " R A N G E S ",
                     "********************"
                     DELIMITED BY "  "   INTO H03-TYPE.

           CALL "C$JUSTIFY"             USING H03-TYPE "C".
           MOVE HEADER-LINE-03             TO PR-REC.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-04             TO PR-REC.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-05             TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

       WRITE-PAGE-HEADER-END.

           MOVE 07                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.

       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO MET2LS-CONAME.
           MOVE EXT-CONAME-SYSDATE TO MET2LS-SYSDATE.
           DISPLAY MET2LS-SCREEN UPON MET2LS-WINDOW-HANDLE.
           MOVE MET2LS-FORM-FIELDS TO WR-BUF.
           MOVE MET2LS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE MET2LS-HELP--FILE TO HELP-LINK-FILE.
           MOVE MET2LS-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/MET2LS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPTRWINFOGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPTRWINFOIN.CPY".
       COPY "LIBSP/SPTWRN.CPY".
       COPY "LIBSP/SPTWERN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

       EXIT-PROG.

           MOVE EXIT-PATH  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY MET2LS-WINDOW-HANDLE.
           EXIT PROGRAM.
