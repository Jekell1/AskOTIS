       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ASLNEX.
       DATE-WRITTEN. 112888
      ***********************************************************
      *    DIR :  LP
      *    DESC:  ADVERTISING/SOLICITATION LOAN SELECTION
      *
      *  !!!!! HEY !!!!!  PLEASE NOTE IF YOU ADD ANY PARAMETER HERE
      *                   THE FIELD MUST ALSO BE ADDED TO THE BUFFER
      *                   IN ASLNE2 OR ELSE THE BORR-STAT GETS MESSED
      *                   UP...THEN THE ??? WON'T DISPLAY IN THE
      *                   BORROWER OPTIONS SCREEN & THIS PROGRAM MAY
      *                   BE CONFUSED ABOUT WHETHER THE BORROWER
      *                   OPTIONS WERE ENTERED.  REALLY, WHEN TIME,
      *                   A THIS PROG COULD BE CHANGED TO OPEN A
      *                   WINDOW & THE BORROWER OPTIONS ENTERED HERE,
      *                   & TRASH ASLNE2.C.          BARB
      *
      * REV:
      *  MJD 051895  ADDED F6 - FORM SCAN OPTION
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  BAH 111596 ADDED TEST OF BR-AVAIL-CREDIT NOT = "S" FOR WDT (335)
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      *  GLF 101697 ADDED COPY MEMBER: LIBLP/LPPDUE LIBLP/LPPDUEW
      *  BAH 082798 REWRITE OF NO1 FILE WAS READING FIRST, WIPING OUT
      *             WHAT 'CRNORC' JUST BUILT....TRY TO WRITE FIRST, THEN
      *             REWRITE..
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      * MJD 000724 ADDED OPTION TO PRINT REPORT IN BR/ZIP/ACCTNO ORDER,
      *             PR# 1251 REGACC
      * BAH 010214 READ-FD1 WAS COMMENTED OUT, I THINK WITH ABOVE CHANGE,
      *            SO FD-1STFILE WAS BEING TESTED FOR 4, AND IT WAS
      *            GARBAGE !! FINMGM
      * BAH 020304 WAS NOT READING THE FD1 FILE WHEN IN BATCH !!!! DUH !!
      * CS  020610 ADD OPTION TO TEST NEXTDUE DATE RANGE [PR# 1813 REGENCY]
      * CS  020618 CORECTED ABOVE DID NOT WORK OR DISPLAY PROPERLY IF BORROWER
      *            OPTIONS 'Y'. NEEDED TO FIX PAGE 2 [ASLNE2]
      * BLV 020916 ADDED OPTION TO SKIP MULTIPLE ACCOUNTS, PL# 379, JIM
      *            SAYS THIS HAS BEEN A LONG-TIME PROBLEM;
      *            NOTE:  THIS TURNED INTO A PRETTY COMPLICATED CHANGE
      *            BECAUSE OF THE OPTIONAL REPORT WITH A WORK FILE, WHICH
      *            MIGHT BE RUN WITH NO FORM SELECTED, OR WITH A FORM
      *            WITH A FIRST FILE OF 2, ETC.
      *            ALSO ADDED RECORD COUNT TO RANGE PRINT
      * BAH 030325 NXDUE DATE RANGE WAS NOT DISPLAYING CORRECTLY IN BATCH
      * BAH 040318 ADDED BEG AND END CREDIT SCORE, PACE PR# 2263
      * BAH 040322 READ SPR WAS ONLY DONE IF MINAV WAS NOT 0, THEREFORE,
      *            ALL CREATE NOTICE FIELDS WERE WRONG THAT NEEDED SPRS.
      *            MOVED GET-SPR OUTSIDE OF THAT IF !
      * BAH 040419 ADDED ORIGIN FOR CASHCO, PR# 2273
      * BLV 040614 MAY ENTER A GROUP, NOW CROSSES DATA PATHS, ADDED OPTION
      *            FOR CORP PRINTING, ADDED BRSECURE.  IF RUNNING FOR
      *            GROUP OR BRANCH RANGE, WILL CHECK FOR DUPLICATE SSNOS
      *            REGARDLESS OF BRANCH IF "N" TO "MULTIPLE ACCOUNTS".
      *            IF CORP PRINTING SELECTED, WILL SORT BY ZIP REGARDLESS
      *            OF BRANCH.  PR#1030, PL# 324
      * BLV 040615 PRINT CORP-FM-FG IN RANGES.  FIX IF 1ST FILE IS 2,
      *            REPORT WAS STILL PRINTING ALL BORROWERS.....
      * BLV 041014 FIX CORP-FM SKIPPING TO EXIT-PARM IF 1 BRANCH OR
      *            SMARTPORT, AND MISSING ENTRY OF LAST 2 PARAMETERS
      * BAH 050118 ADDED CHECK OF BW-CONFIDENTIAL, PR# 2343
      * BAH 071008 WORLD NO LONGER SOLICITING BORROWERS IN KANSAS PR#557
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0205 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *         CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      * BAH 170316 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180116 USE LPBAGE TO CALCULATE AGE SO ITS CONSISTENT #1298
      * BAH 181112 GLOBAL FMFILE
      * BAH 181203 GLOBAL LNFILE
      * BAH 190108 GLOBAL NOFILE
      * BAH 190122 SPLIT CDFILE
      * BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20190920 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2024.0201 READ NOTICE, NOT FOUND THEN CRNOR2
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".

           SELECT MANYACCTS-FILE ASSIGN TO MANYACCTS-PATH
           ORGANIZATION INDEXED
           ACCESS DYNAMIC
           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
           RECORD KEY MANYACCTS-KEY
           FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-OWNBR   PIC 9(4).
               05  WK-ZIP     PIC X(10).
               05  WK-CLASS   PIC 9(3).
               05  WK-ACCTNO  PIC 9(8).
           03  WK-LOAN-NUM    PIC X(13).
           03  WK-NAME        PIC X(25).
           03  WK-SPNAME      PIC X(16).
           03  WK-PHONE       PIC 9(10)    COMP.
           03  WK-LOANDATE    PIC 9(08)    COMP.
           03  WK-COLLCD      PIC XX.
           03  WK-CURBAL      PIC 9(07)V99 COMP.
           03  WK-PROFILE     PIC 9(12)    COMP.
           03  WK-PAYSM       PIC 9(03)    COMP.
           03  WK-PAYSR       PIC 9(03)    COMP.
           03  WK-PERAVL      PIC 9(03)    COMP.
           03  WK-AVAIL       PIC 9(06)V99 COMP.

       FD  MANYACCTS-FILE
           LABEL RECORDS ARE STANDARD.
       01  MANYACCTS-REC.
           03  MANYACCTS-KEY.
               05 MANYACCTS-SSNO        PIC 9(9).

           03  MANYACCTS-INITKEY.
               05  MANYACCTS-ACCTNO     PIC 9(8).
               05  MANYACCTS-FORMNO     PIC 9(4).
               05  FILLER               PIC X(4).
               05  MANYACCTS-BRNO2      PIC 9(4).

           03  MANYACCTS-POFFDATE       PIC 9(8) COMP-3.
           03  MANYACCTS-OWNBR          PIC 9(4).
           03  MANYACCTS-ZIP            PIC X(10).
           03  MANYACCTS-CLASS          PIC 9(3).
           03  MANYACCTS-DATA-PATH      PIC X(6).
           03  MANYACCTS-MACHINE        PIC XX.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/ASLNEX.CBL J35 12/21/23-08:40:14 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       01  WK-IFN        PIC X(02)   VALUE "WK".

       01  SEL-TYPE             VALUE SPACES.
           03  SEL-FMNO         PIC 9(4).
           03  SEL-FMNOX  REDEFINES SEL-FMNO
                                PIC X(4).

       01  MANYACCTS-PATH  PIC X(35) VALUE SPACES.

       01  HOLD-FM1-KEY                PIC X(28)     VALUE SPACES.

       01  HOLD-CD1-KEY                PIC X(10)     VALUE SPACES.
       01  FILLER                 REDEFINES HOLD-CD1-KEY.
           03  HOLD-CD-TYPE         PIC X(2).
           03  HOLD-CD-CODE         PIC X(8).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.
       01  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 999.
       01  SUB1                 PIC 999.
       01  SOURCE-OK            PIC X.
       01  PURPOSE-OK           PIC X.
       01  SECURITY-OK          PIC X.
       01  MATCH-MAX            PIC 99       VALUE 0.
       01  MATCH-OK             PIC X.
       01  MATCH1-CODE.
           03  MATCH1           OCCURS 10 PIC X.
       01  MATCH2-CODE.
           03  MATCH2           OCCURS 10 PIC X.
       01  ORIGIN-WK.
           03  ORIGIN-CHAR      PIC X        OCCURS 4.
       01  ORIGIN-SUB           PIC 9.
       01  ORIGIN-MATCH         PIC X.

       01  AMT-WK1              PIC 9(6)     COMP-3 VALUE 0.
       01  PAYS-MADE            PIC 999.
       01  PAYS-REMAIN          PIC 999.
       01  REF-YEARS            PIC 999.
       01  TIME-ALP             PIC X.
       01  TIME-WORK            PIC 9999.
       01  TIME-WK-TAB REDEFINES TIME-WORK.
           03  TIME-WK          OCCURS 4 TIMES
                                PIC 9.

       01  PROFILE-OK           PIC X.
       01  PROFILE-HIGH         PIC 999.
       01  PROFILE-WORKER-01.
           03  PROFILE-WORKER   PIC 9(12).
           03  PROFILE-TAB REDEFINES PROFILE-WORKER
                                OCCURS 12 PIC 9.

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  HOLD-BRNO            PIC 9(4)     VALUE 0.
       01  HOLD-CLASS           PIC 9(3)       VALUE 0.
       01  LAST-LC-CODE         PIC 9(3)     COMP-3 VALUE 999.
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.
       01  HOLD-LPS-NUM.
           03  H-LPS-CLASS      PIC XXX.
           03  H-LPS-NUM        PIC X(13).

       01  CREATE-FM-FG         PIC X VALUE "Y".

       01  RECORD-COUNT         PIC 9(7) VALUE 0.

       01  SAVE-INITKEY         PIC X(20) VALUE SPACES.
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  LOAN-FIELDS.
               05  BEG-BRANCH      PIC 9(4).
               05  END-BRANCH      PIC 9(4).
               05  BEG-CLASS       PIC 99.
               05  END-CLASS       PIC 99.
               05  BEG-DEALER      PIC 9(4).99.
               05  END-DEALER      PIC 9(4).99.
               05  BEG-LDATE       PIC 99/99/99.
               05  END-LDATE       PIC 99/99/99.
               05  ACTION-CODE     PIC X(4).
               05  MAXDCNTR1       PIC 99.
               05  MAXDCNTR2       PIC 99.
               05  MINPAYS-MADE    PIC 999.
               05  MINPAYS-REMAIN  PIC 999.
               05  XMON            PIC 99.
               05  HCON            PIC 999.
               05  HREC            PIC 999.
               05  MINCR           PIC 9(6).
               05  MAXCR           PIC 9(6).
               05  MINAV           PIC 9(6).
               05  PERAVL          PIC 999.
               05  MLFORM          PIC 99.
               05  BEGLFORM        PIC 999.
               05  ENDLFORM        PIC 999.
               05  OWNED           PIC X.
               05  ZBAL            PIC X.
               05  MINPOMON        PIC 999.
           03  LOAN-FIELDS2.
               05  MAXPOMON        PIC 999.
               05  SCD-TABLE.
                   07  SCD         OCCURS 9 PIC XX.
               05  PCD-TABLE.
                   07  PCD         OCCURS 9 PIC XXX.
               05  SECURITY-CODE   PIC X.
               05  MANY-ACCOUNTS   PIC X.


               05  BWOPT           PIC X.

               05  FORMNO          PIC 999.
               05  REPT            PIC X.
               05  BEG-NXDUE-DT    PIC 99/99/99.
               05  END-NXDUE-DT    PIC 99/99/99.
               05  GROUP-FLAG       PIC X.
               05  ENT-GROUP        PIC X(4).
               05  CORP-FM-FG       PIC X.


      ****************************************
      * PASSED BORROWER SELECTION FIELDS
      * BORR-STAT:
      *       " " - RANGES NEVER ENTERED
      *       "I" - RANGES ENTERED
      ****************************************
           03  BORR-STAT           PIC X.
           03  BORR-FIELDS         PIC X(106).
           03  BORR-FIELDSX        REDEFINES BORR-FIELDS.
               05  STATE           PIC XX.
               05  BEGZIP          PIC X(10).
               05  ENDZIP          PIC X(10).
               05  COUNTY          PIC XXX.
               05  DWELL           PIC XX.
               05  CENSUS          PIC X(10).
               05  SMSA            PIC X(6).
               05  RENT            PIC X(4).
               05  YRADDR          PIC 99.
               05  MINAGE          PIC 99.
               05  MAXAGE          PIC 99.
               05  MINEMPL         PIC 99.
               05  MINPAY          PIC 9(5).
               05  MINTPAY         PIC 9(5).
               05  MINLIA          PIC 9(6).
               05  MAXLIA          PIC 9(6).
               05  MPO             PIC 99.
               05  BEGSLSMN        PIC 9999.
               05  ENDSLSMN        PIC 9999.
               05  BEGREP          PIC X(4).
               05  ENDREP          PIC X(4).
               05  MARRIED         PIC X.
               05  BEG-CRSCORE     PIC 9(3).
               05  END-CRSCORE     PIC 9(3).
               05  ORIGINX.
                   07  ORIGIN-CD   PIC X OCCURS 4.

           03  SORT-BY-ZIP         PIC X(01).

       01  VERSION-CONTROL         PIC X      VALUE "D".

       01  SV-BEG-BR           PIC 9(4)       VALUE 9999.
       01  SV-END-BR           PIC 9(4)       VALUE 9999.
       01  SV-GROUP-FLAG       PIC X          VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)       VALUE SPACES.

       01  SAVE-FM-ALL-PATH      PIC X(40).
       01  SAVE-NO-ALL-PATH      PIC X(40).

       01  SCREEN-BUF              PIC X(98)  VALUE SPACES.
       01  SCREEN-BUF2             PIC X(71)  VALUE SPACES.
       01  SCREEN-BUF4             PIC X      VALUE SPACES.

       01  SCREEN-LIST.
           03  FILLER          PIC X(8)  VALUE
           "BR1 BR2 ".
           03  FILLER          PIC X(47) VALUE
           "CL1 CL2 DL1 DL2 LNDT1 LNDT2 ACTION MDEL1 MDEL2 ".
           03  FILLER          PIC X(51) VALUE
           "MINPAYSM MINPAYSR XMON HCON HREC MINCR MAXCR MINAV ".
           03  FILLER          PIC X(50) VALUE
           "PAVL MLFORM BEGLFORM ENDLFORM OWNED ZBAL MINPOMON.".

       01  SCREEN-LIST2.
           03  FILLER          PIC X(49) VALUE
           "MAXPOMON SCD1 SCD2 SCD3 SCD4 SCD5 SCD6 SCD7 SCD8 ".
           03  FILLER          PIC X(50) VALUE
           "SCD9 PCD1 PCD2 PCD3 PCD4 PCD5 PCD6 PCD7 PCD8 PCD9 ".
           03  FILLER          PIC X(34) VALUE
           "SECUR MANYACCTS BWOPT FORMNO REPT ".
           03  FILLER          PIC X(22) VALUE
           "BEGNXDUEDT ENDNXDUEDT.".

       01  SCREEN-LIST4.
           03  FILLER          PIC X(4) VALUE "ZIP.".

       01  DL1                 PIC 9(4)V99.
       01  DL2                 PIC 9(4)V99.
       01  BEG-LOANDATE        PIC 9(8).
       01  END-LOANDATE        PIC 9(8).
       01  BEG-NXDUEDATE       PIC 9(8).
       01  END-NXDUEDATE       PIC 9(8).

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(21)    VALUE " ".
           03  FILLER           PIC X(50)    VALUE
           "ADVERTISING / SOLICITATION LOAN SELECTION REPORT".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).

       01  HEAD4.
           03  FILLER      PIC X(8)     VALUE "CLASS  :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-LCNO      PIC 99.
           03  FILLER      PIC X        VALUE " ".
           03  H-LCDESC    PIC X(20).

       01  HEAD5.
           03  FILLER      PIC X(99)    VALUE " ".
           03  FILLER      PIC X(33)    VALUE
           "CONTRACTUAL PAYS PAYS      CREDIT".

       01  HEAD6.
           03  FILLER      PIC X(45)    VALUE
               "BRNO ACCOUNT NO.   BORROWER".
           03  FILLER      PIC X(38)    VALUE
               "SPOUSE               PHONE NO LOANDATE".
           03  FILLER      PIC X(40)    VALUE
               " CC   BALANCE  ---PROFILE-- MADE  REM".
           03  FILLER      PIC X(9)    VALUE
               "    LIMIT".

       01  HEAD6B.
           03  FILLER      PIC X(45)    VALUE
               " ZIP       BRNO ACCOUNT NO.   BORROWER       ".
           03  FILLER      PIC X(38)    VALUE
               "           SPOUSE    PHONE NO LOANDATE".
           03  FILLER      PIC X(40)    VALUE
               " CC   BALANCE  ---PROFILE-- MADE  REM   ".
           03  FILLER      PIC X(9)    VALUE
               "    LIMIT".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-LINE1-REDEF       PIC X(61).
           03  FILLER REDEFINES D-LINE1-REDEF.
               05  D-OWNBR     PIC ZZZ9.
               05  FILLER      PIC X.
               05  D-LOAN-NUM  PIC X(13).
               05  FILLER      PIC X.
               05  D-NAME      PIC X(25).
               05  FILLER      PIC X.
               05  D-SPNAME    PIC X(16).
           03  FILLER REDEFINES D-LINE1-REDEF.
               05  D2-ZIP      PIC X(10).
               05  FILLER      PIC X.
               05  D2-OWNBR    PIC ZZZ9.
               05  FILLER      PIC X.
               05  D2-LOAN-NUM PIC X(13).
               05  FILLER      PIC X.
               05  D2-NAME     PIC X(23).
               05  FILLER      PIC X.
               05  D2-SPNAME   PIC X(07).
           03  FILLER      PIC X.
           03  D-PHONE     PIC 999/999/9999.
           03  FILLER      PIC X.
           03  D-LOANDATE  PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-COLLCD    PIC XX.
           03  D-CURBAL    PIC ZZZZZZ9.99.
           03  FILLER      PIC X.
           03  D-PROFILE   PIC 9(12).
           03  FILLER      PIC XX.
           03  D-PAYSM     PIC ZZ9.
           03  FILLER      PIC X(2).
           03  D-PAYSR     PIC ZZ9.
           03  FILLER      PIC X.
           03  D-PERAVL    PIC ZZ9.
           03  D-AVAIL     PIC ZZZZZ9.99.

       01  RANGE-LN             REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(30).
           03  RANGE-SELECTION  PIC X(102).
           03  FILLER           REDEFINES RANGE-SELECTION.
               05  RANGE-DATE   PIC 99/99/99.
               05  FILLER       PIC X(94).
           03  RANGE-TABLE      REDEFINES RANGE-SELECTION.
               05  RANGE-TAB    OCCURS 9
                                PIC XXXXX.
               05  FILLER       PIC X(57).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBWI/FDSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ASLNEX_DEF.CPY".
       COPY "LIBLP/ASLNEX_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/ASLNEX_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE MANYACCTS-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WK-FILE MANYACCTS-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ASLNEX" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO PROCESS-WORK-FILE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.

           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO PROCESS-WORK-FILE.


      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    HAVE TO OVERRIDE THE PATHS FOR FM DUE TO THE PATHS
      *    BEING ALTERED ELSEWHERE IN THIS PROGRAM.  WAS EASIER TO
      *    USE THE "Y" OVERRIDE OPTION HERE.               JAY 2014-1029
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( CORP-FM-FG = "N" )
              MOVE FILPATH-BASE            TO FM-ALL-BASE
              MOVE FILPATH-MACHINE         TO FM-ALL-MACHINE
              MOVE FILPATH-DATA            TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH
              MOVE "Y"                     TO FM-PATH-OVERRIDE
           ELSE
              MOVE EXT-FILPATH-BASE        TO FM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH
              MOVE "Y"                     TO FM-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LOAN FILE EXTRACTION IN PROGRESS BRANCH &&&&"
                 TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              MOVE MESS TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-NO1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE BEG-CLASS     TO LN-CLASS
                                 QLN3-WBEG-CLASS.
           MOVE END-CLASS     TO QLN3-WEND-CLASS.
           MOVE 0             TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH OR LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

           IF LN-CLASS < BEG-CLASS 
              GO TO NEXT-LN.

      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

      * SKIP PL ACCOUNTS:
           IF LN-PLDATE NOT = 0
              GO TO NEXT-LN.

      * SKIP JUDGEMENTS:
           IF LN-JDDATE NOT = 0
              GO TO NEXT-LN.

      * SKIP CONFIDENTIAL ACCOUNTS:
           IF LN-CONFIDENTCD = "Y"
              GO TO NEXT-LN.

      * SKIP REPO, LEGAL, BANKRUPT, AND DEBTAID ACCOUNTS:
           IF (LN-REPOCD = "X" OR "S")
                    OR (LN-DEBTAIDATE NOT = 0)
                     OR (LN-LEGALDATE NOT = 0)
                      OR (LN-BNKRPTDATE NOT = 0)
              GO TO NEXT-LN.

      *********************************************
      * TEST LOAN OPTIONS:
      *********************************************
      * TEST CLASS RANGE:
           IF LN-CLASS < BEG-CLASS
                     OR > END-CLASS
              GO TO NEXT-LN.

      * TEST DEALER RANGE:
           IF LN-DLNO < DL1
                    OR > DL2
              GO TO NEXT-LN.

      * TEST LOAN DATE RANGE:
           IF LN-LOANDATE < BEG-LOANDATE 
              GO TO NEXT-LN.

           IF LN-LOANDATE > END-LOANDATE
              GO TO NEXT-LN.

      * TEST-ACTION-CODE:
           MOVE LN-ACTIONCD TO MATCH1-CODE.
           MOVE ACTION-CODE TO MATCH2-CODE.
           MOVE 4 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST FOR OWNED ACCOUNT:
           MOVE LN-CLASS TO LC-CODE.
           PERFORM GET-LC.

           IF OWNED = "O"
              IF LC-SERVACCT = "Y"
                 GO TO NEXT-LN.
           IF OWNED = "S"
              IF LC-SERVACCT = "N"
                 GO TO NEXT-LN.

      * TEST ZERO BALANCE:
           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0
                 GO TO NEXT-LN.
           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 GO TO NEXT-LN.

      * TEST MIN MONTHS SINCE PAIDOFF:
           IF LN-CURBAL = 0
              MOVE LN-POFFDATE TO NUM-DATE
              MOVE TRANS-DATE  TO SYS-DATE
              PERFORM TIM360
              IF ELAPSED-MONTHS < MINPOMON OR > MAXPOMON
                 GO TO NEXT-LN.


      * TEST DELIQUENCY COUNTERS:
           IF LN-DELCNT > MAXDCNTR1
              GO TO NEXT-LN.

           IF LN-DELCNT2 > MAXDCNTR2
              GO TO NEXT-LN.


      * TEST MINIMUM PAYS MADE:
           IF LN-REGPYAMT NOT = 0
              COMPUTE PAYS-MADE = LN-TOTPAYMNTD / LN-REGPYAMT.
           IF PAYS-MADE < MINPAYS-MADE
              GO TO NEXT-LN.

      * TEST MINIMUM PAYS REMAINING:
           COMPUTE PAYS-REMAIN =
               (LN-ORGTERM * LN-REGPYAMT - LN-TOTPAYMNTD)
                                               / LN-REGPYAMT.
           IF PAYS-REMAIN < MINPAYS-REMAIN
              GO TO NEXT-LN.

      * TEST CONTRACTUAL FG:
           MOVE LN-CONPROFILE(1) TO PROFILE-WORKER.
           MOVE HCON TO PROFILE-HIGH.
           PERFORM TEST-PROFILE.
           IF PROFILE-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST RECENCY PROFILE:
           MOVE LN-RECPROFILE(1) TO PROFILE-WORKER.
           MOVE HREC TO PROFILE-HIGH.
           PERFORM TEST-PROFILE.
           IF PROFILE-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST CREDIT LIMIT:
           IF LN-CREDITLIM < MINCR OR > MAXCR
              GO TO NEXT-LN.

           PERFORM GET-SPR.

           PERFORM GET-NOTICE.

      * TEST MIN AVAIL CREDIT:
           IF MINAV = 0
              GO TO TEST-SOLIC-FORM.

           IF NO-AVAILCREDIT < MINAV
              GO TO NEXT-LN.

      * TEST PERCENT AVAIL TO CREDIT LIMIT:
           IF LN-CREDITLIM NOT = 0
              COMPUTE AMT-WK1 ROUNDED =
                           NO-AVAILCREDIT / LN-CREDITLIM * 100
              IF AMT-WK1 < PERAVL
                 GO TO NEXT-LN.

      * TEST MONTHS SINCE LAST SOLICITATION LETTER SENT:
       TEST-SOLIC-FORM.
           IF MLFORM = 0
              GO TO TEST-LAST-FORM-NO.

           IF LN-LASTSFORMDATE = 0
              GO TO NEXT-LN.

           MOVE LN-LASTSFORMDATE TO NUM-DATE.
           MOVE TRANS-DATE       TO SYS-DATE.
           PERFORM TIM360.
           IF ELAPSED-MONTHS < MLFORM
              GO TO NEXT-LN.

       TEST-LAST-FORM-NO.
      * TEST LAST FORM NUMBER SENT:
           IF LN-LASTSFORMNO < BEGLFORM OR > ENDLFORM
              GO TO NEXT-LN.

      * TEST SOURCE CODES:
           IF SCD(1) NOT = "**"
              PERFORM TEST-SOURCE-CODES
              IF SOURCE-OK NOT = "Y"
                 GO TO NEXT-LN.

      * TEST PURPOSE CODES:
           IF PCD(1) NOT = "***"
              PERFORM TEST-PURPOSE-CODES
              IF PURPOSE-OK NOT = "Y"
                 GO TO NEXT-LN.

      * TEST SECURITY:
           IF SECURITY-CODE NOT = "*"
              PERFORM TEST-SECURITY-TYPE
              IF SECURITY-OK NOT = "Y"
                 GO TO NEXT-LN.

      * TEST NEXT DUE DATE
           IF NO-TRUE-DUE-DATE < BEG-NXDUEDATE
              GO TO NEXT-LN.

           IF NO-TRUE-DUE-DATE > END-NXDUEDATE
              GO TO NEXT-LN.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF NOT IO-GOOD
              GO TO NEXT-LN.

      * WORLD NO LONGER SOLICITING BORROWERS IN KANSAS PR#557
           IF BW-STATE = "KS"
              GO TO NEXT-LN.

           IF BW-CONFIDENTIAL = "Y"
              GO TO NEXT-LN.

           IF BWOPT NOT = "Y"
              GO TO LOAN-TESTS-OK.

      * TEST FOR SOLICITING
           IF BW-SOLICIT NOT = "Y"
              GO TO NEXT-LN.

      * TEST-STATE-CODE:
           MOVE BW-STATE TO MATCH1-CODE.
           MOVE STATE TO MATCH2-CODE.
           MOVE 2 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST-ZIP-CODE RANGE:
           IF BW-ZIP < BEGZIP
                         OR > ENDZIP
              GO TO NEXT-LN.

      * TEST-COUNTY-CODE:
           MOVE BW-COUNTY TO MATCH1-CODE.
           MOVE COUNTY TO MATCH2-CODE.
           MOVE 3 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST-DWELL-CODE:
           MOVE BW-DWELLCD TO MATCH1-CODE.
           MOVE DWELL TO MATCH2-CODE.
           MOVE 2 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST-CENSUS-CODE:
           MOVE BW-CENSUSCD TO MATCH1-CODE.
           MOVE CENSUS TO MATCH2-CODE.
           MOVE 10 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST-SMSA-CODE:
           MOVE BW-STRTGCD TO MATCH1-CODE.
           MOVE SMSA TO MATCH2-CODE.
           MOVE 6 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST-RENT-CODE:
           MOVE BW-RENTCD TO MATCH1-CODE.
           MOVE RENT TO MATCH2-CODE.
           MOVE 6 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-LN.

      * COMPUTE NUMBER OF YEARS ELAPSED SINCE BW-REFDATE:
           MOVE BW-REFDATE TO NUM-DATE.
           MOVE TRANS-DATE TO SYS-DATE.
           PERFORM TIM360.
           DIVIDE 12 INTO ELAPSED-MONTHS GIVING REF-YEARS.
           IF REF-YEARS < 0
              MOVE 0 TO REF-YEARS.

      * TEST MINIMUM YEARS AT ADDRESS:
       TEST-BW-YR-ADDR.
           IF YRADDR = 0
              GO TO TEST-BW-AGE.

           MOVE BW-TIMEADDR TO MATCH1-CODE.
           MOVE 0 TO TIME-WORK.
           MOVE 4 TO SUB SUB1.
           PERFORM TEST-BW-YR-ADDR-MATCH 4 TIMES.
           IF TIME-ALP = "M"
              DIVIDE 12 INTO TIME-WORK
           ELSE
              IF TIME-ALP = "W"
                 DIVIDE 52 INTO TIME-WORK
              ELSE
                 IF TIME-ALP = "D"
                    DIVIDE 360 INTO TIME-WORK.

           COMPUTE AMT-WK1 = TIME-WORK + REF-YEARS.
           IF AMT-WK1 < YRADDR
              GO TO NEXT-LN.

      * TEST BORROWER AGE:
       TEST-BW-AGE.
           IF MINAGE = 0 AND MAXAGE = 99
              GO TO TEST-BW-YR-EMPL.

           MOVE BW-BDATE   TO BAGE-DATE.
           MOVE BW-REFDATE TO BAGE-REFDATE.
           MOVE TRANS-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.

           IF BAGE-AGE < MINAGE OR > MAXAGE
              GO TO NEXT-LN.

      * TEST MINIMUM YEARS EMPLOYED:
       TEST-BW-YR-EMPL.
           COMPUTE AMT-WK1 = BW-EMPYEARS + REF-YEARS.
           IF AMT-WK1 < MINEMPL
              GO TO NEXT-LN.

      * TEST MINIMUM PAY PER MONTH:
           IF BW-EMPPAY < MINPAY
              GO TO NEXT-LN.

      * TEST MINIMUM TOTAL PAY PER MONTH:
           COMPUTE AMT-WK1 = BW-EMPPAY + BW-EMPPAYOT.
           IF AMT-WK1 < MINTPAY
              GO TO NEXT-LN.

      * TEST MINIMUM LIAIBILITY:
           COMPUTE AMT-WK1 = BW-LIASMLN + BW-LIAOTH.
           IF AMT-WK1 < MINLIA OR > MAXLIA
              GO TO NEXT-LN.

      * TEST CREDIT SCORE
           IF BW-CREDIT-SCORE < BEG-CRSCORE OR
                               > END-CRSCORE
              GO TO NEXT-LN.

      * TEST BORROWER ORIGIN (CASHCO PR# 2273)
           MOVE BW-ORIGIN TO ORIGIN-WK.
           PERFORM ORIGIN-CD-CHECK.
           IF ORIGIN-MATCH NOT = "Y"
              GO TO NEXT-LN.

      * TEST MINIMUM MONTHS SINCE LAST PAYOFF:
           IF MPO NOT = 99
              MOVE BW-LASTPODATE TO NUM-DATE
              MOVE TRANS-DATE    TO SYS-DATE
              PERFORM TIM360
              IF ELAPSED-MONTHS > MPO
                 GO TO NEXT-LN.

      * TEST SALESMAN RANGE:
           IF BW-ORGSLSMN < BEGSLSMN
                         OR > ENDSLSMN
              GO TO NEXT-LN.

      * TEST REP RANGE:
           IF BW-REPCODE < BEGREP
                        OR > ENDREP
              GO TO NEXT-LN.

      * TEST FOR MARRIED:
           IF MARRIED NOT = "I"
              IF (BW-SPSSNO = 0 AND BW-SPLNAME = SPACES
                                     AND BW-SPFNAME = SPACES)
                 IF MARRIED = "O"
                    GO TO NEXT-LN
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF MARRIED = "S"
                    GO TO NEXT-LN.

       LOAN-TESTS-OK.

           PERFORM CREATE-FM-REC.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-NO1-FILE.
           GO TO NEXT-GROUP-LOC.

       PROCESS-WORK-FILE.
           IF REPT NOT = "Y"
              GO TO END-ROUTINE.
           MOVE ZEROS TO WK-OWNBR WK-CLASS WK-ACCTNO.
           MOVE SPACES TO WK-ZIP.
           PERFORM START-WK-FILE.
           MOVE "N" TO FIRST-TIME.
       NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           ADD 1 TO RECORD-COUNT.

           IF SORT-BY-ZIP NOT = "Y"
              MOVE WK-CLASS TO LC-CODE
              PERFORM GET-LC.

           IF FIRST-TIME = "Y"
              MOVE WK-OWNBR TO HOLD-BRNO
              MOVE WK-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF WK-OWNBR NOT = HOLD-BRNO
              MOVE WK-OWNBR TO HOLD-BRNO
              MOVE WK-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF WK-CLASS NOT = HOLD-CLASS
              MOVE WK-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-WK.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           IF REPT = "Y"
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      *****************************************************
      *    I N I T I A L I Z A T I O N
      *****************************************************
       INITIALIZATION SECTION.
           MOVE ASLNEX-QUEUE-POS TO Q-POS.
           MOVE ASLNEX-COPIES-POS TO C-POS.
           MOVE ASLNEX-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE BEG-DEALER TO DL1.
           MOVE END-DEALER TO DL2.

           MOVE BEG-LDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEG-LOANDATE.
           MOVE END-LDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO END-LOANDATE.

           MOVE BEG-NXDUE-DT TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEG-NXDUEDATE.
           MOVE END-NXDUE-DT TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO END-NXDUEDATE.

           IF FORMNO NOT = 0
              PERFORM OPEN-FD1-FILE
              MOVE FORMNO TO FD-FMNO
              PERFORM READ-FD1-FILE
              PERFORM CLOSE-FD1-FILE.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TRANS-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LOAN SOLICITATION RUN IN PROGRESS....." TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-CD1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO MANYACCTS-PATH.
           PERFORM OPEN-MANYACCTS-FILE-OUTPUT.
           PERFORM CLOSE-MANYACCTS-FILE.
           PERFORM OPEN-MANYACCTS-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *********************************************
      *    AUDIT TO SETUP DATES FOR BATCH PROCESS
      *********************************************
       EO-EXEC-AUDIT SECTION.
           MOVE BEG-DEALER TO DL1.
           MOVE END-DEALER TO DL2.
           MOVE BEG-LDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEG-LOANDATE.
           MOVE END-LDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO END-LOANDATE.

           MOVE BEG-NXDUE-DT TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEG-NXDUEDATE.
           MOVE END-NXDUE-DT TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO END-NXDUEDATE.

      *********************************************
      *    ENTER REPORT PARAMETERS
      *********************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              IF REPT = "Y"
                 GO TO REC-ZIP
              ELSE
                 GO TO REC-REPT.

           PERFORM GET-USER-BRANCH.

       BR-01.
           MOVE "E  A040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CHECK-BRANCH-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.
      *----------------------------------------------------------------
       REC-CL1.
           MOVE "ENTER BEGINNING LOAN CLASS, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-CL1.
           MOVE VDUNUM0 TO BEG-CLASS.
       REC-CL2.
           MOVE "ENTER ENDING LOAN CLASS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-CL1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-CL2.
           MOVE VDUNUM0 TO END-CLASS.
           IF END-CLASS = ZERO GO TO REC-CL2.
           IF BEG-CLASS > END-CLASS
              GO TO REC-CL1.
           GO TO END-CL.

       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE ZERO TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       END-CL.

       REC-DL1.
           MOVE "ENTER BEGINNING DEALER, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-CL1.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-DL1.
           MOVE VDUNUM2 TO BEG-DEALER DL1.
       REC-DL2.
           MOVE "ENTER ENDING DEALER:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-DL1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-DL2.
           MOVE VDUNUM2 TO END-DEALER DL2.
           IF DL2 = ZERO GO TO REC-DL2.
           IF DL1 > DL2
              GO TO REC-DL1.
           GO TO END-DL.

       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE ZERO TO BEG-DEALER DL1 VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DEALER DL2 VDUNUM2.
           PERFORM SCREENIO.
       END-DL.

       REC-LNDT1.
           MOVE "ENTER BEGINNING LOAN DATE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060LNDT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-DL1.
           IF VDUSTATUS = "16" GO TO ALL-LNDT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-LNDT1.
           MOVE VDUNUM0 TO BEG-LDATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LOANDATE.
       REC-LNDT2.
           MOVE "ENTER ENDING LOAN DATE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060LNDT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-LNDT1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-LNDT2.
           MOVE VDUNUM0 TO END-LDATE.
           MOVE VDU-DATE-YYYYMMDD TO END-LOANDATE.

           IF END-LOANDATE < BEG-LOANDATE
              GO TO REC-LNDT1.

           GO TO END-LNDT.

       ALL-LNDT.
           MOVE "S  A080LNDT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LOANDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO BEG-LDATE VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A080LNDT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-LOANDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO END-LDATE VDU-DATE-EDIT.
           PERFORM SCREENIO.
       END-LNDT.

       REC-ACTION.
           MOVE "ENTER ACTION CODE, '?' = WILD CARD:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040ACTION." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-LNDT1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ACTION.
           MOVE VDUBUF TO ACTION-CODE.

       REC-MDEL1.
           MOVE "ENTER MAXIMUM INCR FOR EXTRACT DELIQ COUNTER:"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MDEL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-ACTION.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MDEL1.
           MOVE VDUNUM0 TO MAXDCNTR1.

       REC-MDEL2.
           MOVE "ENTER MAXIMUM INCR FOR FORM DELIQ COUNTER:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MDEL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MDEL1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MDEL2.
           MOVE VDUNUM0 TO MAXDCNTR2.

       REC-MINPAYSM.
           MOVE "ENTER MINIMUM WHOLE PMTS MADE ON THE LOAN:"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030MINPAYSM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MDEL2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINPAYSM.
           MOVE VDUNUM0 TO MINPAYS-MADE.

       REC-MINPAYSR.
           MOVE "ENTER MINIMUM WHOLE PMTS REMAINING ON THE LOAN:"
                                                                TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030MINPAYSR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINPAYSM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINPAYSR.
           MOVE VDUNUM0 TO MINPAYS-REMAIN.

       REC-XMON.
           MOVE "ENTER NUMBER OF MONTHS TO VIEW PROFILES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020XMON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINPAYSR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-XMON.
           MOVE VDUNUM0 TO XMON.
           IF XMON = 0 OR > 12
              MOVE "ENTER 1 THRU 12" TO MESS
              PERFORM SEND-MESS
              GO TO REC-XMON.

       REC-HCON.
           MOVE "ENTER HIGHEST CONTRACTUAL PROFILE RATING ALLOWED:"
                                                              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030HCON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-XMON.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-HCON.
           MOVE VDUNUM0 TO HCON.
           DIVIDE 30 INTO VDUNUM0.
           MULTIPLY 30 BY VDUNUM0.
           IF VDUNUM0 NOT = HCON
              MOVE "MUST BE '0' OR A MULTIPLE OF 30" TO MESS
              PERFORM SEND-MESS
              GO TO REC-HCON.

       REC-HREC.
           MOVE "ENTER HIGHEST RECENCY PROFILE RATING ALLOWED:"
                                                              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030HREC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-HCON.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-HREC.
           MOVE VDUNUM0 TO HREC.
           DIVIDE 30 INTO VDUNUM0.
           MULTIPLY 30 BY VDUNUM0.
           IF VDUNUM0 NOT = HREC
              MOVE "MUST BE '0' OR A MULTIPLE OF 30" TO MESS
              PERFORM SEND-MESS
              GO TO REC-HREC.

       NXDUE-DT1.
           MOVE "ENTER BEGINNING NEXT DUE DATE: " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEGNXDUEDT."   TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO REC-HREC.
           IF F3-KEY GO TO ALL-NXDUE-DT.
           IF NOT(ENTER-KEY OR DOWN-KEY) GO TO NXDUE-DT1.

           MOVE VDUNUM0 TO BEG-NXDUE-DT.
           MOVE VDU-DATE-YYYYMMDD TO BEG-NXDUEDATE.

       NXDUE-DT2.
           MOVE "ENTER ENDING NEXT DUE DATE: " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060ENDNXDUEDT."   TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO NXDUE-DT1.
           IF NOT(ENTER-KEY OR DOWN-KEY) GO TO NXDUE-DT2.

           MOVE VDUNUM0 TO END-NXDUE-DT.
           MOVE VDU-DATE-YYYYMMDD TO END-NXDUEDATE.

           IF END-NXDUEDATE < BEG-NXDUEDATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO NXDUE-DT2.

           GO TO REC-MINCR.

       ALL-NXDUE-DT.
           MOVE "S  A080BEGNXDUEDT." TO VDU.
           MOVE  EXT-JULIAN-BEG TO BEG-NXDUEDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO BEG-NXDUE-DT VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A080ENDNXDUEDT." TO VDU.
           MOVE  EXT-JULIAN-END TO END-NXDUEDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO END-NXDUE-DT VDU-DATE-EDIT.
           PERFORM SCREENIO.

       REC-MINCR.
           IF BR-AVAIL-CREDIT NOT = "Y" AND NOT = "S"
              MOVE 0 TO MINCR MINAV PERAVL
              MOVE 999999 TO MAXCR
              GO TO REC-MLFORM.
           MOVE "ENNN060MINCR." TO VDU.
           MOVE "ENTER MINIMUM CREDIT LIMIT, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO NXDUE-DT2.
           IF VDUSTATUS = "16" GO TO ALL-CR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINCR.
           MOVE VDUNUM0 TO MINCR.
       REC-MAXCR.
           MOVE "ENTER MAXIMUM CREDIT LIMIT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060MAXCR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINCR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MAXCR.
           MOVE VDUNUM0 TO MAXCR.
           IF MINCR > MAXCR
              GO TO REC-MINCR.
           GO TO END-CR.

       ALL-CR.
           MOVE "SNNN060MINCR." TO VDU.
           MOVE ZERO TO MINCR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060MAXCR." TO VDU.
           MOVE 999999 TO MAXCR VDUNUM0.
           PERFORM SCREENIO.
       END-CR.

       REC-MINAV.
           MOVE "ENTER MINIMUM AVAILABLE CREDIT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060MINAV." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINCR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINAV.
           MOVE VDUNUM0 TO MINAV.
           IF MINAV = 0
              MOVE 0 TO PERAVL
              GO TO REC-MLFORM.

       REC-PERAVL.
           MOVE "ENTER MINIMUM % AVAILABLE OF CREDIT LIMIT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030PAVL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINAV.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PERAVL.
           MOVE VDUNUM0 TO PERAVL.
           IF PERAVL > 100
              GO TO REC-PERAVL.

       REC-MLFORM.
           MOVE "ENTER MINIMUM MONTHS SINCE LAST SOLIC. FORM SENT:"
                                                             TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MLFORM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF BR-AVAIL-CREDIT NOT = "Y" AND NOT = "S"
                 GO TO REC-HREC
              ELSE
                 IF MINAV = 0
                    GO TO REC-MINAV
                 ELSE
                    GO TO REC-PERAVL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MLFORM.
           MOVE VDUNUM0 TO MLFORM.

       REC-BEGLFORM.
           MOVE "ENTER BEGINNING SOLIC. FORM SENT, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030BEGLFORM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MLFORM.
           IF VDUSTATUS = "16" GO TO ALL-LFORM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BEGLFORM.
           MOVE VDUNUM0 TO BEGLFORM.
       REC-ENDLFORM.
           MOVE "ENTER ENDING SOLIC. FORM SENT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030ENDLFORM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGLFORM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ENDLFORM.
           MOVE VDUNUM0 TO ENDLFORM.
           IF BEGLFORM > ENDLFORM
              GO TO REC-BEGLFORM.
           GO TO END-LFORM.

       ALL-LFORM.
           MOVE "SNNN030BEGLFORM." TO VDU.
           MOVE ZERO TO BEGLFORM VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030ENDLFORM." TO VDU.
           MOVE 999 TO ENDLFORM VDUNUM0.
           PERFORM SCREENIO.
       END-LFORM.

       REC-OWNED.
           MOVE "TYPE OF ACCOUNTS ('O'-OWNED,'S'-SERVICE,'B'-BOTH):"
                                                                TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010OWNED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGLFORM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-OWNED.
           MOVE VDUBUF TO OWNED.
           IF NOT (OWNED = "O" OR "S" OR "B")
              GO TO REC-OWNED.

       REC-ZBAL.
           MOVE "ZERO BALANCES ('I'-INCLUDE,'S'-SKIP,'O'-ONLY):"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-OWNED.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ZBAL.
           MOVE VDUBUF TO ZBAL.
           IF NOT (ZBAL = "I" OR "S" OR "O")
              GO TO REC-ZBAL.
           IF ZBAL = "S"
              MOVE ZERO TO MINPOMON MAXPOMON
              MOVE "S  A030MINPOMON." TO VDU
              PERFORM SCREENIO
              MOVE "S  A030MAXPOMON." TO VDU
              PERFORM SCREENIO
              GO TO REC-SCD1.

       REC-MINPOMON.
           MOVE "ENTER MINIMUM NO. OF MONTHS SINCE PAYOFF, F3-ALL:"
                                                            TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030MINPOMON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-ZBAL.
           IF VDUSTATUS = "16" GO TO ALL-POMON.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINPOMON.
           MOVE VDUNUM0 TO MINPOMON.
       REC-MAXPOMON.
           MOVE "ENTER MAXIMUM NO. OF MONTHS SINCE PAIDOFF:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030MAXPOMON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINPOMON.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MAXPOMON.
           MOVE VDUNUM0 TO MAXPOMON.
           IF MINPOMON > MAXPOMON
              GO TO REC-MINPOMON.
           GO TO END-POMON.

       ALL-POMON.
           MOVE "SNNN030MINPOMON." TO VDU.
           MOVE ZERO TO MINPOMON VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030MAXPOMON." TO VDU.
           MOVE 999 TO MAXPOMON VDUNUM0.
           PERFORM SCREENIO.
       END-POMON.



       REC-SCD1.
           MOVE "ENTER THE 1ST OF NINE SOURCE CODES, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SCD1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-ZBAL.
           IF VDUSTATUS = "16" GO TO ALL-SCD.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SCD1.
           MOVE VDUBUF TO SCD(1).
           IF SCD(1) = "**" GO TO REC-SCD1.
           GO TO END-SCD1.

       ALL-SCD.
           MOVE SPACES TO SCD-TABLE.
           MOVE "**" TO SCD(1).
           MOVE "SCD1 SCD2 SCD3 SCD4 SCD5 SCD6 SCD7 SCD8 SCD9."
                                                       TO WR-LIST.
           MOVE SCD-TABLE TO WR-BUF.
           PERFORM CALL-WRFORM.
           GO TO REC-PCD1.
       END-SCD1.

       REC-SCD2.
           MOVE "ENTER THE 2ND OF NINE SOURCE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SCD2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SCD1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SCD2.
           MOVE VDUBUF TO SCD(2).

       REC-SCD3.
           MOVE "ENTER THE 3RD OF NINE SOURCE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SCD3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SCD2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SCD3.
           MOVE VDUBUF TO SCD(3).

       REC-SCD4.
           MOVE "ENTER THE 4TH OF NINE SOURCE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SCD4." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SCD3.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SCD4.
           MOVE VDUBUF TO SCD(4).

       REC-SCD5.
           MOVE "ENTER THE 5TH OF NINE SOURCE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SCD5." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SCD4.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SCD5.
           MOVE VDUBUF TO SCD(5).

       REC-SCD6.
           MOVE "ENTER THE 6TH OF NINE SOURCE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SCD6." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SCD5.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SCD6.
           MOVE VDUBUF TO SCD(6).

       REC-SCD7.
           MOVE "ENTER THE 7TH OF NINE SOURCE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SCD7." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SCD6.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SCD7.
           MOVE VDUBUF TO SCD(7).

       REC-SCD8.
           MOVE "ENTER THE 8TH OF NINE SOURCE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SCD8." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SCD7.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SCD8.
           MOVE VDUBUF TO SCD(8).

       REC-SCD9.
           MOVE "ENTER THE 9TH SOURCE CODE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SCD9." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SCD8.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SCD9.
           MOVE VDUBUF TO SCD(9).

       REC-PCD1.
           MOVE "ENTER THE 1ST OF NINE PURPOSE CODES,F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCD1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SCD1.
           IF VDUSTATUS = "16" GO TO ALL-PCD.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PCD1.
           MOVE VDUBUF TO PCD(1).
           IF PCD(1) = "**" GO TO REC-PCD1.
           GO TO END-PCD1.

       ALL-PCD.
           MOVE SPACES TO PCD-TABLE.
           MOVE "***" TO PCD(1).
           MOVE "PCD1 PCD2 PCD3 PCD4 PCD5 PCD6 PCD7 PCD8 PCD9."
                                                       TO WR-LIST.
           MOVE PCD-TABLE TO WR-BUF.
           PERFORM CALL-WRFORM.
           GO TO REC-SECUR.
       END-PCD1.

       REC-PCD2.
           MOVE "ENTER THE 2ND OF NINE PURPOSE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCD2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PCD1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PCD2.
           MOVE VDUBUF TO PCD(2).

       REC-PCD3.
           MOVE "ENTER THE 3RD OF NINE PURPOSE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCD3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PCD2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PCD3.
           MOVE VDUBUF TO PCD(3).

       REC-PCD4.
           MOVE "ENTER THE 4TH OF NINE PURPOSE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCD4." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PCD3.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PCD4.
           MOVE VDUBUF TO PCD(4).

       REC-PCD5.
           MOVE "ENTER THE 5TH OF NINE PURPOSE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCD5." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PCD4.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PCD5.
           MOVE VDUBUF TO PCD(5).

       REC-PCD6.
           MOVE "ENTER THE 6TH OF NINE PURPOSE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCD6." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PCD5.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PCD6.
           MOVE VDUBUF TO PCD(6).

       REC-PCD7.
           MOVE "ENTER THE 7TH OF NINE PURPOSE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCD7." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PCD6.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PCD7.
           MOVE VDUBUF TO PCD(7).

       REC-PCD8.
           MOVE "ENTER THE 8TH OF NINE PURPOSE CODES:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCD8." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PCD7.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PCD8.
           MOVE VDUBUF TO PCD(8).

       REC-PCD9.
           MOVE "ENTER THE 9TH PURPOSE CODE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCD9." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PCD8.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PCD9.
           MOVE VDUBUF TO PCD(9).

       REC-SECUR.
           MOVE "SECURITY(S-SECURE,U-UNSECUR,R-REAL ESTATE),F3-ALL:"
                                                                 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010SECUR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PCD1.
           IF VDUSTATUS = "16" GO TO ALL-SECUR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SECUR.
           MOVE VDUBUF TO SECURITY-CODE.
           IF NOT (SECURITY-CODE = "S" OR "U" OR "R") GO TO REC-SECUR.
           GO TO END-SECUR.

       ALL-SECUR.
           MOVE "S  A010SECUR." TO VDU.
           MOVE "*" TO SECURITY-CODE VDUBUF.
           PERFORM SCREENIO.
       END-SECUR.

       INCLUDE-MULTIPLE-ACCTS.
           MOVE "INCLUDE MULTIPLE ACCOUNTS ( Y/N :)" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A010MANYACCTS." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO REC-SECUR.
           IF NOT ENTER-KEY AND NOT DOWN-KEY
              GO TO INCLUDE-MULTIPLE-ACCTS.

           MOVE VDUBUF TO  MANY-ACCOUNTS.

           IF MANY-ACCOUNTS NOT = "N"
              IF MANY-ACCOUNTS NOT = "Y"
              GO TO INCLUDE-MULTIPLE-ACCTS.

       REC-BWOPT.
           MOVE "INCLUDE BORROWER OPTIONS (Y/N):" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010BWOPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO INCLUDE-MULTIPLE-ACCTS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BWOPT.
           MOVE VDUBUF TO BWOPT.
           IF NOT (BWOPT = "Y" OR "N")
              GO TO REC-BWOPT.
           IF BWOPT = "Y"
               IF BORR-STAT NOT = "Y"
                  GO TO GOTO-SCREEN2
               ELSE
                  GO TO REC-RECBWOPT.

           MOVE "S  A010RECBWOPT." TO VDU.
           PERFORM SCREENIO.
           GO TO REC-FORM.

       REC-RECBWOPT.
           MOVE "ENTER BORROWER OPTIONS AGAIN (Y/N):" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010RECBWOPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BWOPT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-RECBWOPT.
           IF NOT (VDUBUF = "Y" OR "N")
              GO TO REC-RECBWOPT.
           IF VDUBUF = "N"
              IF BORR-STAT = "I"
                 GO TO REC-FORM
              ELSE
                 GO TO REC-BWOPT.

       GOTO-SCREEN2.

           MOVE "LP/ASLNE2" TO FORM-NAM.
           CALL "ASLNE2" USING FORM-PATH.
           CANCEL "ASLNE2".

      *.............LOGIC FOR REFRESH OF ASLNEX.FM
      *
           MOVE LOAN-FIELDS  TO SCREEN-BUF.
           MOVE LOAN-FIELDS2 TO SCREEN-BUF2.
           MOVE SORT-BY-ZIP  TO SCREEN-BUF4.

           IF BORR-STAT = " "
              GO TO REC-BWOPT.

       REC-FORM.
           MOVE "ENTER FORM TO BE SENT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BWOPT.
           IF VDUSTATUS = "1<"
              PERFORM FORM-SCAN
              IF NOT IO-GOOD
                 GO TO REC-FORM
              ELSE
                 MOVE SEL-FMNO TO VDUNUM0
           ELSE
             IF NOT (VDUSTATUS = "00" OR "1D")
                GO TO REC-FORM
             END-IF
           END-IF.
           MOVE VDUNUM0 TO FORMNO FD-FMNO.
           IF FORMNO NOT = 0
              PERFORM OPEN-FD1-FILE
              PERFORM READ-FD1-FILE
              PERFORM CLOSE-FD1-FILE
              IF NOT IO-GOOD
                 MOVE "REQUESTED FORM NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 GO TO REC-FORM
              ELSE
                 IF NOT (FD-1STFILE = 1 OR 2 OR 4)
                    MOVE "FIRST FILE MUST BE LN, BW, OR NO FILE"
                     TO MESS
                    PERFORM SEND-MESS
                    GO TO REC-FORM
                 ELSE
                    IF (FD-1STFILE = 1 OR 4) AND (FD-FMTYPE NOT = "S")
                       MOVE "FORM IS NOT SOLICITATION TYPE" TO MESS
                       PERFORM SEND-MESS
                       GO TO REC-FORM.

       CORP-FM.
      * IF BUILDING EXTRACTION FILE (LONPEX), MUST USE CORP PRINT FLAG
           IF ( FD-OPTION-EXTRACTION )
              MOVE "S  A010CORPFM." TO VDU
              MOVE "Y" TO CORP-FM-FG VDUBUF
              PERFORM SCREENIO
              GO TO REC-REPT.
           IF BEG-BRANCH = END-BRANCH
              MOVE "S  A010CORPFM." TO VDU
              MOVE "N" TO CORP-FM-FG VDUBUF
              PERFORM SCREENIO
              GO TO REC-REPT.
           MOVE "E  A010CORPFM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-FORM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CORP-FM.
           MOVE VDUBUF TO CORP-FM-FG.
           IF CORP-FM-FG NOT = "Y"
              IF CORP-FM-FG NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO CORP-FM.

       REC-REPT.
           MOVE "INCLUDE SELECTED ACCOUNT REPORT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010REPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CORP-FM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-REPT.
           MOVE VDUBUF TO REPT.
           IF NOT (REPT = "Y" OR "N")
              GO TO REC-REPT.
           IF FORMNO = 0
              IF REPT = "N"
                 GO TO REC-REPT.
       REC-ZIP.
           IF REPT = "N"
              MOVE "S  A010ZIP." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-PARM.

           MOVE "PRINT REPORT IN BRANCH/ZIP/ACCOUNT ORDER?"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010ZIP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-REPT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ZIP.
           MOVE VDUBUF TO SORT-BY-ZIP.
           IF NOT (SORT-BY-ZIP = "Y" OR "N")
              GO TO REC-ZIP.

       EXIT-PARM.
           PERFORM CLEAR-LEGEND.

      **************************************
      *   DISPLAY REPORT PARAMETERS
      **************************************
       DISPLAY-PARAMETERS SECTION.

           MOVE LOAN-FIELDS TO SCREEN-BUF.
           MOVE LOAN-FIELDS2 TO SCREEN-BUF2.
           MOVE SORT-BY-ZIP TO SCREEN-BUF4.
           MOVE SCREEN-BUF TO WR-BUF.
           MOVE SCREEN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SCREEN-BUF2 TO WR-BUF.
           MOVE SCREEN-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SCREEN-BUF4 TO WR-BUF.
           MOVE SCREEN-LIST4 TO WR-LIST.
           PERFORM CALL-WRFORM.

      /***************************
      *   DETAIL LINE PROCESS    *
      ****************************
       DETAIL-LINE-PROCESS SECTION.
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME.

           IF SORT-BY-ZIP = "Y"
              MOVE WK-ZIP TO D2-ZIP
              MOVE WK-OWNBR TO D2-OWNBR
              MOVE WK-LOAN-NUM TO D2-LOAN-NUM
              MOVE WK-NAME TO D2-NAME
              MOVE WK-SPNAME TO D2-SPNAME
           ELSE
              MOVE WK-OWNBR TO D-OWNBR
              MOVE WK-LOAN-NUM TO D-LOAN-NUM
              MOVE WK-NAME TO D-NAME
              MOVE WK-SPNAME TO D-SPNAME.

           IF WK-PHONE NOT = 0
              MOVE WK-PHONE TO D-PHONE.
           INSPECT D-PHONE REPLACING ALL "/" BY "-".
           MOVE WK-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE WK-COLLCD   TO D-COLLCD.
           IF WK-CURBAL NOT = 0
              MOVE WK-CURBAL TO D-CURBAL.
           MOVE WK-PROFILE TO D-PROFILE.
           INSPECT D-PROFILE REPLACING ALL "9" BY "-".
           IF WK-PAYSM NOT = 0
              MOVE WK-PAYSM TO D-PAYSM.
           IF WK-PAYSR NOT = 0
              MOVE WK-PAYSR TO D-PAYSR.
           MOVE WK-AVAIL TO D-AVAIL.
           IF WK-PERAVL NOT = 0
              MOVE WK-PERAVL TO D-PERAVL.

           PERFORM WRITE-DETAIL-LINE.
       END-DETAIL-LN-PROCESS.
           EXIT.

      /***************************
      *   DETAIL LINE PROCESS    *
      ****************************
       CREATE-WORK-FILE SECTION.
           IF FD-1STFILE = 2
              PERFORM CHECK-FOR-MANY-ACCTS-2.

           IF CREATE-FM-FG = "N"
              GO TO END-CREATE-WORK-REC.

           PERFORM CLEAR-WK-REC.

           MOVE SPACES TO D-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "  "
                INTO D-NAME.
           MOVE BW-SPFNAME TO D-SPNAME WK-SPNAME.
           MOVE D-NAME TO WK-NAME.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO HOLD-LPS-NUM.
           MOVE H-LPS-NUM TO WK-LOAN-NUM.
           IF BW-PHONE NOT = 0
              MOVE BW-PHONE TO WK-PHONE.
           MOVE LN-LOANDATE TO WK-LOANDATE.
           MOVE LN-COLLCD TO WK-COLLCD.
           IF LN-CURBAL NOT = 0
              MOVE LN-CURBAL TO WK-CURBAL.
           MOVE LN-CONPROFILE(1) TO WK-PROFILE.
           IF PAYS-MADE NOT = 0
              MOVE PAYS-MADE TO WK-PAYSM.
           IF PAYS-REMAIN NOT = 0
              MOVE PAYS-REMAIN TO WK-PAYSR.
           MOVE LN-CREDITLIM TO WK-AVAIL.

           IF MINAV NOT = 0
              MOVE NO-AVAILCREDIT TO WK-AVAIL
              IF LN-CREDITLIM NOT = 0
                 COMPUTE WK-PERAVL ROUNDED =
                      NO-AVAILCREDIT / LN-CREDITLIM * 100.

           MOVE LN-OWNBR TO WK-OWNBR.
           IF SORT-BY-ZIP = "Y"
              MOVE BW-ZIP TO WK-ZIP
              MOVE ZEROS TO WK-CLASS
           ELSE
              MOVE SPACES TO WK-ZIP
              MOVE LN-CLASS TO WK-CLASS.
           MOVE LN-ACCTNO TO WK-ACCTNO.

           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       END-CREATE-WORK-REC.
           EXIT.

      **************************
       ORIGIN-CD-CHECK SECTION.
      **************************
           MOVE "Y" TO ORIGIN-MATCH.
           MOVE 0 TO ORIGIN-SUB.

       ORIGIN-NEXT-CHAR.
           ADD 1 TO ORIGIN-SUB.

           IF ORIGIN-CHAR(ORIGIN-SUB) NOT = ORIGIN-CD(ORIGIN-SUB) AND
              ORIGIN-CD(ORIGIN-SUB) NOT = "?"
              MOVE "N" TO ORIGIN-MATCH
              GO TO ORIGIN-CD-CHECK-EXIT.

           IF ORIGIN-SUB < 4
              GO TO ORIGIN-NEXT-CHAR.

       ORIGIN-CD-CHECK-EXIT.
           EXIT.

      **************************
       CREATE-FM-REC SECTION.
      **************************

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.

      * CUTSHEET PRINT WONT WORK IF BRNO2 IS FILLED IN !!! IF THEY USE
      * CUSTSHEET, DO NOT ANSWER "Y" TO CORPORATE PRINT !!
           IF (CORP-FM-FG = "Y")
              MOVE LN-OWNBR   TO LPWSFM-EDIT-FM-BRNO2
                                 LPWSFM-EDIT-FM-BW-BRNO2.

           IF FD-1STFILE = 2
              MOVE LN-SSNO(1)             TO LPWSFM-EDIT-FM-SSNO
              MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY
           ELSE
              MOVE LN-ACCTNO          TO LPWSFM-EDIT-FM-ACCTNO
              MOVE FORMNO             TO LPWSFM-EDIT-FM-FORMNO
              MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY
              PERFORM CHECK-FOR-MANY-ACCTS
              IF CREATE-FM-FG = "N"
                 GO TO END-CREATE-FM-REC.

           IF FORMNO = 0
              GO TO SKIP-CREATE-FM.

      * THESE MUST STAY DOWN HERE BELOW CHECK-FOR-MANY-ACCTS

           PERFORM OPEN-FM1-FILE.

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE FORMNO        TO FM-FMNO.

           MOVE FM1-KEY TO HOLD-FM1-KEY.

           PERFORM READ-FM1-FILE.

           MOVE BW-ZIP             TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.

           IF IO-GOOD
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.

           PERFORM CLOSE-FM1-FILE.

       SKIP-CREATE-FM.
           IF REPT = "Y"
              PERFORM CREATE-WORK-FILE.

       END-CREATE-FM-REC.
           EXIT.

      ***********************************************
       CHECK-FOR-MANY-ACCTS SECTION.
      ***********************************************
           MOVE "Y" TO CREATE-FM-FG.

           IF MANY-ACCOUNTS = "Y"
              GO TO EXIT-CHECK-FOR-MANY-ACCTS.

           MOVE LN-SSNO(1)    TO MANYACCTS-SSNO.
           PERFORM READ-MANYACCTS-FILE.
           IF IO-BAD
              MOVE LN-POFFDATE  TO MANYACCTS-POFFDATE
              MOVE LN-OWNBR     TO MANYACCTS-OWNBR
              MOVE LN-CLASS     TO MANYACCTS-CLASS
              MOVE BW-ZIP       TO MANYACCTS-ZIP
              MOVE FM-INITKEY   TO MANYACCTS-INITKEY
              MOVE BR-DATA-PATH TO MANYACCTS-DATA-PATH
              MOVE BR-MACHINE   TO MANYACCTS-MACHINE
              PERFORM WRITE-MANYACCTS-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO EXIT-CHECK-FOR-MANY-ACCTS.

      *MULTIPLE ACCOUNTS - GET ACCOUNT WITH LATEST PAYOFF DATE
      *TO CREATE REQUEST
           IF LN-POFFDATE < MANYACCTS-POFFDATE
              MOVE "N" TO CREATE-FM-FG
              GO TO EXIT-CHECK-FOR-MANY-ACCTS.

           MOVE FM-INITKEY TO SAVE-INITKEY.
           MOVE "Y" TO CREATE-FM-FG.
           PERFORM REMOVE-PRIOR-REQUEST.
           MOVE SAVE-INITKEY TO FM-INITKEY.

       END-UPDATE-MANYACCTS.
           MOVE LN-SSNO(1)   TO MANYACCTS-SSNO.
           MOVE LN-POFFDATE  TO MANYACCTS-POFFDATE.
           MOVE FM-INITKEY   TO  MANYACCTS-INITKEY.
           MOVE LN-OWNBR     TO MANYACCTS-OWNBR.
           MOVE LN-CLASS     TO MANYACCTS-CLASS.
           MOVE BW-ZIP       TO MANYACCTS-ZIP.
           MOVE BR-DATA-PATH TO MANYACCTS-DATA-PATH.
           MOVE BR-MACHINE   TO MANYACCTS-MACHINE.
           PERFORM REWRITE-MANYACCTS-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       EXIT-CHECK-FOR-MANY-ACCTS.
           EXIT.


      ********************************************
       REMOVE-PRIOR-REQUEST SECTION.
      ********************************************
       REMOVE-PRIOR-FM.

           IF FORMNO = 0
              GO TO REMOVE-WORK-REC.

           MOVE MANYACCTS-OWNBR    TO FM-BRNO.
           MOVE MANYACCTS-INITKEY  TO FM-INITKEY.
           MOVE FORMNO             TO FM-FMNO.

           IF CORP-FM-FG = "N"
              MOVE FM-ALL-PATH TO SAVE-FM-ALL-PATH
              IF FM-ALL-DATA NOT = MANYACCTS-DATA-PATH
                 MOVE EXT-FILPATH-BASE     TO FM-ALL-BASE
                 MOVE MANYACCTS-MACHINE    TO FM-ALL-MACHINE
                 MOVE MANYACCTS-DATA-PATH  TO FM-ALL-DATA
                 MOVE FM-ALL-PATH          TO FM-PATH
                 MOVE "Y"                  TO FM-PATH-OVERRIDE.

           PERFORM OPEN-FM1-FILE.
           PERFORM READ-FM1-FILE.
           IF IO-GOOD
              PERFORM DELETE-FM1-FILE.
           PERFORM CLOSE-FM1-FILE.

           IF CORP-FM-FG = "N"
              MOVE SAVE-FM-ALL-PATH TO FM-ALL-PATH FM-PATH.

       REMOVE-WORK-REC.
            MOVE MANYACCTS-OWNBR TO WK-OWNBR.
            MOVE MANYACCTS-ZIP TO WK-ZIP.
            MOVE MANYACCTS-CLASS TO WK-CLASS.
            MOVE MANYACCTS-ACCTNO TO WK-ACCTNO.
            IF SORT-BY-ZIP = "N"
               MOVE SPACES TO WK-ZIP
            ELSE
               MOVE ZEROS TO WK-CLASS.
            PERFORM READ-WK-FILE.

            IF IO-GOOD
               PERFORM DELETE-WK-FILE.

      *---------------------------------------------
       EXIT-REMOVE-PRIOR-REQUEST.
           EXIT.

      *--------------------------------------------------------
      * THIS IS ANOTHER ROUTINE TO CHECK FOR MULTIPLE ACCTS,
      * JUST FOR KEEPING THEM OFF REPORT (OUT OF WKFILE) IF
      * 1ST FILE IS 2, CREATE-FM DOESN'T CHECK FOR MULTIPLES
      * WILL CHECK REGARDLESS OF MANY-ACCTS FLAG, SINCE 1ST
      * FILE OF 2 WILLL NEVER HAVE MULTIPLES!
      *--------------------------------------------------------
       CHECK-FOR-MANY-ACCTS-2 SECTION.
           MOVE "Y" TO CREATE-FM-FG.

           MOVE LN-SSNO(1)    TO MANYACCTS-SSNO.
           PERFORM READ-MANYACCTS-FILE.

           IF IO-BAD
              MOVE LN-POFFDATE TO MANYACCTS-POFFDATE
              MOVE LN-OWNBR    TO MANYACCTS-OWNBR
              MOVE LN-CLASS    TO MANYACCTS-CLASS
              MOVE BW-ZIP      TO MANYACCTS-ZIP
              MOVE LN-ACCTNO   TO MANYACCTS-ACCTNO
              MOVE FORMNO      TO MANYACCTS-FORMNO
              MOVE BR-DATA-PATH TO MANYACCTS-DATA-PATH
              MOVE BR-MACHINE  TO MANYACCTS-MACHINE
              PERFORM WRITE-MANYACCTS-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO EXIT-CHECK-FOR-MANY-ACCTS-2.

      *MULTIPLE ACCOUNTS - GET ACCOUNT WITH LATEST PAYOFF DATE
      *TO CREATE REQUEST
           IF LN-POFFDATE < MANYACCTS-POFFDATE
              MOVE "N" TO CREATE-FM-FG
              GO TO EXIT-CHECK-FOR-MANY-ACCTS-2.

           MOVE "Y" TO CREATE-FM-FG.
           PERFORM REMOVE-PRIOR-REQUEST-2.

       END-UPDATE-MANYACCTS-2.
           MOVE LN-SSNO(1)  TO MANYACCTS-SSNO.
           MOVE LN-POFFDATE TO MANYACCTS-POFFDATE.
           MOVE LN-ACCTNO TO MANYACCTS-ACCTNO.
           MOVE FORMNO TO MANYACCTS-FORMNO.
           MOVE LN-OWNBR TO MANYACCTS-OWNBR.
           MOVE LN-CLASS TO MANYACCTS-CLASS.
           MOVE BW-ZIP TO MANYACCTS-ZIP.
           MOVE BR-DATA-PATH TO MANYACCTS-DATA-PATH.
           MOVE BR-MACHINE TO MANYACCTS-MACHINE.
           PERFORM REWRITE-MANYACCTS-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      *---------------------------------------------
       EXIT-CHECK-FOR-MANY-ACCTS-2.
           EXIT.


       REMOVE-PRIOR-REQUEST-2 SECTION.
       REMOVE-WORK-REC-2.
            MOVE MANYACCTS-OWNBR TO WK-OWNBR.
            MOVE MANYACCTS-ZIP TO WK-ZIP.
            MOVE MANYACCTS-CLASS TO WK-CLASS.
            MOVE MANYACCTS-ACCTNO TO WK-ACCTNO.
            IF SORT-BY-ZIP = "N"
               MOVE SPACES TO WK-ZIP
            ELSE
               MOVE ZEROS TO WK-CLASS.
            PERFORM READ-WK-FILE.

            IF IO-GOOD
               PERFORM DELETE-WK-FILE.

      *---------------------------------------------
       EXIT-REMOVE-PRIOR-REQUEST-2.
           EXIT.

      /******************************
      *        HEAD PAGE ROUTINE
      *******************************
       HEAD-PAGE SECTION.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           IF SORT-BY-ZIP NOT = "Y"
              MOVE HEAD4 TO PR-REC
              PERFORM WRITE-PR-REC.
           MOVE HEAD5 TO PR-REC.
           PERFORM WRITE-PR-REC.
           IF SORT-BY-ZIP = "Y"
              MOVE HEAD6B TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 7 TO LN-COUNT
           ELSE
              MOVE HEAD6 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 8 TO LN-COUNT
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
      *       RANGE LINES           *
      *******************************
       PRINT-RANGE-LINES SECTION.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE SPACES TO HEAD3 HEAD4 HEAD5 HEAD6.

           MOVE "RECORD COUNT                :" TO RANGE-LINE.
           MOVE RECORD-COUNT                    TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION.


           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS             :" TO RANGE-LINE.
           MOVE BEG-CLASS                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING                      :" TO RANGE-LINE.
           MOVE END-CLASS                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DEALER            :" TO RANGE-LINE.
           MOVE BEG-DEALER                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING                      :" TO RANGE-LINE.
           MOVE END-DEALER                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING LOAN DATE         :" TO RANGE-LINE.
           MOVE BEG-LDATE                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING                      :" TO RANGE-LINE.
           MOVE END-LDATE                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACTION CODE                 :" TO RANGE-LINE.
           MOVE ACTION-CODE                    TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM INCR EXTR DEL CNTR2 :" TO RANGE-LINE.
           MOVE MAXDCNTR1                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM INCR FORM DEL CNTR1 :" TO RANGE-LINE.
           MOVE MAXDCNTR2                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM WHOLE PAYS MADE     :" TO RANGE-LINE.
           MOVE MINPAYS-MADE                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM WHOLE PAYS REMAIN   :" TO RANGE-LINE.
           MOVE MINPAYS-REMAIN                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "IN LAST 'X' MONTHS          :" TO RANGE-LINE.
           MOVE XMON                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING NEXT DUE DATE     :" TO RANGE-LINE.
           MOVE  BEG-NXDUE-DT                   TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING NEXT DUE DATE        :" TO RANGE-LINE.
           MOVE  END-NXDUE-DT                   TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "   HIGH CONTRACTUAL         :" TO RANGE-LINE.
           MOVE HCON                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   HIGH RECENCY             :" TO RANGE-LINE.
           MOVE HREC                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN CREDIT LIMIT            :" TO RANGE-LINE.
           MOVE MINCR                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAX CREDIT LIMIT            :" TO RANGE-LINE.
           MOVE MAXCR                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN AVAIL CREDIT            :" TO RANGE-LINE.
           MOVE MINAV                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "% OF CREDIT AVAIL           :" TO RANGE-LINE.
           MOVE PERAVL                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTHS SINCE LAST FORM      : " TO RANGE-LINE.
           MOVE MLFORM                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING LAST FORM NO      :" TO RANGE-LINE.
           MOVE BEGLFORM                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING LAST FORM NO         :" TO RANGE-LINE.
           MOVE ENDLFORM                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OWNED,SERVICE,BOTH          :" TO RANGE-LINE.
           MOVE OWNED                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ZERO BAL ACCOUNTS           :" TO RANGE-LINE.
           MOVE ZBAL                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM MONTHS SINCE PAYOFF :" TO RANGE-LINE.
           MOVE MINPOMON                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM MONTHS SINCE PAYOFF :" TO RANGE-LINE.
           MOVE MAXPOMON                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SOURCE CODES                :" TO RANGE-LINE.
           MOVE SCD(1)                         TO RANGE-TAB(1).
           MOVE SCD(2)                         TO RANGE-TAB(2).
           MOVE SCD(3)                         TO RANGE-TAB(3).
           MOVE SCD(4)                         TO RANGE-TAB(4).
           MOVE SCD(5)                         TO RANGE-TAB(5).
           MOVE SCD(6)                         TO RANGE-TAB(6).
           MOVE SCD(7)                         TO RANGE-TAB(7).
           MOVE SCD(8)                         TO RANGE-TAB(8).
           MOVE SCD(9)                         TO RANGE-TAB(9).
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PURPOSE CODES               :" TO RANGE-LINE.
           MOVE PCD(1)                         TO RANGE-TAB(1).
           MOVE PCD(2)                         TO RANGE-TAB(2).
           MOVE PCD(3)                         TO RANGE-TAB(3).
           MOVE PCD(4)                         TO RANGE-TAB(4).
           MOVE PCD(5)                         TO RANGE-TAB(5).
           MOVE PCD(6)                         TO RANGE-TAB(6).
           MOVE PCD(7)                         TO RANGE-TAB(7).
           MOVE PCD(8)                         TO RANGE-TAB(8).
           MOVE PCD(9)                         TO RANGE-TAB(9).
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SECURITY TYPE               :" TO RANGE-LINE.
           MOVE SECURITY-CODE                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE MULTIPLE ACCOUNTS   :" TO RANGE-LINE.
           MOVE MANY-ACCOUNTS                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BORROWER OPTIONS (Y/N)      :" TO RANGE-LINE.
           MOVE BWOPT                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FORM REQUEST NO             :" TO RANGE-LINE.
           MOVE FORMNO                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CORPORATE FORM PRINTING     :" TO RANGE-LINE.
           MOVE CORP-FM-FG                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "PRINT IN ZIP ORDER          :" TO RANGE-LINE.
           MOVE SORT-BY-ZIP                    TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


           IF BWOPT NOT = "Y"
              GO TO END-RANGE.

           MOVE "STATE                       :" TO RANGE-LINE.
           MOVE STATE                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGIN ZIP CODE              :" TO RANGE-LINE.
           MOVE BEGZIP                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING ZIP CODE             :" TO RANGE-LINE.
           MOVE ENDZIP                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "COUNTY CODE                 :" TO RANGE-LINE.
           MOVE COUNTY                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DWELLING CODE               :" TO RANGE-LINE.
           MOVE DWELL                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CENSUS TRACK CODE           :" TO RANGE-LINE.
           MOVE CENSUS                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S.M.S.A. CODE               :" TO RANGE-LINE.
           MOVE SMSA                            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RENT / OWN CODE             :" TO RANGE-LINE.
           MOVE RENT                            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM YEARS ADDRESS       :" TO RANGE-LINE.
           MOVE YRADDR                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM BORROWER AGE        :" TO RANGE-LINE.
           MOVE MINAGE                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM BORROWER AGE        :" TO RANGE-LINE.
           MOVE MAXAGE                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM YEARS EMPLOYED      :" TO RANGE-LINE.
           MOVE MINEMPL                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM PAY/MONTH           :" TO RANGE-LINE.
           MOVE MINPAY                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM TOTAL PAY/MONTH     :" TO RANGE-LINE.
           MOVE MINTPAY                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM TOTAL LIABILITY     :" TO RANGE-LINE.
           MOVE MINLIA                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM TOTAL LIABILITY     :" TO RANGE-LINE.
           MOVE MAXLIA                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CREDIT SCORE      :" TO RANGE-LINE.
           MOVE BEG-CRSCORE                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    CREDIT SCORE      :" TO RANGE-LINE.
           MOVE END-CRSCORE                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN MONTHS SINCE PAYOFF     :" TO RANGE-LINE.
           MOVE MPO                             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SALESMAN          :" TO RANGE-LINE.
           MOVE BEGSLSMN                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING SALESMAN             :" TO RANGE-LINE.
           MOVE ENDSLSMN                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SALES REP         :" TO RANGE-LINE.
           MOVE BEGREP                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING SALES REP            :" TO RANGE-LINE.
           MOVE ENDREP                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MARRIED   (I,S,O)           :" TO RANGE-LINE.
           MOVE MARRIED                        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ORIGIN                      :" TO RANGE-LINE.
           MOVE ORIGINX                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

      ****************************************
       FORM-SCAN SECTION.
      ****************************************
           MOVE 0   TO FDSCAN-KEY.
           MOVE "S" TO FDSCAN-FMTYPE.
           MOVE "S" TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF FDSCAN-STATUS NOT = "00" OR FDSCAN-KEY = " "
              MOVE 9 TO IO-FG
              GO TO EXIT-FORM-SCAN.
           MOVE "S  A030FORMNO." TO VDU.
           MOVE FDSCAN-KEY-ALP TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "00" TO VDUSTATUS.
           MOVE FDSCAN-KEY-ALP TO SEL-FMNOX.

       EXIT-FORM-SCAN.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      *******************************************
      *    TEST PROFILES:
      *
      *    0 - CUR       6 - 180 TO 269
      *    1 - 30        7 - 270+
      *    2 - 60        8 - NOT USED
      *    3 - 90        9 - MONTH NOT GRADED
      *    4 - 120
      *    5 - 150
      *******************************************
       TEST-PROFILE SECTION.
           MOVE "Y" TO PROFILE-OK.
           DIVIDE 30 INTO PROFILE-HIGH.
           IF PROFILE-HIGH > 6
              IF PROFILE-HIGH < 9
                 MOVE 6 TO PROFILE-HIGH
              ELSE
                 MOVE 7 TO PROFILE-HIGH.
           MOVE 1 TO SUB.
       TEST-PROFILE-AGAIN.
           IF SUB > XMON OR > 12
              GO TO TEST-PROFILE-EXIT.
           IF PROFILE-TAB(SUB) = 9
              GO TO TEST-PROFILE-EXIT.
           IF PROFILE-TAB(SUB) NOT > PROFILE-HIGH
              ADD 1 TO SUB
              GO TO TEST-PROFILE-AGAIN.
           MOVE " " TO PROFILE-OK.
       TEST-PROFILE-EXIT.
           EXIT.

      ********************************************
      *    TEST FOR MATCH USING WILD CARDS
      *
      *    MATCH1-CODE - FIELD TO BE TESTED
      *    MATCH2-CODE - FIELD CONTAINING WILD CARDS
      *    MATCH-MAX   - SIZE OF FIELD TO BE TESTED
      *
      *    MATCH-OK    - 'Y' SUCESSFUL MATCH
      ********************************************
       TEST-FOR-MATCH SECTION.
           MOVE "Y" TO MATCH-OK.
           MOVE 1 TO SUB.
       TEST-FOR-MATCH-AGAIN.
           IF MATCH2(SUB) NOT = "?"
              IF MATCH2(SUB) NOT = MATCH1(SUB)
                 MOVE " " TO MATCH-OK
                 GO TO EXIT-TEST-FOR-MATCH.
           IF SUB < MATCH-MAX
              ADD 1 TO SUB
              GO TO TEST-FOR-MATCH-AGAIN.
       EXIT-TEST-FOR-MATCH.
           EXIT.

      ************************************
      *    TEST FOR MATCHING SOURCE CODE
      ************************************
       TEST-SOURCE-CODES SECTION.
           MOVE " " TO SOURCE-OK.
           MOVE 1 TO SUB.
       TEST-SOURCE-CODES-AGAIN.
           IF SUB > 9
              GO TO TEST-SOURCE-CODES-EXIT.
           IF LN-SRCD NOT = SCD(SUB)
              ADD 1 TO SUB
              GO TO TEST-SOURCE-CODES-AGAIN.
           MOVE "Y" TO SOURCE-OK.
       TEST-SOURCE-CODES-EXIT.
           EXIT.

      ************************************
      *    TEST FOR MATCHING PURPOSE CODE
      ************************************
       TEST-PURPOSE-CODES SECTION.
           MOVE " " TO PURPOSE-OK.
           MOVE 1 TO SUB.
       TEST-PURPOSE-CODES-AGAIN.
           IF SUB > 9
              GO TO TEST-PURPOSE-CODES-EXIT.
           IF LN-PURCD NOT = PCD(SUB)
              ADD 1 TO SUB
              GO TO TEST-PURPOSE-CODES-AGAIN.
           MOVE "Y" TO PURPOSE-OK.
       TEST-PURPOSE-CODES-EXIT.
           EXIT.

       CLEAR-WK-REC SECTION.
           MOVE SPACES TO WK-REC.
           MOVE ZEROS TO WK-OWNBR WK-CLASS WK-ACCTNO WK-PHONE
                         WK-LOANDATE WK-CURBAL WK-PROFILE WK-PAYSM
                         WK-PAYSR WK-PERAVL WK-AVAIL.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO ASLNEX-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ASLNEX-SYSDATE.
           DISPLAY ASLNEX-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ASLNEX-FORM-FIELDS TO WR-BUF.
           MOVE ASLNEX-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE ASLNEX-HELP--FILE TO HELP-LINK-FILE.
           MOVE ASLNEX-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ASLNEX_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
      *    MISC ROUTINES SECTION
      ************************************************
       MISC-ROUTINES SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       GET-USER-BRANCH.
           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "N" TO BR-AVAIL-CREDIT
           ELSE
              MOVE EXT-CONAME-USER-LOC TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF NOT IO-GOOD
                 MOVE "N" TO BR-AVAIL-CREDIT.
       GET-NOTICE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-FG NOT = 0
              MOVE LN-ACCTNO     TO CRNOR2-ACCTNO
              MOVE TRANS-DATE    TO CRNOR2-SYSDATE
              MOVE "CL/CRNOR2"   TO FORM-NAM
              MOVE "N"           TO CRNOR2-LPREC-FG
              CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                    POFF-WORKERS LN-REC
              CANCEL FORM-PROGX.

       GET-SPR.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.
       GET-LC.
           IF LC-CODE NOT = LAST-LC-CODE
              MOVE LC-CODE TO LAST-LC-CODE
              PERFORM READ-LC1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.
       TEST-BW-YR-ADDR-MATCH.
           IF MATCH1(SUB1) NUMERIC
              MOVE MATCH1(SUB1) TO TIME-WK(SUB)
              SUBTRACT 1 FROM SUB
           ELSE
              MOVE MATCH1(SUB1) TO TIME-ALP.
           SUBTRACT 1 FROM SUB1.
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "INVALID BRANCH" TO H-BRDESC
           ELSE
              MOVE BR-NAME TO H-BRDESC.
           MOVE 99 TO LN-COUNT.
       SET-CLASS-HEADINGS.
           MOVE HOLD-CLASS TO H-LCNO.
           MOVE LC-DESC TO H-LCDESC.
           MOVE 99 TO LN-COUNT.
       TEST-SECURITY-TYPE.
           MOVE " " TO SECURITY-OK.
           MOVE "CO"      TO HOLD-CD-TYPE.
           MOVE LN-COLLCD TO HOLD-CD-CODE.
           IF HOLD-CD1-KEY NOT = CD1-KEY
              MOVE HOLD-CD1-KEY TO CD1-KEY
              PERFORM READ-CD1-FILE
              IF NOT IO-GOOD
                 MOVE SPACES TO CD-CO-SECURED.
           IF CD-CO-SECURED = SECURITY-CODE
              MOVE "Y" TO SECURITY-OK.

      /***************************************
      *         I / O      R O U T I N E S
      ****************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

       OPEN-MANYACCTS-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           OPEN OUTPUT MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO OPEN-MANYACCTS-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE MANYACCTS-FILE
              GO TO OPEN-MANYACCTS-FILE-OUTPUT.
           UNLOCK MANYACCTS-FILE.

       OPEN-MANYACCTS-FILE.
           PERFORM OPEN-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           OPEN I-O MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO OPEN-MANYACCTS-FILE.
           IF IO-FG = 7
              CLOSE MANYACCTS-FILE
              GO TO OPEN-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.

       READ-MANYACCTS-FILE.
           PERFORM READ-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           READ MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO READ-MANYACCTS-FILE.

       WRITE-MANYACCTS-FILE.
           PERFORM WRITE-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           WRITE MANYACCTS-REC.
           IF IO-FG = 8
              GO TO WRITE-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.


       REWRITE-MANYACCTS-FILE.
           PERFORM REWRITE-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           REWRITE MANYACCTS-REC.
           IF IO-FG = 8
              GO TO REWRITE-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.

       DELETE-MANYACCTS-FILE.
           PERFORM DELETE-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           DELETE MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO DELETE-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.

       CLOSE-MANYACCTS-FILE.
           PERFORM CLOSE-IT.
           CLOSE MANYACCTS-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/ASMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE MANYACCTS-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ASLNEX-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
