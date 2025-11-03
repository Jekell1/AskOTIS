       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHREPO.
       DATE-WRITTEN. 10/10/95.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   CHANGE REPOSSESSION STATUS
      *                 CALLED FROM F9 INTERRUPT
      *                        LONPFC (SS), LONPFE, LONPFG
      *
      *  REV:
      *  MJD 100995 REWROTE CHREPO USING DSMAIN AS SHELL.
      *              FIELDS ARE ENTERED DIRECTLY INTO RECORD, NOT BUFFER.
      *              RECORD IS WRITTEN BETWEEN PAGES, EVEN NEW RECORDS.
      *              F1-RESET WORKS FROM ANY SCREEN FIELD
      *              OLD SAVED AS CHREPO.1009.
      *  BAH 033098 ADDED PASSWORD TO MODIFY ANY FIELD, FEDERATED PR# 650
      *  CS  010615 ADDED LS-X-AUCTION-EXPENSE (REGACC PR# 1478)
      *  CS  010618 IF GP-REPO-CD = "A" SKIP ENTRY OF LS-X-SOLD-TO-AMT,
      *             LS-X-AUCTION-EXPENSE, LS-X-10DAY-COST  ON NEW RECORD,
      *             OTHERWISE VERIFY PASSWORD ENTERED
      *             [DIR:WI PROG:CHREPO  SEQ:2] REGACC PR#1478
      *  CS  010618 IF GP-REPO-CD = "A" VERIFY PASSWORD IF ALTERING
      *             LS-X-OTH-EXP-CKAMT(FIELD 6 PAGE2 UPDATED WITH RE AMOUNTS
      *             REGACC PR#1478
      *  BAH 020626 ADDED LS-X-REASON-CODE, IF GP-PL-REPO-REASON = "Y",
      *             YOU ENTER THE REASON CODE, OTHERWISE, YOU ENTER THE
      *             REASON DESCRIPTION, REGACC PR# 1755
      *  BLV 020701 FIX PROBLEMS ON PAGE 2 & 3, GP-PL-REASON LOGIC
      *             SHOULD BE ONLY ON PAGE 1
      *  BLV 020718 ADDED PAGE 4 WITH NEW FIELDS, REGACC PR # 1840,
      *             ALSO DISPLAYING SOME EXTRA FIELDS FROM OTHER PAGES
      *             & RECORDS, HAD TO ADD LN, LT, BW, BR FILES;
      *             NOTE, NOT ENOUGH ROOM IN LS-REC, & THEY WANTED 40
      *             BYTES OF OTHER EQUIPMENT, SO I'M USING THE 2ND LINE
      *             OF REPAIR DESC FOR THIS FOR THEM;
      *             ALSO, NOTE REGACC IS PLANNING TO JUST USE THIS PAGE
      *             SO THERE ARE FIELDS FROM OTHER PAGES WHICH ARE
      *             DISPLAYED HERE & CAN BE CHANGED HERE.
      *             A LOT OF THESE FIELDS REALLY BELONG IN THE
      *             COLLATERAL RECORD (COLOR, TRIM, ETC) BUT THERE'S
      *             NOT ENOUGH ROOM...NEXT RELEASE!
      *  BLV 020820 ADDED LS-X-EXTRACTED TO PAGE 4 , REGACC PR# 1872
      *  BAH 020830 IF GP-REPO-CD = "A" VERIFY PASSWORD IF ALTERING
      *             LS-X-RB-AMT (FIELD 3 PAGE 3)  REGACC PR#1896
      *  BAH 020924 IF GP-REPO-CD = "A", GO RIGHT TO PAGE 4, REGACC PR# 2
      *  BAH 020924 PAGE 4, ADDED 13. TITLE DATE SENT, LS-X-TITLE-SENT-DATE
      *             AND TITLE SENT LOC: LS-X-OTH-EXP(3) < COPIED FROM PAGE 2
      *             REGACC PR# 2
      *  BAH 060718 PASSWORD PROTECT PAGE 1 FIELD 2. TITLE MILEAGE,
      *             SOAUTO PR# 2816
      *  BAH 140909 ONLY ALLOW "I" OR "V" FOR LS-X-VOLUNTARY-FG, PR#4607 PFS
      *   CS 150212 NEW-RECORD-SETUP MISSING;IF CUR-FORM = 4 DISPLAY-PAGE4-
      *             HEADER SO ALL THE HEADER INFO, ACCT,COLLATERAL,BR MISSING
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      *  BAH 160719 ADDED LS-X-KEYS TO PAGE 2, FIELD 021, SOAUTO PR#5133
      *  BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 190125 SPLIT CDFILE
      * BAH 2024.0417 REMOVED CHREPOW AND CALL PROGRAM INSTEAD OF WINDOW
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CHREPO.CBL S35 02/23/24-11:38:34 >".

      ******************************************************************
      *
      *     P R O G R A M    W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *-----------------------------------------------------------------
       01  LEGEND          PIC X(70).

       01  ALLOW-EXIT      PIC X     VALUE SPACES.

       01  CHREPO-LINES              PIC 99  VALUE 22.
       01  CHREPO-COLUMNS            PIC 99  VALUE 78.
       01  CHREPO-BEGIN-Y            PIC 99  VALUE 2.
       01  CHREPO-BEGIN-X            PIC 99  VALUE 2.

       01  ERRCD           PIC X     VALUE SPACES.

       01  HOLD-SYS-DATE   PIC 9(8).

       01  SAVE-PATH       PIC X(35).
       01  NAME-WORKER     PIC X(34).

      *************************************
      * CALCULATION WORKERS FOR TOTALS
      *************************************
       01 TOTAL-EXP-PAID   PIC S9(7)V99   COMP-3.
       01 SUM1             PIC S9(7)V99   COMP-3.
       01 SUM2             PIC S9(7)V99   COMP-3.
       01 SUM3             PIC S9(7)V99   COMP-3.
       01 A063-C           PIC S9(6)V99   COMP-3.
       01 A073-C           PIC S9(6)V99   COMP-3.
       01 A101-C           PIC S9(6)V99   COMP-3.
       01 A111-C           PIC S9(6)V99   COMP-3.
       01 B044-C           PIC S9(6)V99   COMP-3.
       01 B054-C           PIC S9(6)V99   COMP-3.
       01 B063-C           PIC S9(6)V99   COMP-3.
       01 B073-C           PIC S9(6)V99   COMP-3.
       01 B083-C           PIC S9(6)V99   COMP-3.

      *-----------------------------------------------------------------
       01  CHAN              PIC X(4)  VALUE "CHAN".
       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).

      *----------------------------------------------------------------
      * NEW-REC      : "Y" = NEW-RECORD.
      *                "N" = EXISTING RECORD (DEFAULT).
      *
      * FILE-CREATED : "Y" FILE HAS BEEN WRITTEN (DEFAULT).
      *                "N" FILE HAS NOT BEEN WRITTEN (NEW RECORD).
      *----------------------------------------------------------------
       01  NEW-REC           PIC X     VALUE "N".
       01  FILE-CREATED      PIC X     VALUE "Y".

       01  NEW-FLAG          PIC 9.
       01  FIRST-TIME        PIC 9     VALUE 0.

       01  HOLD-LSX1-KEY.
           03  HOLD-BRNO     PIC 9(4).
           03  HOLD-ACCTNO   PIC 9(8).
           03  HOLD-SEQNO    PIC 99.


      * ORIGINAL RECORD
           COPY "LIBLP/LP01LSX.CPY" REPLACING LEADING "LSX" BY "LSX-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LSX.CPY" REPLACING LEADING "LSX" BY "LSX-CH".

      *-----------------------------------------------------------------

       01  FORM-BUF.
           03  FILLER        PIC X(9) VALUE "WI/CHREPO".
           03  FILLER        PIC X(9) VALUE "WI/CHREP2".
           03  FILLER        PIC X(9) VALUE "WI/CHREP3".
           03  FILLER        PIC X(9) VALUE "WI/CHREP4".
       01  FORM-BUF-RED REDEFINES FORM-BUF.
           03  FORM-NAME     PIC X(9) OCCURS 4.
       01  FORM-BUFX REDEFINES FORM-BUF.
           03  FORM-NAMEX     OCCURS 4.
               05  FILLER    PIC XXX.
               05  FORM-VDU-NAME PIC X(6).

       01  FORM-SEL          PIC 9     VALUE 0.
       01  CUR-FORM          PIC 9     VALUE 1.

      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER1
      ******************************************************************
       01  DISPLAY-BUF1      VALUE SPACES.
           03  D-SEQNO     PIC 9.
           03  A01         PIC X.
           03  A02         PIC ZZZZZZ.
           03  A03         PIC X.
           03  A031        PIC ZZ/ZZ/ZZ.
           03  A032        PIC X(16).
           03  A041        PIC XX.
           03  A04         PIC X(60).
           03  A05         PIC X(30).
           03  A051        PIC ZZ/ZZ/ZZ.
           03  A06         PIC ZZ/ZZ/ZZ.
           03  A061        PIC ZZ/ZZ/ZZ.
           03  A062        PIC ZZZZZZ.
           03  A063        PIC ZZZ,ZZ9.99.
           03  A07         PIC X(30).
           03  A071        PIC ZZ/ZZ/ZZ.
           03  A072        PIC ZZZZZZ.
           03  A073        PIC ZZZ,ZZ9.99.
           03  A08         PIC ZZ/ZZ/ZZ.
           03  A09         PIC ZZ/ZZ/ZZ.
           03  A091        PIC X(10).
           03  A092        PIC X(10).
           03  A10         PIC ZZ/ZZ/ZZ.
           03  A101        PIC ZZZ,ZZ9.99.
           03  A102        PIC X(10).
           03  A11         PIC ZZ/ZZ/ZZ.
           03  A111        PIC ZZZ,ZZ9.99.
           03  A112        PIC X(10).

       01  DISPLAY-LIST1.
           03  FILLER      PIC X(12) VALUE
           "SEQ 010 020 ".
           03  FILLER      PIC X(32) VALUE
           "030 031 032 041 040 050 051 060 ".
           03  FILLER      PIC X(28) VALUE
           "061 062 063 070 071 072 073 ".
           03  FILLER      PIC X(28) VALUE
           "080 090 091 092 100 101 102 ".
           03  FILLER      PIC X(12) VALUE
           "110 111 112.".

      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER2
      ******************************************************************
       01  DISPLAY-BUF2    VALUE " ".
           03  B01         PIC ZZZZZZ.
           03  B02         PIC X.
           03  B021        PIC X.
           03  B03         PIC X(60).
           03  B031        PIC X(60).
           03  B032        PIC ZZZ,ZZ9.99.
           03  B04         PIC X(30).
           03  B041        PIC ZZ/ZZ/ZZ.
           03  B042        PIC ZZ/ZZ/ZZ.
           03  B043        PIC ZZZZZZ.
           03  B044        PIC ZZZ,ZZ9.99.
           03  B05         PIC X(30).
           03  B051        PIC ZZ/ZZ/ZZ.
           03  B052        PIC ZZ/ZZ/ZZ.
           03  B053        PIC ZZZZZZ.
           03  B054        PIC ZZZ,ZZ9.99.
           03  B06         PIC X(30).
           03  B061        PIC ZZ/ZZ/ZZ.
           03  B062        PIC ZZZZZZ.
           03  B063        PIC ZZZ,ZZ9.99.
           03  B07         PIC X(30).
           03  B071        PIC ZZ/ZZ/ZZ.
           03  B072        PIC ZZZZZZ.
           03  B073        PIC ZZZ,ZZ9.99.
           03  B08         PIC X(30).
           03  B081        PIC ZZ/ZZ/ZZ.
           03  B082        PIC ZZZZZZ.
           03  B083        PIC ZZZ,ZZ9.99.

       01  DISPLAY-LIST2.
           03  FILLER      PIC X(12) VALUE
           "010 020 021 ".
           03  FILLER      PIC X(28) VALUE
           "030 031 032 040 041 042 043 ".
           03  FILLER      PIC X(28) VALUE
           "044 050 051 052 053 054 060 ".
           03  FILLER      PIC X(24) VALUE
           "061 062 063 070 071 072 ".
           03  FILLER      PIC X(20) VALUE
           "073 080 081 082 083.".

      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER3
      ******************************************************************
       01  DISPLAY-BUF3      VALUE SPACES.
           03  C01         PIC ZZ/ZZ/ZZ.
           03  C011        PIC ZZ/ZZ/ZZ.
           03  C012        PIC ZZZ,ZZ9.99.
           03  C02         PIC ZZ/ZZ/ZZ.
           03  C021        PIC ZZ/ZZ/ZZ.
           03  C022        PIC ZZZ,ZZ9.99.
           03  C03         PIC ZZ/ZZ/ZZ.
           03  C031        PIC ZZ/ZZ/ZZ.
           03  C032        PIC ZZZ,ZZ9.99.
           03  C04         PIC Z,ZZZ,ZZ9.99.
           03  C05         PIC ZZZ,ZZ9.99.
           03  C06         PIC X(30).
           03  C061        PIC X(30).
           03  C062        PIC X(30).
           03  C063        PIC ZZ/ZZ/ZZ.
           03  C064        PIC ZZZ,ZZ9.99.
           03  C065        PIC ZZZ,ZZ9.99.
           03  C07         PIC ZZZZ.ZZ.
           03  C071        PIC ZZZZZZ.
           03  C072        PIC ZZZ,ZZ9.99.
           03  C08         PIC ZZZ,ZZ9.99.
           03  C09         PIC X.
           03  C10         PIC Z,ZZZ,ZZ9.99.

       01  DISPLAY-LIST3.
           03  FILLER      PIC X(24) VALUE
           "010 011 012 020 021 022 ".
           03  FILLER      PIC X(28) VALUE
           "030 031 032 040 050 060 061 ".
           03  FILLER      PIC X(32) VALUE
           "062 063 064 065 070 071 072 080 ".
           03  FILLER      PIC X(08) VALUE
           "090 100.".

       01  DISPLAY-BUF4      VALUE SPACES.
           03  DBWNAME     PIC X(30).
           03  DACCT       PIC ZZZZZ9.
           03  DSEQ        PIC 9.
           03  DBRNO       PIC ZZZ9.
           03  DBRNAME     PIC X(40).
           03  DBRCITYST   PIC X(19).
           03  DYEAR       PIC ZZZZ.
           03  DMAKE       PIC X(15).
           03  DMODEL      PIC X(15).
           03  DVIN        PIC X(20).
           03  D01         PIC XXX.
           03  D011        PIC X.
           03  D012        PIC X(10).
           03  D013        PIC Z9.
           03  D014        PIC ZZZZZZ.
           03  D02         PIC X.
           03  D021        PIC X.
           03  D022        PIC X.
           03  D023        PIC X.
           03  D024        PIC X.
           03  D025        PIC X.
           03  D026        PIC X.
           03  D027        PIC X.
           03  PROMPT028   PIC X(12).
           03  D028        PIC X(60).
           03  D03         PIC X(30).
           03  D04         PIC X(12).
           03  D041        PIC X(12).
           03  D05         PIC X(30).
           03  D06         PIC X(24).
           03  D061        PIC X(15).
           03  D062        PIC X(2).
           03  D063        PIC X(10).
           03  D07         PIC 99/99/99.
           03  D071        PIC 99/99/99.
           03  D08         PIC X.
           03  D081        PIC X.
           03  D082        PIC X.
           03  D09         PIC X.
           03  DCURBAL     PIC ZZZ,999.99.
           03  D10         PIC ZZ/ZZ/ZZ.
           03  D101        PIC X(16).
           03  D11         PIC X.
           03  D111        PIC XX.
           03  D112        PIC X(60).
           03  D120        PIC X.
           03  D130        PIC 99/99/99.
           03  D131        PIC X(30).

       01  DISPLAY-LIST4.
           03  FILLER      PIC X(57) VALUE
           "BWNAME ACCT SEQ BRNO BRNAME BRCITYST YEAR MAKE MODEL VIN ".
           03  FILLER      PIC X(52) VALUE
           "010 011 012 013 014 020 021 022 023 024 025 026 027 ".
           03  FILLER      PIC X(14) VALUE
           "PROMPT028 028 ".
           03  FILLER      PIC X(52) VALUE
           "030 040 041 050 060 061 062 063 070 071 080 081 082 ".
           03  FILLER      PIC X(43) VALUE
           "090 CURBAL 100 101 110 111 112 120 130 131.".
      *-----------------------------------------------------------------

       01  DISPLAY-BUF-HEAD4      VALUE SPACES.
           03  RBWNAME     PIC X(30).
           03  RACCT       PIC ZZZZZ9.
           03  RSEQ        PIC 9.
           03  RBRNO       PIC ZZZ9.
           03  RBRNAME     PIC X(25).
           03  RORGST      PIC XX.
           03  RORGBR      PIC 9(4).
           03  RYEAR       PIC ZZZZ.
           03  RMAKE       PIC X(15).
           03  RMODEL      PIC X(15).
           03  RVIN        PIC X(20).
           03  RCURBAL     PIC ZZZ,999.99.

       01  DISPLAY-LIST-HEAD4.
           03  FILLER      PIC X(40) VALUE
           "BWNAME ACCT SEQ BRNO BRNAME ORGST ORGBR ".
           03  FILLER      PIC X(27) VALUE
           "YEAR MAKE MODEL VIN CURBAL.".

       01  DISPLAY-BUF-REG4      VALUE SPACES.
      * DISP/HLD
           03  R01         PIC XX.
      * NEXT HLD RVW DATE
           03  R011        PIC 99/99/99.
      * CASE#
           03  R012        PIC X(10).
      * COLLATERAL LOC
           03  R02         PIC XX.
           03  R021        PIC X(15).
      * VIN MATCH
           03  R022        PIC X.
      * VOL/INVOL
           03  R03         PIC X.
      * REASON
           03  R031        PIC XX.
           03  R032        PIC X(28).
      * REPODATE
           03  R04         PIC 99/99/99.
      * REDEEM
           03  R041        PIC 99/99/99.
      * PERSONALS
           03  R042        PIC X.
      * MILES
           03  R05         PIC Z(6).
      * KEYS
           03  R051        PIC X.
      * DRIVES
           03  R052        PIC X.
      * DMG
           03  R053        PIC X.
      * COND REP
           03  R054        PIC X.
      * INS CLAIM
           03  R055        PIC X.
      * ASSIGNED TO
           03  R06         PIC X(30).
      * SUPPLIER ID
           03  R061        PIC 9(7).
      * ASGND TO PHONE
           03  R062        PIC X(12).
      * FAX
           03  R063        PIC X(12).
      * TRNSPT EMAIL
           03  R064        PIC X(40).

      * STORED LOC
           03  R07         PIC X(40).
      * STORED ADDR
           03  R08         PIC X(40).
           03  R081        PIC X(20).
           03  R082        PIC X(2).
           03  R083        PIC X(10).
      * AGENCY NAME
           03  R09         PIC X(40).
      * PHONE
           03  R091        PIC X(12).
      * AGENCY ADDR
           03  R10         PIC X(40).
           03  R101        PIC X(20).
           03  R102        PIC X(2).
           03  R103        PIC X(10).
      * EXTRACTED
           03  R11         PIC X.

       01  DISPLAY-LIST-REG4.
           03  FILLER      PIC X(24) VALUE
           "010 011 012 020 021 022 ".
           03  FILLER      PIC X(48) VALUE
           "030 031 032 040 041 042 050 051 052 053 054 055 ".
           03  FILLER      PIC X(40) VALUE
           "060 061 062 063 064 070 080 081 082 083 ".
           03  FILLER      PIC X(28) VALUE
           "090 091 100 101 102 103 110.".
 
       01  MAX-ENTRY.
           03  FILLER        PIC X(8) VALUE "11081013".
       01  MAX-TABLE REDEFINES MAX-ENTRY.
           03  MAX           PIC 99   OCCURS 4.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #1
      ******************************************************************
       01  SPEC-TABLE1.
      * 1  TITLE/LIEN PERFECTION
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 2  TITLE MILEAGE
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 3  VOL/INVOL FLAG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 4  REASON
           03  FILLER      PIC X(8) VALUE "E NA600 ".
      * 5  ASSIGNED TO
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 6  REPOSSESSION DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 7  STORED AT
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 8  WILL CUSTOMER REDEEM DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 9  CREDIT BUREAU NOTIFIED DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *10  REDEMPTION LETTER SENT DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *11  REPO TITLE APPLICATION DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".

       01  SPEC-TAB1      REDEFINES SPEC-TABLE1.
           03  SPEC-REC1  OCCURS 11 TIMES.
               05  SPEC1   PIC X(7).
               05  CHFG1   PIC X.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #2
      ******************************************************************
       01  SPEC-TABLE2.
      * 1  MILEAGE
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 2  CONDITION REPORT RECEIVED
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 3  REPAIRS
           03  FILLER      PIC X(8) VALUE "E NA600 ".
      * 4  ASSIGNED TO 1
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 5  ASSIGNED TO 2
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 6  OTHER EXP 1
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 7  OTHER EXP 2
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 8  OTHER EXP 3
           03  FILLER      PIC X(8) VALUE "E NA300 ".

       01  SPEC-TAB2      REDEFINES SPEC-TABLE2.
           03  SPEC-REC2  OCCURS 8 TIMES.
               05  SPEC2   PIC X(7).
               05  CHFG2   PIC X.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #3
      ******************************************************************
       01  SPEC-TABLE3.
      * 1  REQUESTED DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 2  SIC INS FILE DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 3  REFUNDS REQUESTED DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 4  NET BALANCE AFTER ADD-ONS AND REFUNDS
           03  FILLER      PIC X(8) VALUE "E NN072 ".
      * 5  NADA VALUE
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      * 6  SOLD TO
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 7  DLR RES NO
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 8  CHARGE OFF
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      * 9  IS DEF. COLLECTABLE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *10  DEFICIENCY BALANCE
           03  FILLER      PIC X(8) VALUE "E NN072 ".

       01  SPEC-TAB3      REDEFINES SPEC-TABLE3.
           03  SPEC-REC3  OCCURS 10 TIMES.
               05  SPEC3   PIC X(7).
               05  CHFG3   PIC X.

       01  SPEC-TABLE4.
      * 1  TRIM
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 2  AUTO TRANSMISSION  Y/N
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 3  ASSIGNED TO
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 4  ASSIGNED TO AGENT PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 5  STORED AT
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 6  STOARGE ADDRESS
           03  FILLER      PIC X(8) VALUE "E NA240 ".
      * 7  REPOSSESSION DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 8  TITLE/LIEN PERFECTION
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 9  DEALER INSURANCE REBATES REQUESTED
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *10  DATE AUTHORIZED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *11  VOL/INVOL FLAG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *12  EXTRACTED
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *13  TITLE SENT DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".

       01  SPEC-TAB4      REDEFINES SPEC-TABLE4.
           03  SPEC-REC4  OCCURS 13 TIMES.
               05  SPEC4   PIC X(7).
               05  CHFG4   PIC X.

      *****************************************************
       01  SPEC-TABLE-REG4.
      * 1 DISP/HLD
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 2 COLLATERAL LOC
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 3 VOL/INVOL
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 4 REPODATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 5 MILES
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 6 ASSIGNED TO
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 7 STORED LOC
           03  FILLER      PIC X(8) VALUE "E NA400 ".
      * 8 STORED ADDR
           03  FILLER      PIC X(8) VALUE "E NA400 ".
      * 9 AGENCY NAME
           03  FILLER      PIC X(8) VALUE "E NA400 ".
      * 10 AGENCY ADDR
           03  FILLER      PIC X(8) VALUE "E NA400 ".
      * 11 EXTRACTED
           03  FILLER      PIC X(8) VALUE "E NA010 ".

       01  SPEC-TAB-REG4      REDEFINES SPEC-TABLE-REG4.
           03  SPEC-REC-REG4  OCCURS 11 TIMES.
               05  SPEC-REG4   PIC X(7).
               05  CHFG-REG4   PIC X.

      ******************************************************************
      *    *   ELEMENT SPECIFICATION WORKER
      ******************************************************************
       01  SPEC-TAB.
           03  SPEC-REC.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBWI/CHREPO_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBGB/PHON400.CPY".

       01  CHREPO-WINDOW-HANDLE      PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHREPO_DEF.CPY".
       COPY "LIBWI/CHREP2_DEF.CPY".
       COPY "LIBWI/CHREP3_DEF.CPY".
       COPY "LIBWI/CHREP4_DEF.CPY".
       COPY "LIBWI/CHREG4_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHREPO_WKS.CPY".
       COPY "LIBWI/CHREP2_WKS.CPY".
       COPY "LIBWI/CHREP3_WKS.CPY".
       COPY "LIBWI/CHREP4_WKS.CPY".
       COPY "LIBWI/CHREG4_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  MODIFY-SEQ              PIC 99 VALUE 01.
       01  MODIFY-REPO-CD-A-SEQ    PIC 99 VALUE 02.
       01  MILEAGE-SEQ             PIC 99 VALUE 03.

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CHREPO_SCN.CPY".
       COPY "LIBWI/CHREP2_SCN.CPY".
       COPY "LIBWI/CHREP3_SCN.CPY".
       COPY "LIBWI/CHREP4_SCN.CPY".
       COPY "LIBWI/CHREG4_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE FORM-PATHNAME TO FORM-PATH
                                 SAVE-PATH.
           MOVE FORM-NAME(1) TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE "CHREPO" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

       COPY "LIBGB/CHKSEC.CPY".

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CHREPO-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CHREPO-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CHREPO-COLUMNS + VDU-WIN-W-MOD
                      LINES   CHREPO-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CHREPO-WINDOW-HANDLE.

           PERFORM GET-GPENV.

           IF GP-REPO-CODE = "A"
              MOVE 4 TO CUR-FORM
              PERFORM GET-FORM
              MOVE 0 TO FIRST-TIME.

           MOVE "S  A240TITLE:FR." TO VDU.
           MOVE "REPOSSESSION INFORMATION" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE EXT-WI-CL-OWNBR  TO LSX-BRNO. 
           MOVE EXT-WI-CL-ACCTNO TO LSX-ACCTNO. 
           MOVE 1                TO LSX-SEQNO.
           MOVE LSX1-KEY TO HOLD-LSX1-KEY.

           PERFORM OPEN-LN1-FILE.
           MOVE LSX-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           IF NEW-REC = "Y"
              MOVE "S  A000NEW." TO VDU
              MOVE "*** NEW RECORD ***" TO VDUBUF
              PERFORM SCREENIO
              MOVE LEGEND-NEW TO LEGEND
              PERFORM SEND-LEGEND.

           PERFORM REC-LS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE
              GO TO END-ROUTINE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "SEQ"
              PERFORM ENTER-NEW-SEQ
              IF IO-TYPE = "END"
                 GO TO END-ROUTINE
              ELSE
                 MOVE "N" TO NEW-REC
                 GO TO MAIN-MODULE.

           IF FORM-SEL NOT = 0
              PERFORM SETUP-NEW-FORM
              IF NEW-REC = "Y"
                 MOVE "S  A000NEW." TO VDU
                 MOVE "*** NEW RECORD ***" TO VDUBUF
                 PERFORM SCREENIO
                 MOVE LEGEND-NEW TO LEGEND
                 PERFORM SEND-LEGEND.

           GO TO MAIN-MODULE.

       SETUP-NEW-FORM.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.

           EVALUATE CUR-FORM
              WHEN 1 DESTROY CHREPO-SCREEN
              WHEN 2 DESTROY CHREP2-SCREEN
              WHEN 3 DESTROY CHREP3-SCREEN
              WHEN 4
              IF GP-REPO-CODE = "A" AND VDU-FORM-NAME = "CHREG4"
                 DESTROY CHREG4-SCREEN
              ELSE
                 DESTROY CHREP4-SCREEN
              END-IF
           END-EVALUATE.

           MOVE FORM-SEL TO CUR-FORM.
           MOVE 0 TO FIRST-TIME.
           PERFORM GET-FORM.

      **************************
       ENTER-NEW-SEQ SECTION.
      **************************
       ENTER-NEW-SEQNO.
           MOVE "ENNN010SEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTER-NEW-SEQ.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-NEW-SEQNO.

           MOVE EXT-WI-CL-OWNBR  TO LSX-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LSX-ACCTNO.
           MOVE VDUNUM0          TO LSX-SEQNO.
           PERFORM OPEN-LSX1-FILE.
           PERFORM READ-LSX1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           IF IO-FG NOT = 0
              MOVE "RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NEW-SEQNO.

           MOVE LSX1-KEY TO HOLD-LSX1-KEY.
       EXIT-ENTER-NEW-SEQ.
           EXIT.

      ******************************************************************
       REC-LS-MODULE SECTION.
      ******************************************************************
       REC-LS-MODULE-NO.
           MOVE SPACES TO LSX-OR-REC.

           MOVE SPACES TO IO-TYPE.

           PERFORM OPEN-LSX1-FILE.
           PERFORM READ-LSX1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           IF IO-FG = 0
              MOVE LSX-REC TO LSX-OR-REC
              PERFORM DISPLAY-FIELDS
              IF NEW-REC = "Y"
                 MOVE 1 TO NEW-FLAG
              ELSE
                 MOVE 0 TO NEW-FLAG
              END-IF
              GO TO END-REC-LS-MODULE.

           IF CUR-FORM NOT = 1
              IF GP-REPO-CODE = "A" AND CUR-FORM = 4
      * EXTRACTED FLAG, NEW RECORD ALSO SETS TO "N"
                 MOVE "S  A010110." TO VDU
                 MOVE "N" TO VDUBUF
                 PERFORM SCREENIO
              ELSE
                 MOVE 1 TO NEW-FLAG
                 PERFORM DISPLAY-FIELDS
                 GO TO END-REC-LS-MODULE.

           PERFORM NEW-RECORD-SETUP.

       END-REC-LS-MODULE.
           MOVE 0 TO FORM-SEL.

      ******************************************************************
      *    ENTRY-MODULE
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX(CUR-FORM)
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.
           PERFORM DISPLAY-FOOTER.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

      * RESTORE RECORD BACK TO VALUES AT LAST WRITE.
      *----------------------------------------------------------------
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
      * F7 UPDATE AND EXIT
      *----------------------------------------------------------------
           IF VDUSTATUS = "1>"
              PERFORM WRITE-MODULE
              IF NOT (GP-REPO-CODE = "A" AND BW-STATE = "CA")
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTRY-MODULE 
              ELSE
                 PERFORM VERIFY-CALIFORNIA-FIELDS
                 IF ALLOW-EXIT = "N"
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "END" TO IO-TYPE
                    GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
      * UP ARROW TO ENTER A DIFFERENT SEQNO FROM A "SV" REDEEM
      *----------------------------------------------------------------
           IF CUR-FORM = 1 OR 4
              IF VDUSTATUS = "1U"
      *          CALL FRESET USING STAT
                 MOVE "SEQ" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
      * CREATE MEMO : F2 FROM MENU
      *----------------------------------------------------------------
           IF VDUSTATUS = "12"
              MOVE "L"            TO MEMOSC-ONLY
              MOVE "X"            TO MEMOSC-MEMO-TYPE
              MOVE HOLD-SYS-DATE  TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

      *----------------------------------------------------------------
      * IF AT MENU AT BOTTOM OF THE PAGE AND ENTERING A NEW RECORD
      *   THEN YOU ONLY NEED TO HIT RETURN TO GET TO THE NEXT SCREEN
      *----------------------------------------------------------------
           IF ACTION = SPACES
              IF NEW-REC = "Y"
                 IF CUR-FORM = 1
                     MOVE  "    P2" TO ACTION
                 ELSE
                 IF CUR-FORM  = 2
                    MOVE  "    P3" TO ACTION
                 ELSE
                 IF CUR-FORM  = 3
                    MOVE  "    P4" TO ACTION
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 END-IF
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
      * RECORD CAN ONLY BE DELETED WHILE AT PAGE 1.
      *----------------------------------------------------------------
           IF ACTION = "DELETE"
              IF CUR-FORM = 1
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
      * IF ENTERING A NEW RECORD DO NOT ALLOW TO GO TO P3 FROM P1
      *    OR TO P1 FROM P2. MUST ENTER RECORD IN PAGE SEQUENCE.
      *----------------------------------------------------------------
           IF NEW-REC = "Y"
              IF CUR-FORM = 1
                 IF (ACTION = "    P3" OR ACTION = "    P4")
                    GO TO MODIFY-ROUTINE
                  END-IF
              ELSE
              IF CUR-FORM = 2
                 IF (ACTION = "    P1" OR ACTION = "    P4")
                    GO TO MODIFY-ROUTINE
                 END-IF
              ELSE
              IF CUR-FORM = 3
                 IF (ACTION = "    P1" OR ACTION = "    P2")
                    GO TO MODIFY-ROUTINE.

      *----------------------------------------------------------------
      * SWITCH PAGE
      *----------------------------------------------------------------
           IF ACTION = "    P1"
              IF CUR-FORM NOT = 1
                 MOVE 1 TO FORM-SEL
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           IF ACTION = "    P2"
              IF CUR-FORM NOT = 2
                 MOVE 2 TO FORM-SEL
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           IF ACTION = "    P3"
              IF CUR-FORM NOT = 3
                 MOVE 3 TO FORM-SEL
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           IF ACTION = "    P4"
              IF CUR-FORM NOT = 4
                 MOVE 4 TO FORM-SEL
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX(CUR-FORM)
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      * TEST FOR PASSWORD TO BE ABLE TO MODIFY ANY FIELD...
           MOVE MODIFY-SEQ TO PASSWDW-SEQ.
           MOVE "TO ALLOW MODIFY:" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO MODIFY-ROUTINE.

      * EXTRACTED FLAG
           IF (GP-REPO-CODE = "A" AND ELE = 11 AND CUR-FORM = 4)
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                MOVE MODIFY-REPO-CD-A-SEQ TO PASSWDW-SEQ
                MOVE "TO MODIFY EXTRACTED FLAG" TO PASSWDW-PROMPT2
                PERFORM VERIFY-PASSWORD
                IF PASSWDW-STAT NOT = "OK"
                   GO TO CREATE-ROUTINE.

      * PAGE 1 FIELD 2. TITLE MILEAGE
           IF ELE = 2 AND CUR-FORM = 1
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 MOVE MILEAGE-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE MILEAGE:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF CUR-FORM = 1
              MOVE SPEC-REC1(ELE) TO SPEC-REC.

           IF CUR-FORM = 2
              MOVE SPEC-REC2(ELE) TO SPEC-REC.

           IF CUR-FORM = 3
              MOVE SPEC-REC3(ELE) TO SPEC-REC.

           IF CUR-FORM = 4
              IF GP-REPO-CODE = "A"
                 MOVE SPEC-REC-REG4(ELE) TO SPEC-REC
              ELSE
                 MOVE SPEC-REC4(ELE) TO SPEC-REC.

           IF CHKEY NOT = ZEROS
              IF CHFG = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.



       ENTER-FIELD.
           MOVE SPEC TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           IF (ELE = 4 AND GP-PL-REPO-REASON = "Y" AND CUR-FORM = 1)
              MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.

           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F6.

      * DISPOSAL/HOLD
           IF VDUSTATUS = "1<"
              IF (ELE = 01 AND CUR-FORM = 4 AND GP-REPO-CODE = "A")
                 MOVE "DI" TO CDSCAN-TYPE
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    MOVE "S  A020   ." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE 0    TO VDUCOL
                    MOVE CDSCAN-CODE TO VDUBUF
                    PERFORM SCREENIO
                    MOVE CDSCAN-CODE TO VDUBUF
                    MOVE 1 TO ONE
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.
      * COLLATERAL LOCATION
           IF VDUSTATUS = "1<"
              IF (ELE = 02 AND CUR-FORM = 4 AND GP-REPO-CODE = "A")
                 MOVE "XL" TO CDSCAN-TYPE
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    MOVE "S  A020   ." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE 0    TO VDUCOL
                    MOVE CDSCAN-CODE TO VDUBUF
                    PERFORM SCREENIO
                    MOVE CDSCAN-CODE TO VDUBUF
                    MOVE 1 TO ONE
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              IF (ELE = 04 AND GP-PL-REPO-REASON = "Y" AND CUR-FORM = 1)
                 MOVE "LS" TO CDSCAN-TYPE
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    MOVE "S  A020   ." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE 1    TO VDUCOL
                    MOVE CDSCAN-CODE TO VDUBUF
                    PERFORM SCREENIO
                    MOVE CDSCAN-CODE TO VDUBUF
                    MOVE 1 TO ONE
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.
      *-----------------------------------------------------------------
       TEST-F7.
           IF VDUSTATUS = "1>"
              PERFORM WRITE-MODULE
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.
           GO TO GOTO-FIELD1 GOTO-FIELD2 GOTO-FIELD3 GOTO-FIELD4
                 DEPENDING ON CUR-FORM.

      *-----------------------------------------------------------------
       GOTO-FIELD1.
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11
                 DEPENDING ON ELE.


      *-----------------------------------------------------------------
       GOTO-FIELD2.
           GO TO B-01 B-02 B-03 B-04 B-05
                 B-06 B-07 B-08
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *-----------------------------------------------------------------
       GOTO-FIELD3.
           GO TO C-01 C-02 C-03 C-04 C-05
                 C-06 C-07 C-08 C-09 C-10
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

       GOTO-FIELD4.
           IF GP-REPO-CODE = "A"
              GO TO R-01 R-02 R-03 R-04 R-05
                    R-06 R-07 R-08 R-09 R-10
                    R-11
                 DEPENDING ON ELE
           ELSE
              GO TO D-01 D-02 D-03 D-04 D-05
                    D-06 D-07 D-08 D-09 D-10
                    D-11 D-12 D-13
                 DEPENDING ON ELE.

      *****************************************************************
      *    *      SET RECORD ELEMENTS
      *****************************************************************
       A-01.
           MOVE VDUBUF TO LSX-PERFECT.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM0 TO LSX-TITLE-MILEAGE.
           GO TO CREATE-ROUTINE.
       A-03.
           IF VDUBUF NOT = "I" AND
                      NOT = "V"
              MOVE "ENTER: I-INVOLUNTARY OR V-VOLUNTARY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LSX-VOLUNTARY-FG.
       A-031.
           MOVE "ERNM060031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-031.
           MOVE VDU-DATE-YYYYMMDD TO LSX-AUTHORIZED-DATE.
       A-032.
           MOVE "E NA100032." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-031.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-032.
           MOVE VDUBUF TO LSX-AUTHORIZED-BY.
           GO TO CREATE-ROUTINE.

       A-04.

      * ONLY ENTER THE 2 CHARACTER REASON CODE
           IF GP-PL-REPO-REASON = "Y"
              MOVE "LS"   TO CD-TYPE
              MOVE VDUBUF TO CD-CODE
              PERFORM OPEN-CD1-FILE
              PERFORM READ-CD1-FILE
              PERFORM CLOSE-CD1-FILE
              IF IO-FG NOT = 0
                 MOVE "INVALID REASON CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
      * FORCE THE REASON DESCRIPTION
                 MOVE VDUBUF TO LSX-REASON-CODE
                 MOVE "S  A060040." TO VDU
                 MOVE CD-DESC TO LSX-REASON VDUBUF
                 PERFORM SCREENIO
                 GO TO CREATE-ROUTINE.

      * ONLY ENTER THE REASON DESCRIPTION
           MOVE VDUBUF TO LSX-REASON.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO LSX-X-ASSIGNED-TO.
       A-051.
           MOVE "ERNM060051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-051.
           MOVE VDU-DATE-YYYYMMDD TO LSX-ASSIGNED-DATE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDU-DATE-YYYYMMDD TO LSX-REPODATE.
       A-061.
           MOVE "ERNM060061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-061.
           MOVE VDU-DATE-YYYYMMDD TO LSX-REPO-CKDATE.
       A-062.
           MOVE "ENNN060062." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-061.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-062.
           MOVE VDUNUM0 TO LSX-REPO-CKNO.
       A-063.
           MOVE "E NN062063." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-062.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-063.
           MOVE VDUNUM2 TO LSX-REPO-CKAMT
                           A063-C.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO LSX-STORED-ADR1.
       A-071.
           MOVE "ERNM060071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-071.
           MOVE VDU-DATE-YYYYMMDD TO LSX-STORED-CKDATE.
       A-072.
           MOVE "ENNN060072." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-071.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-072.
           MOVE VDUNUM0 TO LSX-STORED-CKNO.
       A-073.
           MOVE "E NN062073." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-072.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-073.
           MOVE VDUNUM2 TO LSX-STORED-CKAMT
                           A073-C.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDU-DATE-YYYYMMDD TO LSX-REDEEM-DATE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDU-DATE-YYYYMMDD TO LSX-BUREAU-DATE.
       A-091.
           MOVE "E NA100091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-091.
           MOVE VDUBUF TO LSX-BUREAU-NOTIFIED.
       A-092.
           MOVE "E NA100092." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-091.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-092.
           MOVE VDUBUF TO LSX-BUREAU-BY.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDU-DATE-YYYYMMDD TO LSX-10DAY-DATE.

       A-101.
      *--CS REGACC PR#1478 CHECK FOR PASSWORD IF REPO OPTIONS
           IF GP-REPO-CODE = "A"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                MOVE MODIFY-REPO-CD-A-SEQ TO PASSWDW-SEQ
                MOVE "TO MODIFY REDEMPTION AMOUNT" TO PASSWDW-PROMPT2
                PERFORM VERIFY-PASSWORD
                IF PASSWDW-STAT NOT = "OK"
                   GO TO CREATE-ROUTINE.

           MOVE "E NN062101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-101.
           MOVE VDUNUM2 TO LSX-10DAY-COST
                           A101-C.


       A-102.
           MOVE "E NA100102." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-101.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-102.
           MOVE VDUBUF TO LSX-10DAY-BY.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDU-DATE-YYYYMMDD TO LSX-TITLE-APPL-DATE.
       A-111.
           MOVE "E NN062111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-111.
           MOVE VDUNUM2 TO LSX-TITLE-APPL-COST
                           A111-C.
       A-112.
           MOVE "E NA100112." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-111.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-112.
           MOVE VDUBUF TO LSX-TITLE-APPL-BY.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       B-01.
           MOVE VDUNUM0 TO LSX-COND-MILEAGE.
           GO TO CREATE-ROUTINE.
       B-02.
           MOVE VDUBUF TO LSX-COND-REPORT-Y-N.
       B-021.
           MOVE "E  A010021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-021.
           MOVE VDUBUF TO LSX-KEYS.
           IF LSX-KEYS NOT = "Y" AND NOT = "N" AND NOT = "O"
              MOVE "ENTER 'Y', 'N', OR 'O' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO B-021.
           GO TO CREATE-ROUTINE.
       B-03.
           MOVE VDUBUF TO LSX-REPAIRS-DESC(1).
       B-031.
           MOVE "E NA600031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-031.
           MOVE VDUBUF TO LSX-REPAIRS-DESC(2).
       B-032.
           MOVE "E NN062032." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-031.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-032.
           MOVE VDUNUM2 TO LSX-REPAIRS-COST.
           GO TO CREATE-ROUTINE.
       B-04.
           MOVE VDUBUF TO LSX-ASSIGN-TO(1).
       B-041.
           MOVE "ERNM060041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-041.
           MOVE VDU-DATE-YYYYMMDD TO LSX-ASSIGN-CKDATE(1).
       B-042.
           MOVE "ERNM060042." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-041.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-042.
           MOVE VDU-DATE-YYYYMMDD TO LSX-ASSIGN-COMPLETE-DATE(1).
       B-043.
           MOVE "ENNN060043." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-042.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-043.
           MOVE VDUNUM0 TO LSX-ASSIGN-CKNO(1).
       B-044.
           MOVE "E NN062044." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-043.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-044.
           MOVE VDUNUM2 TO LSX-ASSIGN-CKAMT(1)
                           B044-C.
           PERFORM TOTAL-EXPENSES-PAID.
           GO TO CREATE-ROUTINE.
       B-05.
           MOVE VDUBUF TO LSX-ASSIGN-TO(2).
       B-051.
           MOVE "ERNM060051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-051.
           MOVE VDU-DATE-YYYYMMDD TO LSX-ASSIGN-CKDATE(2).
       B-052.
           MOVE "ERNM060052." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-051.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-052.
           MOVE VDU-DATE-YYYYMMDD TO LSX-ASSIGN-COMPLETE-DATE(2).
       B-053.
           MOVE "ENNN060053." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-052.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-053.
           MOVE VDUNUM0 TO LSX-ASSIGN-CKNO(2).
       B-054.
           MOVE "E NN062054." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-053.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-054.
           MOVE VDUNUM2 TO LSX-ASSIGN-CKAMT(2)
                           B054-C.
           PERFORM TOTAL-EXPENSES-PAID.
           GO TO CREATE-ROUTINE.
       B-06.
           MOVE VDUBUF TO LSX-OTH-EXP(1).
       B-061.
           MOVE "ERNM060061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-061.
           MOVE VDU-DATE-YYYYMMDD TO LSX-OTH-EXP-CKDATE(1).
       B-062.
           MOVE "ENNN060062." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-061.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-062.
           MOVE VDUNUM0 TO LSX-OTH-EXP-CKNO(1).
       B-063.
           IF GP-REPO-CODE = "A"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                MOVE MODIFY-REPO-CD-A-SEQ TO PASSWDW-SEQ
                MOVE "TO MODIFY EXPENSE AMOUNT" TO PASSWDW-PROMPT2
                PERFORM VERIFY-PASSWORD
                IF PASSWDW-STAT NOT = "OK"
                   GO TO CREATE-ROUTINE.

           MOVE "E NN062063." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-062.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-063.

           MOVE VDUNUM2 TO LSX-OTH-EXP-CKAMT(1)
                           B063-C.
           PERFORM TOTAL-EXPENSES-PAID.
           GO TO CREATE-ROUTINE.
       B-07.
           MOVE VDUBUF TO LSX-OTH-EXP(2).
       B-071.
           MOVE "ERNM060071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-071.
           MOVE VDU-DATE-YYYYMMDD TO LSX-OTH-EXP-CKDATE(2).
       B-072.
           MOVE "ENNN060072." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-071.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-072.
           MOVE VDUNUM0 TO LSX-OTH-EXP-CKNO(2).
       B-073.
           MOVE "E NN062073." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-072.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-073.
           MOVE VDUNUM2 TO LSX-OTH-EXP-CKAMT(2)
                           B073-C.
           PERFORM TOTAL-EXPENSES-PAID.
           GO TO CREATE-ROUTINE.
       B-08.
           MOVE VDUBUF TO LSX-OTH-EXP(3).
       B-081.
           MOVE "ERNM060081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-081.
           MOVE VDU-DATE-YYYYMMDD TO LSX-OTH-EXP-CKDATE(3).
       B-082.
           MOVE "ENNN060082." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-081.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-082.
           MOVE VDUNUM0 TO LSX-OTH-EXP-CKNO(3).
       B-083.
           MOVE "E NN062083." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO B-082.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO B-083.
           MOVE VDUNUM2 TO LSX-OTH-EXP-CKAMT(3)
                           B083-C.
           PERFORM TOTAL-EXPENSES-PAID.
           GO TO CREATE-ROUTINE.

      ***********************************
      * ENTER ELEMENTS FOR PAGE 3
      ***********************************
       C-01.
           MOVE VDU-DATE-YYYYMMDD TO LSX-INS-RB-REQ-DATE.
       C-011.
           MOVE "ERNM060011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-011.
           MOVE VDU-DATE-YYYYMMDD TO LSX-INS-RB-REC-DATE.
       C-012.
           MOVE "E NN062012." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO C-011.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-012.
           MOVE VDUNUM2 TO LSX-INS-RB-AMT.
           GO TO CREATE-ROUTINE.
       C-02.
           MOVE VDU-DATE-YYYYMMDD TO LSX-SIC-INS-FILE-DATE.
       C-021.
           MOVE "ERNM060021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-021.
           MOVE VDU-DATE-YYYYMMDD TO LSX-SIC-INS-PAID-DATE.
       C-022.
           MOVE "E NN062022." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO C-021.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-022.
           MOVE VDUNUM2 TO LSX-SIC-INS-PAID-AMT
           GO TO CREATE-ROUTINE.
       C-03.
           MOVE VDU-DATE-YYYYMMDD TO LSX-RB-REQ-DATE.
       C-031.
           MOVE "ERNM060031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-031.
           MOVE VDU-DATE-YYYYMMDD TO LSX-RB-REC-DATE.
       C-032.
           IF GP-REPO-CODE = "A"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                MOVE MODIFY-REPO-CD-A-SEQ TO PASSWDW-SEQ
                MOVE "TO MODIFY REBATE AMOUNT" TO PASSWDW-PROMPT2
                PERFORM VERIFY-PASSWORD
                IF PASSWDW-STAT NOT = "OK"
                   GO TO CREATE-ROUTINE.
           MOVE "E NN062032." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO C-031.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-032.
           MOVE VDUNUM2 TO LSX-RB-AMT.
           GO TO CREATE-ROUTINE.
       C-04.
           MOVE VDUNUM2 TO LSX-NET-BAL-DUE.
           GO TO CREATE-ROUTINE.
       C-05.
           MOVE VDUNUM2 TO LSX-BOOK-VALUE.
           GO TO CREATE-ROUTINE.
       C-06.
           MOVE VDUBUF TO LSX-SOLD-TO(1).
       C-061.
           MOVE "E NA300061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-061.
           MOVE VDUBUF TO LSX-SOLD-TO(2).
       C-062.
           MOVE "E NA300062." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO C-061.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-062.
           MOVE VDUBUF TO LSX-SOLD-TO(3).
       C-063.
           MOVE "ERNM060063." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO C-062.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-063.
           MOVE VDU-DATE-YYYYMMDD TO LSX-SOLD-TO-DATE.
       C-064.
           IF GP-REPO-CODE = "A"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                MOVE MODIFY-REPO-CD-A-SEQ TO PASSWDW-SEQ
                MOVE "TO MODIFY AMOUNTS" TO PASSWDW-PROMPT2
                PERFORM VERIFY-PASSWORD
                IF PASSWDW-STAT NOT = "OK"
                   GO TO CREATE-ROUTINE.

           MOVE "E NN062064." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO C-063.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-064.
           MOVE VDUNUM2 TO LSX-SOLD-TO-AMT.

      *-- 01/6/15  AUCTION EXPENSE (REGACC PR# 1478)
       C-065.
           MOVE "E NN062065." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO C-064.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-065.
           MOVE VDUNUM2 TO LSX-AUCTION-EXPENSE.
           GO TO CREATE-ROUTINE.

       C-07.
           MOVE VDUNUM2 TO LSX-DLNO.
       C-071.
           MOVE "ENNN060071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-071.
           MOVE VDUNUM0 TO LSX-DLR-RSV-CKNO.
       C-072.
           MOVE "E NN062072." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO C-071.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO C-072.
           MOVE VDUNUM2 TO LSX-DLR-RSV-CKAMT.
           GO TO CREATE-ROUTINE.
       C-08.
           MOVE VDUNUM2 TO LSX-PL-EXPENSE.
           GO TO CREATE-ROUTINE.
       C-09.
           MOVE VDUBUF TO LSX-DEFICIENCY-Y-N.
           GO TO CREATE-ROUTINE.
       C-10.
           MOVE VDUNUM2 TO LSX-DEFICIENCY-BAL.
           GO TO CREATE-ROUTINE.

      *****************************************************************
      * ENTER ELEMENTS FOR PAGE 4
      *****************************************************************
       D-01.
           MOVE VDUBUF TO LSX-TRIM.
       D-011.
           MOVE "E  A010011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-011.
           MOVE VDUBUF TO LSX-DOORS.
       D-012.
           MOVE "E  A030012." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-011.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-012.
           MOVE VDUBUF TO LSX-COLOR.
       D-013.
           MOVE "ENNN020013." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-012.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-013.
           MOVE VDUNUM0 TO LSX-CYLINDERS.
       D-014.
           MOVE "ENNN060014." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-013.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-014.
           MOVE VDUNUM0 TO LSX-COND-MILEAGE.
           GO TO CREATE-ROUTINE.

       D-02.
           MOVE VDUBUF TO LSX-AUTO-TRANS.
       D-021.
           MOVE "E  A010021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-021.
           MOVE VDUBUF TO LSX-AC.
       D-022.
           MOVE "E  A010022." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-021.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-022.
           MOVE VDUBUF TO LSX-SROOF.
       D-023.
           MOVE "E  A010023." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-022.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-023.
           MOVE VDUBUF TO LSX-LEATHER.
       D-024.
           MOVE "E  A010024." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-023.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-024.
           MOVE VDUBUF TO LSX-4WD.
       D-025.
           MOVE "E  A010025." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-024.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-025.
           MOVE VDUBUF TO LSX-DIESEL.
       D-026.
           MOVE "E  A010026." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-025.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-026.
           MOVE VDUBUF TO LSX-DUAL-AIR.
       D-027.
           MOVE "E  A010027." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-026.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-027.
           MOVE VDUBUF TO LSX-EXT-CAB.
       D-028.
           MOVE "E  A023028." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-027.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-028.
           MOVE VDUBUF TO LSX-REPAIRS-DESC(2).
           GO TO CREATE-ROUTINE.

       D-03.
           MOVE VDUBUF TO LSX-X-ASSIGNED-TO.
           GO TO CREATE-ROUTINE.

       D-04.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO LSX-AGENT-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120040." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
       D-041.
           MOVE "E  A120041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-041.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO LSX-AGENT-FAX.
           PERFORM SEND-PHONE.
           MOVE "S  A120041." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       D-05.
           MOVE VDUBUF TO LSX-STORED-ADR1.
           GO TO CREATE-ROUTINE.

       D-06.
           MOVE VDUBUF TO LSX-STORED-ADR2.
       D-061.
           MOVE "E  A150061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-061.
           MOVE VDUBUF TO LSX-X-STORED-CITY.
       D-062.
           MOVE "E  A020062." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-061
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-062.
           MOVE VDUBUF TO LSX-X-STORED-STATE.
       D-063.
           MOVE "E  A100063." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-062.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-063.
           MOVE VDUBUF TO LSX-X-STORED-ZIP.
           GO TO CREATE-ROUTINE.

       D-07.
           MOVE VDU-DATE-YYYYMMDD TO LSX-REPODATE.
       D-071.
           MOVE "ERNM060071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-071.
           MOVE VDU-DATE-YYYYMMDD TO LSX-REDEEM-DATE.
           GO TO CREATE-ROUTINE.

       D-08.
           MOVE VDUBUF TO LSX-PERFECT.
       D-081.
           MOVE "E  A010081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-081.
           MOVE VDUBUF TO LSX-INS-CLAIM-PENDING.
       D-082.
           MOVE "E  A010082." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO D-081.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-082.
           MOVE VDUBUF TO LSX-WARRANTY-AVAILABLE.
           GO TO CREATE-ROUTINE.
       D-09.
           MOVE VDUBUF TO LSX-INS-REBATE-REQUESTED.
           GO TO CREATE-ROUTINE.

       D-10.
           MOVE VDU-DATE-YYYYMMDD TO LSX-AUTHORIZED-DATE.
       D-101.
           MOVE "E  A160101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-101.
           MOVE VDUBUF TO LSX-AUTHORIZED-BY.
           GO TO CREATE-ROUTINE.

       D-11.
           IF VDUBUF NOT = "I" AND
                      NOT = "V"
              MOVE "ENTER: I-INVOLUNTARY OR V-VOLUNTARY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LSX-VOLUNTARY-FG.
       D-111.
           IF GP-PL-REPO-REASON = "Y"
              MOVE "E  A020111." TO VDU
           ELSE
              MOVE "E  A600112." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1<"
              IF GP-PL-REPO-REASON = "Y"
                 MOVE "LS" TO CDSCAN-TYPE
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    MOVE "S  A020111." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE 1    TO VDUCOL
                    MOVE CDSCAN-CODE TO VDUBUF
                    PERFORM SCREENIO
                    MOVE "00" TO VDUSTATUS
                    MOVE CDSCAN-CODE TO VDUBUF
                 ELSE
                    GO TO D-111.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-111.

      * ONLY ENTER THE 2 CHARACTER REASON CODE
           IF GP-PL-REPO-REASON = "Y"
              MOVE "LS"   TO CD-TYPE
              MOVE VDUBUF TO CD-CODE
              PERFORM OPEN-CD1-FILE
              PERFORM READ-CD1-FILE
              PERFORM CLOSE-CD1-FILE
              IF IO-FG NOT = 0
                 MOVE "INVALID REASON CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO D-111
              ELSE
      * FORCE THE REASON DESCRIPTION
                 MOVE VDUBUF TO LSX-REASON-CODE
                 MOVE "S  A000112." TO VDU
                 MOVE CD-DESC TO LSX-REASON VDUBUF
                 PERFORM SCREENIO
                 GO TO CREATE-ROUTINE.

      * ONLY ENTER THE REASON DESCRIPTION
           MOVE VDUBUF TO LSX-REASON.
           GO TO CREATE-ROUTINE.

       D-12.
           MOVE VDUBUF TO LSX-EXTRACTED.
           IF LSX-EXTRACTED NOT = "Y" AND
                              NOT = "N"
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       D-13.
           MOVE VDU-DATE-YYYYMMDD TO LSX-TITLE-SENT-DATE.
       D-131.
           MOVE "E  A300131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO D-131.
           MOVE VDUBUF TO LSX-OTH-EXP(3).
           GO TO CREATE-ROUTINE.

      *****************************************************************
      * ENTER ELEMENTS FOR PAGE 4 GP-REPO-CODE = "A" (REGACC)
      * GP-REPO-CODE "A" HAS A CUSTOM PAGE 4
      *****************************************************************
       R-01.
           MOVE VDUBUF TO LSX-DISPOSAL-HOLD.
           MOVE "DI"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE.
           PERFORM GET-CD.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       R-011.
           MOVE "ERNM060011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-011.
           MOVE VDU-DATE-YYYYMMDD TO LSX-NEXT-HLD-RVW-DATE.
       R-012.
           MOVE "ENNN100012." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-011.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-012.
           MOVE VDUNUM0 TO LSX-CASE-ID.
           GO TO CREATE-ROUTINE.
       R-02.
           MOVE VDUBUF TO LSX-COLLATERAL-LOCATION.
           MOVE "XL"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE.
           PERFORM GET-CD.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE
           ELSE
              MOVE "S  A150021." TO VDU
              MOVE CD-DESC TO VDUBUF
              PERFORM SCREENIO.
       R-022.
           MOVE "E  A010022." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-022.
           MOVE VDUBUF TO LSX-VIN-MATCH.
           IF LSX-VIN-MATCH NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO R-022.
           GO TO CREATE-ROUTINE.

       R-03.
           IF VDUBUF NOT = "I" AND
                      NOT = "V"
              MOVE "ENTER: I-INVOLUNTARY OR V-VOLUNTARY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LSX-VOLUNTARY-FG.
       R-031.
           IF GP-PL-REPO-REASON = "Y"
              MOVE "E  A020031." TO VDU
           ELSE
              MOVE "E  A280032." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1<"
              IF GP-PL-REPO-REASON = "Y"
                 MOVE "LS" TO CDSCAN-TYPE
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    MOVE "S  A020031." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE 1    TO VDUCOL
                    MOVE CDSCAN-CODE TO VDUBUF
                    PERFORM SCREENIO
                    MOVE "00" TO VDUSTATUS
                    MOVE CDSCAN-CODE TO VDUBUF
                 ELSE
                    GO TO R-031.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-031.

      * ONLY ENTER THE 2 CHARACTER REASON CODE
           IF GP-PL-REPO-REASON = "Y"
              MOVE "LS"   TO CD-TYPE
              MOVE VDUBUF TO CD-CODE
              PERFORM GET-CD
              IF IO-FG NOT = 0
                 MOVE "INVALID REASON CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO R-031
              ELSE
      * FORCE THE REASON DESCRIPTION
                 MOVE VDUBUF TO LSX-REASON-CODE 
                 MOVE "S  A280032." TO VDU
                 MOVE CD-DESC TO LSX-REASON VDUBUF
                 PERFORM SCREENIO
                 GO TO CREATE-ROUTINE.

      * ONLY ENTER THE REASON DESCRIPTION
           MOVE VDUBUF TO LSX-REASON.
           GO TO CREATE-ROUTINE.

       R-04.
           MOVE VDU-DATE-YYYYMMDD TO LSX-REPODATE.
       R-041.
           MOVE "ERNM060041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-041.
           MOVE VDU-DATE-YYYYMMDD TO LSX-REDEEM-DATE.
       R-042.
           MOVE "E  A010042." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-041.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-042.
           MOVE VDUBUF TO LSX-PERSONALS.
           IF LSX-PERSONALS NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO R-042.
           GO TO CREATE-ROUTINE.


       R-05.
           MOVE VDUNUM0 TO LSX-COND-MILEAGE.
       R-051.
           MOVE "E  A010051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-051.
           MOVE VDUBUF TO LSX-KEYS.
           IF LSX-KEYS NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO R-051.
       R-052.
           MOVE "E  A010052." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-051.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-052.
           MOVE VDUBUF TO LSX-DRIVES.
           IF LSX-DRIVES NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO R-052.
       R-053.
           MOVE "E  A010053." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-052.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-053.
           MOVE VDUBUF TO LSX-DMG.
           IF LSX-DMG NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO R-053.
       R-054.
           MOVE "E  A010054." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-053.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-054.
           MOVE VDUBUF TO LSX-COND-REPORT-Y-N.
       R-055.
           MOVE "E  A010055." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-054.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-055.
           MOVE VDUBUF TO LSX-INS-CLAIM-PENDING.
           GO TO CREATE-ROUTINE.

       R-06.
           MOVE VDUBUF TO LSX-Y-ASSIGNED-TO.
       R-061.
           MOVE "ENNN070061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-061.
           MOVE VDUNUM0 TO LSX-SUPPLIER-ID.

       R-062.
           MOVE "E  A120062." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-061.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-062.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO LSX-AGENT-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120062." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
       R-063.
           MOVE "E  A120063." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-062.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-063.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO LSX-AGENT-FAX.
           PERFORM SEND-PHONE.
           MOVE "S  A120063." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
       R-064.
           MOVE "E  A400064." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-063.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-064.
           MOVE VDUBUF TO LSX-TRANSPORT-EMAIL.
           GO TO CREATE-ROUTINE.

       R-07.
           MOVE VDUBUF TO LSX-STORED-LOCATION.
           GO TO CREATE-ROUTINE.
       R-08.
           MOVE VDUBUF TO LSX-STORED-ADDRESS.
       R-081.
           MOVE "E  A150081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-081.
           MOVE VDUBUF TO LSX-Y-STORED-CITY.
       R-082.
           MOVE "E  A020082." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-081.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-082.
           MOVE VDUBUF TO LSX-Y-STORED-STATE.
       R-083.
           MOVE "E  A100083." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-082.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-083.
           MOVE VDUBUF TO LSX-Y-STORED-ZIP.
           GO TO CREATE-ROUTINE.

       R-09.
           MOVE VDUBUF TO LSX-AGENCY-NAME.
           IF LSX-AGENCY-NAME = SPACES AND BW-STATE = "CA"
              GO TO ENTER-ELE.
       R-091.
           MOVE "E  A120091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-091.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO LSX-AGENCY-PHONE.
           IF LSX-AGENCY-PHONE = 0 AND BW-STATE = "CA"
              GO TO R-091.
           PERFORM SEND-PHONE.
           MOVE "S  A120091." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       R-10.
           MOVE VDUBUF TO LSX-AGENCY-ADDRESS.
           IF LSX-AGENCY-ADDRESS = SPACES AND BW-STATE = "CA"
              GO TO ENTER-ELE.
       R-101.
           MOVE "E  A150101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-101.
           MOVE VDUBUF TO LSX-AGENCY-CITY.
           IF LSX-AGENCY-CITY = SPACES AND BW-STATE = "CA"
              GO TO R-101.
       R-102.
           MOVE "E  A020102." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-101.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-102.
           MOVE VDUBUF TO LSX-AGENCY-STATE.
           IF LSX-AGENCY-STATE = SPACES AND BW-STATE = "CA"
              GO TO R-102.
       R-103.
           MOVE "E  A100103." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO R-102.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-103.
           MOVE VDUBUF TO LSX-AGENCY-ZIP.
           IF LSX-AGENCY-ZIP = SPACES AND BW-STATE = "CA"
              GO TO R-103.
           GO TO CREATE-ROUTINE.

       R-11.
           MOVE VDUBUF TO LSX-EXTRACTED.
           IF LSX-EXTRACTED NOT = "Y" AND 
                              NOT = "N"
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ****************************************
       VERIFY-CALIFORNIA-FIELDS SECTION.
      ****************************************
           MOVE "Y" TO ALLOW-EXIT.

           IF LSX-AGENCY-NAME = SPACES OR
               LSX-AGENCY-PHONE = 0 OR
                 LSX-AGENCY-ADDRESS = SPACES OR
                  LSX-AGENCY-CITY = SPACES OR
                   LSX-AGENCY-STATE = SPACES OR
                    LSX-AGENCY-ZIP = SPACES 
               MOVE "N" TO ALLOW-EXIT
               MOVE "MUST ENTER AGENCY INFO" TO MESS
               PERFORM SEND-MESS.

       EXIT-VERIFY-CALIFORNIA.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-LSX1-FILE.

           IF FILE-CREATED  = "N"
              GO TO WRITE-RECORD.

           MOVE LSX-REC TO LSX-CH-REC.

           IF LSX-OR-REC = LSX-CH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LSX1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LSX-REC NOT = LSX-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE LSX-CH-REC TO LSX-REC.
           PERFORM REWRITE-LSX1-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LSX1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

           MOVE "Y" TO FILE-CREATED.

       END-WRITE-MODULE.
           IF NEW-REC = "Y"
              IF CUR-FORM = 4
                 MOVE "N" TO NEW-REC.
           PERFORM CLOSE-LSX1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LSX1-FILE.
           PERFORM READ-LSX1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF LSX-REC NOT = LSX-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LSX1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LSX1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************

           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "Y" TO NEW-REC.
           MOVE "N" TO FILE-CREATED.
           MOVE LEGEND-NEW TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM CLEAR-LSX-FILE.
           MOVE HOLD-LSX1-KEY TO LSX1-KEY.

           MOVE 0 TO A063-C A073-C A101-C A111-C
                     B044-C B054-C B063-C B073-C B083-C.
      *2/12/15 THIS WAS MISSING & SO YOU DIDNT HAVE THE LOAN NUMBER OR
      *        COLLATERAL IN THE HEADER.
           IF CUR-FORM = 4
              PERFORM DISPLAY-PAGE4-HEADER.

       NEW-RECORD-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES               TO DISPLAY-BUF1.
           MOVE SPACES               TO DISPLAY-BUF2.
           MOVE SPACES               TO DISPLAY-BUF3.

           IF CUR-FORM NOT = 1
              GO TO SETUP-BUFFER2.

           MOVE LSX-SEQNO TO                D-SEQNO.
           MOVE LSX-PERFECT TO             A01.
           MOVE LSX-TITLE-MILEAGE TO       A02.
           MOVE LSX-VOLUNTARY-FG TO        A03.
           MOVE LSX-AUTHORIZED-DATE TO     DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO             A031.
           MOVE LSX-AUTHORIZED-BY TO       A032.
           MOVE LSX-REASON-CODE TO         A041.
           MOVE LSX-REASON TO              A04.
           MOVE LSX-X-ASSIGNED-TO TO       A05.
           MOVE LSX-ASSIGNED-DATE TO       DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO             A051.
           MOVE LSX-REPODATE TO            DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO             A06.
           MOVE LSX-REPO-CKDATE TO         DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO             A061.
           MOVE LSX-REPO-CKNO TO           A062.
           MOVE LSX-REPO-CKAMT TO          A063.
           MOVE LSX-REPO-CKAMT TO          A063-C.
           MOVE LSX-STORED-ADR1 TO         A07.
           MOVE LSX-STORED-CKDATE TO       DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO             A071.
           MOVE LSX-STORED-CKNO TO         A072.
           MOVE LSX-STORED-CKAMT TO        A073.
           MOVE LSX-STORED-CKAMT TO        A073-C.
           MOVE LSX-REDEEM-DATE TO         DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO            A08.
           MOVE LSX-BUREAU-DATE TO         DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO             A09.
           MOVE LSX-BUREAU-NOTIFIED TO     A091.
           MOVE LSX-BUREAU-BY TO           A092.
           MOVE LSX-10DAY-DATE TO          DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO             A10.
           MOVE LSX-10DAY-COST TO          A101.
           MOVE LSX-10DAY-COST TO          A101-C.
           MOVE LSX-10DAY-BY TO            A102.
           MOVE LSX-TITLE-APPL-DATE TO     DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO             A11.
           MOVE LSX-TITLE-APPL-COST TO     A111.
           MOVE LSX-TITLE-APPL-COST TO     A111-C.
           MOVE LSX-TITLE-APPL-BY TO       A112.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT.

      *-----------------------------------------------------------------
       SETUP-BUFFER2.
           IF CUR-FORM NOT = 2
              GO TO SETUP-BUFFER3.

           MOVE LSX-COND-MILEAGE TO        B01.
           MOVE LSX-COND-REPORT-Y-N TO     B02.
           MOVE LSX-KEYS TO                B021.
           MOVE LSX-REPAIRS-DESC(1) TO     B03.
           MOVE LSX-REPAIRS-DESC(2) TO     B031.
           MOVE LSX-REPAIRS-COST TO        B032.
           MOVE LSX-ASSIGN-TO(1) TO        B04.
           MOVE LSX-ASSIGN-CKDATE(1) TO    DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO             B041.
           MOVE LSX-ASSIGN-COMPLETE-DATE(1) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO B042.
           MOVE LSX-ASSIGN-CKNO(1) TO      B043.
           MOVE LSX-ASSIGN-CKAMT(1) TO     B044.
           MOVE LSX-ASSIGN-CKAMT(1) TO     B044-C.
           MOVE LSX-ASSIGN-TO(2) TO        B05.
           MOVE LSX-ASSIGN-CKDATE(2) TO    DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO    B051.
           MOVE LSX-ASSIGN-COMPLETE-DATE(2) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO B052.
           MOVE LSX-ASSIGN-CKNO(2) TO      B053.
           MOVE LSX-ASSIGN-CKAMT(2) TO     B054.
           MOVE LSX-ASSIGN-CKAMT(2) TO     B054-C.
           MOVE LSX-OTH-EXP(1) TO          B06.
           MOVE LSX-OTH-EXP-CKDATE(1) TO   DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO   B061.
           MOVE LSX-OTH-EXP-CKNO(1) TO     B062.
           MOVE LSX-OTH-EXP-CKAMT(1) TO    B063.
           MOVE LSX-OTH-EXP-CKAMT(1) TO    B063-C.
           MOVE LSX-OTH-EXP(2) TO          B07.
           MOVE LSX-OTH-EXP-CKDATE(2) TO   DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO   B071.
           MOVE LSX-OTH-EXP-CKNO(2) TO     B072.
           MOVE LSX-OTH-EXP-CKAMT(2) TO    B073.
           MOVE LSX-OTH-EXP-CKAMT(2) TO    B073-C.
           MOVE LSX-OTH-EXP(3) TO          B08.
           MOVE LSX-OTH-EXP-CKDATE(3) TO   DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO   B081.
           MOVE LSX-OTH-EXP-CKNO(3) TO     B082.
           MOVE LSX-OTH-EXP-CKAMT(3) TO    B083.
           MOVE LSX-OTH-EXP-CKAMT(3) TO    B083-C.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM TOTAL-EXPENSES-PAID.

           GO TO DISPLAY-FIELDS-EXIT.

      *-----------------------------------------------------------------
       SETUP-BUFFER3.
           IF CUR-FORM NOT = 3
              GO TO SETUP-BUFFER4.

           MOVE LSX-INS-RB-REQ-DATE TO     DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO     C01.
           MOVE LSX-INS-RB-REC-DATE TO     DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO     C011.
           MOVE LSX-INS-RB-AMT TO          C012.
           MOVE LSX-SIC-INS-FILE-DATE TO   DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO   C02.
           MOVE LSX-SIC-INS-PAID-DATE TO   DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO   C021.
           MOVE LSX-SIC-INS-PAID-AMT TO    C022.
           MOVE LSX-RB-REQ-DATE TO         DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO         C03.
           MOVE LSX-RB-REC-DATE TO         DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO         C031.
           MOVE LSX-RB-AMT TO              C032.
           MOVE LSX-NET-BAL-DUE TO         C04.
           MOVE LSX-BOOK-VALUE TO          C05.
           MOVE LSX-SOLD-TO(1) TO          C06.
           MOVE LSX-SOLD-TO(2) TO          C061.
           MOVE LSX-SOLD-TO(3) TO          C062.
           MOVE LSX-SOLD-TO-DATE TO        DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO        C063.
           MOVE LSX-SOLD-TO-AMT TO         C064.
           MOVE LSX-AUCTION-EXPENSE TO     C065.
           MOVE LSX-DLNO TO                C07.
           MOVE LSX-DLR-RSV-CKNO TO        C071.
           MOVE LSX-DLR-RSV-CKAMT TO       C072.
           MOVE LSX-PL-EXPENSE TO          C08.
           MOVE LSX-DEFICIENCY-Y-N TO      C09.
           MOVE LSX-DEFICIENCY-BAL TO      C10.

           MOVE DISPLAY-BUF3 TO WR-BUF.
           MOVE DISPLAY-LIST3 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT.

      *-----------------------------------------------------------------
       SETUP-BUFFER4.
           IF CUR-FORM NOT = 4
              GO TO DISPLAY-FIELDS-EXIT.

           IF GP-REPO-CODE = "A"
              GO TO SETUP-REGACC4.

           PERFORM OPEN-LTC1-FILE.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           IF IO-GOOD
              MOVE LTC-YEAR(1)  TO  DYEAR
              MOVE LTC-MAKE(1)  TO  DMAKE
              MOVE LTC-MODEL(1) TO  DMODEL
              MOVE LTC-VIN(1)   TO  DVIN
           ELSE
              MOVE 0      TO  DYEAR
              MOVE SPACES TO  DMAKE
              MOVE SPACES TO  DMODEL
              MOVE SPACES TO  DVIN.

           MOVE LSX-TRIM TO                 D01.
           MOVE LSX-DOORS TO                D011.
           MOVE LSX-COLOR TO                D012.
           MOVE LSX-CYLINDERS TO            D013.
           MOVE LSX-COND-MILEAGE TO         D014.
           MOVE LSX-AUTO-TRANS TO           D02.
           MOVE LSX-AC TO                   D021.
           MOVE LSX-SROOF TO                D022.
           MOVE LSX-LEATHER TO              D023.
           MOVE LSX-4WD TO                  D024.
           MOVE LSX-DIESEL TO               D025.
           MOVE LSX-DUAL-AIR TO             D026.
           MOVE LSX-EXT-CAB TO              D027.
           MOVE "REPAIR DESC:" TO PROMPT028.
           MOVE LSX-REPAIRS-DESC(2) TO      D028.
           MOVE LSX-X-ASSIGNED-TO TO        D03.
           MOVE LSX-AGENT-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO                  D04.
           MOVE LSX-AGENT-FAX TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO                 D041.
           MOVE LSX-STORED-ADR1 TO          D05.
           MOVE LSX-STORED-ADR2 TO          D06.
           MOVE LSX-X-STORED-CITY TO        D061.
           MOVE LSX-X-STORED-STATE TO       D062.
           MOVE LSX-X-STORED-ZIP TO         D063.
           MOVE LSX-REPODATE TO             DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO             D07.
           MOVE LSX-REDEEM-DATE TO          DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO          D071.
           MOVE LSX-PERFECT TO              D08.
           MOVE LSX-INS-CLAIM-PENDING TO    D081.
           MOVE LSX-WARRANTY-AVAILABLE TO   D082.
           MOVE LSX-INS-REBATE-REQUESTED TO D09.

           MOVE LN-CURBAL TO                 DCURBAL.
           MOVE LN-ACCTNO TO                 DACCT.
           MOVE LSX-SEQNO TO                 DSEQ.

           MOVE SPACES TO DBWNAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO DBWNAME.

           PERFORM OPEN-BR-FILE.
           MOVE LN-OWNBR TO BR-NO DBRNO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           MOVE BR-NAME TO DBRNAME.
           MOVE BR-CITY TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "? "
               BY BR-STATE.
           MOVE NAME-WORKER TO DBRCITYST.

           MOVE LSX-AUTHORIZED-DATE TO      DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO      D10.
           MOVE LSX-AUTHORIZED-BY TO        D101.
           MOVE LSX-VOLUNTARY-FG TO         D11.
           MOVE LSX-REASON-CODE TO          D111.
           MOVE LSX-REASON TO               D112.
           MOVE LSX-EXTRACTED TO            D120.
           MOVE LSX-TITLE-SENT-DATE TO      DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO      D130.
      * TITLE SENT LOCATION D131 LSX-OTH-EXP(3)
           MOVE LSX-OTH-EXP(3) TO           D131.

           MOVE DISPLAY-BUF4 TO WR-BUF.
           MOVE DISPLAY-LIST4 TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO DISPLAY-FIELDS-EXIT.

      *******************************************************
       SETUP-REGACC4.
           PERFORM DISPLAY-PAGE4-HEADER.
 
           MOVE LSX-DISPOSAL-HOLD  TO          R01.
           MOVE LSX-NEXT-HLD-RVW-DATE TO       DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO       R011.
           MOVE LSX-CASE-ID TO                 R012.

           MOVE LSX-COLLATERAL-LOCATION TO     R02.
           MOVE "XL"                    TO CD-TYPE.
           MOVE LSX-COLLATERAL-LOCATION TO CD-CODE.
           PERFORM GET-CD.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO CD-DESC.
           MOVE CD-DESC TO                     R021.
           MOVE LSX-VIN-MATCH TO               R022.

           MOVE LSX-VOLUNTARY-FG TO         R03.
           MOVE LSX-REASON-CODE TO          R031.
           MOVE LSX-REASON TO               R032.

           MOVE LSX-REPODATE TO             DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO             R04.
           MOVE LSX-REDEEM-DATE TO          DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO          R041.
           MOVE LSX-PERSONALS TO               R042.

           MOVE LSX-COND-MILEAGE TO         R05.
           MOVE LSX-KEYS TO                    R051.
           MOVE LSX-DRIVES TO                  R052.
           MOVE LSX-DMG TO                     R053.
           MOVE LSX-COND-REPORT-Y-N TO      R054.
           MOVE LSX-INS-CLAIM-PENDING TO    R055.

           MOVE LSX-Y-ASSIGNED-TO TO           R06.
           MOVE LSX-SUPPLIER-ID TO             R061.
           MOVE LSX-AGENT-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO                  R062.
           MOVE LSX-AGENT-FAX TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO                  R063.
           MOVE LSX-TRANSPORT-EMAIL TO         R064.

           MOVE LSX-STORED-LOCATION TO         R07.
           MOVE LSX-STORED-ADDRESS TO          R08.
           MOVE LSX-Y-STORED-CITY TO           R081.
           MOVE LSX-Y-STORED-STATE TO            R082.
           MOVE LSX-Y-STORED-ZIP TO              R083.

           MOVE LSX-AGENCY-NAME TO             R09.
           MOVE LSX-AGENCY-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO                  R091.

           MOVE LSX-AGENCY-ADDRESS  TO         R10.
           MOVE LSX-AGENCY-CITY TO             R101.
           MOVE LSX-AGENCY-STATE TO            R102.
           MOVE LSX-AGENCY-ZIP TO              R103.

           MOVE LSX-EXTRACTED TO            R11.

           MOVE DISPLAY-BUF-REG4 TO WR-BUF.
           MOVE DISPLAY-LIST-REG4 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      **********************************
       CODE-SCAN SECTION.
      **********************************

      * CDSCAN-TYPE ALREADY FILLED IN
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS NOT = "00"    ) OR
              ((CDSCAN-TYPE = "DI") AND (CDSCAN-CODE = SPACES )) OR
              ((CDSCAN-TYPE = "LS") AND (CDSCAN-CODE = SPACES )) OR
              ((CDSCAN-TYPE = "XL") AND (CDSCAN-CODE = SPACES ))
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      ***********************************
       TOTAL-EXPENSES-PAID SECTION.
      ***********************************
            IF CUR-FORM = 2
                MOVE 0 TO SUM1
                          SUM2
                          SUM3
                COMPUTE SUM1 = A063-C + A073-C + A101-C + A111-C
                COMPUTE SUM2 = B044-C + B054-C + B063-C + B073-C
                COMPUTE SUM3 = B083-C
                COMPUTE TOTAL-EXP-PAID = SUM1 + SUM2 + SUM3
                MOVE "S NS072TOTPAID." TO VDU
                MOVE TOTAL-EXP-PAID TO VDUNUM2
                PERFORM SCREENIO.

       EXIT-COMPUTE-TOTALS.
            EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLSXCL.CPY".

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CHREPO" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           DISPLAY SPACES UPON CHREPO-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.

           EVALUATE CUR-FORM
              WHEN 1  DISPLAY CHREPO-SCREEN UPON CHREPO-WINDOW-HANDLE
                  MOVE CHREPO-FORM-FIELDS TO WR-BUF 
                  MOVE CHREPO-FORM-LIST TO WR-LIST 
              WHEN 2  DISPLAY CHREP2-SCREEN UPON CHREPO-WINDOW-HANDLE
                  MOVE CHREP2-FORM-FIELDS TO WR-BUF 
                  MOVE CHREP2-FORM-LIST TO WR-LIST 
              WHEN 3  DISPLAY CHREP3-SCREEN UPON CHREPO-WINDOW-HANDLE
                  MOVE CHREP3-FORM-FIELDS TO WR-BUF 
                  MOVE CHREP3-FORM-LIST TO WR-LIST 
              WHEN 4
                  IF GP-REPO-CODE = "A"
                     DISPLAY CHREG4-SCREEN UPON CHREPO-WINDOW-HANDLE
                     MOVE CHREG4-FORM-FIELDS TO WR-BUF
                     MOVE CHREG4-FORM-LIST   TO WR-BUF
                  ELSE
                     DISPLAY CHREP4-SCREEN UPON CHREPO-WINDOW-HANDLE
                     MOVE CHREP4-FORM-FIELDS TO WR-BUF 
                     MOVE CHREP4-FORM-LIST TO WR-LIST 
                  END-IF
           END-EVALUATE.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************
           EVALUATE CUR-FORM
              WHEN 1  MOVE CHREPO-HELP--FILE TO HELP-LINK-FILE
                      MOVE CHREPO-HELP--DIR TO HELP-LINK-DIR
              WHEN 2  MOVE CHREP2-HELP--FILE TO HELP-LINK-FILE
                      MOVE CHREP2-HELP--DIR TO HELP-LINK-DIR
              WHEN 3  MOVE CHREP3-HELP--FILE TO HELP-LINK-FILE
                      MOVE CHREP3-HELP--DIR TO HELP-LINK-DIR
              WHEN 4  MOVE CHREP4-HELP--FILE TO HELP-LINK-FILE
                      MOVE CHREP4-HELP--DIR TO HELP-LINK-DIR

           END-EVALUATE.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHREPO_EVA.CPY".
              COPY "LIBWI/CHREP2_EVA.CPY".
              COPY "LIBWI/CHREP3_EVA.CPY".
              COPY "LIBWI/CHREP4_EVA.CPY".
              COPY "LIBWI/CHREG4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
 
       DISPLAY-PAGE4-HEADER.
           MOVE LN-ACCTNO TO RACCT.
           MOVE LSX-SEQNO TO RSEQ.

           MOVE SPACES TO RBWNAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO RBWNAME.

           PERFORM GET-BR.

           MOVE BR-NO TO RBRNO.
           MOVE BR-NAME TO RBRNAME.

           MOVE LN-ORGST TO RORGST.
           MOVE LN-ORGBR TO RORGBR.
           MOVE LN-CURBAL TO RCURBAL.

           PERFORM OPEN-LTC1-FILE.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           IF IO-GOOD
              MOVE LTC-YEAR(1)  TO   RYEAR
              MOVE LTC-MAKE(1)  TO   RMAKE
              MOVE LTC-MODEL(1) TO   RMODEL
              MOVE LTC-VIN(1)   TO   RVIN
           ELSE
              MOVE 0 TO              RYEAR
              MOVE SPACES TO         RMAKE
              MOVE SPACES TO         RMODEL
              MOVE SPACES TO         RVIN.

           MOVE DISPLAY-BUF-HEAD4 TO WR-BUF.
           MOVE DISPLAY-LIST-HEAD4 TO WR-LIST.
           PERFORM CALL-WRFORM.

       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       GET-BR.
           PERFORM OPEN-BR-FILE.

      *----------------------------------------------------------------
      * DISPLAY FOOTER : DISPLAYS MENU OPTONS AT BOTTOM OF FORM.
      *----------------------------------------------------------------
       DISPLAY-FOOTER.
           IF NEW-REC NOT = "Y"
              IF CUR-FORM = 1
                 MOVE LEGEND-01 TO LEGEND
              ELSE
              IF CUR-FORM = 2
                 MOVE LEGEND-02 TO LEGEND
              ELSE
              IF CUR-FORM = 3
                 MOVE LEGEND-03 TO LEGEND
              ELSE
                 MOVE LEGEND-04 TO LEGEND.
           IF NEW-REC = "Y"
              IF CUR-FORM = 1
                 MOVE LEGEND-01B TO LEGEND
              ELSE
              IF CUR-FORM = 2
                 MOVE LEGEND-02B TO LEGEND
              ELSE
              IF CUR-FORM = 3
                 MOVE LEGEND-03 TO LEGEND
              ELSE
                 MOVE LEGEND-04 TO LEGEND.
           PERFORM SEND-LEGEND.

      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
      *----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       GET-FORM.
           MOVE FORM-NAME(CUR-FORM) TO FORM-NAM.
           IF GP-REPO-CODE = "A" AND FORM-NAM = "WI/CHREP4"
              MOVE "WI/CHREG4" TO FORM-NAM
              MOVE 11 TO MAX(4)
              MOVE "CHREG4" TO FORM-VDU-NAME(CUR-FORM).
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE FORM-VDU-NAME(CUR-FORM) TO VDU-FORM-NAME.
      *    PERFORM GETFM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLSX1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EVALUATE CUR-FORM
              WHEN 1 DESTROY CHREPO-SCREEN
              WHEN 2 DESTROY CHREP2-SCREEN
              WHEN 3 DESTROY CHREP3-SCREEN
              WHEN 4 DESTROY CHREP4-SCREEN
           END-EVALUATE.
           DESTROY CHREPO-WINDOW-HANDLE.
           MOVE SAVE-PATH TO FORM-PATHNAME.

       STOP-RUN.
           EXIT PROGRAM.
