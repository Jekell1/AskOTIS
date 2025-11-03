       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPQ1.
       DATE-WRITTEN.    112596.
      ******************************************************************
      *                 (LP)
      *  DESCRIPTION:   RESCISSION UPDATE/DELETE/INQUIRY
      *
      *  UPDATE : LOAN MUST BE IN RESCISSION (LN-POCD = "*I")
      *           CLEARS LN-POCD
      *           UPDATE TR1,BW1,DS1,TY1,MA1,BI1 (JUST LIKE LONPC2)
      *           DELETE THE RX
      *
      *  DELETE : LOAN MUST BE IN RESCISSION (LN-POCD = "*I")
      *           "*R" TO LN-POCD, POFFDATE, 0 TO CURBAL
      *           "R" TO RX-STATUS
      *           DELETE CX AND FM RECORDS
      *
      *
      * REV:
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READ-BW
      *  MJD 011097 ADDED EXT-WI-CL-PAY-SCHLD-FG.
      *  MJD 012397 CHANGED TO ONLY HIT LN-INSTBL(1-8) AND
      *             TY-INS-TYPE(1-8).
      *  KEC 012397 REVISED ACCESSING MA-FILE: NEW FORMAT FOR MA-FILE.
      *  BAH 050997 REMOVE CX AND FX ON THE DELETE
      *  BAH 050997 ADDED MOVE OF MONTH AND YEAR TO TRANS-PATH
      *  MJD 060497 ADDED INS-RB-TBL(6-8), U-INSOURS(6-8),
      *             AND REB-AMOUNT(11-13)
      *  MJD 070297 ADDED DS-DLDISC-VOL AND TY-DLDISC
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 091797 REMOVED DELSUB-SET LOGIC; ADDED LPPDUE(W) COPY MEMBERS
      *  MJD 101797 MOVED WK-UPDATE-BUF TO LP01WK
      *  GLF 101797 ADDED MOVE LN-APPNO TO EXT-WI-CL-APPNO
      *  MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      *  BLV 020698 ADDED LN-GIFTNET TO BICREA CALL
      *  MJD 030398 ADDED LN-PLCD AND LN-CONVERCD TO BICREA CALL
      *  MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      *  KEC 042898 CORRECTED SETUP OF LP-REC FOR PASS TO LONPFA.C, TO
      *  &JTG       SETUP THE LP-PAYDATE = LN-INTDATE OR RESCESSION DATE
      *      042898 ADDED "RB" AND "RO" RENEWAL CODES
      *  KEC 051498 ADDED 'SET-TRACK-STATUS'.  WAS NOT UPDATING THE
      *              MA-STATUS-TRACK.
      *  JTG 061598 ADDED LOGIC TO UPDATE BANKRUPT DELINQUENT STATISTICS
      *  JTG 063098 EXPANDED DS-APP-TYPE-TBL TO 3 LEVELS
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  KEC 031199 REVISED DUE TO PROBLEMS LORI (REGENCY) FOUND:
      *                 WHEN RESCINDING A LOAN WITH A PRIOR LOAN DOES
      *                   NOT REMOVE TEMP CHECKS
      *                   ('IF' CONDITION OUT OF PLACE);
      *                 CANNOT PRINT CHECKS IN 'CHECK PRINTING' AND
      *                   CLOSING DAYS DOES NOT FLAG THAT CHECKS NEED TO
      *                   BE PRINTED FOR A LOAN THAT CAME OUT OF
      *                   RESCISSION (CX-RESCISSION REMAINED "Y").
      *  MJD 990420 CHANGED TO INCLUDE/EXCLUDE O2 IN DELINQ BASED
      *             ON GP-DELINQ-WITH-O2
      *
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  MJD 990910 CHANGED TO SKIP INSURANCE BUCKETS 7 & 8 (O4 & O5) WHEN
      *             UPDATING TY. CHANGED TO UPDATE INS-ELIG FOR O1 IF
      *             APPLICABLE. ADDED LOGIC TO UPDATE INS-NOT-WRITTEN (9 & 10)
      *             IF AD&D OR ROADGARD ARE NOT WRITTEN (WORLD ONLY)
      *             (WORLD PR# AS PER JOHN)
      *  KEC 991217 {PR#0000} WAS A PROBLEM THAT THE MA-TRACK-STATUS
      *             WAS NOT BEING PROPERLY SETUP.
      *                CHANGED THE FOLLOWING:
      *                  1.  ORIGINAL-TRACK-STATUS WAS X(1) NOT X(6).
      *                  2.  WAS MOVING MA-STATUS NOT MA-TRACK-STATUS
      *                       TO ORIGINAL-TRACK-STATUS.
      *  BAH 000121 MOVE LN-INTDATE TO LN-ENTDATE WHEN YOU UPDATE SO THE
      *             NFMORP WILL FIND THE ACCT IN THE CORRECT MONTH, WHEN THE
      *             DATES ARE IN DIFFERENT MONTHS...SEE DETAIL DESCRIPTION
      *             WHERE THE CODE GOT ADDED....AMERICAN THRIFT PR# 270
      *  JTG 000327 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      *  MJD 000406 ADDED UPDATE OF TY-PROCEEDS AND DS-PROCEEDS (MERC PR#223)
      * KEC 000724 {PR#00000} PARADATA FINANCIAL [BETH&KEN]
      *             FIXED A PROBLEM THAT LORI FOUND WHERE A RESCISSION
      *             LOAN WAS UPDATED OUT OF RESCISSION TWICE
      *             (WITH THE SAME ACCTNO).
      *  BAH 000731 ADDED "PERFORM DUAL-CHECK 4 TIMES" JUST LIKE LOAN UPDATE
      *             HAS (LONPC2), YOU COULD DO A SECOND LOAN AS A RESCISSION
      *             PR# 1231
      *  MG  010201 REMOVED "01 WK-LXE" AND ADDED LIBLP/LPWKLXE.
      *  BAH 010516 ADD START-CX1-FILE IN UPDATE-TEMP-CHECKS AND
      *             IN REMOVE-TEMP-CHECKS
      *  BAH 010619 ADDED CLEAR OF HOLD-REPO-AUCTION-EXP, REGACC PR# 1478
      *             (LPPFBUF)  ALSO CLEARED HOLD-OVPAID-CHECK !!
      *  KEC 020806 {DR#00009} PARADATA <A15> [KEN]
      *             ADDED LN-RESCISSION AND LN-RESCISSION-DATE.
      *  KEC 020830 {PR#01798} PARADATA <A15> [JIM ROSENAUER]
      *             ADDED "RESCINDED-UPDATE-MA SECTION."
      *  BLV 020910 MOVE LN-MIP-FG TO TL-MIP-FG, REGWAN PR# 1801
      *  BAH 031120 ADDED GP-NON-FILE-FEECODE TO UPDATE TY/BY FILE WITH
      *             NON FILE INSURANCE FOR WORLD SMITH REPLACEMENT
      *  BAH 061107 CHANGED THE UPDATE OF CX-RESCISSION FROM "N" TO "U" SO
      *             THESE CHECKS CAN BE IDENTIFIED IN CLOSING THE DAY. THE
      *             TRANS-DATE DOES NOT EQUAL THE CX-DATE, SO THE CLOSING
      *             DAY PROGRAM (LONPL0) WAS NOT STOPPING YOU, NOW IT WILL
      *             CAUSE WE WILL LOOK FOR CX-RESCISSION = "U", REGWAN PL#556
      *  BAH 120723 PRIOR LOAN WITH NO ESCROW WAS FORCING MESSAGE TO DISBURSE
      *             ESCROW BALANCE WHEN THERE WAS NO ESCROW RECORD. FIXED
      *             BAD 'IF' STATEMENT AND ALSO HOLD-LN-REC, REGENCY PL#778
      * KEC 2013.0206 {PR#04196} REGENCY <A15> [LORI MCBEE]
      *         MADE CHANGES TO LOAN APPS - ADDED MX-FILE.
      *
      * KEC 2013.0625 {PR#04285} SOUTHERN AUTO <A15> [DAVID SINCLAIR]
      *            ADDED NEW UPDATE ROUTINE FOR SOAU IN NOT UPDATING
      *            THE MA-STATUS INFO FIELDS (KEEPS THE 'A' ITEMS).
      *  BAH 140926 MOVE "F" TO FULL-DISPLAY INSTEAD OF "P", AUTOMOTIVE PL#860
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0211 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *   CS 160222 REMOVE COPY LPWKLXE, ITS AN EARNINGS COPY MEMBER THAT IS
      *             PASSED IN LOAN PROCESSING VIA LP01WK USING WK-LX2-REC. 
      *             REPLACED WK-LX2-REC WITH WK-LXE-REC IN LP01WK 
      *             MOVED LPWKLXE TO LPWKLXE.NO
      *             SAVED VERSION LONPQL.160222
      * KEC 2016.0310 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *         CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      * BLM 160311 FIX FOR NEW TY DATE FORMAT, PD0003
      *
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      *         CHANGED MA-APPNO TO USE MA-APP-NUMBER FOR MA1-KEY.
      *         CHANGED MA-STATUS-CD TO MA-STATUS-REASON.
      * CS 160510 REMOVED REDEF OF LE-FLAG/LE-SSNO AND MADE SEPARATE FIELDS
      *           CHANGED LE-FLAG FROM PIC X(4) TO PIC X.
      * CS 160513 ADD MOVE OF LN-OWNBR TO LE-BRNO 
      * BLM 160513 FIX FOR NEW DS-DATE FORMAT, FIX DS-UNEARNED, PD0003
      *
      * KEC 2016.0524 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED USE OF MA-APPDATE & MA-STATUS-DATE
      *          TO USE CCYYMMDD.
      *
      * KEC 2016.0525 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED USE OF MA-TRACK-STATUS TO BE X(10).
      *
      * KEC 2016.0622 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF THE MX-FILE [LIBLA/LAFDMX].
      *         REMOVED USE OF OLD STATUS ROUTINES [LIBLA/MASTATUS(W)].
      *         ADDED USE OF NEW MA-STATUS-INFO FIELDS; AS WELL AS
      *         ADDED USE OF NEW ROUTINES [LIBLA/LAFRMA & LAWSMA].
      *
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * KEC 2017.0425 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED 'LE' FILE TO HAVE SUB-PREFIX OF 'E' OR 'R'.
      *
      * BAH 170630 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 171129 FIXED WK-LXE-REC
      * BLM 180228 FIXED TO UPDATE TY-FEE TABLE INSTEAD OF TY-INS TABLE
      *            FOR NONFILE, AD&D AND ROADGARD
      * KEC 180824 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 181010 GLOBAL CXFILE
      * BAH 181115 GLOBAL FMFILE
      * BAH 181211 GLOBAL LNFILE
      * BAH 190121 GLOBAL LXFILE
      * BAH 190314 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 2019.0627 REMOVED LN-REVOLVING, PT0153
      * BAH 2019.0701 REMOVED U-INSOURS,AVAILABLE-CREDIT FROM LPPFBUF
      * BAH 2019.0723 CLEAR LXG-FUTURE FIELDS
      * BAH 2022.0422 UPDATE TL-ACTIONCD2, SECURITIZATION #1551
      * BAH 2023.0712 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      * BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      * BAH 2024.0206 REMOVED CREATE OF LEFILE "E" RECORDS PER KEVIN
      * BAH 2024.0311 REMOVED EXT-WI-CL-APP-NUMBER
      * BAH 2024.0315 REMOVED BISCAN-LIASMLN AND BISCAN-PRLOANS
      * BAH 2025.0228 LXE-EARN(4) - (7) WERE NOT GETTING CLEARED LIKE
      *               LONPF1 DOES! S35Q-168
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPQ1.CBL S34 05/21/21-08:38:26 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01ES.CPY".
       COPY "LIBLP/LP01ES_SQL.CPY".
       COPY "LIBLP/LPWSES.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/LP01RX.CPY".
       COPY "LIBLP/LP01RX_SQL.CPY".
       COPY "LIBLP/LPWSRX.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       01  WK-KEY          PIC 9(2) VALUE 0.

       01  ELE             PIC 9(6).
       01  ACTION          PIC X(6).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9       VALUE 0.
       01  FIRST-TIME      PIC 9       VALUE 0.
       01  REC-EXISTS      PIC X       VALUE "N".
       01  UPDATE-FG       PIC X       VALUE "N".

       01  SAVE-TL-LINE       PIC 9(3).

       01  ADD-WRITTEN         PIC X(01)   VALUE "N".
       01  ROADGARD-WRITTEN    PIC X(01)   VALUE "N".
       01  NON-FILE-WRITTEN    PIC X(01)   VALUE "N".

       01  HOLD-AMOUNT        PIC S9(6)V99 COMP-3.
       01  HOLD-LN-SRCD       PIC XX.
       01  HOLD-LN-ACCTNO     PIC 9(8)    COMP-3.
       01  HOLD-LN-LNAMT      PIC 9(6)V99 COMP-3.
       01  HOLD-LN-LOANDATE   PIC 9(8)    COMP-3.
       01  HOLD-LN-SSNOS.
           03  HOLD-LN-SSNO   PIC 9(10)   COMP-3  OCCURS 4.
       01  SAVE-OUTBAL        PIC S9(7)V99 COMP   VALUE 0.

       01  M-LE-MSG.
           03  FILLER        PIC X(32)   VALUE
           "ENDORSER RECORD EXISTS !, KEY = ".
           03  M-LE-ACCTNO   PIC 9(8).
           03  FILLER        PIC X       VALUE " ".
           03  M-LE-SSNO     PIC 9(9).
           03  FILLER        PIC X       VALUE " ".
           03  M-LE-OSACCTNO PIC 9(8).
       01  SUB             PIC 9(6) COMP-3.
       01  B-SUB           PIC 9(6) COMP-3.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).

       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LNNO          PIC X(14).
           03  D-LC-DESC       PIC X(15).
           03  D-RESCISSION    PIC X(15).
           03  D-010-040       OCCURS 4.
               05  D-010       PIC X(11).
               05  D-011       PIC X(33).
               05  D-012       PIC X(1).
           03  D-050           PIC XX.
           03  D-051           PIC 9(4)      BLANK ZERO.
           03  D-060           PIC 99/99/99  BLANK ZERO.
           03  D-061           PIC 99/99/99  BLANK ZERO.
           03  D-062           PIC 99/99/99  BLANK ZERO.
      *--->03  D-070           PIC 99/99/99  BLANK ZERO.
           03  D-070           PIC X(8).
           03  D-071           PIC ZZZ.
           03  D-080           PIC X(3).
           03  D-081           PIC X(15).
           03  D-090           PIC X(2).
           03  D-091           PIC X(15).
           03  D-100           PIC X(2).
           03  D-101           PIC X(15).
           03  D-110           PIC 9(6)      BLANK ZERO.
           03  D-111           PIC Z(5)9.99  BLANK ZERO.
           03  D-120           PIC Z(5)9.99  BLANK ZERO.
           03  D-130           PIC X(3).
           03  FILLER          PIC X(5).
           03  D-140-180       OCCURS 5.
               05  D-1FEECD    PIC XX.
               05  D-1FEEAMT   PIC Z(3)9.99  BLANK ZERO.
               05  D-2FEECD    PIC XX.
               05  D-2FEEAMT   PIC Z(3)9.99  BLANK ZERO.
           03  D-190           PIC 9(4).99   BLANK ZERO.
           03  D-191           PIC X(15).

       01  DISPLAY-BUF2        REDEFINES DISPLAY-BUF1.
           03  D-200           PIC X(20).
           03  D-201           PIC X(20).
           03  D-202           PIC X(20).
           03  D-203           PIC X(20).
           03  D-210           PIC ZZ9.999   BLANK ZERO.
           03  D-220           PIC ZZ9.999   BLANK ZERO.
           03  D-230           PIC ZZ9.999   BLANK ZERO.
           03  D-240           PIC ZZ9.999   BLANK ZERO.
           03  D-LOANAMT       PIC Z(5)9.99  BLANK ZERO.
           03  D-250-270       OCCURS 3.
               05  D-1COMP      PIC 99        BLANK ZERO.
               05  D-1OUR       PIC X.
               05  D-1TERM      PIC ZZ9       BLANK ZERO.
               05  D-1COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-280-290       OCCURS 2.
               05  D-2COMP      PIC 99        BLANK ZERO.
               05  D-2OUR       PIC X.
               05  D-2EXP       PIC 9(6)       BLANK ZERO.
               05  D-2COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-300           PIC Z(5)9.99  BLANK ZERO.
           03  D-310           PIC Z(5)9.99  BLANK ZERO.
           03  D-320           PIC Z(5)9.99  BLANK ZERO.
           03  D-330           PIC Z(5)9.99  BLANK ZERO.
           03  D-340           PIC Z(5)9.99  BLANK ZERO.
           03  D-350           PIC Z(5)9.99  BLANK ZERO.
           03  D-360           PIC Z(5)9.99  BLANK ZERO.
           03  D-370           PIC Z(5)9.99  BLANK ZERO.
           03  D-380           PIC Z(5)9.99  BLANK ZERO.

       01  DISPLAY-LIST1.
           03  FILLER          PIC X(17)  VALUE
           "LNNO LCDESC R1:R ".
           03  FILLER          PIC X(40)  VALUE
           "010 011 012 020 021 022 030 031 032 040 ".
           03  FILLER          PIC X(56)  VALUE
           "041 042 050 051 060 061 062 070 071 080 081 090 091 100 ".
           03  FILLER          PIC X(56)  VALUE
           "101 110 111 120 130 131 140 141 142 143 150 151 152 153 ".
           03  FILLER          PIC X(56)  VALUE
           "160 161 162 163 170 171 172 173 180 181 182 183 190 191.".
       01  DISPLAY-LIST2.
           03  FILLER          PIC X(56)  VALUE
           "200 201 202 203 210 220 230 240 LOANAMT 250 251 252 253 ".
           03  FILLER          PIC X(56)  VALUE
           "254 260 261 262 263 264 270 271 272 273 274 280 281 282 ".
           03  FILLER          PIC X(56)  VALUE
           "283 284 290 291 292 293 294 300 310 320 330 340 350 360 ".
           03  FILLER          PIC X(8)  VALUE
           "370 380.".

       01  DISPLAY-LIST3.
           03  FILLER          PIC X(32) VALUE
           "010 011 020 021 030 031 040 041.".

       01  PRIOR-LNAMT     PIC S9(7)V99.

       COPY "LIBWI/RXSCANW.CPY".
       COPY "LIBWI/BWLNS2W.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CREARXW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBCL/BISCANW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/SCREENW.CPY".

       COPY "LIBLP/LONPQ1_DEF.CPY".

       COPY "LIBLP/LONPQ1_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPQ1_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-RX1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-ES1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPQ1" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.
           MOVE " " TO ERRCD.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       MAIN-MODULE.
           MOVE " " TO VDU-CLMENU-SELECTION.
           IF FIRST-TIME = 0
              MOVE SPACES TO ROUTE-CD
              MOVE 1 TO FIRST-TIME
           ELSE
              MOVE 0 TO LOAN-KEY
              PERFORM FORM-RESET
              MOVE " " TO MESS
              PERFORM MSG-LINE.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           MOVE "R" TO VDU-CLMENU-SELECTION.
           PERFORM SET-COLLECTOR-INFO.

       ENTER-LOAN.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       LOAN-GET-NUMBER.
           MOVE 0 TO ELE.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "F7-MENU, F6-SCAN, ENTER LOAN NO:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           IF ( VDUSTATUS = "1<" )
              MOVE ZEROES     TO RXSCAN-BEG-ACCTNO
              MOVE "Y"        TO RXSCAN-RETURN
              MOVE "I"        TO RXSCAN-RETURN-TYPE
              MOVE "WI/RXSCAN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH RXSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF ( RXSCAN-BEG-ACCTNO = ZEROES )
                 GO TO LOAN-GET-NUMBER
              ELSE
                 MOVE RXSCAN-BEG-ACCTNO TO LN-ACCTNO
                 GO TO START-LN-LOOKUP.

           IF VDUSTATUS NOT = "00"
              GO TO LOAN-GET-NUMBER.
           IF VDUNUM0 = 0
              GO TO LOAN-GET-NUMBER.

           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.

       START-LN-LOOKUP.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO LOAN-GET-NUMBER.
           IF LN-POCD = "**"
              MOVE "ACCOUNT HAS BEEN VOIDED" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.
           IF NOT LN-POCD-INRESCISSION
              IF NOT LN-POCD-RESCIND
                 MOVE "ACCOUNT IS NOT RESCISSION TYPE" TO MESS
                 PERFORM SEND-MESS
                 GO TO LOAN-GET-NUMBER.

           IF NOT (GP-ESCROW-FG = "Y" OR "I")
               GO TO ESCROW-CHECK-OK.

           IF LN-PRLNNO NOT = 0
              MOVE LN-REC TO HOLD-LN-REC
           ELSE
              GO TO ESCROW-CHECK-OK.

           MOVE LN-PRLNNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      * DOES THE PRIOR LOAN HAVE ESCROW
           IF LN-ESCROW-FG = "Y" OR "I"
              MOVE LN-ACCTNO TO ES-ACCTNO
              PERFORM OPEN-ES1-FILE
              PERFORM READ-ES1-FILE
              PERFORM CLOSE-ES1-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 IF ES-BALANCE > 0
                    MOVE "DISBURSE ESCROW BALANCE " TO MESS
                    PERFORM SEND-MESS
                    GO TO LOAN-GET-NUMBER
                 ELSE
                    IF ES-BALANCE < 0
                       MOVE "COLLECT NEGATIVE ESCROW BALANCE" TO MESS
                       PERFORM SEND-MESS
                       GO TO LOAN-GET-NUMBER.

      * IF ES-BALANCE = 0
           MOVE HOLD-LN-REC TO LN-REC.

         ESCROW-CHECK-OK.
           PERFORM DISPLAY-FIELDS.

       EXIT-ENTER-KEYS.
           EXIT.

      *************************************************
      *      ENTRY-MODULE
      *      NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                 1 - INITIAL NEW RECORD ENTRY
      *                 2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
      *************************************************
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "F1, F2-MEMO, F3-LOANS, F7-MENU, UPDATE, DELETE :"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              MOVE "L" TO MEMOSC-ONLY
              MOVE LN-OWNBR      TO EXT-WI-CL-OWNBR
              MOVE LN-ACCTNO     TO EXT-WI-CL-ACCTNO
      *       MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER
              MOVE 1             TO EXT-WI-CL-SSNO-NO
              MOVE BW-SSNO       TO EXT-WI-CL-SSNO(1)
              MOVE TRANS-DATE    TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX.
           IF VDUSTATUS = "16"
              MOVE "Y"        TO BWLNS2-SCAN-ONLY
              MOVE BW-SSNO    TO BWLNS2-KEY
              MOVE TRANS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE   TO BWLNS2-SYSDATE
              MOVE "WI/BWLNS2" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH BWLNS2-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.

      * LOAN IN RESCISSION, CUSTOMER DECLINING THE LOAN, RESCIND IT !
           IF ( ACTION = "DELETE" )
              IF LN-POCD-RESCIND
                 MOVE "ACCOUNT HAS ALREADY BEEN RESCINDED" TO MESS
                 PERFORM SEND-MESS
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF LN-POCD-INRESCISSION
                    PERFORM RESCINDED-UPDATE
                    MOVE "CLR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE.

      * LOAN IN RESCISSION, CUSTOMER ACCEPTING THE LOAN, FINISH
      * UPDATING
           IF ( ACTION = "UPDATE" )
              IF LN-POCD-RESCIND
                 MOVE "ACCOUNT HAS ALREADY BEEN RESCINDED" TO MESS
                 PERFORM SEND-MESS
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF LN-POCD-INRESCISSION
                    PERFORM UPDATE-RESCISSION
                    IF UPDATE-FG = "N"
                       GO TO MODIFY-ROUTINE
                    ELSE
                       MOVE "CLR" TO IO-TYPE
                       GO TO END-ENTRY-MODULE.

           GO TO MODIFY-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE 0 TO LOAN-KEY.

           MOVE " " TO DISPLAY-BUF1.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE LN-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.

           IF LN-POCD-INRESCISSION
              MOVE " IN RESCISSION" TO D-RESCISSION
           ELSE
              IF LN-POCD-RESCIND
                 MOVE "   RESCINDED" TO D-RESCISSION.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO SUB.
       DISP-NEXT-BW.
           MOVE LN-SSNO(SUB) TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF NUM-BW NOT = 0
              MOVE NUM-BW TO BW-SSNO
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(SUB)
              MOVE INSPECT-W TO D-011(SUB)
              MOVE LN-MAKERCD(SUB) TO D-012(SUB).
           IF SUB = 1
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-LNNO.
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISP-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-ORGST TO D-050.
           MOVE LN-ORGBR TO D-051.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-060.
           MOVE LN-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-061.
           MOVE LN-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-062.
           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-070.
           MOVE LN-SUM-ORGTERM TO D-071.
           MOVE LN-PURCD TO D-080 CD-CODE.
           MOVE "PU"     TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-081.
           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE  D-090.
           PERFORM GET-CD.
           MOVE CD-DESC   TO D-091.
           MOVE LN-SRCD TO D-100 CD-CODE.
           MOVE "SR"    TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-101.

           MOVE LN-PRLNNO TO D-110.
           COMPUTE PRIOR-LNAMT ROUNDED = (LN-PRBAL + LN-PRINTDUE) -
                (LN-PRFINCHGRB + LN-PRINSRB).
           MOVE PRIOR-LNAMT TO D-111.
           MOVE LN-PROCEEDS TO D-120.
           MOVE LN-INSCODES TO D-130.

           MOVE 1 TO ELE.
           MOVE 1 TO SUB.
       DISP-NEXT-GL.
           MOVE LN-FEECODE(SUB) TO D-1FEECD(ELE).
           MOVE LN-FEEAMT(SUB) TO D-1FEEAMT(ELE).
           ADD 1 TO SUB.
           MOVE LN-FEECODE(SUB) TO D-2FEECD(ELE).
           MOVE LN-FEEAMT(SUB) TO D-2FEEAMT(ELE).
           IF SUB < 10
              ADD 1 TO ELE SUB
              GO TO DISP-NEXT-GL.

           MOVE LN-DLNO TO D-190 DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-191.
           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO DISPLAY-BUF2.

           MOVE LN-COLLID TO D-200.

           PERFORM GET-COLLATERAL.
           MOVE LTC-COLL-INFO1(1) TO D-201.
           MOVE LTC-COLL-INFO2(1) TO D-202.
           MOVE LTC-COLL-INFO3(1) TO D-203.

           MOVE LN-OVRDRATE TO D-210.
           MOVE LN-EFFRATE TO D-220.
           MOVE LN-APRATE TO D-230.
           MOVE LN-ENDORATE TO D-240.
           MOVE LN-SUM-LNAMT TO D-LOANAMT.

           MOVE 1 TO SUB.
       DISP-NEXT-IC.
           MOVE LN-INSCOMP(SUB) TO D-1COMP(SUB).
           MOVE LN-INSPREM(SUB) TO D-1PREM(SUB).
           MOVE SUB TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-1TERM(SUB).
           MOVE LN-INSCOVR(SUB) TO D-1COVR(SUB).
           MOVE LN-INSOURS(SUB) TO D-1OUR(SUB).
           IF SUB < 3
              ADD 1 TO SUB
              GO TO DISP-NEXT-IC.

           MOVE 1 TO SUB.
           MOVE 4 TO ELE.
       DISP-NEXT-2IC.
           MOVE LN-INSCOMP(ELE) TO D-2COMP(SUB).
           MOVE LN-INSPREM(ELE) TO D-2PREM(SUB).
           MOVE LN-INSEXP-DATE(ELE) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-2EXP(SUB).
           MOVE LN-INSCOVR(ELE) TO D-2COVR(SUB).
           MOVE LN-INSOURS(ELE) TO D-2OUR(SUB).
           IF ELE < 5
              ADD 1 TO ELE SUB
              GO TO DISP-NEXT-2IC.

           MOVE LN-SUM-SERCHG TO D-300.
           MOVE LN-SUM-MAINTFEE TO D-310.
           MOVE LN-SUM-EXTCHG TO D-320.
           MOVE LN-SUM-INTCHG TO D-330.
           PERFORM LOAN-CALCULATIONS.
           MOVE FINANCED-AMOUNT TO D-340.
           MOVE TOTAL-REPAY TO D-350.
           COMPUTE HOLD-AMOUNT = LN-ANTICERN(1) + LN-ANTICERN(2)
                               + LN-ANTICERN(3) + LN-ANTICERN(4)
                               + LN-ANTICERN(5).
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD LN-ANTICERN(6) TO HOLD-AMOUNT.
           MOVE HOLD-AMOUNT TO D-360.
           MOVE LN-1STPYAMT TO D-370.
           MOVE LN-REGPYAMT TO D-380.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF LN-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.

      ******************************************************************
       RESCINDED-UPDATE SECTION.
      ******************************************************************

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF ( IO-GOOD )
              MOVE "*R"       TO LN-POCD
              MOVE TRANS-DATE TO LN-POFFDATE
              SUBTRACT LN-ACCERN(1) LN-ACCERN(2) LN-ACCERN(3)
                       LN-ACCERN(4) LN-ACCERN(5)
                       FROM LN-NONACRERN
              IF NOT GP-INCL-OTH-IN-EARNED-N
                 SUBTRACT LN-ACCERN(6) FROM LN-NONACRERN
              END-IF
              MOVE ZEROES TO LN-CURBAL LN-TOTERN
              PERFORM REWRITE-LN1-FILE
              PERFORM UPDATE-NOTICE-FILE.

           MOVE LN-ACCTNO TO RX-ACCTNO.
           PERFORM OPEN-RX1-FILE.
           PERFORM READ-RX1-FILE.
           IF ( IO-GOOD )
              MOVE "R" TO RX-STATUS
              PERFORM REWRITE-RX1-FILE.

           PERFORM CLOSE-RX1-FILE.

           PERFORM REMOVE-TEMP-CHECKS.

      * IF LOAN WAS A RENEWAL, FIX THE RENEWED LOAN
      
           IF ( LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO" )
              MOVE LN-PRLNNO TO LN-ACCTNO
              PERFORM READ-LN1-FILE
              IF ( IO-GOOD )
                 MOVE SPACES TO LN-POCD
                 PERFORM REWRITE-LN1-FILE
                 PERFORM UPDATE-NOTICE-FILE.

       RESCINDED-UPDATE-EXIT.
           EXIT.

      ******************************************************************
       REMOVE-TEMP-CHECKS SECTION.
      ******************************************************************

           PERFORM OPEN-CX1-FILE.

           MOVE BRANCH TO CX-BRNO.
           MOVE SPACES TO CX-TYPE.
           MOVE ZEROES TO CX-SEQNO.

           PERFORM START-CX1-FILE.

       REMOVE-TEMP-CHECKS-NEXT.

           PERFORM READ-CX1-FILE-NEXT.
           IF ( IO-BAD ) OR (CX-BRNO NOT = CX-PATH-OWNBR)
              GO TO REMOVE-TEMP-CHECKS-END.

           IF ( CX-REF  NOT = LN-ACCTNO )
              GO TO REMOVE-TEMP-CHECKS-NEXT.

           PERFORM REMOVE-CHECK-FORM-REQ.

           PERFORM DELETE-CX1-FILE.

           GO TO REMOVE-TEMP-CHECKS-NEXT.

       REMOVE-TEMP-CHECKS-END.
           PERFORM CLOSE-CX1-FILE.

       REMOVE-TEMP-CHECKS-EXIT.
           EXIT.

      ******************************************************************
       REMOVE-CHECK-FORM-REQ SECTION.
      ******************************************************************

           MOVE "REMOVING CHECK FORM REQUEST........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-FM1-FILE.

           MOVE FM-PATH-OWNBR TO FM-BRNO.

           MOVE SPACES        TO LPWSFM-EDIT-FM-INITKEY.
           MOVE CX-BRNO       TO LPWSFM-EDIT-FM-BRNO.
           MOVE CX-TYPE       TO LPWSFM-EDIT-FM-TYPE
           MOVE CX-SEQNO      TO LPWSFM-EDIT-FM-SEQNO.
           MOVE ZEROES        TO LPWSFM-EDIT-FM-CKFMNO FM-FMNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.

           PERFORM START-FM1-FILE.

       REMOVE-NEXT-CHECK-REQUEST.

           PERFORM READ-FM1-FILE-NEXT.
           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           IF ( IO-GOOD )
              IF ( LPWSFM-EDIT-FM-BRNO NOT = CX-BRNO ) OR
                 ( LPWSFM-EDIT-FM-TYPE NOT = CX-TYPE )
                 MOVE 9 TO IO-FG.

           IF ( IO-BAD ) OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO REMOVE-FM1-END.

           IF ( LPWSFM-EDIT-FM-SEQNO NOT = CX-SEQNO )
      *       UNLOCK FM-FILE
              GO TO REMOVE-NEXT-CHECK-REQUEST.

           PERFORM DELETE-FM1-FILE.

       REMOVE-FM1-END.
           PERFORM CLOSE-FM1-FILE.

       REMOVE-FM1-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-RESCISSION SECTION.
      ******************************************************************

           MOVE "Y" TO UPDATE-FG.

      * ONLY ALLOW THE UPDATE, IF ON OR PAST THE RESCISSION DATE
           IF LN-INTDATE > TRANS-DATE
              MOVE "STILL IN RESCISSION PERIOD" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO UPDATE-FG
              GO TO EXIT-UPDATE-RESCISSION.

      * VERIFY RECORD IS UNCHANGED.    

           MOVE LN-REC TO HOLD-LN-REC.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              MOVE "N" TO UPDATE-FG
              GO TO EXIT-UPDATE-RESCISSION.

           IF ( LN-REC NOT = HOLD-LN-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              MOVE "N" TO UPDATE-FG
              GO TO EXIT-UPDATE-RESCISSION.

           MOVE 0 TO SUB BISCAN-CALL-CODE IO-FG.
           PERFORM DUAL-CHECK 4 TIMES.
           IF IO-FG NOT = 0
              MOVE "N" TO UPDATE-FG
              GO TO EXIT-UPDATE-RESCISSION.

      * CHECK FOR RENEWAL  

           IF NOT (LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
              GO TO UPDATE-CONTINUE.

      * FROM HERE TO UPDATE-CONTINUE, YOU HAVE THE PRIOR LOAN IN THE LN-REC

           MOVE LN-REC  TO HOLD-LN-REC.
           MOVE LN-SRCD TO HOLD-LN-SRCD.

           PERFORM SETUP-LP-RECORD.

           PERFORM SETUP-LONPF-BUFFER.

           IF ( UPDATE-FG = "N" ) GO TO EXIT-UPDATE-RESCISSION.

           MOVE LN-ACCTNO TO LOAN-KEY.
           PERFORM SAVE-SCREEN.

           MOVE "LP/LONPFA" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

           MOVE HOLD-LN-REC TO LN-REC.

      * DON'T PASS ACCTNO TO NEXT SCREEN

           MOVE ZEROES TO LOAN-KEY BORROWER-KEY.
           MOVE SPACES TO WK-SAVE-RECORD.

       UPDATE-CONTINUE.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.

           MOVE SPACES TO LN-POCD.

      * 01/21/00 BAH...MOVE LN-INTDATE TO LN-ENTDATE..BECAUSE WHEN A
      *   RESC TAKES PLACE ACROSS A MONTH-END, NFMORP DOES NOT PICK UP THE
      *   ACCOUNT CORRECTLY. NFMORP IS BASED ON THE LNFILE, OTHER MORP'S READ
      *   THE TRFILE.....EXAMPLE: AMERICAN THRIFT
      *     ENTDATE : 11/29/99  LOANDATE : 11/29/99   INTDATE :  12/3/99
      *   NOV'S NFMORP WILL SKIP THE ACCT BECAUSE OF THE "*I" IN LN-POCD,
      *   WHICH IS CORRECT, IT IS STILL IN RESCISSION. THEN THE ACCT IS
      *   TAKEN OUT OF RESC AND UPDATED INTO AN ACTIVE LOAN, AND DEC'S NFMORP
      *   WILL NOT FIND THE ACCT BECAUSE THE LN-ENTDATE IS 11/29/99 NOT DEC !
      *   AND NOW IF YOU WERE TO RERUN NOV'S NFMORP, IT WOULD PRINT. WRONG !
      *   SO WE DECIDED TO MOVE THE INTDATE TO THE ENTDATE WHEN YOU UPDATE

           MOVE LN-INTDATE TO LN-ENTDATE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO LN-RESCISSION-DATE.

           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.

           PERFORM SET-DELINQUENCY.
           PERFORM UPDATE-TR1-FILE.
           PERFORM UPDATE-BW1-FILE.
           PERFORM UPDATE-DS1-FILE.
           PERFORM UPDATE-TY1-FILE.
           MOVE 0 TO IO-FG.

           MOVE "W" TO STAT
           PERFORM UPDATE-BI-FILE.
           PERFORM UPDATE-TEMP-CHECKS.

      * DELETING 'RX' RECORDS 

           MOVE "D"         TO CREARX-ACTION.
           MOVE LN-OWNBR    TO CREARX-RX-BRNO.
           MOVE LN-ACCTNO   TO CREARX-RX-ACCTNO.

           MOVE "CL/CREARX" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CREARX-AREA.
           CANCEL FORM-PROGX.

           IF ( CREARX-ERRCD NOT = SPACES )
              MOVE "ERROR DELETING RESCISSION TRAILER (RX)" TO MESS
              PERFORM SEND-MESS.

       EXIT-UPDATE-RESCISSION.
           EXIT.

      ******************************************************************
       UPDATE-TEMP-CHECKS SECTION.
      ******************************************************************

           PERFORM OPEN-CX1-FILE.
           MOVE BRANCH TO CX-BRNO.
           MOVE SPACES TO CX-TYPE.
           MOVE ZEROES TO CX-SEQNO.

           PERFORM START-CX1-FILE.

       UPDATE-TEMP-CHECKS-NEXT.

           PERFORM READ-CX1-FILE-NEXT.
           IF IO-BAD OR (CX-BRNO NOT = CX-PATH-OWNBR)
              GO TO UPDATE-TEMP-CHECKS-END.

           IF CX-REF NOT = LN-ACCTNO
              GO TO UPDATE-TEMP-CHECKS-NEXT.

           MOVE "U" TO CX-RESCISSION.
           PERFORM REWRITE-CX1-FILE.

           GO TO UPDATE-TEMP-CHECKS-NEXT.

       UPDATE-TEMP-CHECKS-END.

           PERFORM CLOSE-CX1-FILE.

       UPDATE-TEMP-CHECKS-EXIT.
           EXIT.

      ***************************************
       UPDATE-TR1-FILE SECTION.
      ***************************************
           MOVE "UPDATING LOAN TRANSACTION FILE.........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-TRL1-FILE.
           MOVE 1 TO SAVE-TL-LINE.
       UPDATE-NEXT-TR1.
           PERFORM CLEAR-TRL-FILE.

           MOVE BRANCH       TO TL-BRNO.
           MOVE TRANS-DATE   TO TL-DATE.
           MOVE LN-CLASS     TO TL-CLASS.
           MOVE LN-ACCTNO    TO TL-ACCTNO.
           MOVE LN-LAWCODE   TO TL-LAWCODE.
           MOVE SAVE-TL-LINE TO TL-LINE.

           MOVE LN-LOANTYPE TO TL-LOANTYPE.
           MOVE LN-LOANDATE TO TL-LOANDATE.
           MOVE LN-INTDATE  TO TL-INTDATE.
           MOVE LN-PURCD    TO TL-PURCD.
           MOVE LN-SRCD     TO TL-SRCD.
           MOVE LN-COLLCD   TO TL-COLLCD.
           MOVE LN-ORGTERM  TO TL-ORGTERM.
           MOVE LN-PROCEEDS TO TL-PROCEEDS.
           MOVE LN-LNAMT    TO TL-LNAMT.
           MOVE LN-FINCHG   TO TL-FINCHG.
           MOVE LN-NOREGPY  TO TL-NOREGPY.
           MOVE LN-REGPYAMT TO TL-REGPYAMT.
           MOVE LN-1STPYAMT TO TL-1STPYAMT.
           MOVE LN-INSURED  TO TL-INSURED.
           MOVE LN-DLNO     TO TL-DLNO.
           MOVE LN-PRLNNO   TO TL-PRLNNO.
           MOVE LN-PRBAL    TO TL-PRBAL.
           MOVE LN-PRINTDUE TO TL-PRINTDUE.
           MOVE LN-PRFINCHGRB TO TL-PRFINCHGRB.
           MOVE LN-PRINSRB  TO TL-PRINSRB.
           MOVE LN-SERVACCT TO TL-SERVACCT.
           MOVE LN-STATE-WORKER TO TL-STATE-WORKER.

           MOVE LN-ANTICERN(1) TO TL-ANTICERN(1).
           MOVE LN-ANTICERN(2) TO TL-ANTICERN(2).
           MOVE LN-ANTICERN(3) TO TL-ANTICERN(3).
           MOVE LN-ANTICERN(4) TO TL-ANTICERN(4).
           MOVE LN-ANTICERN(5) TO TL-ANTICERN(5).
           MOVE LN-ANTICERN(6) TO TL-ANTICERN(6).
           MOVE LN-ACCERN(1) TO TL-EARNINGS(1).
           MOVE LN-ACCERN(2) TO TL-EARNINGS(2).
           MOVE LN-ACCERN(3) TO TL-EARNINGS(3).
           MOVE LN-ACCERN(4) TO TL-EARNINGS(4).
           MOVE LN-ACCERN(5) TO TL-EARNINGS(5).
           MOVE LN-ACCERN(6) TO TL-EARNINGS(6).

           MOVE LN-MAINTFEE TO TL-MAINTFEE.
           MOVE LN-SERCHG TO TL-SERCHG.
           MOVE LN-EXTCHG TO TL-EXTCHG.
           MOVE LN-INSTYPES TO TL-INSTYPES.
           MOVE LN-INSELIGIBLE TO TL-INSELIGIBLE.
           MOVE LN-INTCHG TO TL-INTCHG.

           MOVE 1 TO SUB.
       NEXT-TL-BORRTBL.
           IF SUB NOT > 4
              MOVE LN-SSNO(SUB) TO TL-SSNO(SUB)
              MOVE LN-MAKERCD(SUB) TO TL-MAKERCD(SUB).
           IF SUB NOT > 8
              MOVE LN-INSCOMP(SUB) TO TL-INSCOMP(SUB)
              MOVE LN-INSOURS(SUB) TO TL-INSOURS(SUB)
              MOVE LN-INSEFF-DATE(SUB) TO TL-INSEFF-DATE(SUB)
              MOVE LN-INSEXP-DATE(SUB) TO TL-INSEXP-DATE(SUB)
              MOVE LN-INSPREM(SUB) TO TL-INSPREM(SUB)
              MOVE LN-INSCOMM(SUB) TO TL-INSCOMM(SUB).
           IF SUB NOT > 10
              MOVE LN-FEECODE(SUB) TO TL-FEECODE(SUB)
              MOVE LN-FEEAMT(SUB) TO TL-FEEAMT(SUB).
           IF SUB < 10
              ADD 1 TO SUB
              GO TO NEXT-TL-BORRTBL.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TL-USERID.
           MOVE POST-TIME TO TL-POSTTIME.
           MOVE POST-DATE TO TL-POSTDATE.

           MOVE LN-MIP-FG    TO TL-MIP-FG.
           MOVE LN-ACTIONCD2 TO TL-ACTIONCD2.

           PERFORM WRITE-TRL1-FILE.
           IF IO-FG = 0
              GO TO END-UPDATE-TL.

           IF SAVE-TL-LINE < 999
              ADD 1 TO SAVE-TL-LINE
              GO TO UPDATE-NEXT-TR1.

           MOVE "TRANSACTION SEQ = 999, REC NOT CREATED !" TO MESS.
           PERFORM SEND-MESS.

       END-UPDATE-TL.
           PERFORM CLOSE-TRL1-FILE.
       EXIT-UPDATE-TR1.
           EXIT.

      ********************************
       UPDATE-BW1-FILE SECTION.
      ********************************
           MOVE "UPDATING BORROWER FILE.................." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE.
           PERFORM GET-CD.

           MOVE 1 TO SUB.
           PERFORM OPEN-BW1-FILE.
       UPDATE-NEXT-BW.
           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           ADD 1 TO BW-LOANS.
           IF SUB = 1
              IF CD-CO-SECURED = "R"
                 ADD LN-YTDINTPD TO BW-RE-INTYTD
              ELSE
                 ADD LN-YTDINTPD TO BW-INTYTD.
           IF LN-SMALLOAN = "Y"
              ADD LN-LNAMT TO BW-LIASMLN
            ELSE
              ADD LN-LNAMT TO BW-LIAOTH.
           PERFORM REWRITE-BW1-FILE.
           IF SUB = 4
              GO TO EXIT-UPDATE-BW1.
           ADD 1 TO SUB.
           IF LN-SSNO(SUB) NOT = 0
              GO TO UPDATE-NEXT-BW.
       EXIT-UPDATE-BW1.
           PERFORM CLOSE-BW1-FILE.

      ************************
       UPDATE-DS1-FILE SECTION.
      ************************
           MOVE "UPDATING DEALER STATISTICS FILE........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-DS1-FILE.
           MOVE "N" TO REC-EXISTS.
       DS1-CHECK-EXISTS.
           PERFORM SET-DS1-KEY.
           PERFORM READ-DS1-FILE.
           IF IO-FG = 0
              MOVE "Y" TO REC-EXISTS
              GO TO DS1-UPDATE-REC.
           PERFORM CLEAR-DS-FILE.
           PERFORM SET-DS1-KEY.

       DS1-UPDATE-REC.
           ADD LN-ORGTERM TO DS-SUMWGTTM.
           ADD LN-LNAMT TO DS-SUMWGTAV.
           COMPUTE DS-SUMWGTTA =
              DS-SUMWGTTA + (LN-ORGTERM * LN-LNAMT).

           IF LN-SERVACCT NOT = "Y"
              ADD LN-DLDISC TO DS-DLDISC
                               DS-DLDISC-VOL(1)
                               DS-DLDISC-VOL(2)
              ADD LN-DLRVPRIN(1) TO DS-DLRVPRIN(1)
              ADD LN-DLRVPRIN(2) TO DS-DLRVPRIN(2)
              ADD LN-DLPARVHELD TO DS-DLPARVHELD.

      *    -------------------------------------------------------------
      *    UPDATE PROCEEDS: WE DO NOT CARE ABOUT ACCOUNT STATUS.
      *    -------------------------------------------------------------
           IF GP-UNEARNED-STATS = "Y"
              ADD LN-PROCEEDS TO DS-PROCEEDS(1)
                                 DS-PROCEEDS(2).

           ADD 1 TO DS-OUTSUNIT(1).
           ADD LN-LNAMT TO DS-OUTSAMT(1).

           IF LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              ADD 1 TO DS-RNEWUNIT(1) DS-RNEWUNIT(2)
              ADD LN-LNAMT TO DS-RNEWAMT(1) DS-RNEWAMT(2)
           ELSE
           IF LC-SALEFIN = "Y" AND GP-THE-ONLY-SF-SRCD NOT = " "
              IF LN-SRCD = GP-THE-ONLY-SF-SRCD
                 ADD 1 TO DS-NPURUNIT(1) DS-NPURUNIT(2)
                 ADD LN-LNAMT TO DS-NPURAMT(1) DS-NPURAMT(2)
              END-IF
           ELSE
           IF LN-SRCD = "NB"
              ADD 1 TO DS-NPURUNIT(1) DS-NPURUNIT(2)
              ADD LN-LNAMT TO DS-NPURAMT(1) DS-NPURAMT(2)
           ELSE
              ADD 1 TO DS-OPURUNIT(1) DS-OPURUNIT(2)
              ADD LN-LNAMT TO DS-OPURAMT(1) DS-OPURAMT(2).

      * UPDATE DELINQUENCY:
           ADD 1 TO DS-UNITS(DSNOW DSREC RDSUB)
                    DS-UNITS(DSNOW DSCON CSUB).
      *    -------------------------------------------------------------
      *    SETUP OUTBAL WORKER FOR UPDATE OF STATS
      *    -------------------------------------------------------------
           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL
                         GIVING SAVE-OUTBAL.

           ADD SAVE-OUTBAL TO DS-OUTBAL(DSNOW DSREC RDSUB)
                              DS-OUTBAL(DSNOW DSCON CSUB).
      * UPDATE DELINQUENCY BANKRUPT:
           IF LN-SRCD = "RB"
              ADD 1 TO DS-UNITS-BNKRPT(DSNOW DSREC RDSUB)
                       DS-UNITS-BNKRPT(DSNOW DSCON CSUB)
              ADD SAVE-OUTBAL TO DS-OUTBAL-BNKRPT(DSNOW DSREC RDSUB)
                                 DS-OUTBAL-BNKRPT(DSNOW DSCON CSUB).

      * UPDATE UNEARNED STATISTICS NET AND VOLUME
      
           IF GP-UNEARNED-STATS = "Y"
              PERFORM COMPUTE-EARNINGS
              PERFORM VARYING DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 6
                        AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2
                  IF DSSUB2 = 1
                     ADD EARN-UNEARN(DSSUB3) TO
                                 DS-UNEARNED-AMT(1 DSSUB2 DSSUB3)
                  ELSE
                     IF (DSSUB3 < 6) OR
                        (DSSUB3 = 6 AND NOT GP-INCL-OTH-IN-EARNED-N)
                        ADD LN-ANTICERN(DSSUB3)
                                 TO DS-UNEARNED-AMT(1 DSSUB2 DSSUB3)
                                    DS-UNEARNED-AMT(2 DSSUB2 DSSUB3)
                     END-IF
                  END-IF
              END-PERFORM.

           IF REC-EXISTS = "Y"
              PERFORM REWRITE-DS1-FILE
           ELSE
              PERFORM WRITE-DS1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-DS1-FILE.

      ************************
       UPDATE-TY1-FILE SECTION.
      ************************
           MOVE "UPDATING TODAY'S TOTAL FILE............." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-TY1-FILE.
           MOVE "N" TO REC-EXISTS.
       TY-CHECK-EXISTS.
           PERFORM SET-TY-KEY.
           PERFORM READ-TY1-FILE.
           IF IO-FG = 0
              GO TO TY-UPDATE-REC.
           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           PERFORM CLEAR-TY-FILE.
           PERFORM SET-TY-KEY.
           PERFORM WRITE-TY1-FILE.
           IF IO-FG = 9 AND E-CODE NOT = "22"
              GO TO IO-ERROR.
           GO TO TY-CHECK-EXISTS.
       TY-UPDATE-REC.
           ADD LN-ORGTERM TO TY-SUMWGTTM.
           ADD LN-LNAMT TO TY-SUMWGTAV.
           COMPUTE TY-SUMWGTTA =
              TY-SUMWGTTA + (LN-ORGTERM * LN-LNAMT).

           PERFORM GET-SRCD-SUB.
           IF SRCD-NOT-PL
              ADD 1 TO TY-NO(1 SRCD)
                       TY-NO(2 SRCD)
              ADD LN-LNAMT TO TY-AMT(1 SRCD)
                              TY-AMT(2 SRCD)
              ADD LN-OT2BAL TO TY-O2(1 SRCD)
                               TY-O2(2 SRCD)
              ADD LN-INTCHG TO TY-INT(1 SRCD)
                               TY-INT(2 SRCD).

      *    -------------------------------------------------------------
      *    UPDATE PROCEEDS: WE DO NOT CARE ABOUT ACCOUNT STATUS.
      *    -------------------------------------------------------------
           IF GP-UNEARNED-STATS = "Y"
              ADD LN-PROCEEDS TO TY-PROCEEDS.

      * UPDATE DELINQUENCY:
           ADD 1 TO TY-UNITS(1 RDSUB)
                    TY-UNITS(2 CSUB).
      *    -------------------------------------------------------------
      *    SETUP OUTBAL WORKER FOR UPDATE OF STATS
      *    -------------------------------------------------------------
           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL
                         GIVING SAVE-OUTBAL.

           ADD SAVE-OUTBAL TO TY-OUTBAL(1 RDSUB)
                              TY-OUTBAL(2 CSUB).
      * UPDATE DELINQUENCY BANKRUPT:
           IF LN-SRCD = "RB"
              ADD 1 TO TY-UNITS-BNKRPT(1 RDSUB)
                       TY-UNITS-BNKRPT(2 CSUB)
              ADD SAVE-OUTBAL TO TY-OUTBAL-BNKRPT(1 RDSUB)
                                 TY-OUTBAL-BNKRPT(2 CSUB).

      ***************************************
      * UPDATE INSURANCES
      *  DO NOT INCLUDE 7&8 HERE THEY ARE NOT BEING USED YET
      *  AND WE DO NOT WANT TO COUND THEN AS NOT WRITTEN IN STATS
      *                                         (MJD 09/10/1999).
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF LN-INSPREM(SUB) NOT = 0
                 ADD 1 TO TY-INS-NO-WROTE(SUB)
                 ADD LN-INSPREM(SUB) TO TY-INS-AMT-WROTE(SUB)
              ELSE
                 ADD 1 TO TY-INS-NO-NOTWROTE(SUB)
              END-IF
           END-PERFORM.

           IF LN-INSELIGIBLE-DL OR
              LN-INSELIGIBLE-LL
              ADD 1 TO TY-INS-NO-ELIG(1).

           IF LN-INSELIGIBLE-AH
              ADD 1 TO TY-INS-NO-ELIG(2).

           IF LN-INSELIGIBLE-PP OR
              LN-INSELIGIBLE-PP2
              ADD 1 TO TY-INS-NO-ELIG(3).

           IF LN-INSELIGIBLE-O1
              ADD 1 TO TY-INS-NO-ELIG(4).

      * TEST FOR ROADGARD INSURANCE AND AD&D (WORLD)
           MOVE "N" TO ROADGARD-WRITTEN ADD-WRITTEN NON-FILE-WRITTEN
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF LN-FEECODE(SUB) NOT = SPACES
                 IF LN-FEECODE(SUB) = GP-NON-FILE-FEECODE
                    ADD 1 TO TY-FEE-NO-WROTE(1)
                    ADD LN-FEEAMT(SUB) TO TY-FEE-AMT-WROTE(1)
                    MOVE "Y" TO NON-FILE-WRITTEN
                 END-IF
                 IF LN-FEECODE(SUB) = BR-ADD-INS-FEECODE
                    ADD 1 TO TY-FEE-NO-WROTE(2)
                    ADD LN-FEEAMT(SUB) TO TY-FEE-AMT-WROTE(2)
                    MOVE "Y" TO ADD-WRITTEN
                 END-IF
                 IF LN-FEECODE(SUB) = BR-ROAD-G-SGL-FEECODE OR
                    LN-FEECODE(SUB) = BR-ROAD-G-FAM-FEECODE
                    ADD 1 TO TY-FEE-NO-WROTE(3)
                    ADD LN-FEEAMT(SUB) TO TY-FEE-AMT-WROTE(3)
                    MOVE "Y" TO ROADGARD-WRITTEN
                 END-IF
              END-IF
           END-PERFORM.
           IF NON-FILE-WRITTEN = "N"
              ADD 1 TO TY-FEE-NO-NOTWROTE(1).
           IF ADD-WRITTEN = "N"
              ADD 1 TO TY-FEE-NO-NOTWROTE(2).
           IF ROADGARD-WRITTEN = "N"
              ADD 1 TO TY-FEE-NO-NOTWROTE(3).

      ************************************************
      *  UPDATE DEALER RESERVES:

           IF LN-SERVACCT NOT = "Y"
              ADD LN-DLDISC TO TY-DLDISC
              ADD LN-DLRVPRIN(1) TO TY-DLRVPRIN(1 1)
                                    TY-DLRVPRIN(2 1)
              ADD LN-DLRVPRIN(2) TO TY-DLRVPRIN(1 2)
                                    TY-DLRVPRIN(2 2)
              ADD LN-DLPARVHELD TO TY-DLPARVHELD(1)
                                   TY-DLPARVHELD(2).

      * UPDATE UNEARNED STATISTICS NET AND VOLUME
      
           IF GP-UNEARNED-STATS = "Y"
              PERFORM COMPUTE-EARNINGS
              PERFORM VARYING TYSUB3 FROM 1 BY 1 UNTIL TYSUB3 > 6
                        AFTER TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 2
                  IF TYSUB2 = 1
                     ADD EARN-UNEARN(TYSUB3) TO
                                 TY-UNEARNED-AMT(TYSUB2 TYSUB3)
                  ELSE
                     IF (TYSUB3 < 6) OR
                        (TYSUB3 = 6 AND NOT GP-INCL-OTH-IN-EARNED-N)
                        ADD LN-ANTICERN(TYSUB3)
                                   TO TY-UNEARNED-AMT(TYSUB2 TYSUB3)
                     END-IF
                  END-IF
              END-PERFORM.

           PERFORM REWRITE-TY1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-TY1-FILE-EXIT.
           PERFORM CLOSE-TY1-FILE.


      ********************************
       DUAL-CHECK SECTION.
      ********************************
           ADD 1 TO SUB.

           IF ( LN-SSNO(SUB)     = ZEROES ) OR
              ( BISCAN-CALL-CODE = 9      )
              GO TO EXIT-DUAL-CHECK.

           MOVE LN-SSNO(SUB) TO BISCAN-SSNO.
           MOVE LN-SRCD      TO BISCAN-SRCD.
           MOVE LN-PRLNNO    TO BISCAN-PRLNNO.

           IF ( SP-DUAL-IN-STATE  NOT = "Y" ) AND
              ( SP-DUAL-IN-BRANCH NOT = "Y" )
              MOVE 2 TO BISCAN-CALL-CODE
              PERFORM CALL-SCAN.

           IF ( BISCAN-CALL-CODE = 9 )
              MOVE 9 TO IO-FG.

       EXIT-DUAL-CHECK.
           EXIT.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE TRANS-DATE     TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.
       EXIT-UPDATE-NOTICE-FILE.
           EXIT.

      ******************************************************************
       CALL-SCAN SECTION.
      ******************************************************************

           MOVE SP-DUAL-IN-STATE  TO BISCAN-STATE-FG.
           MOVE SP-DUAL-IN-BRANCH TO BISCAN-BRANCH-FG.
           MOVE LN-OWNBR          TO BISCAN-BRANCH.
           MOVE LN-ORGST          TO BISCAN-STATE.
           MOVE TRANS-DATE        TO BISCAN-TRANS-DATE.
           MOVE "CL/BISCAN"       TO FORM-NAM.

           CALL FORM-PROGX USING FORM-PATH
                                BISCAN-WORKERS.
           CANCEL FORM-PROGX.

       EXIT-CALL-SCAN.
           EXIT.

      ******************************************
       SAVE-SCREEN SECTION.
      ******************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           MOVE SPACES  TO WK-SAVE-RECORD.
           MOVE 0       TO WK-ELE.
           MOVE LN-REC  TO WK-SAVE-RECORD.
           MOVE LP-REC  TO WK-LP-REC.
           MOVE LX-REC  TO WK-LX-REC.
           MOVE LXG-REC TO WK-LXG-REC.

           MOVE 0       TO LXE-EARN(1)
                           LXE-EARN(2)
                           LXE-EARN(3)
                           LXE-EARN(4)
                           LXE-EARN(5)
                           LXE-EARN(6)
                           LXE-EARN(7).
           IF SP-ERNFRMLA(1) = 15 OR SP-ERNFRMLA(5) = 16
              MOVE LN-ERN-IB-INTOWE TO LXE-WORKER
           ELSE
              MOVE 0                TO LXE-WORKER.
           MOVE 0      TO LXE-WORKER2.

           MOVE LXE-REC TO WK-LXE-REC.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.

       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *******************************
       SETUP-LP-RECORD SECTION.
      *******************************
           PERFORM CLEAR-LP-FILE.
           MOVE LN-STATUSFG TO LP-STATUSFG.
           MOVE LN-ACCTNO TO LP-ACCTNO.

           MOVE TRANS-DATE TO LP-TRDATE.
           MOVE LN-INTDATE TO LP-PAYDATE.

           MOVE LN-SSNO(1) TO LP-SSNO.
           MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE.

           PERFORM CLEAR-LXG-FILE.

           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.

      *******************************
       SETUP-LONPF-BUFFER SECTION.
      *******************************
      * GET THE PRIOR LOAN TO PASS INTO LONPFA
           PERFORM OPEN-LN1-FILE.
           MOVE LN-PRLNNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO UPDATE-FG
              GO TO EXIT-SETUP-LONPF-BUFFER.

           MOVE 0 TO ENTRY-FLAG BYPASS-AMT-ENT-FG.
           MOVE "F" TO FULL-DISPLAY.
           MOVE " " TO REV-REBATE FULL-PAGE-FG ENTER-AMT-FG
                       HOLD-DISPLAY-ERN
                       HOLD1-KEY-DOWN HOLD2-KEY-UP.

           MOVE "RESCI" TO LP-REFNO.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           MOVE SP1-KEY TO HOLD-SP1-KEY.

           MOVE 0 TO HOLD-PARTIALS.
           MOVE 0 TO HOLD-LCPAID.
           MOVE 0 TO HOLD-DLPARVHELD-ADJ.
           MOVE 0 TO HOLD-AFPDLLP.
           MOVE 0 TO HOLD-OVPAID-CHECK.
           MOVE 0 TO HOLD-REPO-AUCTION-EXP.

           MOVE LN-TOTINT TO HOLD-LN-TOTINT.
           MOVE LN-TOTERN TO HOLD-LN-TOTERN.
           MOVE LN-TOTDEF TO HOLD-LN-TOTDFMNTS.
           MOVE LN-TOTLCHG TO HOLD-LN-TOTLCHG.
           MOVE LN-TOTSERACCT TO HOLD-LN-TOTSERACCT.
           MOVE LN-JDRATE TO HOLD-LN-JDRATE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              MOVE 0 TO REB-AMOUNT(SUB)
              IF SUB < 7
                 MOVE LN-ACCERN(SUB) TO HOLD-LN-ACCERN(SUB)
              END-IF
              IF SUB < 9
                 MOVE " " TO INS-RB-TBL(SUB)
              END-IF
           END-PERFORM.

       FINISH-SETUP-LONPF.

           MOVE HOLD-LN-SRCD TO HOLD-BP-TRCD LP-TRCD HOLD-POCD.

       EXIT-SETUP-LONPF-BUFFER.

      ***********************************************
       SET-COLLECTOR-INFO SECTION.
      ***********************************************
           MOVE SPACES             TO EXT-WI-CL-WK-PATH.
           MOVE TRANS-DATE         TO EXT-WI-CL-TRANS-DATE.
           MOVE LN-OWNBR           TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO          TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER      TO EXT-WI-CL-APP-NUMBER.
           MOVE 1                  TO EXT-WI-CL-SSNO-NO.
           MOVE LN-SSNO(1)         TO EXT-WI-CL-SSNO(1).
           MOVE LN-SSNO(2)         TO EXT-WI-CL-SSNO(2).
           MOVE LN-SSNO(3)         TO EXT-WI-CL-SSNO(3).
           MOVE LN-SSNO(4)         TO EXT-WI-CL-SSNO(4).
           MOVE LN-MAKERCD(1)      TO EXT-WI-CL-MAKERCD(1).
           MOVE LN-MAKERCD(2)      TO EXT-WI-CL-MAKERCD(2).
           MOVE LN-MAKERCD(3)      TO EXT-WI-CL-MAKERCD(3).
           MOVE LN-MAKERCD(4)      TO EXT-WI-CL-MAKERCD(4).
      * SET OF (WI-CL-SSNO-NO) DO DONE IN 'DISPLAY-LOAN':
           MOVE LN-DLNO            TO EXT-WI-CL-DLNO.
           MOVE LN-SALEFIN         TO EXT-WI-CL-SALEFIN.
           MOVE LN-PAY-SCHLD-FG    TO EXT-WI-CL-PAY-SCHLD-FG.
           MOVE EXT-CONAME-SYSDATE TO EXT-WI-CL-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO EXT-WI-CL-PASSWD.

      /*****************************************************************
      *
      *     C O P Y   L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPUPWK.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE LPLOAN-SYSDATE TO LONPQ1-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPQ1-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPQ1-FORM-FIELDS TO WR-BUF.
           MOVE LONPQ1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE LONPQ1-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPQ1-HELP--DIR TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPQ1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SET-DELINQUENCY.
           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 9
              MOVE " INVALID CODE" TO CD-DESC.
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE " INVALID DEALER" TO DL-SHORT.
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       GET-COLLATERAL.
           PERFORM OPEN-LTC1-FILE.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG = 9
              MOVE SPACES TO LTC-COLLATERAL-INFO.
           PERFORM CLOSE-LTC1-FILE.
       SET-DS1-KEY.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
           MOVE LN-OWNBR    TO DS-BRANCH.
           MOVE LN-DLNO     TO DS-DLNO.
           MOVE LN-CLASS TO DS-CLASS.
       SET-TY-KEY.
           MOVE LN-OWNBR   TO TY-BRNO.
           MOVE TRANS-DATE TO TY-DATE.
           MOVE LN-CLASS   TO TY-CLASS.
       SEND-CD-DESC.
           PERFORM GET-CD.
           MOVE "S  A000  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       ADD-IT2.
           PERFORM ADD-IT.
           MOVE "/" TO ADD-ON.
           PERFORM ADD-IT.
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF NUM-BW NOT = 0
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       SEND-MEMO.
           MOVE "S  A060M1." TO VDU.
           MOVE "MEMO: " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300M2." TO VDU.
           MOVE LN-MESSAGE TO VDUBUF.
           PERFORM SCREENIO.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       MSG-LINE.
           PERFORM CALL-TMMSG.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       RESET-LEGEND.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *********************************
      *      F I L E   I / O          *
      *********************************
       COPY "LIBLP/LPDSCL.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPTRLCL.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPLXGCL.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPRXGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPESGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPES1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBLP/LPTRL1IN.CPY".
       COPY "LIBLP/LPRX1IN.CPY".
       COPY "LIBLP/LPCX1IN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPWKI.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPQ1-SCREEN.
       STOP-RUN.
           EXIT PROGRAM.
